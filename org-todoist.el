;;; org-todoist.el --- Syncs Todoist tasks to org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Austin Kearns
;;
;; Author: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Maintainer: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Created: September 15, 2024
;; Modified: September 15, 2024
;; Version: 0.0.2
;; Keywords: calendar org todoist
;; Homepage: https://github.com/lillenne/org-todoist
;; Package-Requires: ((emacs "29.1") (s "1.13.1") (org "9.4") (ts "0.3") (dash "2.19.1") (json "1.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Syncs Todoist tasks to org mode
;;
;;; Code:

(require 'url)
(require 's)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 'ts)
(require 'dash)
(require 'json)
(require 'outline)
(require 'xdg)

                                        ;Config;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO likely don't want to override user configuration. Todoist has 4 priorities.
(setq org-priority-highest ?A
      org-priority-default ?D
      org-priority-lowest ?D)
;; (defvar url-http-end-of-headers nil) ;; defined in url, but emacs provides warning about free var when using
(defvar org-todoist-api-token nil "The API token to use to sync with Todoist.")
(defvar org-todoist-delete-remote-items t
  "Delete remote items on Todoist when deleted from the `'org-todoist-file'.
WARNING items archived to sibling files will be detected as deleted!")
(defvar org-todoist-file "todoist.org" "The name of the todoist org file.
If relative, it is taken as relative to the org directory.")
(defvar org-todoist-user-headline "Collaborators" "The name of the root collaborators metadata node.")
(defvar org-todoist-project-headline "Projects" "The name of the project root node.")
(defvar org-todoist-tz nil "The timezone to use when converting from Todoist time (UTC). Currently this is ignored in favor of `'current-time'.")
(defvar org-todoist-lang "en")
(defvar org-todoist-use-auto-reminder t)
(defvar org-todoist-show-n-levels -1
  "The number of headline levels to show by default.
If visiting the todoist buffer when sync is called, it will attempt to respect the
visibility of the item at pos.
2=projects,
3=sections,
4=root tasks,
5=root tasks + 1 level of subtasks
<0 no fold")
(defvar org-todoist-todo-keyword "TODO" "TODO keyword for active Todoist tasks.")
(defvar org-todoist-done-keyword "DONE" "TODO keyword for completed Todoist tasks.")
(defvar org-todoist-deleted-keyword "CANCELED" "TODO keyword for deleted Todoist tasks.")

(defvar org-todoist-storage-dir (concat (file-name-as-directory (xdg-cache-home)) "org-todoist")
  "The directory for org-todoist storage.

If using multiple computers and a synced file solution,
this directory must be accessible on all PCs")
(defun org-todoist-file () (expand-file-name org-todoist-file org-directory))

                                        ;Constants;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst org-todoist-resource-types '("projects" "notes" "labels" "items" "sections" "collaborators") "The list of resource types to sync.")
(defconst org-todoist-sync-endpoint "https://api.todoist.com/sync/v9/sync" "The todoist sync endpoint.")
(defconst org-todoist-request-type "application/x-www-form-urlencoded; charset=utf-8" "The request type for Todoist sync requests.")
(defconst org-todoist-http-method "POST" "The http method for Todoist sync requests.")
(defconst org-todoist--type "TODOIST_TYPE" "The org property name for Todoist object type metadata.")
(defconst org-todoist--collaborator-type "USER" "The org-todoist--type value for collaborator objects.")
(defconst org-todoist--section-type "SECTION" "The org-todoist--type value for section objects.")
(defconst org-todoist--project-type "PROJECT" "The org-todoist--type value for project objects.")
(defconst org-todoist--default-id "default" "The org-todoist--type value for the default section.")
(defconst org-todoist--id-property "tid" "The org-todoist--type value for the default section.")
(defconst org-todoist--task-type "TASK" "The org-todoist--type value for task objects.")
(defconst org-todoist--user-node-type "USER_HEADLINE" "The org-todoist--type value for the collaborator metadata node.")
(defconst org-todoist--default-section-name "Default Section")
(defconst org-todoist--project-node-type "PROJECT_HEADLINE" "The org-todoist--type value for the project root node.")
(defconst org-todoist--sync-areas ["collaborators", "projects", "items", "sections"] "The types of Todoist items to sync.")

(defconst org-todoist--project-skip-list '(name can_assign_tasks color is_deleted is_favorite is_frozen sync_id v2_id v2_parent_id view_style))
(defconst org-todoist--section-skip-list '(name sync_id updated_at is_deleted v2_id v2_project_id archived_at))
(defconst org-todoist--task-skip-list '(name completed_at is_deleted content duration description checked deadline due labels priority project_id section_id sync_id v2_id v2_parent_id v2_project_id v2_section_id completed_at content day_order))
(defconst org-todoist--sync-token-file "SYNC-TOKEN")
(defconst org-todoist--sync-buffer-file "SYNC-BUFFER")

                                        ;Hooks;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--item-close-hook ()
  (when (and (equal org-state org-todoist-done-keyword)
             (org-todoist--task-is-recurring (org-element-resolve-deferred (org-element-at-point))))
    ;; TODO send only item complete command. Remove item_update for recurring tasks?
    ;; (org-todoist-sync nil)
    ))
(add-hook 'org-after-todo-state-change-hook 'org-todoist--item-close-hook) ;; TODO this requires it to be done from emacs and not eg. orgzly

(add-to-list 'org-fold-show-context-detail '(todoist . lineage))

(defun org-todoist--sync-after-capture ()
  (unless org-note-abort
    (save-excursion
      (save-window-excursion
        (let ((inhibit-message t))
          (org-capture-goto-last-stored))
        (when (string= (buffer-file-name) (org-todoist-file))
          (org-todoist-sync t))))))

(add-hook 'org-capture-after-finalize-hook #'org-todoist--sync-after-capture)

                                        ;Data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar org-todoist--sync-err nil "The error from the last sync, if any.")
(defvar org-todoist-log-last-request t)
(defvar org-todoist--last-request nil "The last request body, if any and org-todoist-log-last-request is non-nil.")
(defvar org-todoist-log-last-response t)
(defvar org-todoist--last-response nil "The last parsed response, if any and org-todoist-log-last-response is non-nil.")

(defun org-todoist--set-last-response (JSON)
  (with-temp-file (org-todoist--storage-file "PREVIOUS.json")
    (insert JSON)))

(defun org-todoist--storage-file (FILE) (expand-file-name FILE org-todoist-storage-dir))

(defun org-todoist--set-sync-token (TOKEN)
  (let ((file (org-todoist--storage-file org-todoist--sync-token-file)))
    (with-temp-file file
      (when (file-exists-p file)
        (insert-file-contents file))
      (insert TOKEN)
      (insert "\n"))))

(defun org-todoist--get-sync-token ()
  (if (file-exists-p (org-todoist--storage-file org-todoist--sync-token-file))
      (let ((res (with-temp-buffer
                   (insert-file-contents (org-todoist--storage-file org-todoist--sync-token-file))
                   (goto-char (point-max))
                   (forward-line -1)
                   (thing-at-point 'line t))))
        (if res res "*"))
    "*"))

(defun org-todoist--set-last-sync-buffer (AST)
  (with-temp-file (org-todoist--storage-file org-todoist--sync-buffer-file)
    (org-mode)
    (insert (org-todoist-org-element-to-string AST))))

(defun org-todoist--get-last-sync-buffer-ast ()
  (let ((file (org-todoist--storage-file org-todoist--sync-buffer-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-element-parse-buffer)))))


(defun org-todoist--get-comments (HEADLINE)
  (org-element-map HEADLINE 'item (lambda (item) (when (and (eq (org-todoist--first-parent-of-type item 'headline) HEADLINE)
                                                            (org-todoist--is-note item))
                                                   item))))

(defun org-todoist--is-note (ITEM)
  (s-contains? "Note" (org-todoist--item-text ITEM)))

(defun org-todoist--item-text (ITEM)
  (org-todoist-org-element-to-string ITEM))

(defun org-todoist--note-text (ITEM)
  (let ((text (org-todoist--item-text ITEM)))
    (when (s-contains? "Note taken on [" text) (s-trim-right (s-join "\n" (--map (s-trim it) (cdr (s-lines text))))))))

(defun org-todoist--get-comments-text (HEADLINE)
  (--map (org-todoist--note-text it) (org-todoist--get-comments HEADLINE)))

                                        ;API Requests;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--select-user (TEXT)
  (if-let* ((ast (org-todoist--file-ast)) ;; TODO cache?
            (userelements (org-element-map (org-todoist--user-node ast) 'headline
                            (lambda (node) (when (org-todoist--node-type-query node org-todoist--collaborator-type) node))))
            (usernames (--map (org-element-property :raw-value it) userelements))
            (selected (completing-read TEXT usernames))
            (selectedelement (--first (string= (org-element-property :raw-value it) selected) userelements)))
      selectedelement))

(defun org-todoist--all-users ()
  (org-element-map (org-todoist--user-node (org-todoist--file-ast)) 'headline #'identity))

(defun org-todoist--encode (DATA)
  (mapconcat
   (lambda (data)
     (if (null (cdr data)) "[]"
       (concat (car data) "="
               (if (listp (cdr data))
                   (json-encode (cdr data))
	         (if (stringp (cdr data))
                     (cdr data)
                   (prin1-to-string (cdr data)))))))
   (-filter (lambda (item) (cdr item)) DATA)
   "&"))

(defun org-todoist--encode-item (ITEM)
  (if (json-alist-p (cdr ITEM))
      (concat (car ITEM)
              (org-todoist--encode-item ITEM))
    (json-encode ITEM)))

(defun org-todoist--do-sync (TOKEN OPEN)
  (when (or (null org-todoist-api-token) (not (stringp org-todoist-api-token)))
    (error "No org-todoist-api-token API token set"))
  (setq org-todoist--sync-err nil)
  (let* ((cur-headline (if (equal (buffer-name (current-buffer)) org-todoist-file)
                           (org-entry-get (point) "ITEM")
                         nil))
         (show-n-levels (when cur-headline (org-todoist--get-hl-level-at-point)))
         (url-request-method org-todoist-http-method)
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " org-todoist-api-token))
                                      ("Content-Type" . ,org-todoist-request-type)))
         (ast (org-todoist--file-ast))
         (old (org-todoist--get-last-sync-buffer-ast))
         (request-data `(("sync_token" . ,TOKEN)
                         ("resource_types" . ,(json-encode org-todoist-resource-types))
                         ("commands" . , (org-todoist--push ast old))))
         (url-request-data (org-todoist--encode request-data)))
    (when org-todoist-log-last-request
      (setq org-todoist--last-request url-request-data))
    (message (if OPEN "Syncing with todoist. Buffer will open when sync is complete..." "Syncing with todoist. This may take a moment..."))
    (url-retrieve org-todoist-sync-endpoint
                  (lambda (events open ast old cur-headline show-n-levels)
                    (if (plist-member events :error)
                        (progn (setq org-todoist--sync-err events)
                               (message "Sync failed. See org-todoist--sync-err for details."))
                      (let ((resp (progn (goto-char url-http-end-of-headers)
                                         (decode-coding-region (point) (point-max) 'utf-8 t))))
                        (when org-todoist-log-last-response
                          (org-todoist--set-last-response resp)
                          (setq org-todoist--last-response (json-read-from-string resp))))
                      (org-todoist--parse-response org-todoist--last-response ast old)
                      (org-todoist--handle-display cur-headline show-n-levels))
                    (message "Sync complete."))
                  `(,OPEN ,ast ,old ,cur-headline ,show-n-levels)
                  'silent
                  'inhibit-cookies)))

(defun org-todoist--handle-display (cur-headline show-n-levels)
  (unless show-n-levels (setq show-n-levels org-todoist-show-n-levels))
  (find-file (org-todoist-file))
  (save-buffer)
  (if (< show-n-levels 1)
      (org-content)
    (org-content show-n-levels))
  (org-fold-hide-drawer-all)
  (let ((pos nil)
        (inhibit-message t))
    (org-map-entries (lambda ()
                       (when (equal (org-entry-get nil "ITEM") cur-headline)
                         (setq pos (point)))
                       (when (equal (org-entry-get nil "collapsed") "t")
                         (+org/close-fold))
                       (when (equal (org-entry-get nil "is_archived") "t")
                         (org-archive-subtree-default))
                       (org-delete-property "collapsed")
                       (org-delete-property "is_archived")))
    (when cur-headline
      (progn (when pos (goto-char pos))
             (org-fold-show-context 'todoist)
             ;; (+org/open-fold)
             )))
  (recenter))

(defun org-todoist--first-parent-of-type (NODE TYPES)
  "Gets the first parent which is one of the given TYPES.
TYPES can be a single symbol or a list of symbols."
  (org-element-lineage-map NODE #'identity TYPES nil t))

(defun org-todoist--get-planning-date (NODE KEYWORD)
  (let ((prop (org-element-property KEYWORD NODE)))
    (when prop (org-todoist--date-to-todoist prop))))
;; (org-todoist--date-to-todoist (org-element-property KEYWORD (org-todoist--get-planning NODE)))) ;; Can just grab from the headline. May not have planning

(defun org-todoist--get-planning (NODE)
  "Gets the planning element for specified headline"
  (unless (eq (org-element-type NODE) 'headline) (error "org-todoist--get--planning only supports headline input nodes"))
  (car (org-element-map NODE 'planning (lambda (planning)
                                         (when (eq NODE
                                                   (org-todoist--first-parent-of-type planning 'headline))
                                           planning)))))

(defun org-todoist--date-to-todoist (TIMESTAMP)
  (let ((month (org-element-property :month-start TIMESTAMP))
        (day (org-element-property :day-start TIMESTAMP)))
    (concat
     (number-to-string (org-element-property :year-start TIMESTAMP))
     "-"
     (when (< month 10) "0")
     (number-to-string month)
     "-"
     (when (< day 10) "0")
     (number-to-string day)
     (when (org-element-property :hour-start TIMESTAMP) ;; has time component
       (let ((hour (org-element-property :hour-start TIMESTAMP))
             (minute (org-element-property :minute-start TIMESTAMP)))
         (concat "T"
                 (when (> 10 hour) "0") ;; pad to 2 char with 0
                 (number-to-string hour)
                 ":"
                 (when (> 10 minute) "0") ;; pad to 2 char with 0
                 (number-to-string minute)
                 ":00.0"))))))

(defun org-todoist--todoist-date-object-for-kw (NODE KEYWORD)
  (let ((dl (org-todoist--get-planning-date NODE KEYWORD)))
    (when dl
      (let ((res `(("date" . ,dl)
                   ("lang" . ,org-todoist-lang)
                   ;; ("string" . ,dl)
                   ;; ("timezone" . ,(if org-todoist-tz org-todoist-tz (cadr (current-time-zone))))
                   ))
            (is-recurring (org-todoist--task-is-recurring NODE)))
        ;; (when (and (eq KEYWORD :scheduled) is-recurring)
        (when is-recurring
          (push `("is_recurring" . ,t) res)
          (push `("string" . ,(org-todoist--repeater-to-string (org-element-property KEYWORD NODE))) res))
        ;; (when org-todoist-tz (push `("timezone" . ,org-todoist-tz) res))
        res))))

(defun org-todoist--repeater-to-string (TIMESTAMP)
  (let ((value (org-element-property :repeater-value TIMESTAMP))
        (unit (org-element-property :repeater-unit TIMESTAMP)))
    (when (and value unit)
      (concat "every " (number-to-string value) " " (symbol-name unit)))))

(defun org-todoist--add-repeater (TIMESTAMP STRING)
  ;; TODO more repeater cases https://todoist.com/help/articles/introduction-to-recurring-due-dates-YUYVJJAV
  (let ((match (s-match ".*\\([0-9]*\\) \\([week|day|month|hour]+\\)" STRING)))
    (when match
      (org-element-put-property TIMESTAMP :repeater-type (if (s-contains? "!" STRING) 'restart 'cumulate))
      (org-element-put-property TIMESTAMP :repeater-unit (intern (nth 2 match)))
      (org-element-put-property TIMESTAMP :repeater-value (if (string= (cadr match) "") 1 (string-to-number (cadr match)))))))

(defun org-todoist--task-is-recurring (TASK)
  (not (null (org-element-property :repeater-type (org-element-property :scheduled TASK)))))

(defun org-todoist--log-drawer (HL)
  (org-element-map HL 'drawer (lambda (drawer)
                                ;; make sure the arg HL is the direct parent headline of the drawer
                                (when (and (string= (org-element-property :drawer-name drawer) "LOGBOOK")
                                           (eq (org-element-lineage-map drawer #'identity 'headline nil t) HL))
                                  drawer))
                   nil t))

(defun org-todoist--log-drawer-add-note (HL TEXT DATE)
  (let ((drawer (org-todoist--log-drawer HL))
        (note (org-element-create 'item '(:bullet "-" :pre-blank 0)
                                  (org-element-create 'plain-text nil (concat "Note taken on "
                                                                              (org-todoist-org-element-to-string (org-todoist--get-ts-from-date DATE t))
                                                                              ;; (org-todoist-org-element-to-string (org-timestamp-from-time (current-time) t t))
                                                                              " \\\\\n"
                                                                              TEXT)))))
    (if drawer
        (if-let ((children (org-element-contents drawer)))
            (org-element-insert-before note (car children)) ;; TODO this adds as first note in org, per standard org (todoist is the invers)
          (org-element-adopt drawer note))
      (setq drawer (org-element-create 'drawer '(:drawer-name "LOGBOOK") note))
      (org-todoist--adopt-drawer HL drawer)))
  HL)

(defun org-todoist--first-direct-descendent-of-type (NODE TYPE)
  (let ((ptype (org-element-type NODE)))
    (org-element-map (org-element-contents NODE) TYPE
      (lambda (node) (when (eq NODE (org-todoist--first-parent-of-type node ptype))
                       node))
      nil t)))

(defun org-todoist--adopt-drawer (HL DRAWER)
  "Adopts a DRAWER in the correct location under HL.
If the new drawer isn't added by the other drawers, it may get pushed under
the wrong headline!"
  ;; Check for a description to insert before
  (if (eq 'property-drawer (org-element-type DRAWER))
      ;; NOTE Having other drawers before property drawer makes property drawers parse as regular drawers. Avoid this.
      (if-let ((first-drawer (org-todoist--first-direct-descendent-of-type HL 'drawer)))
          (org-element-insert-before DRAWER first-drawer)
        (org-todoist--adopt-drawer-regular HL DRAWER))
    (org-todoist--adopt-drawer-regular HL DRAWER)))

(defun org-todoist--adopt-drawer-regular (HL DRAWER)
  (let ((description (org-todoist--get-description-elements HL)))
    (if description
        ;; Has a description, insert before that (end of drawers)
        (org-element-insert-before DRAWER (car description))
      (let ((child-hl (org-element-map (org-element-contents HL) 'headline #'identity nil t)))
        (if child-hl
            ;; Has a child headline, insert before that
            (org-element-insert-before DRAWER child-hl)
          ;; Doesn't have child headlines or description. Safe to adopt at end of children
          (org-element-adopt HL DRAWER))))))

(defun org-todoist--last-recurring-task-completion (TASK)
  (when (org-todoist--task-is-recurring TASK)
    (org-timestamp-from-string (org-element-property :LAST_REPEAT TASK))))

(defun org-todoist--last-recurring-task-completion-as-doist (TASK)
  (org-todoist--date-to-todoist (org-todoist--last-recurring-task-completion TASK)))

(defun org-todoist--push-test ()
  (org-todoist--push (org-todoist--file-ast) (org-todoist--get-last-sync-buffer-ast)))

(defun org-todoist--timestamp-times-equal (T1 T2)
  (and (equal (org-element-property :year-start T1) (org-element-property :year-start T2))
       (equal (org-element-property :month-start T1) (org-element-property :month-start T2))
       (equal (org-element-property :day-start T1) (org-element-property :day-start T2))
       (equal (org-element-property :hour-start T1) (org-element-property :hour-start T2))
       (equal (org-element-property :minute-start T1) (org-element-property :minute-start T2))))

(defun org-todoist--get-labels (TAGS)
  (--filter (string= org-archive-tag it) TAGS))

(defun org-todoist--get-section-id-position-non-default (HL)
  "Use this when pushing updates (we don't want to send id=default) to Todoist."
  (let ((res (org-todoist--get-section-id-position HL)))
    (unless (string= res org-todoist--default-id)
      res)))

(defun org-todoist--push (ast old)
  (let ((commands nil))
    (org-element-map ast 'headline
      (lambda (hl)
        ;; TODO would be easier as oop, need to learn lisp oop
        (let* ((type (org-todoist--get-todoist-type hl))
               (id (org-todoist--get-prop hl org-todoist--id-property))
               (hastid (null id))
               (todo-type (org-element-property :todo-type hl))
               (todo-kw (org-element-property :todo-keyword hl))
               (title (org-element-property :raw-value hl))
               (desc (org-todoist--description-text hl))
               (sch (org-element-property :scheduled hl))
               (pri (org-element-property :priority hl))
               (dead (org-element-property :deadline hl))
               (effstr (org-todoist--get-prop hl "EFFORT"))
               (eff (when effstr (org-duration-to-minutes effstr)))
               (tags (org-element-property :tags hl))
               (isarchived (member org-archive-tag tags))
               (labels tags)
               ;; (labels (org-todoist--get-labels tags))
               (proj (org-todoist--get-project-id-position hl))
               (section (org-todoist--get-section-id-position-non-default hl))
               (parenttask (org-todoist--get-task-id-position hl))
               (rid (org-element-property :RESPONSIBLE_UID hl))
               (is-recurring (org-todoist--task-is-recurring hl))
               (comments (org-todoist--get-comments-text hl))
               (lr (org-element-property :LAST_REPEAT hl))
               (last-repeat (when (and lr is-recurring) (org-timestamp-from-string lr)))
               (oldtask (org-todoist--get-by-id nil id old))
               (oldsection (org-todoist--get-section-id-position-non-default oldtask))
               (oldparenttask (org-todoist--get-task-id-position oldtask))
               (oldproj (org-todoist--get-project-id-position oldtask))
               (old-todo-type (org-element-property :todo-type oldtask))
               (old-todo-kw (org-element-property :todo-keyword oldtask))
               (oldtitle (org-element-property :raw-value oldtask))
               (olddesc (org-todoist--description-text oldtask))
               (oldpri (org-element-property :priority oldtask))
               (oldsch (org-element-property :scheduled oldtask))
               (olddead (org-element-property :deadline oldtask))
               (oldeffstr (org-todoist--get-prop oldtask "EFFORT"))
               (oldeff (when oldeffstr (org-duration-to-minutes oldeffstr)))
               (oldtags (org-element-property :tags oldtask))
               (oldlabels oldtags)
               ;; (oldlabels (org-todoist--get-labels oldtags))
               (oldisarchived (member org-archive-tag oldtags))
               (oldrid (org-element-property :RESPONSIBLE_UID oldtask))
               (oldcomments (org-todoist--get-comments-text oldtask))
               )

          ;; new object. Create temp id
          (unless id
            (setq id (org-id-uuid))
            (org-todoist--add-prop hl "temp_id" id))

          (cond ((string-equal type org-todoist--task-type)
                 (if hastid
                     (progn
                       ;; item_add. No ID -> new item
                       (org-todoist--insert-identifier hl org-todoist--task-type)
                       (push `(("uuid" . ,(org-id-uuid))
                               ("temp_id" . ,id)
                               ("type" . "item_add")
                               ("args" . (("content" . ,title)
                                          ("description" . ,(when desc desc))
                                          ("duration" . ,(when eff `(("amount" . ,eff) ("unit" . "minute"))))
                                          ("due" . ,(org-todoist--todoist-date-object-for-kw hl :scheduled))
                                          ("deadline" . ,(org-todoist--todoist-date-object-for-kw hl :deadline))
                                          ("priority" . ,(org-todoist--get-priority hl))
                                          ("labels" . ,labels)
                                          ("auto_reminder" . ,org-todoist-use-auto-reminder)
                                          ("parent_id" . ,(org-todoist--get-task-id-position hl))
                                          ("section_id" . ,section)
                                          ("project_id" . ,(org-todoist--get-project-id-position hl))
                                          ("responsible_uid" . ,rid))))
                             commands)
                       (when comments
                         ;; Add comments
                         (dolist (comment comments)
                           (push `(("uuid" . ,(org-id-uuid)) ;; command uuid
                                   ("type" . "note_add")
                                   ("temp_id". ,(org-id-uuid)) ;; note uuid
                                   ("args" . (("item_id" . ,id) ;; task uuid/tempid
                                              ("content" . ,comment))))
                                 commands))))
                   ;; when is somewhat redundant, as oldtask should not be null if there is a tid (new item)
                   (when oldtask
                     (unless (equal comments oldcomments)
                       ;; TODO support comment editing. This will push any edited comments as new comments
                       (dolist (comment (--filter (not (member it oldcomments)) comments))
                         (push `(("uuid" . ,(org-id-uuid)) ;; command uuid
                                 ("type" . "note_add")
                                 ("temp_id". ,(org-id-uuid)) ;; note uuid
                                 ("args" . (("item_id" . ,id) ;; task uuid/tempid
                                            ("content" . ,comment))))
                               commands)))

                     ;; item_move. Only one parameter can be specified
                     (unless (and (string= section oldsection)
                                  (string= proj oldproj)
                                  (string= parenttask oldparenttask))
                       ;; TODO subtasks need to only follow their parent tasks, ignore section / project changes
                       (cond ((and parenttask (not (string= parenttask oldparenttask)))
                              ;; item_move - parent task
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("parent_id" . ,parenttask))))
                                    commands))

                             ;; special case: move to unsectioned in another project
                             ((and (or (null section) (string= "" section))
                                   (not (string= proj oldproj)))
                              (unless parenttask
                                (push `(("uuid" . ,(org-id-uuid))
                                        ("type" . "item_move")
                                        ("args" . (("id" . ,id)
                                                   ("project_id" . ,proj))))
                                      commands)))

                             ;; move to another section (may be another project)
                             ((not (string= section oldsection))
                              ;; item_move - section
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("section_id" . ,section))))
                                    commands))


                             ((not (string= proj oldproj))
                              (unless (string= section oldsection) ;; whole section moved, ignore
                                ;;item_move - project
                                (push `(("uuid" . ,(org-id-uuid))
                                        ("type" . "item_move")
                                        ("args" . (("id" . ,id)
                                                   ("project_id" . ,proj))))
                                      commands)))))

                     ;; item_update
                     (when (or
                            (not (string-equal title oldtitle))
                            (not (equal desc olddesc))
                            (not (string-equal (org-element-property :raw-value sch) (org-element-property :raw-value oldsch)))
                            (not (string-equal (org-element-property :raw-value dead) (org-element-property :raw-value olddead)))
                            (not (equal eff oldeff))
                            (not (equal pri oldpri))
                            (not (equal labels oldlabels)) ;; TODO archive tag
                            (not (equal rid oldrid))
                            )
                       ;; TODO HERE compare last repeat to old scheduled date. If same, we completed a recurring task and need to call item close
                       ;; Doesn't work - last repeat logs when it was closed not the due date. Functionally the same except it doesn't log the completion on the todoist side, I think
                       ;; (when (org-todoist--timestamp-times-equal last-repeat oldsch) ;; completed recurring task
                       ;;   (push `(("uuid" . ,(org-id-uuid))
                       ;;           ("type" . "item_close")
                       ;;           ("args" . (("id" . ,id))))
                       ;;         commands))
                       (let ((req `(("type" . "item_update")
                                    ("uuid" . ,(org-id-uuid))
                                    ("args" . (("id" . ,id)
                                               ("content" . ,title)
                                               ("description" . ,(when desc desc))
                                               ("duration" . ,(when eff `(("amount" . ,eff) ("unit" . "minute"))))
                                               ("due" . ,(org-todoist--todoist-date-object-for-kw hl :scheduled))
                                               ("deadline" . ,(org-todoist--todoist-date-object-for-kw hl :deadline))
                                               ("priority" . ,(org-todoist--get-priority hl))
                                               ("labels" . ,labels)
                                               ("responsible_uid" . ,rid))))))
                         (push req commands)))

                     ;; todo-state changed
                     ;; TODO recurring task support
                     (when (not (equal todo-type old-todo-type))
                       (if (eq 'done todo-type)
                           (if (string= org-todoist-deleted-keyword todo-kw)
                               (push `(("uuid" . ,(org-id-uuid))
                                       ("type" . "item_delete")
                                       ("args" . (("id" . ,id))))
                                     commands)
                             (if is-recurring
                                 (push `(("uuid" . ,(org-id-uuid)) ;; TODO this doesn't work since org auto-reopens with new date. The task will instead be updated with item_update
                                         ("type" . "item_close")
                                         ("args" . (("id" . ,id))))
                                       commands)
                               (push `(("uuid" . ,(org-id-uuid))
                                       ("type" . "item_complete")
                                       ("args" . (("id" . ,id)
                                                  ("date_completed" .
                                                   ,(org-todoist--timestamp-to-utc-str (org-element-property :closed hl))))))
                                     commands)))
                         (push `(("uuid" . ,(org-id-uuid))
                                 ("type" . "item_uncomplete")
                                 ("args" . (("id" . ,id))))
                               commands))))))
                ((and (not section) (string= type org-todoist--section-type) (not (string= org-todoist--default-section-name title)))
                 (cond ((not oldtask)
                        ;; new section
                        (org-todoist--insert-identifier hl org-todoist--section-type)
                        (push `(("uuid" . ,(org-id-uuid))
                                ("temp_id" . ,id)
                                ("type" . "section_add")
                                ("args" . (("name" . ,title)
                                           ("project_id" . ,proj))))
                              commands))
                       ((not (string= title oldtitle))
                        ;; update section
                        (push `(("uuid" . ,(org-id-uuid))
                                ("type" . "section_update")
                                ("args" . (("name" . ,title)
                                           ("id" . ,id))))
                              commands)
                        )
                       ((and oldtask (not (cl-equalp proj oldproj)))
                        ;; section_move
                        (push `(("uuid" . ,(org-id-uuid))
                                ("type" . "section_move")
                                ("args" . (("id" . ,id)
                                           ("project_id" . ,proj))))
                              commands))
                       ))
                ((and (string= type org-todoist--project-type) (not (string= title "Inbox")))
                 (cond ((not oldtask)
                        ;; new project
                        (org-todoist--insert-identifier hl org-todoist--project-type)
                        (push `(("uuid" . ,(org-id-uuid))
                                ("temp_id" . ,id)
                                ("type" . "project_add")
                                ("args" . (("name" . ,title))))
                              commands))
                       ((not (string= title (org-element-property :raw-value oldtask)))
                        ;; update project
                        (push `(("uuid" . ,(org-id-uuid))
                                ("type" . "project_update")
                                ("args" . (("name" . ,title)
                                           ("id" . ,id))))
                              commands)
                        )))))))
    (when org-todoist-delete-remote-items
      (org-element-map
          old
          'headline
        (lambda (hl)
          (let ((id (org-todoist--get-prop hl org-todoist--id-property))
                (type (org-todoist--get-todoist-type hl)))
            (when id
              (let ((new (org-todoist--get-by-id type id ast)))
                (unless new
                  ;; item was deleted (or archived to another file...)
                  (cond
                   ((string= org-todoist--task-type type)
                    (push `(("uuid" . ,(org-id-uuid))
                            ("type" . "item_delete")
                            ("args" . (("id" . ,id))))
                          commands))
                   ((and (string= org-todoist--section-type type) (not (string= id org-todoist--default-id))) ;; Don't delete the default section
                    (push `(("uuid" . ,(org-id-uuid))
                            ("type" . "section_delete")
                            ("args" . (("id" . ,id))))
                          commands))
                   ((string= org-todoist--project-type type)
                    (push `(("uuid" . ,(org-id-uuid))
                            ("type" . "project_delete")
                            ("args" . (("id" . ,id))))
                          commands))))))))))
    (nreverse commands)))

(defun org-todoist--is-subtask (NODE)
  (org-todoist--get-parent-of-type org-todoist--task-type NODE t))

(defun org-todoist--is-project (NODE)
  (string= (org-todoist--get-todoist-type NODE) org-todoist--project-type))

(defun org-todoist--is-section (NODE)
  (string= (org-todoist--get-todoist-type NODE) org-todoist--section-type))

(defun org-todoist--is-task (NODE)
  (string= (org-todoist--get-todoist-type NODE) org-todoist--task-type))
;; (org-element-lineage-map NODE (lambda (parent) (string= org-todoist--section-type (org-todoist--get-todoist-type parent))) 'headline nil t))
;; (or (org-element-property :TODOIST_TYPE NODE) ;; causes issues when refiling
;;     (org-element-lineage-map NODE (lambda (parent) (string= org-todoist--section-type (org-todoist--get-todoist-type parent))) 'headline nil t)))

(defun org-todoist--parse-response (RESPONSE AST OLDAST)
  (let ((tasks (assoc-default 'items RESPONSE))
        (projects (assoc-default 'projects RESPONSE))
        (collab (assoc-default 'collaborators RESPONSE))
        (sections (assoc-default 'sections RESPONSE))
        (comments (assoc-default 'notes RESPONSE))
        (token (assoc-default 'sync_token RESPONSE))
        (tid_mapping (assoc-default 'temp_id_mapping RESPONSE)))
    (org-todoist--temp-id-mapping tid_mapping AST)
    (org-todoist--update-users collab AST)
    (org-todoist--update-projects projects AST)
    (org-todoist--update-sections sections AST)
    (org-todoist--update-tasks tasks AST)
    (org-todoist--update-comments comments AST)
    (org-todoist--set-sync-token token)
    (org-todoist--set-last-sync-buffer AST)
    (org-todoist--update-file AST)))

(defun org-todoist--temp-id-mapping (TID_MAPPING AST)
  (dolist (elem TID_MAPPING)
    (org-element-map AST 'headline
      (lambda (hl)
        (let* ((tid (car elem))
               (tidstr (if (symbolp tid) (symbol-name tid) tid)))
          (when (string= (org-todoist--get-prop hl "temp_id") tidstr)
            (org-todoist--insert-id hl (cdr elem)))))
      nil t)))

(defun org-todoist--update-comments (COMMENTS AST)
  (cl-loop for comment across COMMENTS do
           ;; TODO support comment IDs and add/update/delete
           (let* ((task (org-todoist--get-by-id org-todoist--task-type (assoc-default 'item_id comment) AST))
                  (comments (org-todoist--get-comments-text task))
                  (text (assoc-default 'content comment)))
             (unless (member text comments)
               (org-todoist--log-drawer-add-note task (assoc-default 'content comment) (assoc-default 'posted_at comment)))
             ;; TODO sort comments by time.
             )))

(defun org-todoist--insert-header ()
  ;; TODO use element api
  (goto-char (point-min))
  (insert "#+title: Todoist
#+STARTUP: logdone
#+STARTUP: logdrawer
"))


;; (section
;;  (:standard-properties
;;   [1 1 1 80 80 0 nil first-section nil nil nil 1 80 nil #<buffer todoist.org<2>> nil nil #0])
;;  (keyword
;;   (:standard-properties
;;    [1 1 nil nil 18 0 nil top-comment nil nil nil nil nil nil #<buffer todoist.org<2>> nil nil #1]
;;    :key "TITLE" :value "Todoist"))
;;  (keyword
;;   (:standard-properties
;;    [18 18 nil nil 40 0 nil nil nil nil nil nil nil nil #<buffer todoist.org<2>> nil nil #1]
;;    :key "FILETAGS" :value ":todoist:"))
;;  (keyword
;;   (:standard-properties
;;    [40 40 nil nil 59 0 nil nil nil nil nil nil nil nil #<buffer todoist.org<2>> nil nil #1]
;;    :key "STARTUP" :value "logdone"))
;;  (keyword
;;   (:standard-properties
;;    [59 59 nil nil 80 0 nil nil nil nil nil nil nil nil #<buffer todoist.org<2>> nil nil #1]
;;    :key "STARTUP" :value "logdrawer")))

(defun org-todoist--project-node (AST)
  "Gets or creates the main project headline in AST"
  (org-todoist--category-node-query-or-create AST org-todoist-project-headline org-todoist--project-node-type))

(defun org-todoist--update-projects (PROJECTS AST)
  (let ((projects (org-todoist--project-node AST)))
    ;; TODO test
    (cl-loop for proj across PROJECTS do
             (let ((node (org-todoist--get-or-create-node
                          projects
                          org-todoist--project-type
                          (assoc-default 'id proj)
                          (assoc-default 'name proj)
                          nil
                          proj
                          org-todoist--project-skip-list)))

               (when (eq t (assoc-default 'is_deleted proj))
                 (org-element-extract node))))
    (org-todoist--sort-by-child-order projects "child_order")
    (dolist (proj (org-todoist--project-nodes AST))
      (unless (member org-todoist--default-section-name (org-todoist--get-section-titles proj))
        (let ((default-section (org-todoist--create-node org-todoist--section-type org-todoist--default-section-name nil nil proj)))
          (org-todoist--add-prop default-section org-todoist--id-property org-todoist--default-id))))))

(defun org-todoist--get-sections (PROJECT)
  (org-element-map PROJECT 'headline (lambda (hl) (when (string= (org-todoist--get-todoist-type hl) org-todoist--section-type) hl))))

(defun org-todoist--get-title (NODE)
  (let ((title (org-element-property :title NODE)))
    (while (and title (not (eq 'string (type-of title))))
      (setq title (car title)))
    (substring-no-properties title)))

(defun org-todoist--get-section-titles (NODE)
  (--map (org-todoist--get-title it) (org-todoist--get-sections NODE)))

(defun org-todoist--user-node (AST)
  "Gets or creates the main user headline in AST"
  (org-todoist--category-node-query-or-create AST org-todoist-user-headline org-todoist--user-node-type))

(defun org-todoist--update-users (COLLAB AST)
  "Adds or updates all users"
  (let ((usernode (org-todoist--user-node AST)))
    (cl-loop for usr across COLLAB do
             (org-todoist--get-or-create-node usernode
                                              org-todoist--collaborator-type
                                              (assoc-default 'id usr)
                                              (assoc-default 'full_name usr)
                                              nil
                                              usr))))

(defun org-todoist--update-sections (SECTIONS AST)
  "Updates all sections"
  (cl-loop for sect across SECTIONS do
           (let ((targetprojid (assoc-default 'project_id sect)))
             (if-let* ((sectionid (assoc-default 'id sect))
                       (section (org-todoist--get-by-id org-todoist--section-type sectionid AST))
                       (project (org-todoist--get-parent-of-type org-todoist--project-type section t))
                       (projid (org-todoist--get-project-id-position section)))
                 ;; Section already exists
                 (if (eq t (assoc-default 'is_deleted sect))
                     ;; but it needs to be deleted
                     (org-element-extract section)
                   (if (cl-equalp projid targetprojid)
                       ;; Section is under the right project, add props only
                       (org-todoist--add-all-properties section sect org-todoist--section-skip-list)
                     ;; Move to the correct project
                     (org-element-extract section)
                     (org-element-adopt (org-todoist--get-by-id org-todoist--project-type targetprojid AST) section)))
               ;; Otherwise make the section
               (org-todoist--get-or-create-node
                (org-todoist--get-by-id org-todoist--project-type targetprojid AST)
                org-todoist--section-type
                (assoc-default 'id sect)
                (assoc-default 'name sect)
                nil
                sect
                org-todoist--section-skip-list))))
  (dolist (proj (org-todoist--project-nodes AST))
    (org-todoist--sort-by-child-order proj "section_order" org-todoist--section-type)
    )) ;; TODO add default section here? Otherwise might not be added to new projects without manually added default section?

(defun org-todoist--project-nodes (AST)
  "Gets all project nodes within the org-todoist file"
  (org-element-map (org-todoist--project-node AST) 'headline
    (lambda (hl) (when (string-equal (org-todoist--get-todoist-type hl) org-todoist--project-type) hl))))

(defun org-todoist--section-nodes (AST)
  "Gets all section nodes within the org-todoist file"
  (org-element-map (org-todoist--project-node AST) 'headline
    (lambda (hl) (when (string-equal (org-todoist--get-todoist-type hl) org-todoist--section-type) hl))))

(defun org-todoist--unsectioned-node (AST)
  (org-element-map (org-todoist--project-node AST) 'headline
    (lambda (hl) (when (and (string-equal (org-todoist--get-todoist-type hl) org-todoist--section-type)
                            (string-equal org-todoist--default-section-name (substring-no-properties (org-element-property :title hl))))
                   hl))))

(defun org-todoist--get-headline-level (NODE)
  "Gets the nearest headline level of NODE"
  (if-let (hl (org-element-lineage-map NODE (lambda (n) (when (org-element-type n 'headline) (org-element-property :level n))) nil t t))
      hl
    0))

(defun org-todoist--create-node (TYPE TEXT DESCRIPTION &optional PROPERTIES PARENT SKIP)
  "Creates node of TODOIST_TYPE TYPE with default TEXT and PROPERTIES under PARENT."
  (let ((node (org-element-create 'headline `(:title ,TEXT :level ,(+ 1 (org-todoist--get-headline-level PARENT))))))
    (org-todoist--insert-identifier node TYPE)
    (org-todoist--add-all-properties node PROPERTIES SKIP)
    (when DESCRIPTION (org-todoist--add-description node DESCRIPTION))
    (when PARENT (org-element-adopt PARENT node))
    node))

(defun org-todoist--add-description (NODE DESCRIPTION)
  (org-element-adopt NODE (org-element-create 'plain-text nil DESCRIPTION))) ;; plain-text type also works?

(defun org-todoist--replace-description (NODE DESCRIPTION)
  "Replaces the description of NODE with DESCRIPTION."
  (if DESCRIPTION
      (let ((comment (org-todoist--description-text NODE))
            (idx 0)) ;; Use idx to replace the first paragraph element
        (unless (string-equal-ignore-case comment DESCRIPTION)
          ;; Replace the description
          (dolist (paragraph (org-todoist--get-description-elements NODE))
            (if (eql idx 0)
                (org-element-set paragraph (org-element-create 'plain-text nil DESCRIPTION))
              (org-element-extract paragraph))
            (cl-incf idx))
          (when (eql idx 0) (org-todoist--add-description NODE DESCRIPTION)) ;; There was no description, add the new one
          DESCRIPTION))))

(defun org-todoist--get-or-create-node (PARENT TYPE ID TEXT DESCRIPTION PROPERTIES &optional SKIP SOURCE)
  "Gets or creates a new node of TODOIST_TYPE TYPE with ID, headline TEXT,
body DESCRIPTION, and drawer PROPERTIES, ignoring SKIP, under PARENT.

When SOURCE is specified, search for the node from there. Else search from PARENT"
  (if-let* ((found (org-todoist--get-by-id nil ID (if SOURCE SOURCE PARENT)))
            (updated (org-todoist--add-all-properties found PROPERTIES SKIP)))
      (progn
        (org-todoist--insert-identifier updated TYPE) ;; Probably not the best place for this. Newly created items may not have a type
        ;; (org-todoist--insert-id updated ID)
        (org-element-put-property updated :title TEXT)
        (org-todoist--replace-description updated DESCRIPTION)
        updated)
    (org-todoist--create-node TYPE TEXT DESCRIPTION PROPERTIES PARENT SKIP)))


(defun vector-to-list (vector)
  "Convert a VECTOR into a regular Lisp list using cl-loop."
  (cl-loop for element across vector collect element))

(defun org-todoist--closed-date (TASK)
  (let ((date (assoc-default 'completed_at TASK)))
    (org-todoist--get-ts-from-date date)))

(defun org-todoist--scheduled-date (TASK) (org-todoist--get-timestamp 'due TASK))
(defun org-todoist--deadline-date (TASK) (org-todoist--get-timestamp 'deadline TASK))

(defun org-todoist--get-ts-from-date (date &optional inactive)
  ;; TODO more efficient utc conversions
  (unless (null date)
    (let ((hastime (not (eql 10 (length date))))) ;; Todoist date format for tasks without a time is 10 char string
      (if (not (string= (substring date (- (length date) 1)) "Z"))
          (org-timestamp-from-time (org-read-date nil t date nil) hastime inactive)
        (org-todoist--timestamp-from-utc-str date hastime inactive)))))

(defun org-todoist--get-timestamp (SYMBOL TASK)
  (org-todoist--get-timestamp2 (assoc-default SYMBOL TASK)))

(defun org-todoist--get-timestamp2 (DATEOBJ)
  (when-let* ((date (assoc-default 'date DATEOBJ))
              (hasdate (org-todoist--has-date date))
              (ts (org-todoist--get-ts-from-date date)))
    (when (eq t (assoc-default 'is_recurring DATEOBJ))
      (org-todoist--add-repeater ts (assoc-default 'string DATEOBJ)))
    ts))

(defun org-todoist--timestamp-from-utc-str (STRING &optional WITH-TIME INACTIVE)
  "Creates an an org mode timestamp element from STRING, which is an RFC3339
datetime string. With arg WITH-TIME include the time-of-day portion in the
timestamp"
  (org-timestamp-from-string (concat (if INACTIVE "[" "<")
                                     (ts-format
                                      (if WITH-TIME "%Y-%m-%d %a %H:%M" "Y-%m-%d %a")
                                      (ts-adjust 'second (car (current-time-zone)) (ts-parse-org-element (org-timestamp-from-time (org-read-date nil t STRING nil) WITH-TIME))))
                                     (if INACTIVE "]" ">"))))

(defun org-todoist--timestamp-to-utc-str (TIMESTAMP)
  "Converts org timestamp element to utc RFC3339 string"
  (ts-format "%Y-%m-%dT%H:%M:%S.0Z"
             (ts-adjust 'second (- (car (current-time-zone))) (ts-parse-org-element TIMESTAMP))))

(defun org-todoist--has-date (DATE)
  (not (or (null DATE) (string-equal "" DATE) (string-equal "null" DATE))))

(defun org-todoist--create-planning (TASK)
  (let ((props nil)
        (sch (org-todoist--scheduled-date TASK))
        (dead (org-todoist--deadline-date TASK))
        (closed (org-todoist--closed-date TASK)))
    (when sch (setq props (plist-put props :scheduled sch)))
    (when dead (setq props (plist-put props :deadline dead)))
    (when closed (setq props (plist-put props :closed closed)))
    (when props (org-element-create 'planning props))))

(defun org-todoist--schedule (HEADLINE TASK)
  (let ((planning (org-todoist--create-planning TASK)))
    (if-let ((existing (org-element-map HEADLINE 'planning
                         (lambda (plan)
                           (when (eq (org-todoist--first-parent-of-type plan 'headline) HEADLINE) plan))
                         nil t)))
        (if planning
            (org-element-set existing planning)
          (org-element-extract existing))
      (when planning (org-element-insert-before planning (org-todoist--get-property-drawer HEADLINE))))))

(defun org-todoist--update-tasks (TASKS AST)
  (cl-loop for data across TASKS do
           (let* ((id (assoc-default 'id data))
                  (proj (org-todoist--get-by-id org-todoist--project-type (assoc-default 'project_id data) AST))
                  (section-id (assoc-default 'section_id data))
                  (section (org-todoist--get-by-id org-todoist--section-type (if section-id section-id org-todoist--default-id)
                                                   ;; search in project, since the section search will match the default section if there is no section
                                                   proj))
                  (title (assoc-default 'content data))
                  (description (assoc-default 'description data))
                  (task (org-todoist--get-or-create-node section org-todoist--task-type id title description data org-todoist--task-skip-list AST))
                  (tags (assoc-default 'labels data)))
             (if (eq t (assoc-default 'is_deleted data))
                 (org-element-extract task)

               ;; If unsectioned, add to the default section in that project
               (unless section (setq section (org-todoist--get-by-id org-todoist--section-type org-todoist--default-id proj)))

               ;; TODO not correctly moving sections
               ;; check if we need to move sections
               (unless (cl-equalp (org-todoist--get-section-id-position task) section-id)
                 (org-element-extract task)
                 (org-element-adopt section task))

               (dotimes (i (length tags))
                 (org-todoist--add-tag task (aref tags i)))
               (org-todoist--schedule task data)
               (org-todoist--set-effort task data)
               (org-todoist--set-priority task (assoc-default 'priority data))
               (org-todoist--set-todo task (assoc-default 'checked data) (assoc-default 'is_deleted data))))) ;; TODO checked vs is_archived?
  ;; second loop to set parent tasks after all have been created
  (cl-loop for data across TASKS do
           (when-let* ((parenttaskid (assoc-default 'parent_id data))
                       (task (org-todoist--get-by-id org-todoist--task-type (assoc-default 'id data) AST))
                       (parenttask (org-todoist--get-by-id org-todoist--task-type parenttaskid AST))
                       (wrong-spot (not (cl-equalp parenttaskid (org-todoist--get-task-id-position task)))))
             (org-element-adopt parenttask (org-element-put-property (org-element-extract task) :level (+ 1 (org-element-property :level parenttask)))))))

(defun org-todoist--list-headlines ()
  (let ((headlines nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (push (org-element-property :raw-value headline) headlines)))
    (nreverse headlines)))

(defun org-todoist--get-headline-by-raw-value (NODE VALUE)
  (org-element-map NODE 'headline (lambda (hl) (when (string-equal (org-element-property :raw-value hl) VALUE) hl))))

(defun org-todoist--get-node-attributes (NODE) (cadr NODE))
(defun org-todoist--get-node-attribute (NODE ATTRIBUTE)
  (plist-get (org-todoist--get-node-attributes NODE) (org-todoist--to-symbol ATTRIBUTE)))

;; ;; Not working as a new plist is returned. Needs to mutate
(defun org-todoist--put-node-attribute (NODE ATTRIBUTE VALUE)
  (setcar (cdr NODE) (plist-put (org-todoist--get-node-attributes NODE)
                                (org-todoist--to-symbol ATTRIBUTE)
                                VALUE)))

(defun org-todoist--to-symbol (SYMBOL-OR-STRING)
  (if (stringp SYMBOL-OR-STRING) (intern (concat ":" (upcase SYMBOL-OR-STRING))) SYMBOL-OR-STRING))

;; (defun org-todoist--get-headline-by-title-value (NODE VALUE)
;;   (org-element-map NODE 'headline (lambda (hl) (when (string-equal (org-element-property :raw-title hl) VALUE) hl))))

(defun org-todoist--get-position (NODE PROPERTY)
  (let ((val (org-todoist--get-prop NODE PROPERTY)))
    (if (stringp val) (string-to-number val) val)))

(defun org-todoist--sort-by-child-order (NODE PROPERTY &optional TYPE)
  (let* ((children (org-element-map NODE 'headline
                     (lambda (hl) (when (and (eq (org-todoist--first-parent-of-type hl 'headline) NODE)
                                             (org-todoist--get-prop hl PROPERTY) ;; only move children with the property
                                             (or (not TYPE)
                                                 (string= (org-todoist--get-todoist-type hl) TYPE)))
                                    hl))))
         (sorted (cl-sort children (lambda (a b) (< (org-todoist--get-position a PROPERTY) (org-todoist--get-position b PROPERTY))))))
    (dolist (child sorted)
      (org-element-adopt NODE (org-element-extract child)))))

(defun org-todoist--set-effort (NODE TASK)
  (when-let* ((duration (assoc-default 'duration TASK))
              (amount (assoc-default 'amount duration))
              (unit (assoc-default 'unit duration))
              (effortval (cond
                          ((string-equal unit "minute") amount)
                          ((string-equal unit "day") (* 1440 amount))
                          (t nil))))
    (org-todoist--add-prop NODE "EFFORT" (org-duration-from-minutes effortval))))

(defun org-todoist--set-priority (NODE PRIORITY)
  (org-element-put-property NODE :priority (cond
                                            ((equal PRIORITY 4) ?A)
                                            ((equal PRIORITY 3) ?B)
                                            ((equal PRIORITY 2) ?C)
                                            ((equal PRIORITY 1) ?D))))

(defun org-todoist--get-priority (NODE)
  (let ((priority (org-element-property :priority NODE)))
    (org-todoist--get-priority-cond priority)))

(defun org-todoist--get-priority-cond (priority)
  (cond
   ((equal priority ?A) 4)
   ((equal priority ?B) 3)
   ((equal priority ?C)  2)
   ((equal priority ?D)  1)
   (t (org-todoist--get-priority-cond org-priority-default))))

(defun org-todoist--set-todo (NODE CHECKED &optional DELETED)
  (if (or (eql t CHECKED) ;; :json-false for false currently, t for true
          (eql t DELETED))
      (progn
        (org-element-put-property NODE :todo-keyword (if (eql t DELETED) org-todoist-deleted-keyword org-todoist-done-keyword))
        (org-element-put-property NODE :todo-type 'done))
    (org-element-put-property NODE :todo-keyword org-todoist-todo-keyword)
    (org-element-put-property NODE :todo-type 'todo)))

(defun org-todoist--root (NODE)
  "Gets the syntax root of NODE"
  (let ((root NODE))
    (while-let ((next (org-element-parent root)))
      (setq root next))
    root))

;; (defun org-todoist--assign (NODE ID AST)
;;   (let* ((user (org-todoist--get-by-id org-todoist--collaborator-type ID AST))
;;          (name (org-todoist--get-prop user "full_name")))
;;     (org-todoist--add-tag NODE (concat  "@" (string-replace " " "_" name)))))

(defun org-todoist--add-tag (NODE TAG)
  "Adds a TAG to a NODE"
  (let ((tags (org-element-property :tags NODE)))
    (if (null tags)
        (org-element-put-property NODE :tags `(,TAG))
      (unless (member TAG tags)
        (push TAG tags)))))

(defun org-todoist--archive (NODE)
  "Archives NODE via tag"
  (org-todoist--add-tag NODE "ARCHIVE"))

(defun org-todoist--get-description-elements (NODE)
  "Gets all paragraph elements that are not in a drawer."
  (org-element-map NODE t #'org-todoist--is-description-element nil nil 'drawer))

(defun org-todoist--is-description-element (NODE)
  "t if the NODE is a paragraph or plain-text element and is not in a drawer."
  ;; TODO this doesn't seem to be working properly. Doesn't filter out items in drawers.
  (when (or (eq (org-element-type NODE) 'paragraph) (eq (org-element-type NODE) 'plain-text)) ;; check for plain text newly added descriptions
    (let ((closest-hl-or-drawer (org-element-lineage-map NODE #'identity '('drawer 'headline) nil t)))
      (unless (eq 'drawer (org-element-type closest-hl-or-drawer)) NODE))))

(defun org-todoist--description-text (NODE)
  "Combines all paragraphs under NODE and returns the concatenated string"
  (mapconcat #'org-todoist-org-element-to-string (org-todoist--get-description-elements NODE)))

(defun org-todoist--category-node-query-or-create (AST DEFAULT TYPE)
  "Finds the first node of TODOIST_TYPE TYPE under AST, else create it with
DEFAULT text."
  (let ((target (org-todoist--find-node (lambda (node) (when (org-todoist--node-type-query node TYPE) node)) AST)))
    (if target
        target
      (setq target (org-element-create 'headline `(:title ,DEFAULT :level 1)))
      (org-todoist--insert-identifier target TYPE)
      (org-element-adopt AST target)
      target)))

(defun org-todoist--node-type-query (NODE TYPE)
  (when (equal (org-todoist--get-prop NODE org-todoist--type) TYPE) NODE))

(defun org-todoist--update-file (AST)
  "Replaces the org-todoist file with the org representation AST"
  (save-current-buffer
    (find-file (org-todoist-file))
    (erase-buffer)
    (insert (org-todoist-org-element-to-string AST))
    (save-buffer)))

(defun org-todoist--file-ast ()
  "Parses the AST of the org-todoist-file"
  (save-current-buffer
    (let ((created (not (file-exists-p (org-todoist-file)))))
      (find-file (org-todoist-file))
      (when created (org-todoist--insert-header) (save-buffer))
      (org-element-parse-buffer))))

                                        ;Find node;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--find-node (QUERY AST)
  "Return the first headling in the syntax tree AST which matches QUERY.
QUERY takes a single argument which is the current node."
  (org-element-map AST 'headline (lambda (node) (when (funcall QUERY node) node)) nil t))

(defun org-todoist--get-by-id (TYPE ID AST)
  "Finds the first item of TODOIST_TYPE TYPE with id ID in the syntax tree AST."
  (when ID
    (org-element-map AST 'headline
      (lambda (hl) (when (and (or (null TYPE)
                                  (string-equal (org-todoist--get-prop hl org-todoist--type) TYPE))
                              (string= (org-todoist--get-prop hl org-todoist--id-property) ID))
                     hl))
      nil t)))

(defun org-todoist--get-by-tempid (ID TID_MAPPING AST)
  "Finds the item in AST corresponding to the given ID using the headlines TEMP_ID property
and the TID_MAPPING. Add PROPS not including SKIP to found node."
  (let* ((id (cl-dolist (elem TID_MAPPING)
               (when (equal ID (cdr elem))
                 (cl-return (car elem)))))
         (elem (org-element-map AST 'headline (lambda (hl)
                                                (let ((prop (org-element-property :TEMP_ID hl))) ;; TODO from drawer? may not work? not used atm
                                                  (when (and prop (string-equal prop id)) hl)))
                                nil t)))
    elem))
;; (when elem (org-todoist--add-all-properties elem PROPS SKIP) elem)))

(defun org-todoist--id-or-temp-id (NODE)
  (let ((id (org-todoist--get-prop NODE org-todoist--id-property)))
    (if id
        id
      (org-todoist--get-prop NODE "temp_id"))))

(defun org-todoist--get-project-id-position (NODE)
  "Gets the ID of the project headline the NODE is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--project-type NODE t)))

(defun org-todoist--get-section-id-position (NODE)
  "Gets the ID of the section headline the NODE is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--section-type NODE t)))

(defun org-todoist--get-task-id-position (NODE)
  "Gets the ID of the task headline the NODE is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--task-type NODE t)))

(defun org-todoist--get-parent-of-type (TYPE NODE &optional FIRST)
  "Gets the parent(s) of NODE with TODOIST_TYPE TYPE. If FIRST, only get the first
matching parent."
  (org-element-lineage-map NODE
      (lambda (parent) (when (equal (org-todoist--get-todoist-type parent) TYPE)
                         parent))
    'headline nil FIRST))

(defun org-todoist--get-parent-of-element-type (TYPE NODE)
  "Gets the parent(s) of NODE with org-element-type TYPE."
  (org-element-lineage-map NODE
      (lambda (parent) (when (and (not (eq NODE parent))) (eq (org-element-type NODE) TYPE)
                             parent))
    'headline nil t))

(defun org-todoist--get-hl-level-at-point ()
  (org-element-property :level (org-todoist--get-parent-of-element-type 'headline (org-element-at-point))))


(defun org-todoist--unimplemented () (error "Unimplemented feature!"))

                                        ;Property get/set;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--insert-id (NODE ID)
  (org-todoist--add-prop NODE org-todoist--id-property ID)
  NODE)

(defun org-todoist--insert-identifier (NODE IDENTIFIER)
  "Inserts a property into the node's property drawer identifying the org-todoist i
node. Returns the modified element."
  (org-todoist--add-prop NODE org-todoist--type IDENTIFIER)
  NODE)

(defun org-todoist--get-property-drawer (NODE)
  (org-element-map NODE 'property-drawer
    (lambda (node) (when (eq NODE (org-todoist--first-parent-of-type node 'headline)) node))
    nil t))

(defun org-todoist--create-property (KEY VALUE) (org-element-create 'node-property `(:key ,KEY :value ,VALUE)))

(defun org-todoist--property-exists (NODE KEY)
  (unless (equal (org-element-type NODE) 'property-drawer)
    (error "Can only be called on property drawer elements")
    (org-element-map NODE 'node-property (lambda (prop) (org-todoist--is-property prop KEY)))))

(defun org-todoist--is-property (NODE KEY)
  (when (eq (org-element-type NODE) 'node-property)
    (let ((prop (org-element-property :key NODE)))
      (string-equal-ignore-case (if (not (stringp prop)) (prin1-to-string prop) prop) (if (stringp KEY) KEY (symbol-name KEY))))))

(defun org-todoist--set-prop-in-place (DRAWER KEY VALUE)
  "Checks for property KEY in DRAWER and replaces with VALUE if the key is present."
  (when (not (eq (org-element-type DRAWER) 'property-drawer))
    (error "Expected property drawer"))
  (let ((existing (org-element-map DRAWER 'node-property
                    (lambda (prop) (when (org-todoist--is-property prop KEY) prop)) nil t)))
    (if existing
        ;; (org-element-put-property existing (org-todoist--to-symbol KEY) VALUE)
        (org-element-put-property existing :value VALUE)
      (org-element-adopt DRAWER (org-todoist--create-property KEY VALUE)))))

(defun org-todoist--get-key (KEY)
  (if (or (eq KEY 'id)
          (and (stringp KEY)
               (string-equal-ignore-case KEY "id")))
      org-todoist--id-property
    KEY))

(defun org-todoist--get-value (VALUE)
  (if (stringp VALUE)
      VALUE
    (prin1-to-string VALUE)))

(defun org-todoist--add-prop (DRAWER KEY VALUE)
  "Adds property with KEY and VALUE to property-drawer DRAWER.
If DRAWER is another node type, create and adopt a new property drawer.
Replaces the current value with VALUE if property KEY already exists."
  (let ((key (org-todoist--get-key KEY))
        (value (org-todoist--get-value VALUE))
        (type (org-element-type DRAWER)))
    (cond ((eq type 'property-drawer) ;; Add directly
           (org-todoist--set-prop-in-place DRAWER key value)
           DRAWER)
          ((eq type 'headline) ;; Find drawer and update both drawer and property plist
           (let* ((headline DRAWER)
                  (drawer (org-todoist--get-property-drawer headline)))
             ;; (org-todoist--put-node-attribute headline (org-todoist--to-symbol KEY) VALUE) ;; update property plist
             (unless drawer ;; create drawer if needed
               (setq drawer (org-element-create 'property-drawer))
               (org-todoist--adopt-drawer headline drawer))
             (org-todoist--set-prop-in-place drawer key value)
             drawer))
          (t (signal 'todoist--error "Called org-todoist--add-prop with invalid type")))))

(defun org-todoist--get-prop (NODE KEY)
  "Retrieves the property KEY from the property drawer directly under NODE.
Returns nil if not present"
  (let ((drawer (if (equal (org-element-type NODE) 'property-drawer)
                    NODE
                  (org-todoist--get-property-drawer NODE)))
        (key (org-todoist--get-key KEY)))
    (org-element-map drawer 'node-property
      (lambda (np)
        (when (org-todoist--is-property np key)
          (org-element-property :value np)))
      nil t)))

(defun org-todoist--add-all-properties (NODE PROPERTIES &optional SKIP)
  "Adds or updates the values of all properties in the alist PROPERTIES to
NODE unless they are in plist SKIP. RETURNS the mutated NODE."
  (dolist (kv PROPERTIES)
    (unless (member (car kv) SKIP)
      (org-todoist--add-prop NODE (car kv) (cdr kv))))
  NODE)

(defun org-todoist--get-todoist-type (NODE &optional NO-INFER)
  "Gets the TODOIST_TYPE of a NODE."
  (if NO-INFER
      (org-todoist--get-prop NODE org-todoist--type)
    (cond
     ;; type is already present on headline or drawer
     ((org-todoist--get-prop NODE org-todoist--type) (org-todoist--get-prop NODE org-todoist--type))
     ;; Only tasks can be todos
     ((org-element-property :todo-type NODE) org-todoist--task-type)
     ;; Else guess
     (t (if-let* ((parent (org-element-parent NODE))
                  (parenttype (org-todoist--get-todoist-type parent)))
            (cond ((string= parenttype org-todoist--task-type) org-todoist--task-type)
                  ((string= parenttype org-todoist--section-type) org-todoist--task-type)
                  ((string= parenttype org-todoist--project-type) org-todoist--section-type)
                  ;; TODO max recursion limits reached here
                  ;; (if (org-element-map NODE 'headline
                  ;;       (lambda (child) (when (string= (org-todoist--get-todoist-type child) org-todoist--section-type) t))
                  ;;       nil t)
                  ;;     org-todoist--project-type ;; has section children -> is project
                  ;;   org-todoist--section-type)) ;; otherwise it is probably a new section. Could be wrong.
                  (t org-todoist--project-type)))))))

(defun org-todoist--get-prop-elem (NODE KEY)
  "Gets the org-element property KEY of NODE."
  (org-todoist-org-element-to-string (org-element-property KEY NODE)))

(defun org-todoist-org-element-to-string (DATA)
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (substring-no-properties (org-element-interpret-data DATA)))

                                        ;Test utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-to-string (file)
  "Read FILE as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-todoist--remove-ws-and-newline (STRING) (string-replace "\n" ""(string-replace " " "" STRING))) ;; TODO replace-regexp
(defun org-todoist--remove-ws (STRING) (string-replace " " "" STRING))

(defun org-todoist--generate-ast (contents) (org-todoist--invoke-with-buffer contents #'org-element-parse-buffer))

(defun org-todoist-org-element-to-string-no-ws (DATA)
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (org-todoist--remove-ws (substring-no-properties (org-element-interpret-data DATA))))

(defun org-todoist--element-equals-str (STR DATA)
  "Returns t if the org string from org-element-interpret-data for DATA is STR."
  (string-equal-ignore-case (org-todoist--remove-ws STR) (org-todoist--remove-ws (org-todoist-org-element-to-string DATA))))

(defun org-todoist--invoke-with-buffer (CONTENTS FN)
  "Fills a temp buffer with CONTENTS, applies org-mode, and calls FN."
  (with-temp-buffer (insert CONTENTS) (org-mode) (funcall FN)))

;; User functions

(defun org-todoist-unassign-task ()
  (interactive)
  (org-set-property "responsible_uid" "nil"))

(defun org-todoist-tag-user ()
  (interactive)
  (when-let ((selectedelement (org-todoist--select-user "Tag: ")))
    (insert (concat "@" (org-element-property :raw-value selectedelement)))))

(defun org-todoist-assign-task ()
  (interactive)
  ;; TODO limit to just tasks, but allow assigning for tasks that haven't been added to Todoist yet
  (when-let ((selectedelement (org-todoist--select-user "Assign to: ")))
    (org-set-property "responsible_uid" (org-todoist--get-prop selectedelement org-todoist--id-property))))

(defun org-todoist-sync (&optional ARG)
  (interactive "P")
  (org-todoist--do-sync (org-todoist--get-sync-token) (null ARG)))

(defun org-todoist-reset (&optional ARG)
  (interactive "P")
  (when (get-buffer org-todoist-file)
    (kill-buffer org-todoist-file))
  (delete-file (org-todoist-file))
  (delete-file (org-todoist--storage-file "PREVIOUS.json"))
  (delete-file (org-todoist--storage-file org-todoist--sync-token-file))
  (delete-file (org-todoist--storage-file org-todoist--sync-buffer-file))
  (org-todoist--do-sync "*" (null ARG)))

(provide 'org-todoist)
;;; org-todoist.el ends here
