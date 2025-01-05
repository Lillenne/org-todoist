;;; test.el --- Tests for org-todoist -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Austin Kearns
;;
;; Author: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Maintainer: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Created: September 16, 2024
;; Modified: September 16, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/aus/test
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for org-todoist
;;
;;; Code:
(require 'ert)
(require 'url)
(require 'ts)
(require 'org)
(require 'json)
(require 'dash)

                                        ;Config;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO likely don't want to override user configuration. Todoist has 4 priorities.
(setq org-priority-highest ?A
      org-priority-default ?D
      org-priority-lowest ?D)
(defvar url-http-end-of-headers nil)
(defvar org-todoist-api-token nil "The API token to use to sync with Todoist")
(defvar org-todoist-file "todoist.org")
(defvar org-todoist-user-headline "Collaborators")
(defvar org-todoist-project-headline "Projects")
(defvar org-todoist-tz nil)
(defvar org-todoist-lang "en")
(defvar org-todoist-use-auto-reminder t)
(defvar org-todoist-show-n-levels 4
  "The number of headline levels to show by default.
If visiting the todoist buffer when sync is called, it will instead
show the outline up to one level above the current position.
2=projects,
3=sections,
4=root tasks,
<0 no fold")
(defvar org-todoist-todo-keyword "TODO")
(defvar org-todoist-done-keyword "DONE")
(defvar org-todoist-deleted-keyword "CANCELED")
(defun org-todoist--expand-from-dir (dir file)
  (if (file-name-absolute-p file)
      file
    (concat (expand-file-name dir) file)))
(defun org-todoist-file () (org-todoist--expand-from-dir org-directory org-todoist-file))

                                        ;Constants;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst org-todoist-resource-types '("projects" "notes" "labels" "items" "sections" "collaborators") "The list of resource types to sync")
(defconst org-todoist-sync-endpoint "https://api.todoist.com/sync/v9/sync")
(defconst org-todoist-request-type "application/x-www-form-urlencoded; charset=utf-8")
(defconst org-todoist-http-method "POST")
(defconst org-todoist--type "TODOIST_TYPE")
(defconst org-todoist--collaborator-type "USER")
(defconst org-todoist--section-type "SECTION")
(defconst org-todoist--project-type "PROJECT")
(defconst org-todoist--task-type "TASK")
(defconst org-todoist--user-node-type "USER_HEADLINE")
(defconst org-todoist--project-node-type "PROJECT_HEADLINE")
(defconst org-todoist--sync-areas ["collaborators", "projects", "items", "sections"])
(defconst org-todoist--project-skip-list '(name can_assign_tasks collapsed color is_archived is_deleted is_favorite is_frozen sync_id v2_id v2_parent_id view_style))
(defconst org-todoist--section-skip-list '(name project_id sync_id updated_at v2_id v2_project_id is_deleted is_archived collapsed archived_at section_order))
(defconst org-todoist--task-skip-list '(name completed_at content collapsed duration description checked deadline due labels priority sync_id v2_id v2_parent_id v2_project_id v2_section_id completed_at content day_order is_deleted))
;; (defconst org-todoist--task-skip-list '(name completed_at content collapsed duration description checked deadline due labels priority project_id section_id sync_id v2_id v2_parent_id v2_project_id v2_section_id completed_at content day_order is_deleted))
(defconst org-todoist--sync-token-file "SYNC-TOKEN")
(defconst org-todoist--sync-buffer-file "SYNC-BUFFER")

(add-to-list 'org-fold-show-context-detail '(todoist . lineage))

                                        ;Data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar org-todoist--storage-dir (concat (file-name-as-directory (xdg-cache-home))  "org-todoist") "Directory for org-todoist storage.")
(defvar org-todoist--sync-err nil "The error from the last sync, if any")
(defvar org-todoist--last-request nil "The last request body, if any")
(defvar org-todoist--last-response nil)
(defvar org-todoist-sync-interval 60 "The interval in seconds between sync requests with Todoist servers.")
(defun org-todoist--set-last-response (JSON) (with-file! (org-todoist--storage-file "PREVIOUS.json") (insert JSON)))
(defun org-todoist--storage-file (FILE) (concat (file-name-as-directory org-todoist--storage-dir) FILE))
(defun org-todoist--set-sync-token (TOKEN) (with-file! (org-todoist--storage-file org-todoist--sync-token-file) (goto-char (point-max)) (insert "\n") (insert TOKEN)))
(defun org-todoist--get-sync-token ()
  (if (file-exists-p (org-todoist--storage-file org-todoist--sync-token-file))
      (let ((res (with-file-contents! (org-todoist--storage-file org-todoist--sync-token-file) (get-last-line))))
        (if res res "*"))
    "*"))

(defun org-todoist--set-last-sync-buffer (AST) (with-file! (org-todoist--storage-file org-todoist--sync-buffer-file) (org-mode) (insert (org-todoist-org-element-to-string AST))))
(defun org-todoist--get-last-sync-buffer-ast ()
  (let* ((fn (org-todoist--storage-file org-todoist--sync-buffer-file))
         (existed (file-exists-p fn))
         (buf (find-file-noselect fn))) ;; create buffer. Will be used later in chain
    (when existed
      (save-window-excursion (find-file fn)
                             ;; (with-current-buffer buf ;;not sure why this doesn't work
                             (org-mode)
                             (org-element-parse-buffer)))))

(defun get-last-line ()
  (save-excursion
    (goto-char (point-max))
    (thing-at-point 'line t)))

(defun org-todoist--get-comments (HEADLINE)
  (org-element-map HEADLINE 'item (lambda (item) (when (org-todoist--is-note item) item))))

(defun org-todoist--is-note (ITEM)
  "NOTE must be on the org buffer"
  (s-contains? "Note" (org-todoist--item-text ITEM)))

(defun org-todoist--item-text (ITEM)
  ;; May want to always user org-element-to-string since it doesn't require the buffer. Don't remember about including / excluding bullets
  (if (and (org-element-contents-begin ITEM) (org-element-contents-end ITEM))
      (buffer-substring-no-properties (org-element-contents-begin ITEM) (org-element-contents-end ITEM))
    (org-todoist-org-element-to-string ITEM)))

(defun org-todoist--note-text (ITEM)
  "NOTE must be on the org buffer"
  (let ((text (org-todoist--item-text ITEM)))
    (when (s-contains? "Note taken on [" text) (s-trim-right (s-join "\n" (--map (s-trim it) (cdr (s-lines text))))))))

;; (defun org-todoist--get-comments-text (HEADLINE)
(defun org-todoist--get-comments-text (HEADLINE &optional FILE)
  "NOTE must be on the org buffer or specify buffer as FILE"
  (if FILE
      ;; (with-current-buffer (find-file-noselect FILE) (--map (org-todoist--note-text it) (org-todoist--get-comments HEADLINE))) ;; TODO why does this not work
      (save-window-excursion (find-file FILE) (--map (org-todoist--note-text it) (org-todoist--get-comments HEADLINE)))
    (--map (org-todoist--note-text it) (org-todoist--get-comments HEADLINE))))

                                        ;API Requests;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist-unassign-task ()
  (interactive)
  (org-set-property "responsible_uid" "nil"))

(defun org-todoist-assign-task ()
  (interactive)
  ;; TODO limit to just tasks, but allow assigning for tasks that haven't been added to Todoist yet
  (let* ((ast (org-todoist--file-ast))) ;; TODO cache?
    (if-let* ((userelements (org-element-map (org-todoist--user-node ast) 'headline
                              (lambda (node) (when (org-todoist--node-type-query node org-todoist--collaborator-type) node))))
              (usernames (--map (org-element-property :raw-value it) userelements))
              (selected (completing-read "Assign to: " (append usernames "Unassign")))
              (selectedelement (--first (string= (org-element-property :raw-value it) selected) userelements))
              (rid (org-element-property :ID selectedelement)))
        (org-set-property "responsible_uid" rid))))

(defun org-todoist--all-users ()
  (org-element-map (org-todoist--user-node (org-todoist--file-ast)) 'headline #'identity))

(defun org-todoist-sync (&optional ARG)
  (interactive "P")
  (org-todoist--do-sync (org-todoist--get-sync-token) (null ARG)))

(defun org-todoist-reset (&optional ARG)
  (interactive "P")
  (kill-buffer org-todoist-file)
  (delete-file (org-todoist-file))
  (delete-file (org-todoist--storage-file "PREVIOUS.json"))
  (delete-file (org-todoist--storage-file org-todoist--sync-token-file))
  (delete-file (org-todoist--storage-file org-todoist--sync-buffer-file))
  (org-todoist--do-sync "*" (null ARG)))

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

;; (ert-deftest org-todoist--test--encode-item ()
;;   (let ((args '("args" . (("id" . "5")
;;                                   ("content" . "test content")
;;                                   ("priority" . 1))))
;;         (data '(("type" . "item_update")
;;                      ("uuid" . "393491b2-6ef8-4045-b208-20a84c51eb42")
;;                      ("args" . (("id" . "5")
;;                                   ("content" . "test content")
;;                                   ("priority" . 1))))))

;;     ;; (json-encode (cdr args))
;;     ;; (org-todoist--encode args)
;;     ;; (json-alist-p (cdr args))
;;     ;; ;; (if (json-alist-p (cdr item)
;;     ;;                   ))
;;     ;; (org-todoist--encode-item (cdr args)                  )
;;     (json-encode data)
;;     ;; (org-todoist--encode-item

;;     )
;;   )

;; (ert-deftest org-todoist--test--encode ()
;;   (org-todoist--encode `(("sync_token" . "*")
;;                          ("resource_types" . ,org-todoist-resource-types)
;;                          ("commands" . ,(org-todoist--push)))))

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
    (setq org-todoist--last-req url-request-data)
    (message (if OPEN "Syncing with todoist. Buffer will open when sync is complete..." "Syncing with todoist. This may take a moment..."))
    (url-retrieve org-todoist-sync-endpoint
                  (lambda (events open ast old cur-headline show-n-levels)
                    (if (plist-member events :error)
                        (progn (setq org-todoist--sync-err events)
                               (message "Sync failed. See org-todoist--sync-err for details."))
                      (let ((resp (with-current-buffer (current-buffer)
                                    (goto-char url-http-end-of-headers)
                                    (decode-coding-region (point) (point-max) 'utf-8 t))))
                        (org-todoist--set-last-response resp) ;; TODO remove
                        (setq org-todoist--last-response (json-read-from-string resp))
                        (org-todoist--parse-response org-todoist--last-response ast old)
                        (if open
                            (org-todoist--handle-display cur-headline show-n-levels)
                          (save-window-excursion
                            (org-todoist--handle-display cur-headline show-n-levels)))
                        (message "Sync complete."))))
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
  (when cur-headline
    (let ((pos nil))
      (org-map-entries (lambda () (when (equal (org-entry-get nil "ITEM") cur-headline)
                                    (setq pos (point)))))
      (goto-char pos)
      (org-fold-show-context 'todoist)
      ;; (+org/open-fold)
      )))

(defun org-todoist--first-parent-of-type (NODE TYPES)
  "Gets the first parent which is one of the given TYPES.
TYPES can be a single symbol or a list of symbols."
  (org-element-lineage-map NODE #'identity TYPES nil t))

(ert-deftest org-todoist--test--first-parent-of-type ()
  (let* ((ast (org-todoist--generate-sample-ast))
         (task (org-todoist--get-by-id org-todoist--task-type "3" ast))
         (section (org-todoist--get-by-id org-todoist--section-type "2" ast)))
    (should (eq (org-todoist--first-parent-of-type task 'headline)
                section))))

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
  (let (
        (month (org-element-property :month-start TIMESTAMP))
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

(ert-deftest org-todoist--test--date-to-todoist ()
  (let* ((task (org-todoist--test-task))
         (planning (org-todoist--get-planning task))
         (schtimestamp (org-element-property :scheduled planning)))
    (should (string-equal "2024-12-31T23:59:000000" (org-todoist--get-planning-date task :scheduled)))
    (should (string-equal "2024-12-31T23:59:000000" (org-todoist--date-to-todoist schtimestamp)))))

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
    (if (null drawer)
        (org-element-adopt HL (org-element-create 'drawer '(:drawer-name "LOGBOOK") note))
      (let ((children (org-element-contents drawer)))
        (if children
            (org-element-insert-before note (car children))
          (org-element-adopt drawer note)))))
  HL)

(defun org-todoist--last-recurring-task-completion (TASK)
  (when (org-todoist--task-is-recurring TASK)
    (org-timestamp-from-string (org-element-property :LAST_REPEAT TASK))))

(defun org-todoist--last-recurring-task-completion-as-doist (TASK)
  (org-todoist--date-to-todoist (org-todoist--last-recurring-task-completion TASK)))

(ert-deftest org-todoist--test--task-is-recurring ()
  (should (org-todoist--task-is-recurring (org-todoist--test-recurring-task))))

(defun org-todoist--push-test ()
  (org-todoist--push (org-todoist--file-ast) (org-todoist--get-last-sync-buffer-ast)))

(defun org-todoist--timestamp-times-equal (T1 T2)
  (and (equal (org-element-property :year-start T1) (org-element-property :year-start T2))
       (equal (org-element-property :month-start T1) (org-element-property :month-start T2))
       (equal (org-element-property :day-start T1) (org-element-property :day-start T2))
       (equal (org-element-property :hour-start T1) (org-element-property :hour-start T2))
       (equal (org-element-property :minute-start T1) (org-element-property :minute-start T2))))

(defun org-todoist--push (ast old)
  (let ((commands nil)
        (curfile (org-todoist-file))
        (oldfile (org-todoist--storage-file org-todoist--sync-buffer-file)))
    ;; TODO map old and new so we can see anything deleted?
    (org-element-map ast 'headline
      (lambda (hl)
        ;; TODO would be easier as oop, need to learn lisp oop
        (let* ((type (org-todoist--get-todoist-type hl))
               (id (org-todoist--get-prop hl "ID"))
               (oldtask (org-todoist--get-by-id nil id old))
               (todo-type (org-element-property :todo-type hl))
               (todo-kw (org-element-property :todo-keyword hl))
               (title (org-element-property :raw-value hl))
               (desc (org-todoist--description-text hl))
               (sch (org-element-property :scheduled hl))
               (pri (org-element-property :priority hl))
               (dead (org-element-property :deadline hl))
               (effstr (org-todoist--get-prop hl "EFFORT"))
               (eff (when effstr (org-duration-to-minutes effstr)))
               (labels (org-element-property :tags hl))
               (proj (org-todoist--get-project-id-position hl))
               (section (org-todoist--get-section-id-position hl))
               (parenttask (org-todoist--get-task-id-position hl))
               (rid (org-element-property :RESPONSIBLE_UID hl))
               (is-recurring (org-todoist--task-is-recurring hl))
               (comments (org-todoist--get-comments-text hl curfile))
               (lr (org-element-property :LAST_REPEAT hl))
               (last-repeat (when (and lr is-recurring) (org-timestamp-from-string lr)))
               )
          (cond ((string-equal type org-todoist--task-type)
                 (when oldtask
                   (let* (
                          (oldsection (org-todoist--get-section-id-position oldtask))
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
                          (oldlabels (org-element-property :tags oldtask))
                          (oldrid (org-element-property :RESPONSIBLE_UID oldtask))
                          (oldcomments (org-todoist--get-comments-text oldtask oldfile))
                          )
                     (unless (equal comments oldcomments)
                       ;; TODO support comment editing. This will push any edited comments as new comments
                       (dolist (comment (--filter (not (member it oldcomments)) comments))
                         (push `(("uuid" . ,(org-id-uuid))
                                 ("type" . "note_add")
                                 ("temp_id". ,(org-id-uuid))
                                 ("args" . (("item_id" . ,id)
                                            ("content" . ,comment))))
                               commands)))

                     ;; item_move. Only one parameter can be specified
                     (unless (and (string= section oldsection)
                                  (string= proj oldproj)
                                  (string= parenttask oldparenttask))
                       (cond ((not (string= parenttask oldparenttask))
                              ;; item_move - parent task
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("parent_id" . ,parenttask))))
                                    commands))

                             ;; special case: move to unsectioned in another project
                             ((and (or (null section) (string= "" section))
                                   (not (string= proj oldproj)))
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("project_id" . ,proj))))
                                    commands))

                             ;; move to another section (may be another project)
                             ((not (string= section oldsection))
                              ;; item_move - section
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("section_id" . ,section))))
                                    commands))


                             ((not (string= proj oldproj))
                              ;;item_move - project
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("type" . "item_move")
                                      ("args" . (("id" . ,id)
                                                 ("project_id" . ,proj))))
                                    commands))))

                     ;; item_update
                     (when (or
                            (not (string-equal title oldtitle))
                            (not (equal desc olddesc))
                            (not (string-equal (org-element-property :raw-value sch) (org-element-property :raw-value oldsch)))
                            (not (string-equal (org-element-property :raw-value dead) (org-element-property :raw-value olddead)))
                            (not (equal eff oldeff))
                            (not (equal pri oldpri))
                            (not (equal labels oldlabels))
                            (not (equal rid oldrid))
                            )
                       ;; TODO HERE compare last repeat to old scheduled date. If same, we completed a recurring task and need to call item close
                       ;; Doesn't work - last repeat logs when it was closed not the due date. Functionally the same except it doesn't log the completion on the todoist side, I think
                       (when (org-todoist--timestamp-times-equal last-repeat oldsch) ;; completed recurring task
                         (push `(("uuid" . ,(org-id-uuid))
                                 ("type" . "item_close")
                                 ("args" . (("id" . ,id))))
                               commands))
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
                ((org-todoist--is-task hl)
                 ;; item_add. No todoist type means new task
                 (unless (org-element-property :TODOIST_TYPE hl)
                   (let ((tid (org-id-uuid)))
                     (org-todoist--add-prop hl "temp_id" tid)
                     (push `(("uuid" . ,(org-id-uuid))
                             ("temp_id" . ,tid)
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
                                        ("section_id" . ,(org-todoist--get-section-id-position hl))
                                        ("project_id" . ,(org-todoist--get-project-id-position hl))
                                        ("responsible_uid" . ,rid)
                                        )))
                           commands))))))))
    commands))

(defun org-todoist--is-project (NODE)
  (string= (org-todoist--infer-type NODE) org-todoist--project-type))

(defun org-todoist--is-section (NODE)
  (string= (org-todoist--infer-type NODE) org-todoist--section-type))

(defun org-todoist--infer-type (NODE)
  (if-let ((type (org-todoist--get-todoist-type NODE)))
      type ;; already has type labeled
    (if-let ((parenttype (org-todoist--get-todoist-type (org-element-parent NODE))))
        (cond ((string= parenttype org-todoist--task-type) org-todoist--task-type)
              ((string= parenttype org-todoist--section-type) org-todoist--task-type)
              ((string= parenttype org-todoist--project-type)
               (if (org-element-map NODE 'headline
                     (lambda (child) (when (string= (org-todoist--get-todoist-type child) org-todoist--section-type) t))
                     nil t)
                   org-todoist--project-type ;; has section children -> is project
                 org-todoist--section-type)) ;; otherwise it is probably a new section. Could be wrong.
              (t org-todoist--project-type)))))

(defun org-todoist--is-task (NODE)
  (string= (org-todoist--infer-type NODE) org-todoist--task-type))
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
    (org-todoist--update-users collab AST)
    (org-todoist--update-projects projects AST)
    (org-todoist--update-sections sections AST)
    (org-todoist--update-tasks tasks AST tid_mapping OLDAST)
    (org-todoist--update-comments comments AST (org-todoist-file))
    (org-todoist--set-sync-token token)
    (org-todoist--set-last-sync-buffer AST)
    (org-todoist--update-file AST)))

(defun org-todoist--update-comments (COMMENTS AST FILE)
  (cl-loop for comment across COMMENTS do
           ;; TODO support comment IDs and add/update/delete
           (let* ((task (org-todoist--get-by-id org-todoist--task-type (assoc-default 'item_id comment) AST))
                  (comments (org-todoist--get-comments-text task FILE))
                  (text (assoc-default 'content comment)))
             (unless (member text comments)
               (org-todoist--log-drawer-add-note task (assoc-default 'content comment) (assoc-default 'posted_at comment)))
             ;; TODO sort comments by time.
             )))

(ert-deftest org-todoist--test--parse-response ()
  (with-file-contents! "/home/aus/projects/org-todoist/api-call.json"
    (let ((resp (json-read)))
      ;; TODO actually test
      (org-todoist--parse-response resp (org-todoist--file-ast) (org-todoist--get-last-sync-buffer-ast))))
  )

(defun org-todoist--insert-header ()
  ;; TODO use element api
  (save-excursion
    (goto-char (point-min))
    (insert "#+title: Todoist
#+FILETAGS: :todoist:
#+STARTUP: logdone
#+STARTUP: logdrawer
")))

(defun org-todoist--project-node (AST)
  "Gets or creates the main project headline in AST"
  (org-todoist--category-node-query-or-create AST org-todoist-project-headline org-todoist--project-node-type))

(defun org-todoist--update-projects (PROJECTS AST)
  (let ((projects (org-todoist--project-node AST)))
    ;; TODO test
    (cl-loop for proj across PROJECTS do
             (org-todoist--get-or-create-node
              projects
              org-todoist--project-type
              (assoc-default 'id proj)
              (assoc-default 'name proj)
              nil
              proj
              org-todoist--project-skip-list))
    (org-todoist--sort-by-child-order projects "child_order")
    (dolist (proj (org-todoist--project-nodes AST))
      (when (length= (org-todoist--get-sections proj) 0) (org-todoist--create-node org-todoist--section-type "Default" nil nil proj)))))

(defun org-todoist--get-sections (PROJECT)
  (org-element-map PROJECT 'headline (lambda (hl) (when (equal (org-todoist--get-todoist-type hl) org-todoist--section-type) hl))))

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
             (if-let* ((section (org-todoist--get-by-id org-todoist--section-type (assoc-default 'id sect) AST))
                       (project (org-todoist--get-parent-of-type org-todoist--project-type section t))
                       (projid (org-todoist--get-project-id-position section)))
                 ;; Section already exists
                 (if (cl-equalp projid targetprojid)
                     ;; Section is under the right project, add props only
                     (org-todoist--add-all-properties section sect org-todoist--section-skip-list)
                   ;; Move to the correct project
                   (org-element-extract section)
                   (org-element-adopt (org-todoist--get-by-id org-todoist--project-type targetprojid AST) section))
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
    (org-todoist--sort-by-child-order proj "section_order" org-todoist--section-type)))

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
    (lambda (hl) (when (and (string-equal (org-todoist--get-todoist-type hl) org-todoist--section-type) (string-equal "Unsectioned" (org-element-property :raw-value hl))) hl))))

(ert-deftest org-todoist--test--project-nodes ()
  (should (length= (org-todoist--project-nodes (org-todoist--generate-sample-ast)) 2)))

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

(ert-deftest org-todoist--test--replace-description ()
  (let ((task (org-todoist--test-task))
        (text "New Desc\n"))
    (org-todoist--replace-description task text)
    (should (string-equal text (org-todoist--description-text task)))))

(defun org-todoist--get-or-create-node (PARENT TYPE ID TEXT DESCRIPTION PROPERTIES &optional SKIP)
  "Gets or creates a new node of TODOIST_TYPE TYPE with ID, headline TEXT,
body DESCRIPTION, and drawer PROPERTIES, ignoring SKIP, under PARENT."

  (if-let* ((found (org-todoist--get-by-id TYPE ID PARENT))
            (updated (org-todoist--add-all-properties found PROPERTIES SKIP)))
      (progn
        (org-element-put-property updated :title TEXT)
        (org-todoist--replace-description updated DESCRIPTION)
        updated)
    (org-todoist--create-node TYPE TEXT DESCRIPTION PROPERTIES PARENT SKIP)))

(defun vector-to-list (vector)
  "Convert a VECTOR into a regular Lisp list using cl-loop."
  (cl-loop for element across vector collect element))

(ert-deftest test-vector-to-list ()
  "Test the `vector-to-list` function with various vectors."
  ;; Test case: Empty vector
  (should (equal (vector-to-list []) '()))

  ;; Test case: Vector with single element
  (should (equal (vector-to-list [1]) '(1)))

  ;; Test case: Vector with multiple elements
  (should (equal (vector-to-list [1 2 3 4 5]) '(1 2 3 4 5)))

  ;; Test case: Vector with non-numeric elements
  (should (equal (vector-to-list ["a" "b" "c"]) '("a" "b" "c")))

  ;; Test case: Vector with mixed types
  (should (equal (vector-to-list [1 "two" 3.0]) '(1 "two" 3.0))))

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

(ert-deftest org-todoist--test--timestamp-to-utc-str ()
  (let* ((ts (org-timestamp-from-string "<2025-01-01 23:30>"))
         (str (org-todoist--timestamp-to-utc-str ts)))
    (should (string= str "2025-01-02T07:30:00.0Z"))))

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
  (if-let ((existing (org-element-map HEADLINE 'planning (lambda (plan) plan ;; (when (eq (org-element-parent plan) HEADLINE) plan)
                                                           ) nil t)))
      (org-element-set existing (org-todoist--create-planning TASK))
    (org-element-insert-before (org-todoist--create-planning TASK) (org-todoist--get-property-drawer HEADLINE))))

(ert-deftest org-todoist--test--schedule ()
  (let ((task (json-read-file "sample-task.json"))
        (headline (org-element-create 'headline '(:title "Test Headline" :level 1 :todo-type 'todo :todo-keyword "TODO"))))
    ;; (timestamp (org-timestamp-from-time (org-read-date nil t "2016-12-0T12:00:00.000000" nil ))))
    (org-element-adopt headline (org-todoist--create-planning task))
    (should (org-todoist--element-equals-str "* TODO Test Headline
DEADLINE: <2017-01-06 Fri> SCHEDULED: <2016-12-06 Tue>
" headline))))

(defun org-todoist--update-tasks (TASKS AST TID_MAPPING OLDAST)
  (cl-loop for data across TASKS do
           (let* ((id (assoc-default 'id data))
                  (og (org-todoist--get-by-id org-todoist--task-type id AST))
                  (task nil)
                  (section (org-todoist--get-by-id org-todoist--section-type (assoc-default 'section_id data)
                                                   ;; search in project, since the section search will match the default section if there is no section
                                                   (org-todoist--get-by-id org-todoist--project-type (assoc-default 'project_id data) AST)))
                  (tags (assoc-default 'labels data))
                  )
             ;; check if we need to move sections
             (unless (cl-equalp (org-todoist--get-section-id-position task) (assoc-default 'section_id data))
               (org-element-extract task)
               (org-element-adopt section task))

             (setq task (if og
                            og ;; exists with id, ready to be updated
                          ;; check if was created by our sync (has temp id)
                          (let ((tid (org-todoist--get-by-tempid id TID_MAPPING AST data org-todoist--task-skip-list)))
                            (if tid
                                tid
                              ;;create if needed
                              (org-todoist--create-node
                               org-todoist--task-type
                               (assoc-default 'content data)
                               (assoc-default 'description data)
                               data
                               section
                               org-todoist--task-skip-list)))))

             (dotimes (i (length tags))
               (org-todoist--add-tag task (aref tags i)))
             (org-todoist--schedule task data)
             (org-todoist--set-effort task data)
             (org-todoist--set-priority task (assoc-default 'priority data))
             (org-todoist--set-todo task (assoc-default 'checked data) (assoc-default 'is_deleted data)))))

(defun org-todoist--list-headlines ()
  (let ((headlines '()))
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
  nil
  ;; (let* ((children (org-element-map NODE 'headline (lambda (hl) (when
  ;;                                                                   (and (eq (org-element-parent hl) NODE)
  ;;                                                                        (org-todoist--get-prop hl PROPERTY)
  ;;                                                                        (or (not TYPE)
  ;;                                                                            (equal (org-todoist--get-todoist-type hl) TYPE))) hl))))
  ;;        (sorted (cl-sort children (lambda (a b) (< (org-todoist--get-position a PROPERTY) (org-todoist--get-position b PROPERTY))))))
  ;;   (dolist (child sorted)
  ;;     (org-element-adopt NODE (org-element-extract child))))
  ;; TODO not rearranging sections.
  )

(ert-deftest org-todoist--test--sort-by-child-order ()
  (let ((section (org-todoist--get-by-id org-todoist--section-type "2" (org-todoist--generate-sample-ast)))
        (PROPERTY "child_order"))
    (org-todoist--sort-by-child-order section PROPERTY)
    (let ((prev -1)
          (sorted (org-element-map section 'headline (lambda (hl) (when (org-todoist--get-prop hl PROPERTY) hl)))))
      (dolist (next sorted)
        (should (> (org-todoist--get-position next PROPERTY) prev))
        (setq prev (org-todoist--get-position next PROPERTY))))))

(defun org-todoist--set-effort (NODE TASK)
  (when-let* ((duration (assoc-default 'duration TASK))
              (amount (assoc-default 'amount duration))
              (unit (assoc-default 'unit duration))
              (effortval (cond
                          ((string-equal unit "minute") amount)
                          ((string-equal unit "day") (* 1440 amount))
                          (t nil))))
    (org-todoist--add-prop NODE "EFFORT" (org-duration-from-minutes effortval))))

(ert-deftest org-todoist--test--set-effort ()
  (let ((hl (org-element-create 'headline '(:title "Headline" :level 1))))
    (org-todoist--set-effort hl test-task)
    (should (org-todoist--element-equals-str "* Headline
:PROPERTIES:
:EFFORT:   0:15
:END:
" hl))))

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


(ert-deftest org-todoist--test--set-todo ()
  (let ((task (org-todoist--test-task)))
    (org-todoist--set-todo task t)
    (should (cl-search "DONE" (org-todoist-org-element-to-string task)))
    (org-todoist--set-todo task nil)
    (should (cl-search "TODO" (org-todoist-org-element-to-string task)))))

(defun org-todoist--root (NODE)
  "Gets the syntax root of NODE"
  (let ((root NODE))
    (while-let ((next (org-element-parent root)))
      (setq root next))
    root))

(defun org-todoist--assign (NODE ID AST)
  (let* ((user (org-todoist--get-by-id org-todoist--collaborator-type ID AST))
         (name (org-todoist--get-prop user "full_name")))
    (org-todoist--add-tag NODE (concat  "@" (string-replace " " "_" name)))))

(ert-deftest org-todoist--test--assign ()
  (let ((node (org-todoist--test-task)))
    (org-todoist--assign node "34" (org-todoist--root node))
    (should (-contains-p (org-element-property :tags node) "@Austin_Kearns"))))

(defun org-todoist--add-tag (NODE TAG)
  "Adds a TAG to a NODE"
  (let ((tags (org-element-property :tags NODE)))
    (unless (member TAG tags)
      (push TAG tags))))

(defun org-todoist--archive (NODE)
  "Archives NODE via tag"
  (org-todoist--add-tag NODE "ARCHIVE"))

(ert-deftest org-todoist--test--archived ()
  (let ((node (org-todoist--test-task)))
    (org-todoist--archive node)
    (should (-contains-p (org-element-property :tags node) "ARCHIVE"))
    ;; verify the round trip too
    (with-temp-buffer (insert (org-todoist-org-element-to-string node))
                      (let* ((ast (org-element-parse-buffer))
                             (task (org-todoist--get-by-id org-todoist--task-type "3" ast))
                             (tags (org-element-property :tags task)))
                        (should (-contains-p tags "ARCHIVE"))))))

(ert-deftest org-todoist--test--get-or-create-node ()
  (let* ((node (org-todoist--test-task))
         (section (org-todoist--get-parent-of-type org-todoist--section-type node t))
         (updated (org-todoist--get-or-create-node section org-todoist--task-type (org-todoist--get-prop node "id") "updated" "updated" test-task))
         (newelem (org-todoist--get-or-create-node section org-todoist--task-type "2995104339" "new task" "My new desc" test-task)))
    (should newelem)
    (should (eq (org-element-property :parent newelem) section))
    (should updated)
    (should (eq updated node))
    (should (eq (org-element-property :parent updated) section))
    (should (string-equal-ignore-case "updated\n" (org-todoist--description-text updated)))
    (should (string-equal-ignore-case "updated" (org-todoist--get-prop-elem updated :title)))))

(defun org-todoist--get-description-elements (NODE)
  "Gets all paragraph elements that are not in a drawer."
  (org-element-map NODE t #'org-todoist--is-description-element nil nil 'drawer))

(defun org-todoist--is-description-element (NODE)
  "t if the NODE is a paragraph element and is not in a drawer."
  ;; TODO this doesn't seem to be working properly. Doesn't filter out items in drawers.
  (when (eq (org-element-type NODE) 'paragraph)
    (let ((closest-hl-or-drawer (org-element-lineage-map NODE #'identity '('drawer 'headline) nil t)))
      (unless (eq 'drawer (org-element-type closest-hl-or-drawer)) NODE))))

(defun org-todoist--description-text (NODE)
  "Combines all paragraphs under NODE and returns the concatenated string"
  (mapconcat #'org-todoist-org-element-to-string (org-todoist--get-description-elements NODE)))

(ert-deftest org-todoist--test--description-text ()
  (should (string-equal-ignore-case "Description

More description
" (org-todoist--description-text (org-todoist--test-task)))))

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
  (save-window-excursion (if (not (file-exists-p (org-todoist-file)))
                             (create-file-buffer (org-todoist-file)))
                         (find-file (org-todoist-file))
                         (erase-buffer)
                         (insert (org-todoist-org-element-to-string AST))
                         (save-buffer)))

(defun org-todoist--file-ast ()
  "Parses the AST of the org-todoist-file"
  (save-window-excursion
    (let ((created (not (file-exists-p (org-todoist-file)))))
      (when created (create-file-buffer (org-todoist-file)))
      (find-file (org-todoist-file))
      (when created (erase-buffer) (org-todoist--insert-header))
      (org-element-parse-buffer))))

(ert-deftest org-todoist--test-update-file ()
  "Tests insertion of org syntax from abstract syntax tree into org-todoist-file"
  (org-todoist--update-file (org-todoist--generate-ast test-org-str))
  (with-file-contents! (org-todoist-file) (should (equal (org-todoist--remove-ws test-org-str) (org-todoist--remove-ws (buffer-string))))))

                                        ;Find node;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--find-node (QUERY AST)
  "Return the first headling in the syntax tree AST which matches QUERY.
QUERY takes a single argument which is the current node."
  (org-element-map AST 'headline (lambda (node) (when (funcall QUERY node) node)) nil t))

(defun org-todoist--get-by-id (TYPE ID AST)
  "Finds the first item of TODOIST_TYPE TYPE with id ID in the syntax tree AST."
  (org-element-map AST 'headline (lambda (hl) (when (and (or (null TYPE) (string-equal (org-todoist--get-prop hl org-todoist--type) TYPE))
                                                         (string-equal (org-todoist--get-prop hl "id") ID))
                                                hl))
                   nil t))

(defun org-todoist--get-by-tempid (ID TID_MAPPING AST PROPS SKIP)
  "Finds the item in AST corresponding to the given ID using the headlines TEMP_ID property
and the TID_MAPPING. Add PROPS not including SKIP to found node."
  (let* ((id (cl-dolist (elem TID_MAPPING)
               (when (equal ID (cdr elem))
                 (cl-return (car elem)))))
         (elem (org-element-map AST 'headline (lambda (hl)
                                                (let ((prop (org-element-property :TEMP_ID hl)))
                                                  (when (and prop (string-equal prop id)) hl)))
                                nil t)))
    (when elem (org-todoist--add-all-properties elem PROPS SKIP) elem)))

(defun org-todoist--get-project-id-position (NODE)
  "Gets the ID of the project headline the NODE is under."
  (org-todoist--get-prop (org-todoist--get-parent-of-type org-todoist--project-type NODE t) "ID"))

(defun org-todoist--get-section-id-position (NODE)
  "Gets the ID of the section headline the NODE is under."
  (org-todoist--get-prop (org-todoist--get-parent-of-type org-todoist--section-type NODE t) "ID"))

(defun org-todoist--get-task-id-position (NODE)
  "Gets the ID of the section headline the NODE is under."
  (org-todoist--get-prop (org-todoist--get-parent-of-type org-todoist--task-type NODE t) "ID"))

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

(ert-deftest org-todoist--test--get-parent ()
  "Tests getting the parent node of a specific type"
  (let* ((ast (org-todoist--generate-sample-ast))
         (task (org-todoist--test-task))
         (project (org-todoist--get-parent-of-type org-todoist--project-type task t)))
    (should (equal (org-todoist--get-prop project "ID") "1"))))

(defun org-todoist--unimplemented () (error "Unimplemented feature!"))

                                        ;Property get/set;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--insert-identifier (NODE IDENTIFIER)
  "Inserts a property into the node's property drawer identifying the org-todoist i
node. Returns the drawer element."
  (let ((drawer (org-todoist--get-property-drawer NODE)))
    (if drawer (org-todoist--add-prop drawer org-todoist--type IDENTIFIER)
      (setq drawer (org-element-create 'property-drawer nil
                                       (org-element-create 'node-property `(:key ,org-todoist--type :value ,IDENTIFIER))))
      (org-element-adopt NODE drawer))
    drawer))

(ert-deftest org-todoist--test--insert-identifier ()
  "Tests adding the org-todoist--type property to a property drawer, creating it if necessary"
  (let* ((astwithout (org-todoist--generate-ast "* Headline\n"))
         (astwith (org-todoist--generate-ast "* Headline
:PROPERTIES:
:MYPROP:  VALUE
:END:
")))
    (org-todoist--insert-identifier astwith "TASK")
    (org-todoist--insert-identifier astwithout "TASK")
    (should (org-todoist--element-equals-str  "* Headline
:PROPERTIES:
:TODOIST_TYPE:  TASK
:END:
" astwithout))
    (should (org-todoist--element-equals-str  "* Headline
:PROPERTIES:
:MYPROP:  VALUE
:TODOIST_TYPE:  TASK
:END:
" astwith))))

(defun org-todoist--create-property (KEY VALUE) (org-element-create 'node-property `(:key ,KEY :value ,VALUE)))

(defun org-todoist--property-exists (NODE KEY)
  (unless (equal (org-element-type NODE) 'property-drawer)
    (error "Can only be called on property drawer elements")
    (org-element-map NODE 'node-property (lambda (prop) (org-todoist--is-property prop KEY)))))

(defun org-todoist--is-property (NODE KEY)
  (when (equal (org-element-type NODE) 'node-property)
    (let ((prop (org-element-property :key NODE)))
      (string-equal-ignore-case (if (not (stringp prop)) (prin1-to-string prop) prop) (if (stringp KEY) KEY (symbol-name KEY))))))

(ert-deftest org-todoist--test--is-property ()
  "Verifies detecting existing properties"
  (let* ((ast (org-todoist--generate-ast test-org-str-property-added))
         (drawer (org-todoist--get-property-drawer ast)))
    (let (result)
      (dolist (prop (org-element-map drawer 'node-property #'identity) result)
        (push (org-todoist--is-property prop "NEWPROP") result))
      (should (equal result '(t nil))))))

(defun org-todoist--get-property-drawer (NODE) (org-element-map NODE 'property-drawer #'identity nil t))

(ert-deftest org-todoist--test--get-property-drawer--exists-gets ()
  (let ((drawer (org-todoist--get-property-drawer (org-todoist--generate-ast test-org-str))))
    (should (org-element-type-p drawer 'property-drawer))))

(ert-deftest org-todoist--test--put-node-attribute ()
  (let* ((ast (org-todoist--generate-sample-ast))
         (hl (org-todoist--get-by-id org-todoist--task-type "13" ast)))
    (org-todoist--put-node-attribute hl "testattr" "MYVAL")
    (should (string-equal (org-todoist--get-node-attribute hl "testattr") "MYVAL"))))

(defun org-todoist--add-prop (DRAWER KEY VALUE)
  "Adds property with KEY and VALUE to property-drawer DRAWER.
If DRAWER is another node type, create and adopt a new property drawer.
Replaces the current value with VALUE if property KEY already exists."
  (unless (stringp VALUE) (setq VALUE (prin1-to-string VALUE)))
  (let ((type (org-element-type DRAWER)))
    (cond ((eq type 'property-drawer) ;; Add directly
           (org-todoist--set-prop-in-place DRAWER KEY VALUE)
           DRAWER)
          ((eq type 'headline) ;; Find drawer and update both drawer and property plist
           (let* ((headline DRAWER)
                  (drawer (org-todoist--get-property-drawer headline)))
             (org-todoist--put-node-attribute headline (org-todoist--to-symbol KEY) VALUE) ;; update property plist
             (unless drawer ;; create drawer if needed
               (setq drawer (org-element-create 'property-drawer))
               (org-element-adopt DRAWER drawer))
             (org-todoist--set-prop-in-place drawer KEY VALUE)
             drawer))
          (t (signal 'todoist--error "Called org-todoist--add-prop with invalid type")))))

(defun org-todoist--get-prop (NODE KEY)
  "Retrieves the property KEY from the property drawer directly under NODE.
Returns nil if not present"
  (let ((drawer (if (equal (org-element-type NODE) 'property-drawer)
                    NODE
                  (org-todoist--get-property-drawer NODE)))
        (retval nil))
    (setq retval (car (org-element-map drawer 'node-property (lambda (np) (when (org-todoist--is-property np KEY) (org-element-property :value np))))))
    retval))

(ert-deftest org-todoist--test--get-prop ()
  "Retrieves the property KEY from the property drawer directly under node.
Returns nil if not present"
  (should-not (org-todoist--get-prop (org-todoist--generate-ast "* Headline") "MYPROP"))
  (should (string-equal-ignore-case "test" (org-todoist--get-prop (org-todoist--generate-ast test-org-str) "MYPROP"))))

(defun org-todoist--set-prop-in-place (DRAWER KEY VALUE)
  "Checks for property KEY in DRAWER and replaces with VALUE if the key is present."
  (when (not (eq (org-element-type DRAWER) 'property-drawer)) (error "Expected property drawer"))
  (let ((existing (org-element-map DRAWER 'node-property
                    (lambda (prop) (when (org-todoist--is-property prop KEY) prop)) nil t)))
    (if existing
        (org-element-put-property existing (org-todoist--to-symbol KEY) VALUE)
      (org-element-adopt DRAWER (org-todoist--create-property KEY VALUE)))))

(ert-deftest org-todoist--test--add-prop--does-not-exist-works ()
  "Tests if adding a property works both when a property already exists."
  (let* ((ast (org-todoist--generate-ast test-org-str))
         (drawer (org-todoist--get-property-drawer ast)))
    (should drawer)
    (org-todoist--add-prop drawer "NEWPROP" "VALUE")
    (should (org-todoist--element-equals-str test-org-str-property-added ast))))

(ert-deftest org-todoist--test--add-prop-headline ()
  "Tests if adding a property works both when a property already exists."
  (let* ((ast (org-todoist--generate-sample-ast))
         (hl (org-todoist--get-by-id org-todoist--task-type "13" ast))
         (drawer (org-todoist--get-property-drawer hl)))
    (org-todoist--add-prop hl "NEWPROP" "VALUE")
    (should (org-element-map drawer 'node-property
              (lambda (prop) (when (org-todoist--is-property prop "NEWPROP") t))))
    (should (string-equal (org-todoist--get-node-attribute hl "NEWPROP") "VALUE"))))

(ert-deftest org-todoist--test--add-prop--exists-changes-headline ()
  "Tests if adding a property works both when a property already exists."
  (let* ((ast (org-todoist--generate-ast test-org-str-property-added))
         (hl (org-element-map ast 'headline #'identity nil t)))
    (org-todoist--add-prop hl "NEWPROP" "NEWVALUE")
    (org-todoist--add-prop hl "NEWPROP" "NEWVALUE")
    (org-todoist--add-prop hl "NEWPROP" "NEWVALUE")
    (should (org-todoist--element-equals-str (string-replace "VALUE" "NEWVALUE" test-org-str-property-added) ast))))

(ert-deftest org-todoist--test--add-prop--exists-changes ()
  "Tests if adding a property works both when a property already exists."
  (let* ((ast (org-todoist--generate-ast test-org-str-property-added))
         (drawer (org-todoist--get-property-drawer ast)))
    (org-todoist--add-prop drawer "NEWPROP" "NEWVALUE")
    (should (org-todoist--element-equals-str (string-replace "VALUE" "NEWVALUE" test-org-str-property-added) ast))))

(defun org-todoist--add-all-properties (NODE PROPERTIES &optional SKIP)
  "Adds or updates the values of all properties in the alist PROPERTIES to
NODE unless they are in plist SKIP. RETURNS the mutated NODE."
  (dolist (kv PROPERTIES)
    (unless (member (car kv) SKIP)
      (org-todoist--add-prop NODE (car kv) (cdr kv))))
  NODE)

(ert-deftest org-todoist--test--add-all-properties ()
  "Tests adding a properties alist to a node"
  (should (string-equal-ignore-case
           (org-todoist-org-element-to-string-no-ws
            (org-todoist--add-all-properties (org-element-create 'headline '(:title "Headline" :level 1))
                                             test-full-date))
           (org-todoist--remove-ws test-full-date-headline-text))))

(defun org-todoist--get-todoist-type (NODE)
  "Gets the TODOIST_TYPE of a NODE."
  ;; (org-element-property :TODOIST_TYPE NODE)) ;; doesn't work with newly added
  (org-todoist--get-prop NODE org-todoist--type))

(defun org-todoist--get-prop-elem (NODE KEY)
  "Gets the org-element property KEY of NODE."
  (org-todoist-org-element-to-string (org-element-property KEY NODE)))

                                        ;Test utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-to-string (file)
  "Read FILE as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-todoist--remove-ws-and-newline (STRING) (string-replace "\n" ""(string-replace " " "" STRING))) ;; TODO replace-regexp
(defun org-todoist--remove-ws (STRING) (string-replace " " "" STRING))

(ert-deftest org-todoist--test-add-prop-no-drawer ()
  (should (string-equal-ignore-case (org-todoist--remove-ws "* Headline\n:PROPERTIES:\n:PROP:  VALUE\n:END:\n")
                                    (org-todoist--remove-ws (org-todoist-org-element-to-string (org-todoist--add-prop (org-todoist--generate-ast "* Headline\n") "PROP" "VALUE"))))))

(defun org-todoist--generate-ast (contents) (org-todoist--invoke-with-buffer contents #'org-element-parse-buffer))

(defun org-todoist-org-element-to-string (DATA)
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (substring-no-properties (org-element-interpret-data DATA)))

(defun org-todoist-org-element-to-string-no-ws (DATA)
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (org-todoist--remove-ws (substring-no-properties (org-element-interpret-data DATA))))

(ert-deftest org-todoist--test--org-element-to-string ()
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (should (string-equal-ignore-case "* Headline\n" (org-todoist-org-element-to-string (org-todoist--generate-ast "* Headline")))))

(ert-deftest org-todoist--test--org-element-to-string-2 ()
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (should (string-equal-ignore-case (org-todoist--remove-ws test-org-str)
                                    (org-todoist--remove-ws (substring-no-properties (org-element-interpret-data (org-todoist--generate-ast test-org-str)))))))

(defun org-todoist--element-equals-str (STR DATA)
  "Returns t if the org string from org-element-interpret-data for DATA is STR."
  (string-equal-ignore-case (org-todoist--remove-ws STR) (org-todoist--remove-ws (org-todoist-org-element-to-string DATA))))

(ert-deftest org-todoist--test--element-equals-str--expected ()
  (org-todoist--element-equals-str "* Headline\n" (org-todoist--generate-ast "* Headline\n")))

(defun org-todoist--invoke-with-buffer (CONTENTS FN)
  "Fills a temp buffer with CONTENTS, applies org-mode, and calls FN."
  (with-temp-buffer (insert CONTENTS) (org-mode) (funcall FN)))

(ert-deftest org-todoist--test--invoke-with-buffer-processes-correctly ()
  "Verifies the helper fn org-todoist--invoke-with-buffer acts on the correct contents"
  (should (string-equal "Headlin" (org-todoist--invoke-with-buffer "* Headline" (lambda () (substring (buffer-string) 2 -1))))))

;; TODO not hardcoded full path
(defun org-todoist--generate-sample-string ()
  (org-todoist-org-element-to-string (org-todoist--generate-sample-ast)))

(defun org-todoist--generate-sample-ast ()
  (org-todoist--generate-ast (file-to-string "/home/aus/projects/org-todoist/sample.org")))

(defun org-todoist--sample-response () (with-file-contents! "/home/aus/projects/org-todoist/api-call.json" (json-read)))

(defun org-todoist--test-task () (org-todoist--get-by-id org-todoist--task-type "3" (org-todoist--generate-sample-ast)))
(defun org-todoist--test-recurring-task () (org-todoist--get-by-id org-todoist--task-type "3333333" (org-todoist--generate-sample-ast)))
(defun org-todoist--test-task-section () (org-todoist--get-by-id org-todoist--section-type "555" (org-todoist--generate-sample-ast)))
(defun org-todoist--test-task-done () (org-todoist--get-by-id org-todoist--task-type "8888" (org-todoist--generate-sample-ast)))

                                        ;Test Data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar test-org-str  "#+title: Test
* headline
  :PROPERTIES:
  :MYPROP:  test
  :NEWPROP:  VALUE
  :END:

paragraph
")

(defvar test-org-str-property-added  "#+title: Test
* headline
  :PROPERTIES:
  :MYPROP:  test
  :NEWPROP:  VALUE
  :END:

paragraph
")

(defvar test-task
  '((id . "2995104339")
    (user_id . "2671355")
    (project_id . "2203306141")
    (content . "Buy Milk")
    (description . "")
    (priority . 1)
    (due)
    (parent_id)
    (child_order . 1)
    (section_id)
    (day_order . -1)
    (collapsed . :json-false)
    (labels .
      ["Food" "Shopping"])
    (added_by_uid . "2671355")
    (assigned_by_uid . "2671355")
    (responsible_uid)
    (checked . :json-false)
    (is_deleted . :json-false)
    (sync_id)
    (added_at . "2014-09-26T08:25:05.000000Z")
    (duration
     (amount . 15)
     (unit . "minute"))))

(defvar test-full-date
  '((date . "2016-12-01")
    (timezone)
    (string . "every day")
    (lang . "en")
    (is_recurring . t)))

(defvar test-full-date-headline-text
  "* Headline
:PROPERTIES:
:DATE: 2016-12-01
:TIMEZONE:
:STRING: every day
:LANG: en
:IS_RECURRING: t
:END:
")

(defvar test-floating-date-with-time
  '((date . "2016-12-0T12:00:00.000000")
    (timezone)
    (string . "every day at 12")
    (lang . "en")
    (is_recurring . t)))

(defvar test-floating-date-with-time-fixed-tz
  '((date . "2016-12-06T13:00:00.000000Z")
    (timezone . "Europe/Madrid")
    (string . "every day at 12")
    (lang . "en")
    (is_recurring . t)))

(defvar test-label
  '((id . 790748)
    (name . "Label1")
    (color . 30)
    (item_order . 0)
    (is_deleted . 0)
    (is_favorite . 0)))

(defvar test-user
  '((auto_reminder . 0)
    (avatar_big . "https://*.cloudfront.net/*_big.jpg")
    (avatar_medium . "https://*.cloudfront.net/*_medium.jpg")
    (avatar_s640 . "https://*.cloudfront.net/*_s640.jpg")
    (avatar_small . "https://*.cloudfront.net/*_small.jpg")
    (business_account_id . "1")
    (daily_goal . 15)
    (date_format . 0)
    (dateist_lang)
    (days_off .
              [6 7])
    (email . "me@example.com")
    (feature_identifier . "2671355_0123456789abcdef70123456789abcdefe0123456789abcdefd0123456789abc")
    (features
     (beta . 1)
     (dateist_inline_disabled . :json-false)
     (dateist_lang)
     (gold_theme . t)
     (has_push_reminders . t)
     (karma_disabled . :json-false)
     (karma_vacation . :json-false)
     (restriction . 3))
    (full_name . "Example User")
    (has_password . t)
    (id . "2671355")
    (image_id . "d160009dfd52b991030d55227003450f")
    (inbox_project_id . "220474322")
    (is_biz_admin . :json-false)
    (is_premium . t)
    (joined_at . "2015-07-31T18:32:06.000000Z")
    (karma . 37504)
    (karma_trend . "up")
    (lang . "en")
    (next_week . 1)
    (premium_status . "current_personal_plan")
    (premium_until)
    (share_limit . 51)
    (sort_order . 0)
    (start_day . 1)
    (start_page . "project?id=2203306141")
    (team_inbox_id . "220474455")
    (theme_id . "11")
    (time_format . 0)
    (token . "0123456789abcdef0123456789abcdef01234567")
    (tz_info
     (gmt_string . "-03:00")
     (hours . -3)
     (is_dst . 0)
     (minutes . 0)
     (timezone . "America/Sao_Paulo"))
    (verification_status . "legacy")
    (weekend_start_day . 6)
    (weekly_goal . 30)))

(defvar test-project
  '((id . "2203306141")
    (name . "Shopping List")
    (color . "lime_green")
    (parent_id)
    (child_order . 1)
    (collapsed . :json-false)
    (shared . :json-false)
    (parent_id)
    (sync_id)
    (is_deleted . :json-false)
    (is_archived . :json-false)
    (is_favorite . :json-false)
    (view_style . "list")))

(defvar test-section '((id . "7025")
                       (name . "Groceries")
                       (project_id . "2203306141")
                       (section_order . 1)
                       (collapsed . :json-false)
                       (user_id . "2671355")
                       (sync_id)
                       (is_deleted . :json-false)
                       (is_archived . :json-false)
                       (archived_at)
                       (added_at . "2019-10-07T07:09:27.000000Z")))
