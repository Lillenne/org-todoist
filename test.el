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
(require 'org)
(require 'json)

                                        ;Config;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO likely don't want to override user configuration. Todoist has 4 priorities.
(setq org-priority-highest ?A
      org-priority-default ?C
      org-priority-lowest ?D)
(defvar url-http-end-of-headers nil)
(defvar org-todoist-api-token nil "The API token to use to sync with Todoist")
(defvar org-todoist-file "todoist.org")
(defvar org-todoist-user-headline "Collaborators")
(defvar org-todoist-project-headline "Projects")
(defvar org-todoist-show-n-levels 3
  "The number of headline levels to show. 2=projects, 3=sections, 4=root tasks, -1 no fold")
(defvar org-todoist-todo-keyword "TODO")
(defvar org-todoist-done-keyword "DONE")
(defun org-todoist--expand-from-dir (dir file)
  (if (file-name-absolute-p file)
      file
    (concat (expand-file-name dir) file)))
(defun org-todoist-file () (org-todoist--expand-from-dir org-directory org-todoist-file))

                                        ;Constants;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst org-todoist-resource-types '("projects" "labels" "items" "sections" "collaborators") "The list of resource types to sync")
(defconst org-todoist-sync-endpoint "https://api.todoist.com/sync/v9/sync")
(defconst org-todoist-request-type "application/x-www-form-urlencoded")
(defconst org-todoist-http-method "POST")
(defconst org-todoist--type "TODOIST_TYPE")
(defconst org-todoist--collaborator-type "USER")
(defconst org-todoist--section-type "SECTION")
(defconst org-todoist--project-type "PROJECT")
(defconst org-todoist--task-type "TASK")
(defconst org-todoist--user-node-type "USER_HEADLINE")
(defconst org-todoist--project-node-type "PROJECT_HEADLINE")
(defconst org-todoist--sync-areas ["collaborators", "projects", "items", "sections"])
(defconst org-todoist--project-skip-list `('name ,(intern "name"))) ;; the intern cmd fixes double prop adding. both are required tbd why
(defconst org-todoist--section-skip-list `('name ,(intern "name"))) ;; the intern cmd fixes double prop adding. both are required tbd why
(defconst org-todoist--task-skip-list `('name ,(intern "name") 'description ,(intern "description"))) ;; the intern cmd fixes double prop adding. both are required tbd why
(defconst org-todoist--sync-token-file "SYNC-TOKEN")
(defconst org-todoist--sync-buffer-file "SYNC-BUFFER")


                                        ;Data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar org-todoist--sync-token nil "The sync token string for peforming incremental sync")
(defvar org-todoist--storage-dir (concat (file-name-as-directory (xdg-cache-home))  "org-todoist") "Directory for org-todoist storage.")
(defvar org-todoist--sync-err nil "The error from the last sync, if any")
(defvar org-todoist-sync-interval 60 "The interval in seconds between sync requests with Todoist servers.")
(defun org-todoist--set-last-response (JSON) (with-file! (org-todoist--storage-file "PREVIOUS.json") (insert JSON)))
(defun org-todoist--storage-file (FILE) (concat (file-name-as-directory org-todoist--storage-dir) FILE))
(defun org-todoist--set-sync-token (TOKEN) (with-file! (org-todoist--storage-file org-todoist--sync-token-file) (erase-buffer) (insert TOKEN)))
(defun org-todoist--get-sync-token ()
  (if (file-exists-p (org-todoist--storage-file org-todoist--sync-token-file))
      (let ((res (with-file-contents! (org-todoist--storage-file org-todoist--sync-token-file) (get-last-line))))
        (if res res "*"))
    "*"))

(defun org-todoist--set-last-sync-buffer (AST) (with-file! (org-todoist--storage-file org-todoist--sync-buffer-file) (insert (org-todoist-org-element-to-string AST))))
(defun org-todoist--get-last-sync-buffer-ast () (with-file-contents! (org-todoist--storage-file org-todoist--sync-buffer-file) (org-mode) (org-element-parse-buffer)))

(defun get-last-line ()
  (save-excursion
    (goto-char (point-max))
    (thing-at-point 'line t)))

                                        ;API Requests;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist-sync ()
  (interactive)
  (org-todoist--do-sync (org-todoist--get-sync-token)))

(defun org-todoist-reset ()
  (interactive)
  (delete-file (org-todoist-file))
  (delete-file (org-todoist--storage-file org-todoist--sync-token-file))
  (delete-file (org-todoist--storage-file org-todoist--sync-buffer-file))
  (org-todoist--do-sync "*"))

(defun org-todoist--do-sync (TOKEN)
  (when (or (null org-todoist-api-token) (not (stringp org-todoist-api-token)))
    (error "No org-todoist-api-token API token set"))
  (setq org-todoist--sync-err nil)
  (let* ((url-request-method org-todoist-http-method)
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " org-todoist-api-token))
                                      ("Content-Type" . ,org-todoist-request-type)))
         (request-data `(("sync_token" . ,TOKEN) ("resource_types" . ,(json-encode org-todoist-resource-types))))
         (url-request-data (mm-url-encode-www-form-urlencoded request-data)))
    (url-retrieve org-todoist-sync-endpoint
                  (lambda (events)
                    (if (plist-member events :error)
                        (setq org-todoist--sync-err events)
                      (let ((resp (with-current-buffer (current-buffer)
                                    (goto-char url-http-end-of-headers)
                                    (decode-coding-region (point) (point-max) 'utf-8 t))))
                        (org-todoist--parse-response (json-read-from-string resp) (org-todoist--file-ast))))
                    (find-file (org-todoist-file))
                    (save-buffer)
                    (org-fold-hide-drawer-all)
                    (if (< org-todoist-show-n-levels 1)
                        (org-content)
                      (org-content org-todoist-show-n-levels)))
                  nil
                  'silent
                  'inhibit-cookies)))

(defun org-todoist--parse-response (RESPONSE AST)
  (let ((tasks (assoc-default 'items RESPONSE))
        (projects (assoc-default 'projects RESPONSE))
        (collab (assoc-default 'collaborators RESPONSE))
        (sections (assoc-default 'sections RESPONSE))
        (token (assoc-default 'sync_token RESPONSE)))
    (org-todoist--update-users collab AST)
    (org-todoist--update-projects projects AST)
    (org-todoist--update-sections sections AST)
    (org-todoist--update-tasks tasks AST)
    (org-todoist--set-sync-token token)
    (org-todoist--set-last-sync-buffer AST)
    (org-todoist--update-file AST)))

(ert-deftest org-todoist--test--parse-response ()
  (with-file-contents! "/home/aus/projects/org-todoist/api-call.json"
    (let ((resp (json-read)))
      ;; TODO actually test
      (org-todoist--parse-response resp (org-todoist--file-ast))))
  )

(defun org-todoist--insert-header ()
  (save-excursion
    (goto-char (point-min))
    (insert "#+title: Todoist
#+FILETAGS: :todoist:
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

(defun org-todoist--create-node (TYPE TEXT DESCRIPTION &optional PROPERTIES PARENT SKIP) ;; TODO priority? Other values?
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
                (org-element-set paragraph (org-element-create 'paragraph nil DESCRIPTION))
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

;; TODO method for syncing local changes. E.G assignee
(defun org-todoist-assign () (org-todoist--unimplemented))

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
    (when (org-todoist--has-date date) (org-timestamp-from-time (org-read-date nil t date nil)))))

(defun org-todoist--scheduled-date (TASK) (org-todoist--get-timestamp 'due TASK))

(defun org-todoist--deadline-date (TASK) (org-todoist--get-timestamp 'deadline TASK))

(defun org-todoist--get-timestamp (SYMBOL TASK)
  (let ((date (assoc-default 'date (assoc-default SYMBOL TASK))))
    (when (org-todoist--has-date date) (org-timestamp-from-time (org-read-date nil t date nil)))))

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
  (org-element-insert-before (org-todoist--create-planning TASK) (org-todoist--get-property-drawer HEADLINE)))
;; (org-element-adopt HEADLINE (org-todoist--create-planning TASK)))

(ert-deftest org-todoist--test--schedule ()
  (let ((task (json-read-file "sample-task.json"))
        (headline (org-element-create 'headline '(:title "Test Headline" :level 1 :todo-type 'todo :todo-keyword "TODO"))))
    ;; (timestamp (org-timestamp-from-time (org-read-date nil t "2016-12-0T12:00:00.000000" nil ))))
    (org-element-adopt headline (org-todoist--create-planning task))
    (should (org-todoist--element-equals-str "* TODO Test Headline
DEADLINE: <2017-01-06 Fri> SCHEDULED: <2016-12-06 Tue>
" headline))))

(defun org-todoist--update-tasks (TASKS AST)
  (cl-loop for data across TASKS do
           (let ((og (org-todoist--get-by-id org-todoist--task-type (assoc-default 'id data) AST))
                 (task nil)
                 (section (org-todoist--get-by-id org-todoist--section-type (assoc-default 'section_id data) AST))
                 (tags (assoc-default 'labels data))
                 (assignee (assoc-default 'responsible_uid data)) ;; TODO
                 )
             ;; check if we need to move sections
             (unless (cl-equalp (org-todoist--get-section-id-position task) (assoc-default 'section_id data))
               (org-element-extract task)
               (org-element-adopt section task))

             (setq task (if og og (org-todoist--create-node
                                   org-todoist--task-type
                                   (assoc-default 'content data)
                                   (assoc-default 'description data)
                                   data
                                   section
                                   org-todoist--task-skip-list)))

             (dotimes (i (length tags))
               (org-todoist--add-tag task (aref tags i)))
             (org-todoist--schedule task data)
             (org-todoist--set-effort task data)
             (org-todoist--set-priority task (assoc-default 'priority data))
             (org-todoist--set-todo task (assoc-default 'checked data)))))


;; ;; TODO don't do this all together, do it per node type for applicable. Some (archive, deleted) need to be here.
;; (defun org-todoist--add-prop-special (NODE PROPERTIES)
;;   (org-element-put-property updated :title TEXT)
;;   (org-todoist--replace-description updated DESCRIPTION)
;;   (let (
;;         (full_name (assoc-default 'full_name PROPERTIES)) ;; collaborator name
;;         (name (assoc-default 'name PROPERTIES)) ;; project or section name
;;         (is_deleted (assoc-default 'is_deleted PROPERTIES)) ;; project, section, or task removal
;;         (is_archived (assoc-default 'is_archived PROPERTIES)) ;; archive tag
;;         (section_id (assoc-default 'section_id PROPERTIES)) ;; task position ;; TODO
;;         (parent_id (assoc-default 'parent_id PROPERTIES)) ;; for subtask position ;; TODO
;;         (labels (assoc-default 'labels PROPERTIES)) ;; tags -- do in task fn ;; TODO
;;         (responsible_uid (assoc-default 'responsible_uid PROPERTIES)) ;; assignee tag, --assign -- do in task fn
;;         (checked (assoc-default 'checked PROPERTIES)) ;; set-todo -- do in task fn
;;         (content (assoc-default 'content PROPERTIES)) ;; NOT HERE
;;         (description (assoc-default 'description PROPERTIES)) ;; NOT HERE
;;         (priority (assoc-default 'priority PROPERTIES)) ;; set-priority - do in task fn
;;         (due (assoc-default 'due PROPERTIES)) ;; for tasks TODO !
;;         (child_order (assoc-default 'child_order PROPERTIES)) ;; for projects and tasks - sorting elsewhere
;;         (section_order (assoc-default 'section_order PROPERTIES))) ;; for sections, sorting elsewhere
;;     ;; TODO implement
;;     (org-todoist--unimplemented)
;;     )
;;   ;; labels, responsible_uid, checked, content (title), description, priority (1 & 4 swapped), due, child_order
;;   ;; :closed, :archived, :priority, :deadline

;;   ;; Full name - needed for user nodes
;;   )

(defun org-todoist--list-headlines ()
  (let ((headlines '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (push (org-element-property :raw-value headline) headlines)))
    (nreverse headlines)))

(defun org-todoist--get-headline-by-raw-value (NODE VALUE)
  (org-element-map NODE 'headline (lambda (hl) (when (string-equal (org-element-property :raw-value hl) VALUE) hl))))

(defun org-todoist--get-node-attributes (NODE) (cadr NODE))
(defun org-todoist--get-node-attribute (NODE ATTRIBUTE) (plist-get (org-todoist--get-node-attributes NODE)
                                                                   (if (stringp ATTRIBUTE)
                                                                       (intern (concat ":" (upcase ATTRIBUTE)))
                                                                     ATTRIBUTE)))

;; ;; Not working as a new plist is returned. Needs to mutate
(defun org-todoist--put-node-attribute (NODE ATTRIBUTE VALUE)
  (setcar (cdr NODE) (plist-put (org-todoist--get-node-attributes NODE)
                                (if (stringp ATTRIBUTE) (intern (concat ":" (upcase ATTRIBUTE))) ATTRIBUTE)
                                VALUE)))

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

(defun org-todoist--set-todo (NODE CHECKED)
  (if (eql t CHECKED) ;; :json-false for false currently, t for true
      (progn
        (org-element-put-property NODE :todo-keyword org-todoist-done-keyword)
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

;; (defun org-todoist--get-description-elements (NODE) (org-element-map NODE '('paragraph 'plain-text) #'identity nil nil 'plain-list)) ;; TODO this is failing in replace description with plain text included
(defun org-todoist--get-description-elements (NODE) (org-element-map NODE 'paragraph #'identity nil nil 'plain-list))

;; ;; TODO delete
;; (org-todoist-org-element-to-string (org-todoist--get-by-id org-todoist--task-type "3"  (org-element-parse-buffer)))
;; (org-element-contents (org-todoist--get-by-id org-todoist--task-type "3"  (org-element-parse-buffer)))
;; (org-element-map (org-todoist--get-by-id org-todoist--task-type "3"  (org-element-parse-buffer)) 'paragraph (lambda (p) (org-todoist-org-element-to-string p)) nil nil 'plain-list )
(defun org-todoist--description-text (NODE)
  "Combines all paragraphs under NODE and returns the concatenated string"
  (apply #'cl-concatenate 'string (org-element-map NODE 'paragraph (lambda (p) (org-todoist-org-element-to-string p)) nil nil 'plain-list)))
;; (apply #'concatenate 'string (org-element-map (org-todoist--get-by-id org-todoist--task-type "3"  (org-element-parse-buffer)) 'paragraph (lambda (p) (org-todoist-org-element-to-string p)) nil nil 'plain-list ))
;; merge paragraphs, retain plain list

(ert-deftest org-todoist--test--description-text ()
  (should (string-equal-ignore-case "Description

More description
"
                                    (org-todoist--description-text (org-todoist--test-task)))))

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

(defun org-todoist--node-type-query (NODE TYPE) (when (equal (org-todoist--get-prop NODE org-todoist--type) TYPE) NODE))
(defun org-todoist--update-file (AST)
  "Replaces the org-todoist file with the org representation AST"
  (save-window-excursion (if (not (file-exists-p (org-todoist-file)))
                             (create-file-buffer (org-todoist-file)))
                         (find-file (org-todoist-file))
                         (erase-buffer)
                         (insert (org-todoist-org-element-to-string AST))
                         (save-buffer)
                         ;; (kill-buffer)
                         ))

(defun org-todoist--file-ast ()
  "Parses the AST of the org-todoist-file"
  (save-window-excursion
    (let ((created (not (file-exists-p (org-todoist-file)))))
      (when created (create-file-buffer (org-todoist-file)))
      (find-file (org-todoist-file))
      (when created (org-todoist--insert-header))
      (org-element-parse-buffer))))

(ert-deftest org-todoist--test-update-file ()
  "Tests insertion of org syntax from abstract syntax tree into org-todoist-file"
  (org-todoist--update-file (org-todoist--generate-ast test-org-str))
  (with-file-contents! (org-todoist-file) (should (equal (org-todoist--remove-ws test-org-str) (org-todoist--remove-ws (buffer-string))))))

                                        ;Find node;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--find-node (QUERY AST)
  "Return the first node in the syntax tree AST which matches QUERY.
QUERY takes a single argument which is the current node."
  (org-element-map AST t (lambda (node) (when (funcall QUERY node) node)) nil t))

(defun org-todoist--get-by-id (TYPE ID AST)
  "Finds the first item of TODOIST_TYPE TYPE with id ID in the syntax tree AST"
  (org-element-map AST 'headline (lambda (hl) (when (and (string-equal (org-todoist--get-prop hl org-todoist--type) TYPE)
                                                         (string-equal (org-todoist--get-prop hl "id") ID))
                                                hl))
                   nil t))

(defun org-todoist--get-project-id-position (NODE)
  "Gets the ID of the project headline the NODE is under."
  (org-todoist--get-prop (org-todoist--get-parent-of-type org-todoist--project-type NODE t) "ID"))

(defun org-todoist--get-section-id-position (NODE)
  "Gets the ID of the section headline the NODE is under."
  (org-todoist--get-prop (org-todoist--get-parent-of-type org-todoist--section-type NODE t) "ID"))

(defun org-todoist--get-parent-of-type (TYPE NODE &optional FIRST)
  "Gets the parent(s) of NODE with TODOIST_TYPE TYPE. If FIRST, only get the first
matching parent."
  (org-element-lineage-map NODE
      (lambda (parent) (when (equal (org-todoist--get-todoist-type parent) TYPE)
                         parent)) 'headline nil FIRST))

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
      (string-equal-ignore-case (if (not (stringp prop)) (prin1-to-string prop) prop) (if (stringp KEY) KEY (symbol-name KEY)))))
  )
;; (cl-equalp (org-element-property :key NODE) (if (stringp KEY) KEY (symbol-name KEY)))))

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
          ;; (unless (org-todoist--set-prop-in-place DRAWER KEY VALUE)
          ;;   (org-element-adopt DRAWER (org-todoist--create-property KEY VALUE)))
          ;; DRAWER)
          ((eq type 'headline) ;; Find drawer and update both drawer and property plist
           (let* ((headline DRAWER)
                  (drawer (org-todoist--get-property-drawer headline)))
             (org-todoist--put-node-attribute headline KEY VALUE) ;; update property plist
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
        (org-element-put-property existing KEY VALUE)
      ;; (org-element-set prop (org-todoist--create-property KEY VALUE))
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
    ;; (unless (cl-member (car kv) SKIP)
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

;; input example for posting due dates "due": {"string":  "tomorrow"}

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
