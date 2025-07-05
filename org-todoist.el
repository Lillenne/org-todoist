;;; org-todoist.el --- Syncs Todoist tasks to org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Austin Kearns
;;
;; Author: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Maintainer: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Created: September 15, 2024
;; Modified: January 25, 2025
;; Version: 0.1.1
;; Keywords: calendar org todoist
;; Homepage: https://github.com/lillenne/org-todoist
;; Package-Requires: ((emacs "30.1") (s "1.13") (org "9.7.19") (ts "0.3") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; An Emacs package for bidirectional incremental sync of org-mode elements
;; with Todoist using org exactly how you normally would in Emacs org-mode.
;;
;; Commands are automatically detected and batched by diffing the current
;; `org-todoist-file' with the abstract syntax tree from a snapshot of the
;; previous sync and nodes are updated in place using a single asynchronous
;; request, meaning metadata (such as time tracking information and
;; additional properties) is retained. Syncing is done using the Todoist
;; sync API, which sends a single request for both pulling and pushing.
;;
;; Local changes will overwrite remote changes!
;;
;;; Code:

(require 'url)
(require 's)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 'org-element-ast)
(require 'ts)
(require 'dash)
(require 'json)
(require 'outline)
(require 'xdg)

                                        ;Configuration Variables;;;;;;;;;;;;;;;
(defvar url-http-end-of-headers) ;; defined in url, but emacs provides warning about free var when using

(defgroup org-todoist nil
  "Sync Todoist tasks to org mode and vice versa."
  :group 'external
  :prefix "org-todoist-"
  :link '(url-link "https://github.com/Lillenne/org-todoist"))

(defgroup org-todoist nil
  "Settings for org-todoist."
  :group 'org
  :prefix "org-todoist-")

(defcustom org-todoist-p1 ?A
  "The priority to map to Todoist P1.
This value must fall between `org-priority-lowest' and `org-priority-highest'."
  :type 'character
  :group 'org-todoist)

(defcustom org-todoist-p2 ?B
  "The priority to map to Todoist P2.
This value must fall between `org-priority-lowest' and `org-priority-highest'."
  :type 'character
  :group 'org-todoist)

(defcustom org-todoist-p3 ?C
  "The priority to map to Todoist P3.
This value must fall between `org-priority-lowest' and `org-priority-highest'."
  :type 'character
  :group 'org-todoist)

(defcustom org-todoist-p4 ?D
  "The priority to map to Todoist P4.
This value must fall between `org-priority-lowest' and `org-priority-highest'."
  :type 'character
  :group 'org-todoist)

(defcustom org-todoist-priority-default ?D
  "The default priority for Todoist tasks.
This value must fall between `org-priority-lowest' and `org-priority-highest'."
  :type 'character
  :group 'org-todoist)

(defcustom org-todoist-api-token nil
  "The API token to use to sync with Todoist."
  :type '(choice (const nil) string)
  :group 'org-todoist)

(defcustom org-todoist-delete-remote-items nil
  "Delete items on Todoist when deleted from the variable `org-todoist-file'.
WARNING items archived to sibling files will be detected as deleted!"
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-extract-deleted nil
  "Non-nil if remotely deleted items should be removed from the org file.

Otherwise, leave deleted items in tree but mark as ignored and canceled"
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-file "todoist.org"
  "The name of the todoist org file.
If relative, it is taken as relative to the org directory."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-url-scheme 'browser
  "Open Todoist links with this URL scheme.
Controls if `org-todoist-open-todoist-link' will open links with
the browser or the app."
  :type '(choice (const :tag "Open in browser" browser)
          (const :tag "Open in app" app))
  :group 'org-todoist)

(defcustom org-todoist-user-headline "Collaborators"
  "The name of the root collaborators metadata node."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-metadata-headline "Todoist Metadata"
  "The name of the Todoist metadata node."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-tz nil
  "The timezone to use when converting from Todoist time (UTC).
Currently this is ignored in favor of `current-time'."
  :type '(choice (const nil) string)
  :group 'org-todoist)

(defcustom org-todoist-lang "en"
  "The language for Todoist time strings."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-use-auto-reminder t
  "Whether new tasks should use your Todoist's default reminder."
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-infer-project-for-capture t
  "Whether to infer the active project in `org-capture'."
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-my-id nil
  "Your Todoist id.

Set this if your todoist full_name differs from symbol `user-full-name'.
The correct value can be found in the variable `org-todoist-file' under
Todoist Metadata > Collaborators > <your-name> as the property \"tid\"."
  :type '(choice (const nil) string)
  :group 'org-todoist)

(defcustom org-todoist-show-n-levels nil
  "The number of headline levels to show by default.
If visiting the todoist buffer when sync is called, it will attempt to
respect the visibility of the item at pos.
nil=do not adjust folds
2=projects,
3=sections,
4=root tasks,
5=root tasks + 1 level of subtasks
`no-fold' always fully expand everything except property drawers
`todo-tree' automatically apply `org-show-todo-tree'."
  :type '(choice (const :tag "Don't adjust folds" nil)
          (const :tag "Show projects" 1)
          (const :tag "Show sections" 2)
          (const :tag "Show root tasks" 3)
          (const :tag "Show root tasks + 1 level" 4)
          (const :tag "Show root tasks + 2 levels" 5)
          (const :tag "Show everything" no-fold)
          (const :tag "Show todo tree" todo-tree)
          (const :tag "I should setq to my desired number" nil))
  :group 'org-todoist)

(defcustom org-todoist-todo-keyword "TODO"
  "TODO keyword for active Todoist tasks."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-done-keyword "DONE"
  "TODO keyword for completed Todoist tasks."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-deleted-keyword "CANCELED"
  "TODO keyword for deleted Todoist tasks."
  :type 'string
  :group 'org-todoist)

(defcustom org-todoist-command-batch-size 75
  "The number of commands to send in a single Todoist sync request.
The official limit is 100, but this can be set lower to avoid issues.
100 caused 503 errors during development, so it is set to 75 by default."
  :type 'integer
  :group 'org-todoist)

(defcustom org-todoist-duration-as-timestamp nil
  "If non-nil, use timestamp for duration instead of effort."
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-comment-tag-user-pretty nil
  "Whether tagging users in comments should be pretty.

If non-nil use the org link representation for the comment tag so it
looks nice when viewed in org mode. Else use the Todoist markdown representation
so it looks nice in the Todoist app."
  :type 'boolean
  :group 'org-todoist)

(defcustom org-todoist-storage-dir (concat (file-name-as-directory (xdg-cache-home)) "org-todoist")
  "The directory for org-todoist storage.

If using multiple computers and a synced file solution,
this directory must be accessible on all PCs running the sync command."
  :type 'directory
  :group 'org-todoist)

(defun org-todoist-file ()
  "Gets the full path of the variable `org-todoist-file'."
  (expand-file-name org-todoist-file org-directory))

(defvar org-todoist--last-quick-task-url nil
  "Stores the url of the last quick task.")

(defvar org-todoist-background-sync-offset 30
  "Delay after calling `org-todoist-background-sync' for initial run.")

(defvar org-todoist-background-sync-interval 900
  "Number of seconds between background syncs.")

(defvar org-todoist--background-timer nil
  "Timer for running background syncs.")

(defvar org-todoist--api-version nil "Previously used API version. Detected by Todoist.")
                                        ;Constants;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst org-todoist--default-api-version 'v1)

(defconst org-todoist--api-version-prop "API")

(defconst org-todoist-resource-types '("projects" "notes" "labels" "items" "sections" "collaborators") "The list of resource types to sync.")

(defconst org-todoist-request-type "application/x-www-form-urlencoded; charset=utf-8" "The request type for Todoist sync requests.")

(defconst org-todoist-http-method "POST" "The http method for Todoist sync requests.")

(defconst org-todoist--type "TODOIST_TYPE" "The org property name for Todoist object type metadata.")

(defconst org-todoist--collaborator-type "USER" "The `org-todoist--type' value for collaborator objects.")

(defconst org-todoist--section-type "SECTION" "The `org-todoist--type' value for section objects.")

(defconst org-todoist--project-type "PROJECT" "The `org-todoist--type' value for project objects.")

(defconst org-todoist--default-id "default" "The `org-todoist--type' value for the default section.")

(defconst org-todoist--id-property "tid" "The `org-todoist--type' value for the default section.")

(defconst org-todoist--task-type "TASK" "The `org-todoist--type' value for task objects.")

(defconst org-todoist--user-node-type "USER_HEADLINE" "The `org-todoist--type' value for the collaborator metadata node.")

(defconst org-todoist--metadata-node-type "METADATA" "The `org-todoist--type' value for the root metadata node.")

(defconst org-todoist--ignored-node-type "IGNORE" "The `org-todoist--type' value for the root metadata node.")

(defconst org-todoist--default-section-name "Default Section")

(defconst org-todoist--sync-areas ["collaborators", "projects", "items", "sections"] "The types of Todoist items to sync.")

(defconst org-todoist--project-skip-list '(access description name color is_deleted is_favorite is_frozen sync_id view_style is_collapsed))

(defconst org-todoist--section-skip-list '(name sync_id updated_at is_deleted archived_at is_collapsed))

(defconst org-todoist--task-skip-list '(role name completed_at is_deleted content duration description checked deadline due labels priority project_id section_id sync_id day_order is_collapsed))

(defconst org-todoist--sync-token-file "SYNC-TOKEN")

(defconst org-todoist--sync-buffer-file "SYNC-BUFFER")

(defconst org-todoist--last-response-file "last-response.json")

(defconst org-todoist--last-request-file "last-request.json")

                                        ;Debug data;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar org-todoist--sync-err nil "The error from the last sync, if any.")
(defvar org-todoist-keep-old-sync-tokens nil)

(defun org-todoist--set-last-response (JSON)
  "Store the last Todoist response `JSON' to a file."
  (with-temp-file (org-todoist--storage-file org-todoist--last-response-file)
    (insert JSON)))

(defun org-todoist--get-last-response ()
  "Get the contents of `org-todoist--last-response-file' as a pretty JSON string."
  (when-let ((file (org-todoist--storage-file org-todoist--last-response-file))
             (exists (file-exists-p file)))
    (with-temp-buffer (insert-file-contents file)
                      (json-pretty-print-buffer)
                      (buffer-string))))

(defun org-todoist--set-last-request (REQUEST)
  "Store the last Todoist request `REQUEST' to a file."
  (with-temp-file (org-todoist--storage-file org-todoist--last-request-file)
    (insert REQUEST)))

(defun org-todoist--get-last-request ()
  "Store the last Todoist request `REQUEST' to a file."
  (when-let ((file (org-todoist--storage-file org-todoist--last-request-file))
             (exists (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((decoded (url-unhex-string (buffer-substring-no-properties (point-min) (point-max))))
             (params (url-parse-query-string decoded))
             (formatted (mapconcat
                         (lambda (param)
                           (let* ((key (car param))
                                  (value (cdr param))
                                  (parsed-value (cond
                                                 ((member key '("resource_types" "commands"))
                                                  (json-read-from-string (car value)))
                                                 (t value))))
                             (concat (json-encode key) ": "
                                     (json-encode parsed-value))))
                         params
                         ",\n")))
        (erase-buffer)
        (insert "{\n" formatted "\n}"))
      (json-pretty-print-buffer)
      (buffer-string))))

(defun org-todoist--get-file-string (FILE)
  "Get the contents of `org-todoist-storage-file' `FILE' as a string."
  (if-let* ((file (org-todoist--storage-file FILE))
            (file-exists-p file))
      (with-temp-buffer (insert-file-contents file)
                        (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-todoist--is-current-api (&optional AST)
  "Check if the API version of the `org-todoist-file' is current."
  (eq (org-todoist--api-version AST) org-todoist--default-api-version))

(defun org-todoist--api-version (&optional AST)
  "Return the Todoist API version. Use parsed syntax tree `AST' if available."
  (setq org-todoist--api-version
        (or org-todoist--api-version
            ;; AST supplied, check for version in metadata node
            (when AST
              (let* ((md (org-todoist--metadata-node AST))
                     (vers (org-todoist--get-prop md org-todoist--api-version-prop)))
                ;; If version is set in metadata, use it
                (or (unless (or (null vers) (string-blank-p vers))
                      (intern vers))
                    ;; If no version set, check for v9
                    (when (org-todoist--is-v9 AST)
                      'sync-v9))))
            ;; otherwise try to find the version in the file
            (and (file-exists-p (org-todoist-file))
                 (or (with-todoist-buffer!
                      ;; quicker regexp search
                      (goto-char (point-min))
                      (when (and (re-search-forward (format org-complex-heading-regexp-format
                                                            (regexp-quote org-todoist-metadata-headline))
                                                    nil t)
                                 (not (s-blank? (org-entry-get nil org-todoist--api-version-prop))))
                        (intern (org-entry-get nil org-todoist--api-version-prop))))
                     ;; fallback to AST
                     (let ((vers (org-todoist--get-prop
                                  (org-todoist--metadata-node
                                   (setq AST (org-todoist--file-ast)))
                                  org-todoist--api-version-prop)))
                       (unless (or (null vers) (string-blank-p vers))
                         (intern vers)))
                     ;; No API version found, check for v9
                     (when (org-todoist--is-v9 AST)
                       'sync-v9)))
            ;; no todoist file or version found
            org-todoist--default-api-version)))

                                        ;Hooks;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--item-close-hook ()
  "Send an item_close request for TODO at point."
  (defvar org-state) ; silence byte compilation warning for org-state, which will already be defined here
  (when (and (equal org-state org-todoist-done-keyword)
             (org-todoist--task-is-recurring (org-element-resolve-deferred (org-element-at-point)) t))
    (when-let* ((id (org-entry-get nil org-todoist--id-property))
                (args `(("id" . ,id)))
                (request-data `(("sync_token" . ,(org-todoist--get-sync-token))
                                ("commands" . ((("type" . "item_close")
                                                ("uuid" . ,(org-id-uuid))
                                                ("args" . ,args)))))))
      (setq org-todoist--sync-err nil)
      (unless (org-todoist--is-current-api)
        (user-error "Org todoist has updated API versions! Run 'org-todoist-migrate-to-v1' and set 'org-todoist-use-v1-api' to 't' to continue using org-todoist"))
      (org-todoist--api-call
       request-data
       (lambda (response)
         (if response
             (progn (when-let ((token (assoc-default 'sync_token response)))
                      (org-todoist--set-sync-token token))
                    (org-todoist--set-last-response response))
           (message "item_close hook failed. See org-todoist--sync-err for details.")))))))

;; NOTE this requires it to be done from emacs and not eg. orgzly
(add-hook 'org-after-todo-state-change-hook #'org-todoist--item-close-hook)

(add-to-list 'org-fold-show-context-detail '(todoist . lineage))

                                        ;Org-capture integration;;;;;;;;;;;;;;;
(defun org-todoist--sync-after-capture ()
  "Syncs with todoist after any captures to the variable `org-todoist-file'."
  (unless org-note-abort
    (save-excursion
      (save-window-excursion
        (let ((inhibit-message t))
          (org-capture-goto-last-stored))
        (when (string= (buffer-file-name) (org-todoist-file))
          (org-todoist-sync t))))))

;;;###autoload
(defun org-todoist-project-notes ()
  "Find or create an ignored \"Notes\" heading under a Todoist project."
  (let* ((headlines '("Notes"))
         (ast (org-todoist--file-ast))
         (projects (org-todoist--project-nodes ast))
         (project-names (--map (org-element-property :raw-value it) projects))
         (selected-project (if (and org-todoist-infer-project-for-capture
                                    (require 'projectile nil 'no-error)
                                    (fboundp 'projectile-project-name))
                               (if (string= (projectile-project-name) "-")
                                   (completing-read "Which project? " project-names)
                                 (projectile-project-name))
                             (completing-read "Which project? " project-names)))
         (selected-project-element (--first (string= (org-element-property :raw-value it) selected-project) projects)))
    (push (if (s-blank? selected-project) "Inbox" selected-project) headlines)
    (while-let ((parent-proj (org-todoist--get-parent-of-type org-todoist--project-type selected-project-element t)))
      (push (org-todoist--get-title parent-proj) headlines)
      (setq selected-project-element parent-proj))
    (set-buffer (org-capture-target-buffer (org-todoist-file)))
    (goto-char (point-min))
    (org-todoist--capture-ensure-heading headlines)
    (org-todoist-ignore-subtree)))

(defun org-todoist--capture-ensure-heading (headings &optional initial-level)
  "Starting at `INITIAL-LEVEL', ensures `HEADINGS' are created in the buffer.

This function has been taken from Doom Emacs
\(https://github.com/doomemacs/doomemacs), which is licensed under
the MIT license.

The MIT License (MIT)

Copyright (c) 2014-2024 Henrik Lissner.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
\"Software\"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
  (if (not headings)
      (widen)
    (let ((initial-level (or initial-level 1)))
      (if (and (re-search-forward (format org-complex-heading-regexp-format
                                          (regexp-quote (car headings)))
                                  nil t)
               (= (org-current-level) initial-level))
          (progn
            (beginning-of-line)
            (org-narrow-to-subtree))
        (goto-char (point-max))
        (unless (and (bolp) (eolp)) (insert "\n"))
        (insert (make-string initial-level ?*)
                " " (car headings) "\n")
        (beginning-of-line 0))
      (org-todoist--capture-ensure-heading (cdr headings) (1+ initial-level)))))

;;;###autoload
(defun org-todoist-find-project-and-section ()
  "Find or create headlines for an `org-capture'.
Prompts for or auto-determines \(see `org-todoist-infer-project-for-capture')
the Todoist project, section, and optionally parent task."
  (unless org-note-abort
    (let* ((headlines nil)
           (ast (org-todoist--file-ast))
           (projects (org-todoist--project-nodes ast))
           (project-names (--map (org-element-property :raw-value it) projects))
           (selected-project (if (and org-todoist-infer-project-for-capture
                                      (require 'projectile nil 'no-error)
                                      (fboundp 'projectile-project-name))
                                 (if (string= (projectile-project-name) "-")
                                     (completing-read "Which project? " project-names)
                                   (projectile-project-name))
                               (completing-read "Which project? " project-names)))
           (selected-project-element (--first (string= (org-element-property :raw-value it) selected-project) projects))

           (sections (when selected-project-element (org-todoist--get-sections selected-project-element)))
           (section-names (--map (org-element-property :raw-value it) sections))
           (selected-section (completing-read "Which section? " (or section-names `(,org-todoist--default-section-name))))
           (selected-section-element (--first (string= (org-element-property :raw-value it) selected-section) sections))

           (tasks (when selected-section-element (org-todoist--get-tasks-all selected-section-element)))
           (task-names (--map (org-element-property :raw-value it) tasks))
           (selected-task (when task-names (completing-read "Which parent task? " task-names)))
           (selected-task-element (--first (string= (org-element-property :raw-value it) selected-task) tasks)))
      (unless (s-blank? selected-task) (push selected-task headlines)
              (while-let ((parent-task (org-todoist--get-parent-of-type org-todoist--task-type selected-task-element t)))
                (push (org-todoist--get-title parent-task) headlines)
                (setq selected-task-element parent-task)))
      (push (if (s-blank? selected-section) org-todoist--default-section-name selected-section) headlines)
      (push (if (s-blank? selected-project) "Inbox" selected-project) headlines)
      (while-let ((parent-proj (org-todoist--get-parent-of-type org-todoist--project-type selected-project-element t)))
        (push (org-todoist--get-title parent-proj) headlines)
        (setq selected-project-element parent-proj))
      (set-buffer (org-capture-target-buffer (org-todoist-file)))
      (goto-char (point-min))
      (org-todoist--capture-ensure-heading headlines))))

(add-hook 'org-capture-after-finalize-hook #'org-todoist--sync-after-capture)

                                        ;Implementation;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--sync-endpoint ()
  "Return the correct Todoist API endpoint URL based on version."
  (cond
   ((eq (org-todoist--api-version) 'v1)
    "https://api.todoist.com/api/v1/sync")
   ((or (eq (org-todoist--api-version) 'sync-v9) )
    (error "Sync API v9 is deprecated! Run 'org-todoist-migrate-to-v1'"))))
;; "https://api.todoist.com/sync/v9/sync")))

(defun org-todoist--storage-file (FILE)
  "Determine full path of `FILE' relative to the `org-todoist-storage-dir'."
  (make-directory org-todoist-storage-dir t)
  (expand-file-name FILE org-todoist-storage-dir))

(defun org-todoist--set-sync-token (TOKEN)
  "Store that last Todoist sync `TOKEN'."
  (let ((file (org-todoist--storage-file org-todoist--sync-token-file)))
    (with-temp-file file
      (when (and org-todoist-keep-old-sync-tokens (file-exists-p file))
        (insert-file-contents file))
      (insert TOKEN)
      (insert "\n"))))

(defun org-todoist--get-sync-token ()
  "Get the previous Todoist sync `TOKEN'."
  (if (file-exists-p (org-todoist--storage-file org-todoist--sync-token-file))
      (let ((res (with-temp-buffer
                   (insert-file-contents (org-todoist--storage-file org-todoist--sync-token-file))
                   (goto-char (point-max))
                   (forward-line -1)
                   (thing-at-point 'line t))))
        (if res res "*"))
    "*"))

(defun org-todoist--set-last-sync-buffer (AST)
  "Store the last org syntax tree `AST'."
  (with-temp-file (org-todoist--storage-file org-todoist--sync-buffer-file)
    (org-mode)
    (insert (org-todoist-org-element-to-string AST))))

(defun org-todoist--get-last-sync-buffer-ast ()
  "Retrieve the last org syntax tree `AST'."
  (let ((file (org-todoist--storage-file org-todoist--sync-buffer-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-element-parse-buffer)))))

(defun org-todoist--get-comments (HEADLINE)
  "Get the elements under `HEADLINE' corresponding to Todoist comments."
  (org-element-map HEADLINE 'item
    (lambda (item)
      (when (and (eq (org-todoist--first-parent-of-type item 'headline) HEADLINE)
                 (org-todoist--is-note item))
        item))))

(defun org-todoist--is-note (ITEM)
  "If the `ITEM' is a note entry."
  (s-contains? "Note" (org-todoist--item-text ITEM)))

(defun org-todoist--item-text (ITEM)
  "Convert `ITEM' to its text representation."
  (org-todoist-org-element-to-string ITEM))

(defun org-todoist--note-text (ITEM)
  "Extract the Todoist comment out of an org note `ITEM'."
  (let ((text (org-todoist--item-text ITEM)))
    (when (s-contains? "Note taken on [" text) (s-trim-right (s-join "\n" (--map (s-trim it) (cdr (s-lines text))))))))

(defun org-todoist--get-comments-text (HEADLINE)
  "Extract all Todoist comments under `HEADLINE'."
  (--map (org-todoist--note-text it) (org-todoist--get-comments HEADLINE)))

                                        ;API Requests;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--select-user (TEXT)
  "Interactively select a user from the list of collaborators with prompt `TEXT'."
  (if-let* ((userelements (org-todoist--all-users))
            (usernames (--map (org-element-property :raw-value it) userelements))
            (selected (completing-read TEXT usernames nil t))
            (selectedelement (--first (string= (org-element-property :raw-value it) selected) userelements)))
      selectedelement))

(defun org-todoist--all-users ()
  "Get all headlines representing Todoist collaborators."
  (org-element-map (org-element-contents (org-todoist--user-node (org-todoist--file-ast))) 'headline #'identity))

(defun org-todoist--encode (DATA)
  "Encode the Todoist sync API request alist `DATA'."
  (mapconcat
   (lambda (data)
     (if (null (cdr data)) "[]"
       (concat (car data) "="
               ;; Todoist API does not accept "&" within elements. It is only allowed
               ;; for separating key-value pairs in the x-www-form-urlencoded body.
               (string-replace "&" "%26"
                               (if (listp (cdr data))
                                   (json-encode (cdr data))
	                         (if (stringp (cdr data))
                                     (cdr data)
                                   (prin1-to-string (cdr data))))))))
   (-filter (lambda (item) (cdr item)) DATA)
   "&"))

(defun org-todoist--encode-item (ITEM)
  "Encode an `ITEM' \(e.g., commands\) of the Todoist sync API request."
  (if (json-alist-p (cdr ITEM))
      (concat (car ITEM)
              (org-todoist--encode-item ITEM))
    (json-encode ITEM)))

(defun org-todoist--label-default-sections (AST)
  "Detect and label the default Todoist sections in syntax tree `AST'."
  (dolist (section (org-todoist--get-sections AST))
    (when (string= (org-element-property :raw-value section) org-todoist--default-section-name)
      (org-todoist--insert-id section org-todoist--default-id))))

(defun org-todoist--api-call-curl (encoded-data callback-fn callback-args)
  "Make an API call using `curl'."
  (let ((process-connection-type nil) ; Use a pipe
        (proc-buffer (generate-new-buffer " *org-todoist-curl*"))
        (command `("curl" "-s" "-X" ,org-todoist-http-method
                   "-H" ,(concat "Authorization: Bearer " org-todoist-api-token)
                   "-H" ,(concat "Content-Type: " org-todoist-request-type)
                   "-d" ,encoded-data
                   ,(org-todoist--sync-endpoint))))
    (set-process-sentinel
     (apply #'start-process "org-todoist-curl" proc-buffer command)
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer (process-buffer process)
           (let ((exit-code (process-exit-status process)))
             (if (= exit-code 0)
                 (let* ((resp-body (buffer-string))
                        (response (json-read-from-string resp-body)))
                   (org-todoist--set-last-response resp-body)
                   (apply callback-fn (cons response callback-args)))
               (setq org-todoist--sync-err (format "curl exited with code %d: %s" exit-code (buffer-string)))
               (apply callback-fn (cons nil callback-args)))) ; Call with nil on error
           (kill-buffer (process-buffer process))))))))

(defun org-todoist--api-call-url-retrieve (encoded-data callback-fn callback-args &optional sync-request)
  "Make an API call using `url-retrieve'."
  (let ((url-request-method org-todoist-http-method)
        (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " org-todoist-api-token))
                                     ("Content-Type" . ,org-todoist-request-type)))
        (url-request-data encoded-data))
    (if sync-request
        (progn
          (message "Warning: Synchronous API call may block Emacs UI. Use C-g to abort if needed.")
          (condition-case err
              (let ((response-buffer (url-retrieve-synchronously (org-todoist--sync-endpoint) 'silent 'inhibit-cookies)))
                (with-current-buffer response-buffer
                  (goto-char url-http-end-of-headers)
                  (let* ((resp-str (decode-coding-region (point) (point-max) 'utf-8 t))
                         (response (json-read-from-string resp-str)))
                    (org-todoist--set-last-response resp-str)
                    (apply callback-fn (cons response callback-args)))))
            (error
             (setq org-todoist--sync-err (format "url-retrieve-synchronously failed: %S" err))
             (apply callback-fn (cons nil callback-args)))))
      (url-retrieve (org-todoist--sync-endpoint)
                    (lambda (status)
                      (if (plist-get status :error)
                          (progn
                            (setq org-todoist--sync-err (plist-get status :error))
                            (apply callback-fn (cons nil callback-args))) ; Call with nil on error
                        (with-current-buffer (current-buffer)
                          (goto-char url-http-end-of-headers)
                          (let* ((resp-str (decode-coding-region (point) (point-max) 'utf-8 t))
                                 (response (json-read-from-string resp-str)))
                            (org-todoist--set-last-response resp-str)
                            (apply callback-fn (cons response callback-args))))))
                    nil
                    'silent
                    'inhibit-cookies))))

(defun org-todoist--api-call (request-data callback-fn &optional callback-args sync-request)
  "Make an API call to Todoist sync endpoint.
SYNC-REQUEST forces synchronous execution and blocks Emacs. Use sparingly.
`request-data' is an alist of request parameters.
`callback-fn' is a function to call with the JSON response.
`callback-args' are additional arguments for the callback.
Uses `curl' if available, otherwise falls back to `url-retrieve'."
  (unless (eq (org-todoist--api-version) 'v1)
    (user-error "Sync API v9 is deprecated! Run 'org-todoist-migrate-to-v1'"))
  (let ((encoded-data (org-todoist--encode request-data)))
    (org-todoist--set-last-request encoded-data)
    (if (and (not sync-request) (executable-find "curl"))
        (org-todoist--api-call-curl encoded-data callback-fn callback-args)
      (org-todoist--api-call-url-retrieve encoded-data callback-fn callback-args))))

(defun org-todoist--do-background-sync ()
  "Sync with Todoist servers in the background."
  (if (string= (buffer-file-name) (org-todoist-file))
      (message "Skipping background sync with Todoist servers since buffer is active...")
    (org-todoist-sync t)))

(defun org-todoist--do-sync (TOKEN OPEN)
  "Diff the Todoist org file, perform the API request, and update the file.
`TOKEN' is the previous incremental sync token.
`OPEN' determines whether the Todoist buffer should be opened after sync."
  (when (or (null org-todoist-api-token) (not (stringp org-todoist-api-token)))
    (user-error "No org-todoist-api-token API token set"))
  (unless (org-todoist--is-current-api)
    (user-error "Org todoist has updated API versions! Run 'org-todoist-migrate-to-v1' to continue using org-todoist"))
  (setq org-todoist--sync-err nil)
  (let* ((cur-marker (point-marker))
         (ast (org-todoist--file-ast))
         (commands (if (equal TOKEN "*")
                       ;; For reset syncs, don't send any commands to preserve local changes
                       []
                     ;; Normal sync - diff against last state
                     (let ((old (org-todoist--get-last-sync-buffer-ast)))
                       (org-todoist--push ast old))))
         (request-data `(("sync_token" . ,TOKEN)
                         ("resource_types" . ,(json-encode org-todoist-resource-types))
                         ("commands" . ,commands))))
    (message (if OPEN "Syncing with todoist. Buffer will open when sync is complete..." "Syncing with todoist. This may take a moment..."))
    (if (equal TOKEN "*")
        ;; For full resets, don't batch - just send one request
        (org-todoist--api-call
         request-data
         (lambda (response open ast cur-marker)
           (when (and (equal TOKEN "*")
                      (eq (assoc-default 'full_sync response) :json-false))
             (error (setq org-todoist--sync-err "Full sync failed - Todoist returned full_sync=false")))
           (if open
               (progn (org-todoist--do-sync-callback response ast cur-marker)
                      (find-file (org-todoist-file)))
             (save-current-buffer (org-todoist--do-sync-callback response ast cur-marker))))
         `(,OPEN ,ast ,cur-marker))
      ;; For normal syncs, batch the commands
      (let ((batches (when commands (org-todoist--batch-commands commands))))
        (if (not batches)
            ;; No commands to send - just do a normal sync
            (org-todoist--api-call
             request-data
             (lambda (response open ast cur-marker)
               (if open
                   (progn (org-todoist--do-sync-callback response ast cur-marker)
                          (find-file (org-todoist-file)))
                 (save-current-buffer (org-todoist--do-sync-callback response ast cur-marker))))
             `(,OPEN ,ast ,cur-marker))
          ;; Process batches sequentially
          (cl-labels ((process-batch (batches-left sync-token temp-id-map)
                        (when batches-left
                          (let* ((current-batch (car batches-left))
                                 (batch-request-data `(("sync_token" . ,sync-token)
                                                       ("resource_types" . ,(json-encode org-todoist-resource-types))
                                                       ("commands" . ,(org-todoist--replace-temp-ids current-batch temp-id-map)))))
                            (message "Sending batch %s" (length batches-left))
                            (org-todoist--api-call
                             batch-request-data
                             (lambda (response _ ast _)
                               (if (null response)
                                   (message "Batch failed. See org-todoist--sync-err for details.")
                                 (when-let* ((parsed (org-todoist--parse-response response ast))
                                             (new-token (assoc-default 'sync_token response))
                                             (new-temp-ids (assoc-default 'temp_id_mapping response)))
                                   (if (cdr batches-left)
                                       (progn
                                         (message "Batch complete. %d remaining..." (length (cdr batches-left)))
                                         (process-batch (cdr batches-left) new-token (append temp-id-map new-temp-ids)))
                                     (message "Sync complete.")))))
                             `(,OPEN ,ast ,cur-marker))))))
            (process-batch batches TOKEN nil)
            (org-todoist--handle-display cur-marker)))))))

(defun org-todoist--do-sync-callback (response ast cur-marker)
  "The callback to invoke after syncing with the Todoist API.

`response' is the parsed JSON response from the API.
`AST' is the current abstract syntax tree of the local Todoist buffer.
`CUR-MARKER' is the marker at point when the sync was invoked."
  (if (null response)
      (message "Sync failed. See org-todoist--sync-err for details.")
    (when (org-todoist--parse-response response ast)
      (org-todoist--handle-display cur-marker))
    (message "Sync complete.")))

(defun org-todoist--get-todoist-buffer ()
  "Gets the org-todoist buffer, preferring open buffers."
  (let* ((file (org-todoist-file))
         (fb (find-buffer-visiting file)))
    (or fb (find-file-noselect file))))

(defmacro with-todoist-buffer! (&rest body)
  "Execute `BODY' with a buffer visiting `ORG-TODOIST-FILE' as current.
Automatically widens the buffer to ensure all content is accessible."
  `(with-current-buffer
       (org-todoist--get-todoist-buffer)
     (save-restriction
       (widen)
       ,@body)))

(defun org-todoist--handle-display (cur-marker)
  "Handle saving and folding of the Todoist buffer.

`CUR-MARKER' is the marker prior to the sync call."
  (with-current-buffer (org-todoist--get-todoist-buffer)
    (cond
     ((null org-todoist-show-n-levels)   ; do nothing
      nil)
     ((eq 'todo-tree org-todoist-show-n-levels)
      (org-show-todo-tree nil))
     ((eq org-todoist-show-n-levels 'no-fold)
      (org-fold-show-all))        ; show everything (headings and text)
     ((integerp org-todoist-show-n-levels)
      (if (< org-todoist-show-n-levels 1)
          (org-content 1)   ; top-level only
        (org-content org-todoist-show-n-levels))))
    (when (equal (marker-buffer cur-marker) (org-todoist--get-todoist-buffer))
      (goto-char cur-marker)
      (org-reveal)
      (org-fold-show-siblings)
      (org-fold-show-children))
    (org-fold-hide-drawer-all)   ; always hide drawers
    (write-file (org-todoist-file))))

(defun org-todoist--first-parent-of-type (NODE TYPES)
  "Gets the first parent of `NODE' which is one of the given `TYPES'.
`TYPES' can be a single symbol or a list of symbols."
  (org-element-lineage-map NODE #'identity TYPES nil t))

(defun org-todoist--get-planning-date (NODE KEYWORD)
  "Gets the Todoist-formatted date with `KEYWORD' from `NODE'."
  (let ((prop (org-element-property KEYWORD NODE)))
    (when prop (org-todoist--date-to-todoist prop))))
;; (org-todoist--date-to-todoist (org-element-property KEYWORD (org-todoist--get-planning NODE)))) ;; Can just grab from the headline. May not have planning

(defun org-todoist--get-planning (NODE)
  "Gets the planning element for specified `NODE'."
  (unless (eq (org-element-type NODE) 'headline)
    (error "Org-todoist--get--planning only supports headline input nodes"))
  (car (org-element-map NODE 'planning (lambda (planning)
                                         (when (eq NODE
                                                   (org-todoist--first-parent-of-type planning 'headline))
                                           planning)))))

(defun org-todoist--date-to-todoist (TIMESTAMP)
  "Convert an org-element `TIMESTAMP' to the Todoist date representation."
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
                 (when (> 10 hour) "0") ; pad to 2 char with 0
                 (number-to-string hour)
                 ":"
                 (when (> 10 minute) "0") ; pad to 2 char with 0
                 (number-to-string minute)
                 ":00.0"))))))

(defun org-todoist--todoist-date-object-for-kw (NODE KEYWORD)
  "Create the Todoist date JSON object for `KEYWORD' in `NODE'.

`KEYWORD' is :scheduled, :deadline, or :closed"
  (let ((dl (org-todoist--get-planning-date NODE KEYWORD)))
    (when dl
      (let ((res `(("date" . ,dl)
                   ("lang" . ,org-todoist-lang)
                   ;; ("string" . ,dl)
                   ;; ("timezone" . ,(if org-todoist-tz org-todoist-tz (cadr (current-time-zone))))
                   ))
            (is-recurring (org-todoist--task-is-recurring NODE KEYWORD)))
        ;; (when (and (eq KEYWORD :scheduled) is-recurring)
        (when is-recurring
          (push `("is_recurring" . ,t) res)
          (push `("string" . ,(org-todoist--repeater-to-string (org-element-property KEYWORD NODE))) res))
        ;; (when org-todoist-tz (push `("timezone" . ,org-todoist-tz) res))
        res))))

(defun org-todoist--repeater-to-string (TIMESTAMP)
  "Convert a repeating `TIMESTAMP' to it's Todoist string representation."
  (let ((value (org-element-property :repeater-value TIMESTAMP))
        (unit (org-element-property :repeater-unit TIMESTAMP)))
    (when (and value unit)
      (concat "every " (number-to-string value) " " (symbol-name unit)))))

(defun org-todoist--add-repeater (TIMESTAMP STRING)
  "Add a repeater value to `TIMESTAMP' from Todoist `STRING'."
  ;; TODO more repeater cases https://todoist.com/help/articles/introduction-to-recurring-due-dates-YUYVJJAV
  (let ((match (s-match "ev[[:alpha:]]+!?\\s-+\\([[:digit:]]*\\)\\s-*\\([[:alpha:]]+\\)\\(.*\\)" STRING)))
    (when (and match (s-blank? (nth 3 match)) (member (nth 2 match) '("week" "weeks" "month" "months" "day" "days" "hour" "hours" "year" "years"
                                                                      "mon" "monday"
                                                                      "tue" "tues" "tuesday"
                                                                      "wed" "wednesday"
                                                                      "thu" "thur" "thurs" "thursday"
                                                                      "fri" "friday"
                                                                      "sat" "saturday"
                                                                      "sun" "sunday")))
      (org-element-put-property TIMESTAMP :repeater-type (if (s-contains? "!" STRING) 'restart 'cumulate))
      (org-element-put-property TIMESTAMP :repeater-unit (org-todoist--get-repeater-symbol (nth 2 match)))
      (org-element-put-property TIMESTAMP :repeater-value (if (s-blank? (cadr match)) 1 (string-to-number (cadr match)))))))

(defun org-todoist--get-repeater-symbol (TIME-UNIT-STRING)
  "Get the corrent org repeater-unit symbol from `TIME-UNIT-STRING'."
  (cond
   ((s-contains? "week" TIME-UNIT-STRING t) 'week)
   ((s-contains? "day" TIME-UNIT-STRING t) 'day)
   ((s-contains? "month" TIME-UNIT-STRING t) 'month)
   ((s-contains? "year" TIME-UNIT-STRING t) 'year)
   ((s-contains? "hour" TIME-UNIT-STRING t) 'hour)
   (t 'week)))

(defun org-todoist--task-is-recurring (TASK &optional KEYWORD)
  "If `TASK' is a recurring task.

If `KEYWORD' is `:deadline', check if the deadline is repeating instead
of the scheduled time.
If `KEYWORD' is t, check both scheduled and
deadline properties.
Else check scheduled only."
  (cond ((eq KEYWORD :deadline)
         (org-element-property :repeater-type (org-element-property :deadline TASK)))
        ((eq KEYWORD t)
         (or (org-element-property :repeater-type (org-element-property :deadline TASK))
             (org-element-property :repeater-type (org-element-property :scheduled TASK))))
        (t (org-element-property :repeater-type (org-element-property :scheduled TASK)))))

(defun org-todoist--log-drawer (HL)
  "Gets the LOGBOOK drawer for `HL'."
  (org-element-map HL 'drawer (lambda (drawer)
                                ;; make sure the arg HL is the direct parent headline of the drawer
                                (when (and (string= (org-element-property :drawer-name drawer) "LOGBOOK")
                                           (eq (org-element-lineage-map drawer #'identity 'headline nil t) HL))
                                  drawer))
                   nil t))

(defun org-todoist--log-drawer-add-note (HL TEXT DATE)
  "Add a note with `TEXT' to the LOGBOOK drawer for `HL' timestamped `DATE'."
  (let ((drawer (org-todoist--log-drawer HL))
        (note (org-element-create 'item '(:bullet "-" :pre-blank 0)
                                  (org-element-create 'plain-text nil (concat "Note taken on "
                                                                              (org-todoist-org-element-to-string (org-todoist--get-ts-from-date DATE t))
                                                                              " \\\\\n"
                                                                              TEXT)))))
    (if drawer
        (if-let ((children (org-element-contents drawer)))
            (org-element-insert-before note (car children)) ; TODO this adds as first note in org, per standard org (todoist is the invers)
          (org-element-adopt drawer note))
      (setq drawer (org-element-create 'drawer '(:drawer-name "LOGBOOK") note))
      (org-todoist--adopt-drawer HL drawer)))
  HL)

(defun org-todoist--first-direct-descendent-of-type (NODE TYPE)
  "Get the first descendent from `NODE' of `TYPE'."
  (let ((ptype (org-element-type NODE)))
    (org-element-map (org-element-contents NODE) TYPE
      (lambda (node) (when (eq NODE (org-todoist--first-parent-of-type node ptype))
                       node))
      nil t)))

(defun org-todoist--adopt-drawer (HL DRAWER)
  "Adopts a `DRAWER' in the correct location under `HL'.
If the new drawer isn't added by the other drawers, it may get pushed under
the wrong headline!"
  ;; Check for a description to insert before
  (if (eq 'property-drawer (org-element-type DRAWER))
      ;; NOTE Having other drawers before property drawer makes property drawers parse as regular drawers. Avoid this.
      (if-let ((first-drawer (org-todoist--first-direct-descendent-of-type HL 'drawer)))
          (org-element-insert-before DRAWER first-drawer) ; assume drawers are in front of description elements
        (org-todoist--adopt-drawer-regular HL DRAWER))
    (org-todoist--adopt-drawer-regular HL DRAWER)))

(defun org-todoist--adopt-drawer-regular (HL DRAWER)
  "Adopts a `DRAWER' in the correct location under `HL'."
  (let ((description (org-todoist--get-description-elements HL)))
    (if description
        (org-element-insert-before DRAWER (car description)) ; Has a description, insert before that (end of drawers)
      (let ((child-hl (org-element-map (org-element-contents HL) 'headline #'identity nil t)))
        (if child-hl
            (org-element-insert-before DRAWER child-hl) ; Has a child headline, insert before that
          (org-element-adopt HL DRAWER)))))) ; Doesn't have child headlines or description. Safe to adopt at end of children

(defun org-todoist--last-recurring-task-completion (TASK)
  "Get the :LAST_REPEAT property of a `TASK'."
  (when (org-todoist--task-is-recurring TASK t)
    (org-timestamp-from-string (org-element-property :LAST_REPEAT TASK))))

(defun org-todoist--last-recurring-task-completion-as-doist (TASK)
  "Convert the :LAST_REPEAT property of `TASK' to a Todoist object."
  (org-todoist--date-to-todoist (org-todoist--last-recurring-task-completion TASK)))

(defun org-todoist--generate-push-preview ()
  "Generate the request commands that would be sent given current buffer state."
  (org-todoist--push (org-todoist--file-ast)
                     (org-todoist--get-last-sync-buffer-ast)))

(defun org-todoist--push-test ()
  "Show the commands that will be executed on next sync in a buffer.
The buffer is named `*todoist-push-test*' and contains the
pretty-printed JSON commands."
  (interactive)
  (let ((commands (org-todoist--generate-push-preview)))
    (switch-to-buffer (get-buffer-create "*todoist-push-test*"))
    (erase-buffer)
    (insert (json-encode commands))
    (json-pretty-print-buffer)
    (goto-char (point-min))
    (when (fboundp 'json-mode) (json-mode)))
  (message "Commands (as JSON) written to *todoist-push-test*"))

(defun org-todoist--timestamp-from-start (TIMESTAMP)
  (org-element-create 'timestamp
                      `(:year-start ,(org-element-property :year-start TIMESTAMP)
                        :month-start ,(org-element-property :month-start TIMESTAMP)
                        :day-start ,(org-element-property :day-start TIMESTAMP)
                        :hour-start ,(org-element-property :hour-start TIMESTAMP)
                        :minute-start ,(org-element-property :minute-start TIMESTAMP))))

(defun org-todoist--timestamp-from-end (TIMESTAMP)
  (org-element-create 'timestamp
                      `(:year-start ,(org-element-property :year-end TIMESTAMP)
                        :month-start ,(org-element-property :month-end TIMESTAMP)
                        :day-start ,(org-element-property :day-end TIMESTAMP)
                        :hour-start ,(org-element-property :hour-end TIMESTAMP)
                        :minute-start ,(org-element-property :minute-end TIMESTAMP))))

(defun org-todoist--timestamp-times-equal (T1 T2)
  "Equality comparison for org timestamp elements `T1' and `T2'."
  (and (equal (org-element-property :year-start T1) (org-element-property :year-start T2))
       (equal (org-element-property :month-start T1) (org-element-property :month-start T2))
       (equal (org-element-property :day-start T1) (org-element-property :day-start T2))
       (equal (org-element-property :hour-start T1) (org-element-property :hour-start T2))
       (equal (org-element-property :minute-start T1) (org-element-property :minute-start T2))))

(defun org-todoist--get-tags (NODE)
  "Get all tags of `NODE', including inherited tags."
  (let ((all-tags nil))
    (org-element-lineage-map NODE
        (lambda (hl) (let ((tags (org-element-property :tags hl)))
                       (when tags
                         (dolist (tag tags)
                           (cl-pushnew tag all-tags)))))
      'headline t)
    all-tags))

(defun org-todoist--get-labels (TAGS)
  "Filter `TAGS' to exclude tags that should not be Todoist labels."
  (--filter (not (string= org-archive-tag it)) TAGS))

(defun org-todoist--get-section-id-position-non-default (HL)
  "Get the id of the section `HL' is under if it is not the default.

Use this when pushing updates (we don't want to send id=default) to Todoist."
  (let ((res (org-todoist--get-section-id-position HL)))
    (unless (string= res org-todoist--default-id)
      res)))

(defun org-todoist--get-notify-ids (STRING)
  "Extract the ids to notify from `STRING'."
  (--map (cadr it) (s-match-strings-all "todoist-mention://\\([0-9]+\\)" STRING)))

(defun org-todoist--get-note-add (id comment)
  "Get the note_add command for `COMMENT' on task with `ID'."
  (let ((args `(("item_id" . ,id) ;; task uuid/tempid
                ("content" . ,comment)))
        (matches (org-todoist--get-notify-ids comment)))

    (when matches
      (push `("uids_to_notify" . ,matches) args))

    `(("uuid" . ,(org-id-uuid)) ;; command uuid
      ("type" . "note_add")
      ("temp_id". ,(org-id-uuid)) ;; note uuid
      ("args" . ,args))))

(defun org-todoist--is-ignored (NODE)
  "If the `NODE' should be ignored."
  (or (member (org-todoist--get-todoist-type NODE t) `(,org-todoist--user-node-type ,org-todoist--metadata-node-type))
      (org-element-lineage-map NODE #'org-todoist--is-ignored-type 'headline t t)))

(defun org-todoist--is-ignored-type (NODE)
  "If the `NODE' has the `org-todoist--ignored-node-type'."
  (string= (org-todoist--get-todoist-type NODE t) org-todoist--ignored-node-type))

(defun org-todoist-my-id ()
  "Get the current user's Todoist uid."
  (unless org-todoist-my-id
    (setq org-todoist-my-id (org-todoist--get-prop (--first (string-equal-ignore-case (user-full-name) (org-element-property :raw-value it))
                                                            (org-todoist--all-users))
                                                   "tid")))
  org-todoist-my-id)

(defun org-todoist--id-arg (id &optional command-type)
  "Return the correct id argument for the current API version.
For some commands like item_complete and item_delete, v9 API uses ids
instead of id."
  (if (and (not (eq (org-todoist--api-version) 'v1))
           (member command-type '("item_complete" "item_delete")))
      `("ids" . (,id))
    `("id" . ,id)))

(defun org-todoist--get-duration (hl eff)
  "Gets the todoist item request duration.
`HL' is the org task headline.
`EFF' is the effort number in minutes."
  (if org-todoist-duration-as-timestamp
      (when-let* ((scheduled (org-element-property :scheduled hl))
                  (type (eq (org-element-property :type scheduled) 'active-range))
                  (start (ts-parse-org-element (org-todoist--timestamp-from-start scheduled)))
                  (end (ts-parse-org-element (org-todoist--timestamp-from-end scheduled)))
                  (sec-diff (ts-difference end start)))
        `(("amount" . ,(round (/ sec-diff 60))) ("unit" . "minute")))
    (when eff `(("amount" . ,(round eff)) ("unit" . "minute")))))

(defun org-todoist--push (ast old)
  "Create commands necessary to transform syntax tree from `OLD' to `AST'."
  (let ((commands nil))
    (org-todoist--label-default-sections ast)
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
               (pri (org-todoist--get-priority hl))
               (dead (org-element-property :deadline hl))
               (effstr (org-todoist--get-prop hl "EFFORT"))
               (eff (when effstr (org-duration-to-minutes effstr)))
               (tags (org-todoist--get-tags hl))
               (isarchived (member org-archive-tag tags))
               (labels (org-todoist--get-labels tags))
               (proj (org-todoist--get-project-id-position hl))
               (section (org-todoist--get-section-id-position-non-default hl))
               (parenttask (org-todoist--get-task-id-position hl))
               (rid (org-element-property :RESPONSIBLE_UID hl))
               ;; (is-recurring (org-todoist--task-is-recurring hl))
               (is-todoist-recurring (org-todoist--get-prop hl "is_recurring"))
               (comments (org-todoist--get-comments-text hl))
               ;; (lr (org-element-property :LAST_REPEAT hl))
               ;; (last-repeat (when (and lr is-recurring) (org-timestamp-from-string lr)))
               (oldtask (org-todoist--get-by-id nil id old))
               (oldsection (org-todoist--get-section-id-position-non-default oldtask))
               (oldparenttask (org-todoist--get-task-id-position oldtask))
               (oldproj (org-todoist--get-project-id-position oldtask))
               (old-todo-type (org-element-property :todo-type oldtask))
               ;; (old-todo-kw (org-element-property :todo-keyword oldtask))
               (oldtitle (org-element-property :raw-value oldtask))
               (olddesc (org-todoist--description-text oldtask))
               (oldpri (org-todoist--get-priority oldtask))
               (oldsch (org-element-property :scheduled oldtask))
               (olddead (org-element-property :deadline oldtask))
               (oldeffstr (org-todoist--get-prop oldtask "EFFORT"))
               (oldeff (when oldeffstr (org-duration-to-minutes oldeffstr)))
               (oldtags (org-todoist--get-tags oldtask))
               (oldlabels (org-todoist--get-labels oldtags))
               (oldisarchived (member org-archive-tag oldtags))
               (oldrid (org-element-property :RESPONSIBLE_UID oldtask))
               (oldcomments (org-todoist--get-comments-text oldtask)))
          (unless (org-todoist--is-ignored hl)

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
                                            ("duration" . ,(org-todoist--get-duration hl eff))))
                                 ("due" . ,(org-todoist--todoist-date-object-for-kw hl :scheduled))
                                 ("deadline" . ,(org-todoist--todoist-date-object-for-kw hl :deadline))
                                 ("priority" . ,pri)
                                 ("labels" . ,labels)
                                 ("auto_reminder" . ,org-todoist-use-auto-reminder)
                                 ("parent_id" . ,(org-todoist--get-task-id-position hl))
                                 ("section_id" . ,section)
                                 ("project_id" . ,proj)
                                 ("responsible_uid" . ,rid))
                               commands)
                         (when comments
                           ;; Add comments
                           (dolist (comment comments)
                             (push (org-todoist--get-note-add id comment) commands))))

                     ;; when is somewhat redundant, as oldtask should not be null if there is a tid (new item)
                     (when oldtask
                       (unless (equal comments oldcomments)
                         ;; TODO support comment editing. This will push any edited comments as new comments
                         (dolist (comment (--filter (not (member it oldcomments)) comments))
                           (push (org-todoist--get-note-add id comment) commands)))

                       ;; item_move. Only one parameter can be specified
                       (unless (and (string= section oldsection)
                                    (string= proj oldproj)
                                    (string= parenttask oldparenttask))
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
                                (unless (string= section oldsection) ; whole section moved, ignore
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
                              (not (equal labels oldlabels))
                              (not (equal rid oldrid)))
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
                                                 ("duration" . ,(org-todoist--get-duration hl eff))
                                                 ("due" . ,(org-todoist--todoist-date-object-for-kw hl :scheduled))
                                                 ("deadline" . ,(org-todoist--todoist-date-object-for-kw hl :deadline))
                                                 ("priority" . ,(org-todoist--get-priority hl))
                                                 ("labels" . ,labels)
                                                 ("responsible_uid" . ,rid))))))
                           (push req commands)))

                       ;; todo-state changed
                       (when (not (equal todo-type old-todo-type))
                         (if (eq 'done todo-type)
                             (if (string= org-todoist-deleted-keyword todo-kw)
                                 (push `(("uuid" . ,(org-id-uuid))
                                         ("type" . "item_delete")
                                         ("args" . (,(org-todoist--id-arg id "item_delete"))))
                                       commands)
                               (if is-todoist-recurring
                                   (push `(("uuid" . ,(org-id-uuid)) ; this doesn't work with org recurring tasks since org auto-reopens with new date. The task will instead be updated with item_update
                                           ("type" . "item_close")
                                           ("args" . (("id" . ,id))))
                                         commands)
                                 (push `(("uuid" . ,(org-id-uuid))
                                         ("type" . "item_complete")
                                         ("args" . (,(org-todoist--id-arg id "item_complete")
                                                    ("date_completed" .
                                                     ,(org-todoist--timestamp-to-utc-str (org-element-property :closed hl))))))
                                       commands)))
                           (push `(("uuid" . ,(org-id-uuid))
                                   ("type" . "item_uncomplete")
                                   ("args" . (,(org-todoist--id-arg id "item_uncomplete"))))
                                 commands))))))
                  ((and (not section) (string= type org-todoist--section-type) (not (string= org-todoist--default-section-name title)))
                   (if (and (not oldtask) hastid)
                       (progn
                         ;; new section
                         (org-todoist--insert-identifier hl org-todoist--section-type)
                         (push `(("uuid" . ,(org-id-uuid))
                                 ("temp_id" . ,id)
                                 ("type" . "section_add")
                                 ("args" . (("name" . ,title)
                                            ("project_id" . ,proj))))
                               commands))
                     (cond
                      ;; section archive
                      ((and isarchived (not oldisarchived))
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "section_archive")
                               ("args" . (("id" . ,id))))
                             commands))

                      ;; section unarchive
                      ((and oldisarchived (not isarchived))
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "section_unarchive")
                               ("args" . (("id" . ,id))))
                             commands)))

                     ;; Note, spurious section updates to the same name and id
                     ;; occur if there is no sync buffer
                     (unless (string= title oldtitle)
                       ;; update section
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "section_update")
                               ("args" . (("name" . ,title)
                                          ("id" . ,id))))
                             commands))
                     (unless (cl-equalp proj oldproj)
                       ;; section_move
                       ;; Note, spurious section to the same project_id
                       ;; occur if there is no sync buffer
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "section_move")
                               ("args" . (("id" . ,id)
                                          ("project_id" . ,proj))))
                             commands))))
                  ((and (string= type org-todoist--project-type) (not (string= title "Inbox")))
                   (if (and (not oldtask) hastid)
                       ;; new project
                       (progn (org-todoist--insert-identifier hl org-todoist--project-type)
                              (push `(("uuid" . ,(org-id-uuid))
                                      ("temp_id" . ,id)
                                      ("type" . "project_add")
                                      ("args" . (("name" . ,title)
                                                 ("parent_id" . ,proj))))
                                    commands))
                     (when (not (string= title (org-element-property :raw-value oldtask)))
                       ;; update project
                       ;; Note, spurious project updates to the same name and id
                       ;; occur if there is no sync buffer
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "project_update")
                               ("args" . (("name" . ,title)
                                          ("id" . ,id))))
                             commands))
                     (cond ((and isarchived (not oldisarchived))
                            ;; archive project
                            (push `(("uuid" . ,(org-id-uuid))
                                    ("type" . "project_archive")
                                    ("args" . (("id" . ,id))))
                                  commands))
                           ((and (not isarchived) oldisarchived)
                            ;; unarchive project
                            (push `(("uuid" . ,(org-id-uuid))
                                    ("type" . "project_unarchive")
                                    ("args" . (("id" . ,id))))
                                  commands)))
                     (unless (string= proj oldproj)
                       ;; moved
                       (push `(("uuid" . ,(org-id-uuid))
                               ("type" . "project_move")
                               ("args" . (("id" . ,id)
                                          ("parent_id" . ,proj))))
                             commands)))))))))
    (when org-todoist-delete-remote-items
      (org-element-map
          old
          'headline
        (lambda (hl)
          (unless (org-todoist--is-ignored hl)
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
                              ("args" . (,(org-todoist--id-arg id "item_delete"))))
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
                            commands)))))))))))

    ;; Since commands may be batched, we need to ensure that new items are always added first.
    (let ((sorted-commands
           (append
            ;; Project adds first
            (--filter (string= "project_add" (assoc-default 'type it)) commands)
            ;; Section adds second
            (--filter (string= "section_add" (assoc-default 'type it)) commands)
            ;; Item adds third
            (--filter (string= "item_add" (assoc-default 'type it)) commands)
            ;; All other commands last
            (--filter (not (member (assoc-default 'type it)
                                   '("project_add" "section_add" "item_add")))
                      commands))))
      sorted-commands)))

(defun org-todoist--is-subtask (NODE)
  "If `NODE' is a headline representings a substask."
  (org-todoist--get-parent-of-type org-todoist--task-type NODE t))

(defun org-todoist--is-project (NODE)
  "If `NODE' is a headline representings a project."
  (string= (org-todoist--get-todoist-type NODE) org-todoist--project-type))

(defun org-todoist--is-section (NODE)
  "If `NODE' is a headline representings a section."
  (string= (org-todoist--get-todoist-type NODE) org-todoist--section-type))

(defun org-todoist--is-task (NODE)
  "If `NODE' is a headline representings a task."
  (string= (org-todoist--get-todoist-type NODE) org-todoist--task-type))

(defun org-todoist--parse-response (RESPONSE AST)
  "Parse Todoist sync `RESPONSE' alist and update `AST'."
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
  "Add ids to node with a temp_id in `AST' using `TID_MAPPING'."
  (dolist (elem TID_MAPPING)
    (org-element-map AST 'headline
      (lambda (hl)
        (let* ((tid (car elem))
               (tidstr (if (symbolp tid) (symbol-name tid) tid)))
          (when (string= (org-todoist--get-prop hl "temp_id") tidstr)
            (org-todoist--insert-id hl (cdr elem)))))
      nil t)))

(defun org-todoist--update-comments (COMMENTS AST)
  "Update comments in `AST' using `COMMENTS' section of Todoist sync API response."
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
  "Insert a header appropriate for the Todoist org file."
  ;; TODO use element api
  (goto-char (point-min))
  (insert "# -*- org-list-indent-offset: 2; -*-
#+title: Todoist
#+STARTUP: hidedrawers
#+STARTUP: logdone
#+STARTUP: logdrawer
"))

(defun org-todoist--update-projects (PROJECTS AST)
  "Update projects in `AST' using `PROJECTS' section of Todoist sync API response."
  (cl-loop for proj across PROJECTS do
           (let ((node (org-todoist--get-or-create-node
                        AST
                        org-todoist--project-type
                        (assoc-default 'id proj)
                        (assoc-default 'name proj)
                        nil
                        proj
                        org-todoist--project-skip-list)))
             (when (eq t (assoc-default 'is_deleted proj))
               (org-todoist--handle-deletion node))
             (when (eq t (assoc-default 'is_archived proj))
               (org-todoist--archive node))))
  ;; (org-todoist--sort-by-child-order AST "child_order")
  (dolist (proj (org-todoist--project-nodes AST))
    (when-let* ((pid (org-todoist--get-prop proj 'parent_id))
                (parent (org-todoist--get-by-id org-todoist--project-type pid AST)))
      (org-todoist--adopt-sub parent proj))
    (unless (member org-todoist--default-section-name (org-todoist--get-section-titles proj))
      (let ((default-section (org-todoist--create-node org-todoist--section-type org-todoist--default-section-name nil nil proj)))
        (org-todoist--add-prop default-section org-todoist--id-property org-todoist--default-id)))))

(defun org-todoist--handle-deletion (node)
  "Handle remote deletion of `NODE'."
  (if org-todoist-extract-deleted
      (org-element-extract node)
    (org-todoist--insert-identifier node org-todoist--ignored-node-type)
    ;; To-do status is already handled for tasks, but this will mark deleted sections
    ;; and projects as canceled to-do items as well
    (org-todoist--set-todo node nil t)))

(defun org-todoist--get-tasks (ITEM)
  "Get Todoist task headlines directly under `ITEM'."
  (org-element-map (org-element-contents ITEM) 'headline
    (lambda (hl) (when (and (string= (org-todoist--get-todoist-type hl) org-todoist--task-type)
                            (eq ITEM (org-todoist--first-parent-of-type hl 'headline)))
                   hl))))

(defun org-todoist--get-tasks-all (ITEM)
  "Get all Todoist task headlines (including subtasks) under `ITEM'."
  (org-element-map (org-element-contents ITEM) 'headline (lambda (hl) (when (string= (org-todoist--get-todoist-type hl) org-todoist--task-type)
                                                                        hl))))

(defun org-todoist--get-sections (PROJECT)
  "Get all section headlines belonging to `PROJECT'."
  (org-element-map (org-element-contents PROJECT) 'headline
    (lambda (hl) (when (and (string= (org-todoist--get-todoist-type hl) org-todoist--section-type)
                            (eq (org-todoist--get-parent-of-type org-todoist--project-type hl t) PROJECT))
                   hl))))

(defun org-todoist--get-title (NODE)
  "Get the title string of headline, `NODE'."
  (let ((title (org-element-property :title NODE)))
    (while (and title (not (eq 'string (type-of title))))
      (setq title (car title)))
    (substring-no-properties title)))

(defun org-todoist--get-section-titles (NODE)
  "Extract the titles of all sections under `NODE'."
  (--map (org-todoist--get-title it) (org-todoist--get-sections NODE)))

(defun org-todoist--user-node (AST)
  "Get or create the main user headline in `AST'."
  (org-todoist--category-node-query-or-create (org-todoist--metadata-node AST) org-todoist-user-headline org-todoist--user-node-type))

(defun org-todoist--metadata-node (AST)
  "Get or create the main metadata headline in `AST'."
  (org-todoist--category-node-query-or-create AST org-todoist-metadata-headline org-todoist--metadata-node-type))

(defun org-todoist--update-users (COLLAB AST)
  "Update all users in `AST' from API response collaborator section, `COLLAB'."
  (let ((usernode (org-todoist--user-node AST)))
    (cl-loop for usr across COLLAB do
             (org-todoist--get-or-create-node usernode
                                              org-todoist--collaborator-type
                                              (assoc-default 'id usr)
                                              (assoc-default 'full_name usr)
                                              nil
                                              usr))))

(defun org-todoist--update-sections (SECTIONS AST)
  "Update all sections in `AST' using `SECTIONS' portion of API response."
  (cl-loop for sect across SECTIONS do
           (let ((targetprojid (assoc-default 'project_id sect)))
             (if-let* ((sectionid (assoc-default 'id sect))
                       (section (org-todoist--get-by-id org-todoist--section-type sectionid AST))
                       (project (org-todoist--get-parent-of-type org-todoist--project-type section t))
                       (projid (org-todoist--get-project-id-position section)))
                 ;; Section already exists
                 (if (eq t (assoc-default 'is_deleted sect))
                     ;; but it needs to be deleted
                     (org-todoist--handle-deletion section)
                   (if (cl-equalp projid targetprojid)
                       ;; Section is under the right project, add props only
                       (org-todoist--add-all-properties section sect org-todoist--section-skip-list)
                     ;; Move to the correct project
                     (org-element-adopt
                         (org-todoist--get-by-id org-todoist--project-type targetprojid AST)
                       (org-element-extract section))))
               ;; Otherwise make the section if parent project exists
               (when-let ((project (org-todoist--get-by-id org-todoist--project-type targetprojid AST)))
                 (org-todoist--get-or-create-node
                  project
                  org-todoist--section-type
                  (assoc-default 'id sect)
                  (assoc-default 'name sect)
                  nil
                  sect
                  org-todoist--section-skip-list)))))
  (org-todoist--label-default-sections AST))
;; (dolist (proj (org-todoist--project-nodes AST))
;;   ;; TODO add default section here? Otherwise might not be added to new projects without manually added default section?
;;   (org-todoist--sort-by-child-order proj "section_order" org-todoist--section-type))

(defun org-todoist--project-nodes (AST)
  "Get all Todoist project nodes within the syntax tree, `AST'."
  (org-element-map AST 'headline
    (lambda (hl) (when (string-equal (org-todoist--get-todoist-type hl) org-todoist--project-type) hl))))

(defun org-todoist--section-nodes (AST)
  "Get all Todoist section nodes within the syntax tree, `AST'."
  (org-element-map AST 'headline
    (lambda (hl) (when (string-equal (org-todoist--get-todoist-type hl) org-todoist--section-type) hl))))

(defun org-todoist--get-headline-level (NODE)
  "Get the nearest headline level of `NODE'."
  (if-let (hl (org-element-lineage-map NODE (lambda (n) (when (eq (org-element-type n) 'headline) (org-element-property :level n))) nil t t))
      hl
    0))

(defun org-todoist--create-node (TYPE TEXT DESCRIPTION &optional PROPERTIES PARENT SKIP)
  "Create node of TODOIST_TYPE `TYPE' under `PARENT'.

`TEXT' is the headline title and `PROPERTIES' are added to the
headline property drawer. `DESCRIPTION' is the text added under the headline.
Skip properties in `SKIP' list."
  (let ((node (org-element-create 'headline `(:title ,TEXT :level ,(+ 1 (org-todoist--get-headline-level PARENT))))))
    (org-todoist--insert-identifier node TYPE)
    (org-todoist--add-all-properties node PROPERTIES SKIP)
    (when DESCRIPTION (org-todoist--add-description node DESCRIPTION))
    (when PARENT (org-element-adopt PARENT node))
    node))

(defun org-todoist--add-description (NODE DESCRIPTION)
  "Add `DESCRIPTION' text to `NODE'."
  (org-element-adopt NODE (org-todoist--create-description-element DESCRIPTION)))

(defun org-todoist--create-description-element (DESCRIPTION)
  "Create a description org element for text `DESCRIPTION'."
  (org-element-create 'paragraph nil
                      (org-element-create 'plain-text nil
                                          (if (s-ends-with? "\n" DESCRIPTION)
                                              DESCRIPTION
                                            (concat DESCRIPTION "\n")))))

(defun org-todoist--replace-description (NODE DESCRIPTION)
  "Replace the description of `NODE' with `DESCRIPTION' text."
  (if DESCRIPTION
      (let ((comment (org-todoist--description-text NODE))
            (idx 0)) ;; Use idx to replace the first paragraph element
        (unless (string-equal-ignore-case (s-trim comment) (s-trim DESCRIPTION)) ;; TODO trim necessary?
          ;; Replace the description
          (dolist (paragraph (org-todoist--get-description-elements NODE))
            (if (eql idx 0)
                (org-element-set paragraph (org-todoist--create-description-element DESCRIPTION))
              (org-element-extract paragraph))
            (cl-incf idx))
          (when (eql idx 0) (org-todoist--add-description NODE DESCRIPTION)) ;; There was no description, add the new one
          DESCRIPTION))))

(defun org-todoist--get-or-create-node
    (PARENT TYPE ID TEXT DESCRIPTION PROPERTIES &optional SKIP SOURCE)
  "Get or create a new node.

The node will have of TODOIST_TYPE `TYPE', `org-todoist--id-property'`ID',
headline `TEXT', body `DESCRIPTION', and drawer `PROPERTIES',ignoring `SKIP',
under `PARENT'.

When SOURCE is specified, search for the node from there. Else search
from PARENT."
  (if-let* ((found (org-todoist--get-by-id nil ID (or SOURCE PARENT)))
            (updated (org-todoist--add-all-properties found PROPERTIES SKIP)))
      (progn
        (org-todoist--insert-identifier updated TYPE) ;; Probably not the best place for this. Newly created items may not have a type
        ;; (org-todoist--insert-id updated ID)
        (org-element-put-property updated :title TEXT)
        (org-todoist--replace-description updated DESCRIPTION)
        updated)
    (org-todoist--create-node TYPE TEXT DESCRIPTION PROPERTIES PARENT SKIP)))

(defun org-todoist--closed-date (TASK)
  "Get the timestamp object representing the closed date of `TASK'."
  (let ((date (assoc-default 'completed_at TASK)))
    (org-todoist--get-ts-from-date date)))

(defun org-todoist--scheduled-date (TASK)
  "Get the timestamp object representing the scheduled date of TASK."
  (org-todoist--get-timestamp 'due TASK))

(defun org-todoist--deadline-date (TASK)
  "Get the timestamp object representing the deadline date of `TASK'."
  (org-todoist--get-timestamp 'deadline TASK))

(defun org-todoist--get-ts-from-date (date &optional inactive)
  "Get a timestamp object representing `DATE' in the `current-time-zone'.

When `INACTIVE', return an inactive timestamp."
  (when date
    (let ((hastime (not (eql 10 (length date))))) ;; Todoist date format for tasks without a time is 10 char string
      (if (not (string= (substring date (- (length date) 1)) "Z"))
          (org-timestamp-from-time (org-read-date nil t date nil) hastime inactive)
        (org-todoist--timestamp-from-utc-str date hastime inactive)))))

(defun org-todoist--get-timestamp (SYMBOL TASK)
  "Get a timestamp object for scheduled, deadline, or closed `SYMBOL' under `TASK'."
  (org-todoist--get-timestamp2 (assoc-default SYMBOL TASK)))

(defun org-todoist--get-timestamp2 (DATEOBJ)
  "Create a timestamp from a Todoist `DATEOBJ'."
  (when-let* ((date (assoc-default 'date DATEOBJ))
              (hasdate (org-todoist--has-date date))
              (ts (org-todoist--get-ts-from-date date)))
    (when (eq t (assoc-default 'is_recurring DATEOBJ))
      (org-todoist--add-repeater ts (assoc-default 'string DATEOBJ)))
    ts))

(defun org-todoist--timestamp-from-utc-str (STRING &optional WITH-TIME INACTIVE)
  "Create an an org mode timestamp element from `STRING'.

`STRING' is an RFC3339 datetime string. With arg `WITH-TIME' include the
time-of-day portion in the timestamp. When `INACTIVE', make the timestamp
inactive."
  (org-timestamp-from-string (concat (if INACTIVE "[" "<")
                                     (ts-format
                                      (if WITH-TIME "%Y-%m-%d %a %H:%M" "Y-%m-%d %a")
                                      (ts-adjust 'second (car (current-time-zone)) (ts-parse-org-element (org-timestamp-from-time (org-read-date nil t STRING nil) WITH-TIME))))
                                     (if INACTIVE "]" ">"))))

(defun org-todoist--timestamp-to-utc-str (TIMESTAMP)
  "Convert org `TIMESTAMP' element to a UTC RFC3339 string."
  (ts-format "%Y-%m-%dT%H:%M:%S.0Z"
             (ts-adjust 'second (- (car (current-time-zone))) (ts-parse-org-element TIMESTAMP))))

(defun org-todoist--has-date (DATE)
  "If `DATE' is not nil, empty, or \"null\"."
  (not (or (s-blank? DATE) (string-equal "null" DATE))))

(defun org-todoist--create-planning (TASK)
  "Create a planning element for Todoist `TASK' API response."
  (let ((props nil)
        (sch (org-todoist--scheduled-date TASK))
        (dead (org-todoist--deadline-date TASK))
        (closed (org-todoist--closed-date TASK)))
    ;; Convert duration to timestamp range if enabled
    (when (and org-todoist-duration-as-timestamp
               sch
               (assoc-default 'duration TASK))
      (when-let* ((duration (assoc-default 'duration TASK))
                  (amount (assoc-default 'amount duration))
                  (unit (assoc-default 'unit duration))
                  (start-str (org-todoist-org-element-to-string sch))
                  (start-time (org-time-string-to-time start-str))
                  (seconds (pcase unit
                             ("minute" (* amount 60))
                             ("hour" (* amount 3600))
                             ("day" (* amount 86400))
                             (_ 0)))
                  (end-time (time-add start-time (seconds-to-time seconds)))
                  (end-str (format-time-string "%Y-%m-%d %H:%M" end-time))
                  (range-str (format "%s--<%s>" start-str end-str))
                  (ts (org-timestamp-from-string range-str)))
        (when (eql (org-element-property :day-end ts)
                   (org-element-property :day-start ts))
          (org-element-put-property ts :range-type 'time-range)
          (org-element-put-property ts :raw-value nil))
        (setq sch ts)))
    (when sch (setq props (plist-put props :scheduled sch)))
    (when dead (setq props (plist-put props :deadline dead)))
    (when closed (setq props (plist-put props :closed closed)))
    (when props (org-element-create 'planning props))))

(defun org-todoist--schedule (HEADLINE TASK)
  "Add planning information to `HEADLINE' using API response info `TASK'."
  (let ((planning (org-todoist--create-planning TASK)))
    (if-let ((existing (org-element-map HEADLINE 'planning
                         (lambda (plan)
                           (when (eq (org-todoist--first-parent-of-type plan 'headline) HEADLINE) plan))
                         nil t)))
        (if planning
            (org-element-set existing planning)
          (org-element-extract existing))
      (when planning (org-element-insert-before planning (org-todoist--get-property-drawer HEADLINE))))
    (when (eq t (assoc-default 'is_recurring (assoc-default 'due TASK)))
      (org-todoist--add-prop HEADLINE "is_recurring" t))))

(defun org-todoist--update-tasks (TASKS AST)
  "Update all tasks in `AST' using `TASKS' portion of API response."
  (cl-loop for data across TASKS do
           (let* ((id (assoc-default 'id data))
                  (proj (org-todoist--get-by-id org-todoist--project-type (assoc-default 'project_id data) AST))
                  (section-id (assoc-default 'section_id data))
                  (section (if section-id
                               (org-todoist--get-by-id org-todoist--section-type section-id proj)
                             (org-todoist--get-by-id org-todoist--section-type org-todoist--default-id proj)))
                  (title (assoc-default 'content data))
                  (description (assoc-default 'description data))
                  (parent-id (assoc-default 'parent_id data))
                  (deleted (eq t (assoc-default 'is_deleted data)))
                  (task (if deleted
                            (org-todoist--get-by-id org-todoist--task-type id AST)
                          (org-todoist--get-or-create-node section org-todoist--task-type id
                                                           title description data org-todoist--task-skip-list AST)))
                  (tags (assoc-default 'labels data)))
             (when task ; Not sure if we might receive an update that a new task was deleted. Catch this
                                        ; scenario here
               (if deleted
                   (org-todoist--handle-deletion task)

                 ;; If unsectioned and doesn't have a parent task, add to the default section in that project
                 (unless (or section parent-id)
                   (setq section (org-todoist--get-by-id org-todoist--section-type org-todoist--default-id proj)))

                 ;; Move to correct section if needed
                 (unless (or parent-id
                             (and section
                                  (cl-equalp (org-todoist--get-section-id-position task)
                                             (org-todoist--id-or-temp-id section))))
                   (org-element-extract task)
                   (org-element-adopt section task))

                 (dotimes (i (length tags))
                   (org-todoist--add-tag task (aref tags i)))
                 (org-todoist--schedule task data)
                 (org-todoist--set-effort task data)
                 (org-todoist--set-priority task (assoc-default 'priority data))
                 ;; TODO checked vs is_archived?
                 (org-todoist--set-todo task (assoc-default 'checked data) (assoc-default 'is_deleted data))))))

  ;; second loop to set parent tasks after all have been created
  (let ((with-child nil)) ; Only sort child tasks on the first go around. If you move it, you probably moved it for a reason?
    (cl-loop for data across TASKS do
             (when-let* ((parenttaskid (assoc-default 'parent_id data))
                         (task (org-todoist--get-by-id org-todoist--task-type (assoc-default 'id data) AST))
                         (parenttask (org-todoist--get-by-id org-todoist--task-type parenttaskid AST))
                         (wrong-spot (not (cl-equalp parenttaskid (org-todoist--get-task-id-position task)))))
               (push parenttask with-child)
               (org-todoist--adopt-sub parenttask task)))))
;; (dolist (parent with-child)
;;   (org-todoist--sort-by-child-order parent "child_order"))

(defun org-todoist--adopt-sub (PARENT CHILD)
  "Adopt `CHILD' under `PARENT' and increase the :level to parent + 1."
  (org-element-adopt PARENT (org-element-put-property (org-element-extract CHILD) :level (+ 1 (org-element-property :level PARENT)))))

(defun org-todoist--get-node-attributes (NODE)
  "Get the attributes list of `NODE'."
  (cadr NODE))

(defun org-todoist--get-node-attribute (NODE ATTRIBUTE)
  "Get an `ATTRIBUTE' from the attribute list of `NODE'.

If `ATTRIBUTE' is a string, it will be automatically converted to the
appropriate symbol representation."
  (plist-get (org-todoist--get-node-attributes NODE) (org-todoist--to-symbol ATTRIBUTE)))

;; ;; Not working as a new plist is returned. Needs to mutate
;; (defun org-todoist--put-node-attribute (NODE ATTRIBUTE VALUE)
;;   (setcar (cdr NODE) (plist-put (org-todoist--get-node-attributes NODE)
;;                                 (org-todoist--to-symbol ATTRIBUTE)
;;                                 VALUE)))

(defun org-todoist--to-symbol (SYMBOL-OR-STRING)
  "If `SYMBOL-OR-STRING' is a string, convert it to an uppercase symbol."
  (if (stringp SYMBOL-OR-STRING) (intern (concat ":" (upcase SYMBOL-OR-STRING))) SYMBOL-OR-STRING))

(defun org-todoist--get-position (NODE PROPERTY)
  "Get the numeric value of `PROPERTY' under `NODE'."
  (let ((val (org-todoist--get-prop NODE PROPERTY)))
    (if (stringp val) (string-to-number val) val)))

(defun org-todoist--sort-by-child-order (NODE PROPERTY &optional TYPE)
  "Sort children of `TYPE' under `NODE' by numeric `PROPERTY'."
  (let* ((children (org-element-map NODE 'headline
                     (lambda (hl) (when (and (eq (org-todoist--first-parent-of-type hl 'headline) NODE)
                                             (org-todoist--get-prop hl PROPERTY) ;; only move children with the property
                                             (or (not TYPE)
                                                 (string= (org-todoist--get-todoist-type hl) TYPE)))
                                    hl))))
         (sorted (cl-sort children (lambda (a b)
                                     (< (org-todoist--get-position a PROPERTY)
                                        (org-todoist--get-position b PROPERTY))))))
    (dolist (child children)
      (org-element-extract child))
    (dolist (child sorted)
      (org-element-adopt NODE child))))

(defun org-todoist--set-effort (NODE TASK)
  "Set the EFFORT property of `NODE' using the API response data `TASK'."
  (when-let* ((duration (assoc-default 'duration TASK))
              (amount (assoc-default 'amount duration))
              (unit (assoc-default 'unit duration))
              (effortval (cond
                          ((string-equal unit "minute") amount)
                          ((string-equal unit "day") (* 1440 amount))
                          (t nil))))
    ;; Only set EFFORT if duration-as-timestamp is disabled
    (unless org-todoist-duration-as-timestamp
      (org-todoist--add-prop NODE "EFFORT" (org-duration-from-minutes effortval)))))

(defun org-todoist--set-priority (NODE PRIORITY)
  "Set priority of `NODE' to equivalent of Todoist `PRIORITY'."
  (org-element-put-property NODE :priority (cond
                                            ((equal PRIORITY 4) org-todoist-p1)
                                            ((equal PRIORITY 3) org-todoist-p2)
                                            ((equal PRIORITY 2) org-todoist-p3)
                                            ((equal PRIORITY 1) org-todoist-p4))))

(defun org-todoist--get-priority (NODE)
  "Get the Todoist priority level of `NODE'."
  (let ((priority (org-element-property :priority NODE)))
    (org-todoist--get-priority-cond priority)))

(defun org-todoist--get-priority-cond (priority)
  "Get the Todoist priority level of `PRIORITY'."
  (cond
   ((equal priority org-todoist-p1) 4)
   ((equal priority org-todoist-p2) 3)
   ((equal priority org-todoist-p3)  2)
   ((equal priority org-todoist-p4)  1)
   (t (org-todoist--get-priority-cond org-todoist-priority-default))))

(defun org-todoist--set-todo (NODE CHECKED &optional DELETED)
  "Set the TODO state of `NODE' from the `CHECKED' and `DELETED' properties.
`CHECKED' and `DELETED' are from the Todoist API response."
  (cond ((eql t DELETED)
         (org-element-put-property NODE :todo-keyword org-todoist-deleted-keyword)
         (org-element-put-property NODE :todo-type 'done))
        ((eql t CHECKED)
         (org-element-put-property NODE :todo-keyword org-todoist-done-keyword)
         (org-element-put-property NODE :todo-type 'done))
        (t
         (org-element-put-property NODE :todo-type 'todo)
         (unless (org-element-property :todo-keyword NODE)
           (org-element-put-property NODE :todo-keyword org-todoist-todo-keyword)))))

(defun org-todoist--root (NODE)
  "Gets the syntax root of `NODE'."
  (let ((root NODE))
    (while-let ((next (org-element-parent root)))
      (setq root next))
    root))

(defun org-todoist--add-tag (NODE TAG)
  "Add `TAG' to `NODE'."
  (let ((tags (org-element-property :tags NODE)))
    (if (null tags)
        (org-element-put-property NODE :tags `(,TAG))
      (unless (member TAG tags)
        (push TAG tags)))))

(defun org-todoist--archive (NODE)
  "Archives `NODE' via the `org-archive-tag' tag."
  (org-todoist--add-tag NODE org-archive-tag))

(defun org-todoist--get-description-elements (NODE)
  "Gets all paragraph elements under `NODE' that are not in a drawer."
  (org-element-map NODE t (lambda (node) (when (org-todoist--is-description-element node NODE) node)) nil nil 'drawer))

(defun org-todoist--is-description-element (NODE PARENT)
  "T if the `NODE' is a paragraph element not in a drawer directly under `PARENT'."
  ;; TODO this doesn't seem to be working properly. Doesn't filter out items in drawers.
  (when (and
         ;; Is not a child of a plain-list
         (not (org-element-lineage-map NODE #'identity 'plain-list nil t))
         ;; Is a paragraph or plain-list element
         (or
          (eq (org-element-type NODE) 'paragraph)
          (eq (org-element-type NODE) 'plain-list))
         ;; Is directly under the PARENT headline
         (eq (org-todoist--first-parent-of-type NODE 'headline) PARENT)
         ;; Is not in a drawer
         (not (eq (org-element-type (org-element-lineage-map NODE #'identity '('drawer 'headline) nil t)) 'drawer)))
    NODE))

(defun org-todoist--description-text (NODE)
  "Combine all description elements under `NODE'."
  (mapconcat #'org-todoist-org-element-to-string (org-todoist--get-description-elements NODE)))

(defun org-todoist--category-node-query-or-create (AST DEFAULT TYPE)
  "Find or create the first node of TODOIST_TYPE `TYPE' under `AST'.
If created, use `DEFAULT' text."
  (let ((target (org-todoist--find-node (lambda (node) (when (org-todoist--node-type-query node TYPE) node)) AST))
        (level (org-element-lineage-map AST (lambda (hl) (org-element-property :level hl)) 'headline t t)))
    (if target
        target
      (setq target (org-element-create 'headline `(:title ,DEFAULT :level ,(+ 1 (if level level 0)))))
      (org-todoist--insert-identifier target TYPE)
      (org-element-adopt AST target)
      target)))

(defun org-todoist--node-type-query (NODE TYPE)
  "T if `NODE' has `org-todoist--type' `TYPE'."
  (when (equal (org-todoist--get-prop NODE org-todoist--type) TYPE) NODE))


(defun org-todoist--update-file (AST)
  "Update the org-todoist file with changes from `AST'.
Use diff to only apply changes rather than rewriting the entire file,
preserving marks and point location.

RETURN a value representing if the buffer was modified."
  (with-todoist-buffer!
   (let* ((new-str (org-todoist-org-element-to-string AST))
          (current-buf (current-buffer))
          (current-pos (point))
          (current-mark (when mark-active (mark)))
          (inhibit-read-only t)
          (modified (not (string= new-str (buffer-substring-no-properties (point-min) (point-max))))))
     (when modified
       ;; Create temp buffer with new content
       (with-temp-buffer
         (insert new-str)
         (let ((temp-buf (current-buffer)))
           (with-current-buffer current-buf
             ;; Use replace-buffer-contents to preserve marks and positions
             (replace-buffer-contents temp-buf 0.5 10)
             ;; Restore point and mark positions
             (goto-char current-pos)
             (when current-mark
               (set-mark current-mark)))))
       (save-buffer)
       t))))

(defun org-todoist--file-ast ()
  "Parse the AST of the `'org-todoist-file'."
  (save-current-buffer
    (let ((created (not (file-exists-p (org-todoist-file)))))
      (with-todoist-buffer!
       (when created (org-todoist--insert-header)
             (save-buffer))
       (org-element-parse-buffer)))))

                                        ;Find node;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--find-node (QUERY AST)
  "Return the first headling in the syntax tree `AST' which matches `QUERY'.
`QUERY' takes a single argument which is the current node."
  (org-element-map AST 'headline (lambda (node) (when (funcall QUERY node) node)) nil t))

(defun org-todoist--get-by-id (TYPE ID AST)
  "Find the first item of TODOIST_TYPE `TYPE' with `ID' in the syntax tree `AST'."
  (when ID
    (org-element-map AST 'headline
      (lambda (hl) (when (and (or (null TYPE)
                                  (string-equal (org-todoist--get-prop hl org-todoist--type) TYPE))
                              (string= (org-todoist--get-prop hl org-todoist--id-property) ID))
                     hl))
      nil t)))

(defun org-todoist--id-or-temp-id (NODE)
  "Get the `org-todoist--id-property' or \"temp_id\" property of `NODE'."
  (let ((id (org-todoist--get-prop NODE org-todoist--id-property)))
    (or id (org-todoist--get-prop NODE "temp_id"))))

(defun org-todoist--get-project-id-position (NODE)
  "Gets the ID of the project headline the `NODE' is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--project-type NODE t)))

(defun org-todoist--get-section-id-position (NODE)
  "Gets the ID of the section headline the `NODE' is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--section-type NODE t)))

(defun org-todoist--get-task-id-position (NODE)
  "Gets the ID of the task headline the `NODE' is under."
  (org-todoist--id-or-temp-id (org-todoist--get-parent-of-type org-todoist--task-type NODE t)))

(defun org-todoist--get-parent-of-type (TYPE NODE &optional FIRST)
  "Gets the parent(s) of `NODE' with TODOIST_TYPE `TYPE'.
If `FIRST', only get the first matching parent."
  (org-element-lineage-map NODE
      (lambda (parent) (when (string= (org-todoist--get-todoist-type parent) TYPE)
                         parent))
    'headline nil FIRST))

(defun org-todoist--get-parent-of-element-type (TYPE NODE)
  "Gets the parent(s) of `NODE' with `org-element-type' `TYPE'."
  (org-element-lineage-map NODE
      (lambda (parent) (when (and (not (eq NODE parent))) (eq (org-element-type NODE) TYPE)
                             parent))
    'headline nil t))

(defun org-todoist--get-hl-level-at-point ()
  "Get the level of the closest headline above the cursor."
  (org-element-property :level (org-todoist--get-parent-of-element-type 'headline (org-element-at-point))))

                                        ;Property get/set;;;;;;;;;;;;;;;;;;;;;;
(defun org-todoist--insert-id (NODE ID)
  "Insert `ID' as property `org-todoist--id-property' under `NODE'."
  (org-todoist--add-prop NODE org-todoist--id-property ID)
  NODE)

(defun org-todoist--insert-identifier (NODE IDENTIFIER)
  "Insert `IDENTIFIER' as property `org-todoist--type' under `NODE'."
  (org-todoist--add-prop NODE org-todoist--type IDENTIFIER)
  NODE)

(defun org-todoist--get-property-drawer (NODE)
  "Get the property drawer for `NODE'."
  (org-element-map NODE 'property-drawer
    (lambda (node) (when (eq NODE (org-todoist--first-parent-of-type node 'headline)) node))
    nil t))

(defun org-todoist--create-property (KEY VALUE)
  "Create node-property element with :key `KEY' and :value `VALUE'."
  (org-element-create 'node-property `(:key ,KEY :value ,VALUE)))

(defun org-todoist--property-exists (NODE KEY)
  "T if `NODE' has property with `KEY' in its property drawer."
  (unless (equal (org-element-type NODE) 'property-drawer)
    (error "Can only be called on property drawer elements")
    (org-element-map NODE 'node-property (lambda (prop) (org-todoist--is-property prop KEY)))))

(defun org-todoist--is-property (NODE KEY)
  "T if node-property element `NODE' has :key equivalent to `KEY'."
  (when (eq (org-element-type NODE) 'node-property)
    (let ((prop (org-element-property :key NODE)))
      (string-equal-ignore-case (if (not (stringp prop)) (prin1-to-string prop) prop) (if (stringp KEY) KEY (symbol-name KEY))))))

(defun org-todoist--set-prop-in-place (DRAWER KEY VALUE)
  "Check for property `KEY' in `DRAWER' and replace value with `VALUE' if present."
  (when (not (eq (org-element-type DRAWER) 'property-drawer))
    (error "Expected property drawer"))
  (let ((existing (org-element-map DRAWER 'node-property
                    (lambda (prop) (when (org-todoist--is-property prop KEY) prop)) nil t)))
    (if existing
        ;; (org-element-put-property existing (org-todoist--to-symbol KEY) VALUE)
        (org-element-put-property existing :value VALUE)
      (org-element-adopt DRAWER (org-todoist--create-property KEY VALUE)))))

(defun org-todoist--get-key (KEY)
  "Get the org property key for given `KEY'.
This performs any property name mapping between Todoist and org representations."
  (if (or (eq KEY 'id)
          (and (stringp KEY)
               (string-equal-ignore-case KEY "id")))
      org-todoist--id-property
    KEY))

(defun org-todoist--get-value (VALUE)
  "Get `VALUE' as its string representation."
  (if (stringp VALUE)
      VALUE
    (prin1-to-string VALUE)))

(defun org-todoist--add-prop (DRAWER KEY VALUE)
  "Add property with `KEY' and `VALUE' to property-drawer `DRAWER'.
If `DRAWER' is another node type, create and adopt a new property drawer.
Replaces the current value with `VALUE' if property `KEY' already exists."
  (let ((key (org-todoist--get-key KEY))
        (value (org-todoist--get-value VALUE))
        (type (org-element-type DRAWER)))
    (cond ((eq type 'property-drawer) ; Add directly
           (org-todoist--set-prop-in-place DRAWER key value)
           DRAWER)
          ((eq type 'headline) ; Find drawer and update both drawer and property plist
           (let* ((headline DRAWER)
                  (drawer (org-todoist--get-property-drawer headline)))
             ;; (org-todoist--put-node-attribute headline (org-todoist--to-symbol KEY) VALUE) ;; update property plist
             (unless drawer ; create drawer if needed
               (setq drawer (org-element-create 'property-drawer))
               (org-todoist--adopt-drawer headline drawer))
             (org-todoist--set-prop-in-place drawer key value)
             drawer))
          (t (signal 'todoist--error "Called org-todoist--add-prop with invalid type")))))

(defun org-todoist--get-prop (NODE KEY)
  "Retrieves the property `KEY' from the property drawer directly under `NODE'.
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

(defun org-todoist--remove-prop (NODE KEY)
  "Remove property with `KEY' from `NODE's property drawer."
  (let* ((key (org-todoist--get-key KEY))
         (drawer (if (equal (org-element-type NODE) 'property-drawer)
                     NODE
                   (org-todoist--get-property-drawer NODE)))
         (prop (when drawer
                 (org-element-map drawer 'node-property
                   (lambda (np) (when (org-todoist--is-property np key) np))
                   nil t))))
    (when prop (org-element-extract prop))))

(defun org-todoist--add-all-properties (NODE PROPERTIES &optional SKIP)
  "Add or update the values of all properties in the alist `PROPERTIES'.
Properties are added to `NODE' unless they are in plist `SKIP'.
RETURNS the mutated `NODE'."
  (dolist (kv PROPERTIES)
    (unless (member (car kv) SKIP)
      (org-todoist--add-prop NODE (car kv) (cdr kv))))
  NODE)

(defun org-todoist--get-todoist-type (NODE &optional NO-INFER)
  "Gets the TODOIST_TYPE of a `NODE'.
When `NO-INFER', do not attempt to guess the type of `NODE'."
  (if NO-INFER
      (org-todoist--get-prop NODE org-todoist--type)
    (cond
     ;; type is already present on headline or drawer
     ((org-todoist--get-prop NODE org-todoist--type) (org-todoist--get-prop NODE org-todoist--type))
     ;; Only tasks can be todos
     ((org-element-property :todo-type NODE) org-todoist--task-type)
     ;; Unlabeled level 1 headlines are assumed to be projects
     ((eql (org-element-property :level NODE) 1) org-todoist--project-type)
     ;; Else guess
     (t (if-let* ((parent (org-element-parent NODE))
                  (parenttype (org-todoist--get-todoist-type parent)))
            (cond
             ((string= parenttype org-todoist--task-type) org-todoist--task-type)
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
  "Gets the org-element property `KEY' of `NODE' as a string."
  (org-todoist-org-element-to-string (org-element-property KEY NODE)))

(defun org-todoist-org-element-to-string (DATA)
  "Convert `DATA' (an org-element or tree) to its string representation.
Note, a \n character is appended if not present."
  (substring-no-properties (org-element-interpret-data DATA)))

(defun org-todoist--remove-ws (STRING)
  "Remove spaces from `STRING'."
  (string-replace " " "" STRING))

(defun org-todoist--is-v9 (&optional AST)
  "Check if any org-todoist-ids are numeric (indicates v9 API IDs)."
  (catch 'found
    (org-element-map (or AST (org-todoist--file-ast)) 'headline
      (lambda (hl)
        (when-let* ((supported (member
                                (org-todoist--get-todoist-type hl t)
                                `(,org-todoist--task-type
                                  ,org-todoist--section-type
                                  ,org-todoist--project-type)))
                    (id (org-todoist--get-prop hl org-todoist--id-property))
                    (is-old (org-todoist--id-is-v9 id)))
          (unless (string= id org-todoist--default-id)
            (throw 'found t))))
      nil t)
    nil))

(defun org-todoist--is-v1 ()
  "Check if IDs appear to be in v2 UUID format."
  (not (org-todoist--is-v9)))

(defun org-todoist--do-reset (ARG)
  "Sync with a full reset token and overwrite the SYNC-BUFFER.
`ARG' is passed to `org-todoist--do-sync'."
  (let ((sb (org-todoist--storage-file org-todoist--sync-buffer-file)))
    (when (file-exists-p sb)
      (copy-file sb (org-todoist-file) t))
    (org-todoist--do-sync "*" (null ARG))))

(defun org-todoist--verify-changes-before-reset (&optional ARG)
  "Verify changes with user before performing a full sync reset.
Checks for pending sync commands first, then opens ediff if needed.
After user confirms, performs an incremental sync first, then full reset."
  (let* ((current-ast (org-todoist--file-ast))
         (last-sync-ast (org-todoist--get-last-sync-buffer-ast))
         (pending-commands (and last-sync-ast
                                (org-todoist--push current-ast last-sync-ast)))
         (ediff-buf (get-buffer-create "*Org-Todoist Reset Verify*"))
         (proceed nil))

    ;; Check if there are pending changes first
    (when pending-commands
      (with-current-buffer ediff-buf
        (erase-buffer)
        (insert (format "You have %d pending changes that would be lost during reset:\n\n"
                        (length pending-commands)))
        (insert "Pending changes may be LOST during reset unless you:\n"
                "1) Sync them first (recommended), or\n"
                "2) Merge them manually using ediff\n\n"
                "Options:\n"
                "[s] Sync changes first, then reset\n"
                "[e] Show changes in ediff\n"
                "[r] Reset anyway (DANGER: lose changes)\n"
                "[a] Abort reset completely\n")
        (pop-to-buffer ediff-buf)

        (let ((response (read-char-choice
                         "Select action: (s/e/r/a) "
                         '(?s ?e ?r ?a))))
          (cond ((eq response ?s) ; Sync then reset
                 (kill-buffer ediff-buf)
                 (org-todoist-sync t) ; Sync without opening buffer
                 (setq proceed t))

                ((eq response ?e) ; Show ediff
                 (kill-buffer ediff-buf)
                 (org-todoist-ediff-snapshot)
                 (setq proceed nil))

                ((eq response ?r) ; Reset anyway
                 (kill-buffer ediff-buf)
                 (setq proceed (y-or-n-p "Really reset and lose all pending changes? ")))

                (t ; Abort
                 (kill-buffer ediff-buf)
                 (message "Reset canceled"))))))

    ;; Proceed with reset if confirmed or no pending changes
    (when (or (not pending-commands) proceed)
      (message "Resetting...")
      (org-todoist--do-reset ARG))))

(defun org-todoist--batch-commands (commands)
  "Split COMMANDS into chunks of `org-todoist-command-batch-size'."
  (let ((max-batch-size org-todoist-command-batch-size))
    (cl-loop for i from 0 to (length commands) by max-batch-size
             collect (seq-subseq commands i (min (+ i max-batch-size) (length commands))))))

(defun org-todoist--replace-temp-ids (commands temp-id-map)
  "Replace temporary IDs in COMMANDS with real IDs from TEMP-ID-MAP."
  (cl-loop for cmd in commands collect
           (cl-loop for (key . value) in cmd
                    if (eq key 'args)
                    collect (cons key (org-todoist--map-replace-temp-ids value temp-id-map))
                    else collect (cons key value))))

(defun org-todoist--map-replace-temp-ids (args temp-id-map)
  "Recursively replace temp IDs in ARGS using TEMP-ID-MAP."
  (cl-loop for (k . v) in args
           collect (cons k (if (stringp v)
                               (or (cdr (assoc v temp-id-map)) v)
                             (if (listp v)
                                 (org-todoist--map-replace-temp-ids v temp-id-map)
                               v)))))

(defun org-todoist--id-is-v9 (id)
  "String `ID' contains only numbers."
  (string-match-p "^[0-9]+$" id))

;; User functions

;;;###autoload
(defun org-todoist-show-assignee ()
  "Display assigned collaborator's name as an overlay next to task at point."
  (interactive)
  (unless (string= (buffer-file-name) (org-todoist-file))
    (user-error "This command only works in the org-todoist buffer"))
  (let* ((uid (org-entry-get nil "responsible_uid"))
         (collaborator (when uid (org-todoist--get-by-id org-todoist--collaborator-type uid (org-todoist--file-ast))))
         (name (when collaborator (org-element-property :raw-value collaborator)))
         (pos (save-excursion
                (goto-char (org-entry-beginning-position))
                (search-forward " " (line-end-position) t 3) ; Skip past TODO state and priority
                (point)))
         (ov (make-overlay pos pos)))
    (when name
      (overlay-put ov 'after-string (propertize (concat "[@" name "] ") 'face 'org-todoist-beige-face))
      (overlay-put ov 'org-todoist-assignee-overlay t)
      (add-hook 'before-change-functions (lambda (&rest _) (delete-overlay ov)) nil t))))

;;;###autoload
(defun org-todoist-show-all-assignees ()
  "Display assigned collaborators for all tasks in the current buffer."
  (interactive)
  (unless (string= (buffer-file-name) (org-todoist-file))
    (user-error "This command only works in the org-todoist buffer"))
  (org-map-entries #'org-todoist-show-assignee))

;;;###autoload
(defun org-todoist-capture-task ()
  "Capture a new task in Todoist using org-capture."
  (interactive)
  (unless org-todoist-api-token
    (user-error "Please set org-todoist-api-token"))
  (let ((org-capture-templates
         '(("t" "Task" entry (function org-todoist-find-project-and-section)
            "* TODO %^{What is the task} %^G %^{EFFORT}p %(org-todoist-assign-task) %(progn (org-schedule nil) nil) %(progn (org-deadline nil) nil)\n%?")
           ("n" "Project Notes" entry (function org-todoist-project-notes) "* %?"))))
    (org-capture nil)))

;;;###autoload
(defun org-todoist-quick-task (task-text note reminder)
  "Create a task in Todoist using natural language processing.
TASK-TEXT should be a natural language task description with optional due date.
Example: \"Submit report by tomorrow 5pm p2\""
  (interactive (list (read-string "Task text: ")
                     (read-string "Comment (empty for none): ")
                     (read-string "Reminder time (empty for none): ")))
  (unless org-todoist-api-token
    (user-error "Please set org-todoist-api-token"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " org-todoist-api-token))))
         (url-request-data
          (json-encode `(("text" . ,task-text)
                         ("note" . ,note)
                         ("reminder" . ,reminder)
                         ("auto_reminder" . ,org-todoist-use-auto-reminder)))))

    (url-retrieve "https://api.todoist.com/api/v1/tasks/quick"
                  (lambda (status)
                    (if-let ((http-error (plist-get status :error)))
                        (message "Error creating task: %s" http-error)
                      (with-current-buffer (current-buffer)
                        (goto-char url-http-end-of-headers)
                        (let* ((response (json-read))
                               (task-url (org-todoist--create-link (alist-get 'id response))))
                          (setq org-todoist--last-quick-task-url task-url)
                          (message "Task created: %s (%s)"
                                   (alist-get 'content response)
                                   task-url))))))))

(defun org-todoist--get-parent-id-of-type-no-ast (TYPE &optional POINT)
  "Gets the parent(s) of node at `POINT' with TODOIST_TYPE `TYPE'.
If `TYPE' is nil, returns the first parent of any todoist type."
  (ignore-errors ; when top of tree is reached org will throw an error
    (save-excursion
      (when POINT (goto-char POINT))
      (org-back-to-heading t)
      (let ((found nil))
        (while (and (not found) (org-up-heading-all 1))
          (when-let ((type (org-entry-get nil org-todoist--type)))
            (when (or (null TYPE) (string= type TYPE))
              (setq found (org-entry-get nil org-todoist--id-property)))))
        found))))


(defun org-todoist--create-link (ID TYPE &optional ARG)
  "Create a todoist link of `TYPE' for `ID' with `ARG'.

`ARG' should be:
TASK: nil
SECTION: POINT int or project ID. Nil for current point.
Only required in app scheme.
PROJECT: nil
SEARCH: search query string or nil."
  (cond ((eq org-todoist-url-scheme 'browser)
         (cond ((equal TYPE "TASK") (format "https://app.todoist.com/app/task/%s" ID))
               ((equal TYPE "SECTION") (format "https://app.todoist.com/app/section/%s" ID))
               ((equal TYPE "PROJECT") (format "https://app.todoist.com/app/project/%s" ID))
               ((equal TYPE "PROJECTS") "https://app.todoist.com/app/projects/")
               ((equal TYPE "QUICKADD") "https://app.todoist.com/add")
               ((equal TYPE "SEARCH") (if (or (null ARG) (string-blank-p ARG))
                                          "https://app.todoist.com/app/search"
                                        (concat "https://app.todoist.com/app/search?query=" ARG)))
               ((equal TYPE "UPCOMING") "https://app.todoist.com/app/upcoming")
               ((equal TYPE "INBOX") "https://app.todoist.com/app/inbox")
               ((equal TYPE "TODAY") "https://app.todoist.com/app/today")))
        ((eq org-todoist-url-scheme 'app)
         (cond ((equal TYPE org-todoist--task-type) (concat "todoist://task?id=" ID))
               ((equal TYPE org-todoist--section-type)
                (cond ((stringp ARG) (org-todoist--create-link ID org-todoist--project-type))
                      (t (if-let ((proj (org-todoist--get-parent-id-of-type-no-ast
                                         org-todoist--project-type
                                         (or ARG (point)))))
                             (org-todoist--create-link proj org-todoist--project-type)
                           (user-error "Cannot open sections when org-todoist-url-scheme is 'app and no project found for section")))))
               ((equal TYPE org-todoist--project-type) (concat "todoist://project?id=" ID))
               ((equal TYPE "SEARCH") (if (or (null ARG) (string-blank-p ARG))
                                          "todoist://search"
                                        (concat "todoist://search?query=" ARG)))
               ((equal TYPE "QUICKADD") "todoist://openquickadd")
               ((null TYPE) "todoist://")
               ((equal TYPE "PROJECTS") "todoist://projects")
               ((equal TYPE "UPCOMING") "todoist://upcoming")
               ((equal TYPE "INBOX") "todoist://inbox")
               ((equal TYPE "TODAY") "todoist://today")))
        (t (user-error "Variable org-todoist-url-scheme must be 'browser or 'app"))))

;;;###autoload
(defun org-todoist-xdg-open-project ()
  "Open a Todoist project in the desktop app."
  (interactive)
  (let* ((projects (org-todoist--project-nodes (org-todoist--file-ast)))
         (project-names (--map (org-element-property :raw-value it) projects))
         (selected (completing-read "Open project: " project-names))
         (id (org-todoist--get-prop (--first (string= selected (org-element-property :raw-value it)) projects)
                                    org-todoist--id-property)))
    (browse-url-xdg-open (org-todoist--create-link id org-todoist--project-type))))

(defun org-todoist--type-at-point (&optional point)
  "Get the type of the Todoist item at `point'."
  (org-entry-get point org-todoist--type))

;;;###autoload
(defun org-todoist-jump-to-current-project ()
  "Select the current projectile project and narrow to it in the Todoist buffer."
  (interactive)
  (unless (require 'projectile nil 'no-error)
    (message "Projectile not found, falling back to user-provided project selection. Please install Projectile to use this feature."))
  (org-todoist-jump-to-project
   (when (and (require 'projectile nil 'no-error)
              (fboundp 'projectile-project-name)
              (not (string= (projectile-project-name) "-")))
     (projectile-project-name))))

;;;###autoload
(defun org-todoist-jump-to-project (&optional project)
  "Select `PROJECT' and narrow to it in the Todoist buffer."
  (interactive)
  (let* ((ast (org-todoist--file-ast))
         (projects (org-todoist--project-nodes ast))
         (project-names (--map (org-element-property :raw-value it) projects))
         (selected-project (if (and project
                                    (member project project-names))
                               project
                             (completing-read "Which project? " project-names)))
         (selected-project-element (--first (string= (org-element-property :raw-value it) selected-project) projects)))
    (when selected-project-element
      (find-file (org-todoist-file))
      (widen)
      (goto-char (org-element-property :begin selected-project-element))
      (org-narrow-to-subtree)
      (org-fold-show-children)
      (org-reveal))))

;;;###autoload
(defun org-todoist-xdg-open-quickadd ()
  "Open Todoist quick add panel in desktop app."
  ;; TODO not working with linux appimage?
  (interactive)
  (browse-url-xdg-open (org-todoist--create-link nil "QUICKADD")))

(defun org-todoist-xdg-open-search-query (QUERY)
  "Open a search in Todoist with `QUERY'."
  (interactive "sQuery: ")
  (browse-url-xdg-open (org-todoist--create-link nil "SEARCH" QUERY)))

;;;###autoload
(defun org-todoist-open-last-quick-task-in-app ()
  "Xdg-open the last quick task url."
  (interactive)
  (unless org-todoist--last-quick-task-url
    (user-error "No previous quick task!"))
  (browse-url-xdg-open org-todoist--last-quick-task-url))

;;;###autoload
(defun org-todoist-xdg-open ()
  "Open task using xdg-open."
  (interactive)
  (if-let ((_ (equal (org-todoist-file) (buffer-file-name)))
           (type (org-entry-get nil org-todoist--type))
           (id (org-entry-get nil org-todoist--id-property)))
      (browse-url-xdg-open (org-todoist--create-link id type))
    (org-todoist-xdg-open-task-search)))

;;;###autoload
(defun org-todoist-xdg-open-task-search ()
  "Quickly select a project, section, or task and open in Todoist app.
Uses built-in completion without external dependencies."
  (interactive)
  (let* ((items (with-todoist-buffer!
                 (goto-char (point-min))
                 ;; sort the items by project and section. Within a section, sort by due data.
                 (--filter it (org-map-entries
                               (lambda ()
                                 (when (equal org-todoist--task-type (org-entry-get nil org-todoist--type))
                                   (let* ((id (org-entry-get nil org-todoist--id-property))
                                          (title (org-entry-get nil "ITEM"))
                                          (assignee (org-entry-get nil "RESPONSIBLE_UID"))
                                          (project (org-element-property :raw-value
                                                                         (org-todoist--get-parent-of-type org-todoist--project-type (org-element-at-point) t)))
                                          (section (org-element-property :raw-value
                                                                         (org-todoist--get-parent-of-type org-todoist--section-type (org-element-at-point) t)))
                                          (todo-state (org-entry-get nil "TODO"))
                                          (priority (org-entry-get nil "PRIORITY"))
                                          (effort (org-entry-get nil "EFFORT"))
                                          (is-done (member todo-state `(,org-todoist-done-keyword ,org-todoist-deleted-keyword))))

                                     (cons
                                      (s-join " "
                                              `(,(propertize project 'face 'org-level-1)
                                                "•"
                                                ,(propertize (or section org-todoist--default-section-name) 'face 'org-level-2)
                                                "•"
                                                ,(when priority
                                                   (propertize (format "[#%s]" priority) 'face 'org-priority))
                                                ,(propertize todo-state 'face (if is-done 'shadow 'org-todo))
                                                ,(if is-done
                                                     (propertize title 'face 'shadow)
                                                   title)

                                                ,(when assignee
                                                   (propertize (format "(@%s)" (org-element-property :raw-value
                                                                                                     (org-todoist--get-by-id org-todoist--collaborator-type assignee (org-todoist--file-ast))))
                                                               'face 'org-link))
                                                ,(when effort
                                                   (propertize (format "(%s)" effort) 'face 'org-special-keyword))))
                                      id)
                                     )))))))
         (selection (completing-read "Select Todoist item: " items))
         (id (cdr (assoc selection items))))

    (when id
      (browse-url-xdg-open (org-todoist--create-link id org-todoist--task-type)))))

;;;###autoload
(defun org-todoist-report-bug ()
  "Report a bug with org-todoist.
Generates diagnostics, exports to GitHub Flavored Markdown, copies to clipboard,
and opens the GitHub issue creation page."
  (interactive)
  ;; Generate diagnostics
  (org-todoist-diagnose)

  (save-window-excursion
    (let ((diag-buffer (get-buffer "*Org-Todoist Diagnostics*"))
          (org-export-show-temporary-export-buffer t)
          (gfm-export-fn (if (fboundp 'org-gfm-export-to-markdown)
                             'org-gfm-export-as-markdown
                           'org-md-export-as-markdown)))
      (switch-to-buffer diag-buffer)
      ;; Export diagnostics to markdown
      (funcall gfm-export-fn)
      (clipboard-kill-region (point-min) (point-max))
      (message "GFM diagnostics copied to clipboard. Please redact any sensitive information and paste into GitHub issue body."))
    (kill-buffer))

  ;; Open GitHub issues page
  (browse-url "https://github.com/Lillenne/org-todoist/issues/new"))

;;;###autoload
(defun org-todoist-diagnose ()
  "Display diagnostic information in a temporary buffer.
Includes last request, response, diff, and push information."
  (interactive)
  (message "Preparing diagnostics. This may take a moment...")
  (let ((buf (get-buffer-create "*Org-Todoist Diagnostics*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (org-mode)

      ;; API Version Warning
      (when (or (null (org-todoist--api-version)) (eq (org-todoist--api-version) 'sync-v9))
        (insert "*WARNING:* Using deprecated Todoist Sync API v9\n"
                "Migrate to Unified API v1 with:\n"
                "#+begin_src emacs-lisp\n"
                "(org-todoist-migrate-to-v1)\n"
                "#+end_src\n"
                "===============================================\n\n"))

      ;; Unified Diff section
      (insert "* Unified Diff vs Last Snapshot\n")
      (let ((current-file (org-todoist-file))
            (snapshot-file (org-todoist--storage-file org-todoist--sync-buffer-file)))
        (if-let* ((exists (file-exists-p snapshot-file))
                  (diff-str (shell-command-to-string
                             (format "diff -u %s %s" (shell-quote-argument snapshot-file)
                                     (shell-quote-argument current-file))))
                  (has-diff (not (s-blank? diff-str))))
            (insert "#+BEGIN_SRC diff\n"
                    diff-str
                    "\n#+END_SRC")
          (insert (if (file-exists-p snapshot-file)
                      "No changes since last sync!"
                    "No snapshot exists. Perform a sync first!"))))
      (insert "\n\n")

      ;; Push Test section
      (insert "* Pending Commands (Push Test)\n")
      (if-let ((org-todoist--request-preview (org-todoist--generate-push-preview)))
          (progn
            (insert "#+BEGIN_SRC json\n")
            (insert (with-temp-buffer
                      (insert (json-encode org-todoist--request-preview))
                      (json-pretty-print-buffer)
                      (buffer-string)))
            (insert "\n#+END_SRC"))
        (insert "No pending commands"))
      (insert "\n\n")

      ;; Last Request section
      (insert "* Last Request\n")
      (let ((org-todoist-last-request (org-todoist--get-last-request))
            (org-todoist-last-response (org-todoist--get-last-response)))
        (if org-todoist-last-request
            (insert "#+BEGIN_SRC json\n"
                    org-todoist-last-request
                    "\n#+END_SRC")
          (insert "No request recorded"))
        (insert "\n\n")

        ;; Last Response section
        (insert "* Last Response\n")
        (if org-todoist-last-response
            (insert "#+BEGIN_SRC json\n"
                    org-todoist-last-response
                    "\n#+END_SRC")
          (insert "No response recorded"))

        (goto-char (point-min))
        (org-fold-show-all)
        (view-mode 1)
        (pop-to-buffer buf))))
  (message "Diagostic generation complete."))

;;;###autoload
(defun org-todoist-cancel-background-sync ()
  "Cancel sync with Todoist servers in the background."
  (interactive)
  (when org-todoist--background-timer
    (cancel-timer org-todoist--background-timer)
    (setq org-todoist--background-timer nil)))

;;;###autoload
(defun org-todoist-background-sync ()
  "Sync with Todoist servers in the background."
  (interactive)
  (unless org-todoist--background-timer
    (setq org-todoist--background-timer
          (run-at-time org-todoist-background-sync-offset
                       org-todoist-background-sync-interval
                       #'org-todoist---sync))))

;;;###autoload
(defun org-todoist-toggle-background-sync ()
  "Toggle background sync."
  (interactive)
  (if org-todoist--background-timer
      (org-todoist-cancel-background-sync)
    (org-todoist-background-sync)))

;;;###autoload
(defun org-todoist-unassign-task ()
  "Unassign task(s) at point or in region.
When region is active, unassign all tasks in the region."
  (interactive)
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (let ((count 0))
            (while (re-search-forward org-heading-regexp nil t)
              (when (org-entry-get nil "responsible_uid")
                (org-delete-property "responsible_uid")
                (cl-incf count)))
            (message "Unassigned %d tasks" count))))
    (when (org-entry-get nil "responsible_uid")
      (org-delete-property "responsible_uid"))))

;;;###autoload
(defun org-todoist-ignore-subtree ()
  "Do not push the subtree at point to Todoist.

Only works on subtrees which are not already tracked by Todoist.
Done by settings the `org-todoist-type' propety of the headline at point
to the `org-todoist--ignored-node-type'."
  (interactive)
  (if (org-entry-get nil org-todoist--type)
      (user-error "Can't ignore subtrees that are already tracked by Todoist!")
    (org-set-property org-todoist--type org-todoist--ignored-node-type)))

;;;###autoload
(defun org-todoist-add-subproject (ARG)
  "Add a new subproject to project at point.
`ARG' is passed to `org-insert-subheading'."
  (interactive "P")
  (let ((type (org-entry-get nil org-todoist--type)))
    (if (and type (not (string= type org-todoist--project-type))) ; if there is no type, assume it is probably a new project.
        (user-error "Can only add subproject to project headlines!")
      (org-insert-subheading ARG)
      (org-set-property org-todoist--type org-todoist--project-type))))

;;;###autoload
(defun org-todoist-tag-user ()
  "Tag a Todoist collaborator in the current comment and notify them on next sync."
  (interactive)
  (when-let ((selectedelement (org-todoist--select-user "Tag: ")))
    (if org-todoist-comment-tag-user-pretty
        (insert "[[" (org-element-property :raw-value selectedelement) "][todoist-mention://" (org-todoist--id-or-temp-id selectedelement) "]")
      (insert "[" (org-element-property :raw-value selectedelement) "](todoist-mention://" (org-todoist--id-or-temp-id selectedelement) ")"))))

;;;###autoload
(defun org-todoist-assign-task ()
  "Prompt for collaborator selection and assign task(s) at point.
When region is active, assign all tasks in the region to the selected user."
  (interactive)
  (let ((can-assign (org-element-property :CAN_ASSIGN_TASKS
                                          (car (org-element-resolve-deferred
                                                (org-todoist--get-parent-of-type org-todoist--project-type (org-element-at-point)))))))
    (if (and can-assign (not (equal "t" can-assign)))
        (user-error "Current project does not support task assignment!")
      (when-let ((selectedelement (org-todoist--select-user "Assign to: ")))
        (let ((uid (org-todoist--get-prop selectedelement org-todoist--id-property)))
          (if (use-region-p)
              (save-excursion
                (save-restriction
                  (narrow-to-region (region-beginning) (region-end))
                  (goto-char (point-min))
                  (let ((count 0))
                    (while (re-search-forward org-heading-regexp nil t)
                      (let ((type (org-entry-get nil org-todoist--type)))
                        (when (or (null type) (string= type org-todoist--task-type))
                          (org-set-property "responsible_uid" uid)
                          (cl-incf count))))
                    (message "Assigned %d tasks to %s"
                             count
                             (org-element-property :raw-value selectedelement)))))
            (let ((type (org-entry-get nil org-todoist--type)))
              (if (and type (not (string= type org-todoist--task-type)))
                  (user-error "Can only assign users to task headlines")
                (org-set-property "responsible_uid" uid)))))))))

;;;###autoload
(defun org-todoist-migrate-to-v1 ()
  "Migrate data from Todoist API v9 to v1.
This function updates all Todoist IDs in `org-todoist-file' to the new
format used by API v1. This should only be run once. It requires
`org-todoist-api-token' to be set."
  (interactive)
  (when (y-or-n-p "This will modify your org-todoist file to be compatible with API v1. This is irreversible. Continue? ")
    (message "Starting migration to Todoist API v1...")
    (let* ((ast (org-todoist--file-ast))
           (projects '())
           (sections '())
           (tasks '())
           (id-map (make-hash-table :test 'equal)))
      ;; 1. Collect all old IDs
      (org-element-map ast 'headline
        (lambda (hl)
          (let ((id (org-entry-get hl org-todoist--id-property))
                (type (org-todoist--get-todoist-type hl t)))
            (when (and id (not (string= id org-todoist--default-id)) (org-todoist--id-is-v9 id))
              (cond
               ((string= type org-todoist--project-type) (push id projects))
               ((string= type org-todoist--section-type) (push id sections))
               ((string= type org-todoist--task-type) (push id tasks)))))))

      ;; 2. Fetch new IDs from mapping endpoint
      (message "Fetching new IDs from Todoist...")
      (dolist (type-and-ids `(("projects" . ,projects)
                              ("sections" . ,sections)
                              ("tasks" . ,tasks)))
        (let ((obj-name (car type-and-ids))
              (ids (cdr type-and-ids)))
          (when ids
            (message "Migrating %s..." obj-name)
            (while ids
              (let* ((batch (seq-take ids 100))
                     (ids-str (mapconcat #'identity batch ","))
                     (url (format "https://api.todoist.com/api/v1/id_mappings/%s/%s" obj-name ids-str))
                     (url-request-method "GET")
                     (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " org-todoist-api-token)))))
                (with-current-buffer (url-retrieve-synchronously url)
                  (goto-char (point-min))
                  (if (goto-char url-http-end-of-headers)
                      (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
                             (mappings (json-read-from-string json-string)))


                        (dotimes (i (length mappings))
                          (let ((mapping (aref mappings i)))
                            (puthash (assoc-default 'old_id mapping)
                                     (assoc-default 'new_id mapping)
                                     id-map))))
                    (error "Failed to fetch ID mappings for %s" obj-name))))
              (setq ids (seq-drop ids 100))))))

      ;; 3. Update IDs in AST
      (message "Updating IDs in org file...")
      (org-element-map ast 'headline
        (lambda (hl)
          (when-let* ((old-id (org-todoist--get-prop hl org-todoist--id-property))
                      (new-id (gethash old-id id-map)))
            (org-todoist--insert-id hl new-id))))

      ;; 4. Update API version
      (org-todoist--add-prop (org-todoist--metadata-node ast)
                             org-todoist--api-version-prop
                             (setq org-todoist--api-version 'v1))

      ;; 5. Write updated AST to file
      (org-todoist--update-file ast)

      ;; 6. Update org-todoist--last-sync-buffer to use the updated ids
      (when (file-exists-p (org-todoist--storage-file org-todoist--sync-buffer-file))
        (with-current-buffer (find-file-noselect (org-todoist--storage-file org-todoist--sync-buffer-file))
          (org-mode)
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (let ((old-id (org-entry-get nil org-todoist--id-property)))
               (when old-id
                 (let ((new-id (gethash old-id id-map)))
                   (when new-id
                     (org-set-property org-todoist--id-property new-id)))))))
          (save-buffer)
          (kill-buffer)))

      (message "Migration to API v1 complete. Your Todoist file has been updated with new IDs."))))

;;;###autoload
(defun org-todoist-my-tasks (&optional ARG USER)
  "Create an org agenda view of tasks assigned to a specific `USER'.
With prefix `ARG', include unassigned tasks.
`USER' defaults to the current user."
  (interactive (list current-prefix-arg
                     (or (org-todoist-my-id)
                         (org-todoist--get-prop (org-todoist--select-user "Which user? (set org-todoist-my-id to avoid this prompt) ")
                                                org-todoist--id-property))))
  (let ((org-agenda-files (list (org-todoist-file))))
    (if ARG
        (org-tags-view nil (concat "+TODOIST_TYPE={TASK}+responsible_uid={" USER "}|+TODOIST_TYPE={TASK}-responsible_uid"))
      (org-tags-view nil (concat "+TODOIST_TYPE={TASK}+responsible_uid={" USER "}")))))

;;;###autoload
(defun org-todoist-view-user-tasks (USER)
  "View tasks assigned to `USER' in the org-todoist file."
  (interactive (list (org-todoist--get-prop (org-todoist--select-user "View tasks for user: ") org-todoist--id-property)))
  (let ((org-agenda-files (list (org-todoist-file)))
        (org-todoist-my-id USER))
    (org-todoist-my-tasks current-prefix-arg USER)))

;;;###autoload
(defun org-todoist-ediff-snapshot ()
  "Compare current org-todoist file with last synced snapshot using ediff."
  (interactive)
  (cond ((not (file-exists-p (org-todoist-file)))
         (user-error "No org-todoist-file exists - perform a sync first to create one"))
        ((not (file-exists-p (org-todoist--storage-file org-todoist--sync-buffer-file)))
         (user-error "No snapshot exists - perform a sync first to create one"))
        (t (let ((current-buf (find-file-noselect (org-todoist-file)))
                 (snapshot-file (org-todoist--storage-file org-todoist--sync-buffer-file)))
             (ediff-buffers (find-file-noselect snapshot-file)
                            current-buf)))))

;;;###autoload
(defun org-todoist-toggle-remote-deletion ()
  "Toggle remote item deletion."
  (interactive)
  (setq org-todoist-delete-remote-items (not org-todoist-delete-remote-items)))

;;;###autoload
(defun org-todoist-sync (&optional ARG)
  "Perform a bidirectional incremental sync with Todoist.

The commands to apply will be determined by diffing the copy of the
Todoist file (see function `org-todoist-file') that was saved
during the last sync with the current version of the file.
Commands are processed prior to fetching, thus local changes
will overwrite remote changes.

With `ARG', do not open the Todoist buffer after sync."
  (interactive "P")
  (org-todoist--do-sync (org-todoist--get-sync-token) (null ARG)))

;;;###autoload
(defun org-todoist--reset (&optional ARG)
  "Perform a full sync from Todoist.

This function is often used in development, but end users will likely
have no need for it.

With single prefix `ARG', do not open the Todoist buffer after sync.

With double prefix `ARG', delete the previous todoist file.
NOTE this will irreversibly discard all data not stored in Todoist
\(e.g., time tracking information and ignored sub-trees.\).

Local changes that haven't been synced will be preserved during reset."
  (interactive "P")
  (if (and (not (eql 16 (car ARG)))
           (file-exists-p (org-todoist--storage-file org-todoist--sync-buffer-file)))
      (org-todoist--verify-changes-before-reset ARG)
    (when (eql 16 (car ARG))
      (delete-file (org-todoist-file)))
    (org-todoist--do-reset ARG)))

;;;###autoload
(defun org-todoist-goto ()
  "Jump to the org todoist file."
  (interactive)
  (let ((file (org-todoist-file)))
    (unless (or (not (file-exists-p file))
                (equal (buffer-file-name) file))
      (find-file file))))

;; Transient interface and display functions
(require 'transient)

(defface org-todoist-title-face
  '((t (:foreground "#E44232" :weight bold :height 1.2)))
  "")

(defface org-todoist-heading-face
  '((t (:foreground "#E44232")))
  "")

(defface org-todoist-red-face
  '((t (:foreground "#E44232")))
  "")

(defface org-todoist-gray-face
  '((t (:foreground "#1E1E1E")))
  "")

(defface org-todoist-beige-face
  '((t (:foreground "#FFF9F3")))
  "")

(defface org-todoist-enabled-face
  '((t (:foreground "#009900" :weight bold)))
  "Face for enabled todoist features in transient menu")

(defun org-todoist--remote-deletion-display ()
  (format "Delete Remote Items: %s" (if org-todoist-delete-remote-items
                                        (propertize "Enabled" 'face 'org-todoist-enabled-face)
                                      (propertize "Disabled" 'face 'org-todoist-red-face))))

(defun org-todoist--background-sync-enabled-display ()
  (format "Background Sync: %s" (if org-todoist--background-timer
                                    (propertize "Enabled" 'face 'org-todoist-enabled-face)
                                  (propertize "Disabled" 'face 'org-todoist-red-face))))

(defun org-todoist--background-sync-display ()
  "Display the current background sync interval."
  (let* ((seconds org-todoist-background-sync-interval)
         (unit (cond
                ((< seconds 60) (cons seconds "seconds"))
                ((< seconds 3600) (cons (/ seconds 60) "minutes"))
                ((< seconds 86400) (cons (/ seconds 3600) "hours"))
                (t (cons (/ seconds 86400) "days")))))
    (format "Background Sync: Every %s %s"
            (propertize (format "%d" (car unit)) 'face (if org-todoist--background-timer
                                                           'org-todoist-enabled-face
                                                         'org-todoist-red-face))
            (cdr unit))))

(defun org-todoist--api-version-display ()
  (format "API Version: %s" (let ((str (propertize (symbol-name (org-todoist--api-version))
                                                   'face
                                                   (if (org-todoist--is-current-api)
                                                       'org-todoist-enabled-face
                                                     'org-todoist-red-face))))
                              (if (org-todoist--is-current-api)
                                  str
                                (concat str " "
                                        (propertize "[DEPRECATED! Press to migrate]" 'face '(:inherit shadow :weight bold)))))))


(transient-define-suffix org-todoist-toggle-remote-deletion-suffix ()
  :transient t
  :key "K"
  :description #'org-todoist--remote-deletion-display
  (interactive)
  (org-todoist-toggle-remote-deletion))

(transient-define-suffix org-todoist-toggle-background-sync-suffix ()
  :transient t
  :key "B"
  :description #'org-todoist--background-sync-enabled-display
  (interactive)
  (org-todoist-toggle-background-sync))

(transient-define-suffix org-todoist--api-version-suffix ()
  :transient t
  :key "V"
  :description #'org-todoist--api-version-display
  (interactive)
  (if (org-todoist--is-current-api)
      (message "You are already using the most recent API version.")
    (org-todoist-migrate-to-v1)))

(transient-define-suffix org-todoist--background-sync-interval-suffix ()
  :transient t
  :key "I"
  :description #'org-todoist--background-sync-display
  (interactive)
  (let ((new-interval (read-number "Set background sync interval (in seconds): " org-todoist-background-sync-interval)))
    (setq org-todoist-background-sync-interval new-interval)
    (when org-todoist--background-timer
      (cancel-timer org-todoist--background-timer)
      (setq org-todoist--background-timer nil))
    (org-todoist-background-sync)))

;;;###autoload
(transient-define-prefix org-todoist-dispatch ()
  "Org-Todoist interactive interface"
  :refresh-suffixes t
  [[:description (lambda () (propertize "Org-Todoist\n" 'face 'org-todoist-title-face))
    :pad-keys t
    (org-todoist--api-version-suffix)
    (org-todoist-toggle-remote-deletion-suffix)
    (org-todoist-toggle-background-sync-suffix)
    (org-todoist--background-sync-interval-suffix)]]
  [[:description (lambda () (propertize "Tasks" 'face 'org-todoist-heading-face))
                 ("q" "Quick Task" org-todoist-quick-task)
                 ("c" "Org Capture" org-todoist-capture-task)
                 ("g" "Assign" org-todoist-assign-task)
                 ("u" "Unassign" org-todoist-unassign-task)
                 ("@" "Tag User" org-todoist-tag-user)]
   [:description (lambda () (propertize "Sync Operations" 'face 'org-todoist-heading-face))
                 ("s" "Sync" org-todoist-sync)
                 ("e" "Ediff Changes" org-todoist-ediff-snapshot)
                 ("r" "Force Reset" org-todoist--reset)]
   [:description (lambda () (propertize "Org View" 'face 'org-todoist-heading-face))
                 ("f" "Todoist File" org-todoist-goto)
                 ("j" "Project" org-todoist-jump-to-project)
                 ("J" "Current project" org-todoist-jump-to-current-project)
                 ("m" "My Agenda" org-todoist-my-tasks)
                 ("u" "User Agenda" org-todoist-view-user-tasks)
                 ("G" "Show Assignees" org-todoist-show-all-assignees)]
   [:description (lambda () (propertize "Item Actions" 'face 'org-todoist-heading-face))
                 ("i" "Ignore Subtree" org-todoist-ignore-subtree)
                 ("a" "Add Subproject" org-todoist-add-subproject)]
   [:description (lambda () (propertize "Open in App" 'face 'org-todoist-heading-face))
                 ("o" "Open Task" org-todoist-xdg-open)
                 ("p" "Open Project" org-todoist-xdg-open-project)
                 ("q" "Open Quick Add" org-todoist-xdg-open-quickadd)
                 ("/" "Open Search Query" org-todoist-xdg-open-search-query)
                 ("x" "Last Quick Task" org-todoist-open-last-quick-task-in-app)]
   [:description (lambda () (propertize "Diagnostics" 'face 'org-todoist-heading-face))
                 ("D" "Show Diagnostics" org-todoist-diagnose)
                 ("P" "Test Push Commands" org-todoist--push-test)
                 ("R" "Report Bug" org-todoist-report-bug)
                 ("M" "Migrate to V1 API" org-todoist-migrate-to-v1)]])

(provide 'org-todoist)
;;; org-todoist.el ends here
