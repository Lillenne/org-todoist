;; Unfortunately, these are out of date!

(require 'ert)
(require 'org-todoist)

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

(ert-deftest org-todoist--test--first-parent-of-type ()
  (let* ((ast (org-todoist--generate-sample-ast))
         (task (org-todoist--get-by-id org-todoist--task-type "3" ast))
         (section (org-todoist--get-by-id org-todoist--section-type "2" ast)))
    (should (eq (org-todoist--first-parent-of-type task 'headline)
                section))))

(ert-deftest org-todoist--test--date-to-todoist ()
  (let* ((task (org-todoist--test-task))
         (planning (org-todoist--get-planning task))
         (schtimestamp (org-element-property :scheduled planning)))
    (should (string-equal "2024-12-31T23:59:00.0" (org-todoist--get-planning-date task :scheduled)))
    (should (string-equal "2024-12-31T23:59:00.0" (org-todoist--date-to-todoist schtimestamp)))))

(ert-deftest org-todoist--test--task-is-recurring ()
  (should (org-todoist--task-is-recurring (org-todoist--test-recurring-task))))

(ert-deftest org-todoist--test--project-nodes ()
  (should (length= (org-todoist--project-nodes (org-todoist--generate-sample-ast)) 2)))

(ert-deftest org-todoist--test--replace-description ()
  (let ((task (org-todoist--test-task))
        (text "New Desc\n"))
    (org-todoist--replace-description task text)
    (should (string-equal text (org-todoist--description-text task)))))

(ert-deftest org-todoist--test--timestamp-to-utc-str ()
  (let* ((ts (org-timestamp-from-string "<2025-01-01 23:30>"))
         (str (org-todoist--timestamp-to-utc-str ts)))
    (should (string= str "2025-01-02T07:30:00.0Z"))))

;; (ert-deftest org-todoist--test--schedule ()
;;   (let ((task (json-read-file "sample-task.json"))
;;         (headline (org-element-create 'headline '(:title "Test Headline" :level 1 :todo-type 'todo :todo-keyword "TODO"))))
;;     ;; (timestamp (org-timestamp-from-time (org-read-date nil t "2016-12-0T12:00:00.000000" nil ))))
;;     (org-element-adopt headline (org-todoist--create-planning task))
;;     (should (org-todoist--element-equals-str "* TODO Test Headline
;; DEADLINE: <2017-01-06 Fri> SCHEDULED: <2016-12-06 Tue>
;; " headline))))

;; (ert-deftest org-todoist--test--sort-by-child-order ()
;;   (let ((section (org-todoist--get-by-id org-todoist--section-type "2" (org-todoist--generate-sample-ast)))
;;         (PROPERTY "child_order"))
;;     (org-todoist--sort-by-child-order section PROPERTY)
;;     (let ((prev -1)
;;           (sorted (org-element-map section 'headline (lambda (hl) (when (org-todoist--get-prop hl PROPERTY) hl)))))
;;       (dolist (next sorted)
;;         (should (> (org-todoist--get-position next PROPERTY) prev))
;;         (setq prev (org-todoist--get-position next PROPERTY))))))

(ert-deftest org-todoist--test--set-effort ()
  (let ((hl (org-element-create 'headline '(:title "Headline" :level 1))))
    (org-todoist--set-effort hl test-task)
    (should (org-todoist--element-equals-str "* Headline
:PROPERTIES:
:EFFORT:   0:15
:END:
" hl))))
(ert-deftest org-todoist--test--set-todo ()
  (let ((task (org-todoist--test-task)))
    (org-todoist--set-todo task t)
    (should (cl-search "DONE" (org-todoist-org-element-to-string task)))
    (org-todoist--set-todo task nil)
    (should (cl-search "TODO" (org-todoist-org-element-to-string task)))))

(ert-deftest org-todoist--test--archived ()
  (let ((node (org-todoist--test-task)))
    (org-todoist--archive node)
    (should (-contains-p (org-element-property :tags node) org-archive-tag))
    ;; verify the round trip too
    (with-temp-buffer (insert (org-todoist-org-element-to-string node))
                      (org-mode)
                      (let* ((ast (org-element-parse-buffer))
                             (task (org-todoist--get-by-id org-todoist--task-type "3" ast))
                             (tags (org-element-property :tags task)))
                        (should (-contains-p tags org-archive-tag))))))

;; TODO fix
;; (ert-deftest org-todoist--test--get-or-create-node ()
;;   (let* ((node (org-todoist--test-task))
;;          (section (org-todoist--get-parent-of-type org-todoist--section-type node t))
;;          (updated (org-todoist--get-or-create-node section org-todoist--task-type (org-todoist--get-prop node "id") "updated" "updated" test-task))
;;          (newelem (org-todoist--get-or-create-node section org-todoist--task-type "2995104339" "new task" "My new desc" test-task)))
;;     (should newelem)
;;     (should (eq (org-element-property :parent newelem) section))
;;     (should updated)
;;     (should (eq updated node))
;;     (should (eq (org-element-property :parent updated) section))
;;     (should (string-equal-ignore-case "updated\n" (org-todoist--description-text updated)))
;;     (should (string-equal-ignore-case "updated" (org-todoist--get-prop-elem updated :title)))))

(ert-deftest org-todoist--test--description-text ()
  (should (string-equal-ignore-case "Description

More description

- [ ] With a plain list
- [ ] second bullet
" (org-todoist--description-text (org-todoist--test-task)))))

(ert-deftest org-todoist--test-update-file ()
  (let ((org-todoist-file (make-temp-file "todoist")))
    "Tests insertion of org syntax from abstract syntax tree into org-todoist-file"
    (org-todoist--update-file (org-todoist--generate-ast test-org-str))
    (with-temp-buffer
      (insert-file-contents (org-todoist-file))
      (should (equal (org-todoist--remove-ws test-org-str) (org-todoist--remove-ws (buffer-string)))))))

(ert-deftest org-todoist--test--get-parent ()
  "Tests getting the parent node of a specific type"
  (let* ((ast (org-todoist--generate-sample-ast))
         (task (org-todoist--test-task))
         (project (org-todoist--get-parent-of-type org-todoist--project-type task t)))
    (should (equal (org-todoist--get-prop project "ID") "1"))))

(ert-deftest org-todoist--test--insert-identifier ()
  "Tests adding the org-todoist--type property to a property drawer, creating it if necessary"
  (let* ((astwithout (org-element-map (org-todoist--generate-ast "* Headline\n") 'headline #'identity nil t))
         (astwith (org-element-map (org-todoist--generate-ast "* Headline
:PROPERTIES:
:MYPROP:  VALUE
:END:
") 'headline #'identity nil t)))
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

;; (ert-deftest org-todoist--test--is-property ()
;;   "Verifies detecting existing properties"
;;   (let* ((ast (org-todoist--generate-ast test-org-str-property-added))
;;          (drawer (org-todoist--get-property-drawer ast)))
;;     (let (result)
;;       (dolist (prop (org-element-map drawer 'node-property #'identity) result)
;;         (push (org-todoist--is-property prop "NEWPROP") result))
;;       (should (equal result '(t nil))))))

(ert-deftest org-todoist--test--get-property-drawer--exists-gets ()
  (let ((drawer (org-todoist--get-property-drawer (org-element-map (org-todoist--generate-ast test-org-str) 'headline #'identity nil t))))
    (should (org-element-type-p drawer 'property-drawer))))

;; (ert-deftest org-todoist--test--put-node-attribute ()
;;   (let* ((ast (org-todoist--generate-sample-ast))
;;          (hl (org-todoist--get-by-id org-todoist--task-type "13" ast)))
;;     (org-todoist--put-node-attribute hl "testattr" "MYVAL")
;;     (should (string-equal (org-todoist--get-node-attribute hl "testattr") "MYVAL"))))

(ert-deftest org-todoist--test-parse-time-component ()
  "Test parsing time components from strings"
  (let ((cases '(("at 2pm" (14 0))
                 ("5:30pm" (17 30))
                 ("9am" (9 0))
                 ("at 15:45" (15 45))
                 ("3" (3 0)))))
    (dolist (case cases)
      (should (equal (org-todoist--parse-time-component (car case))
                    (cadr case))))))

(ert-deftest org-todoist--test-parse-weekday ()
  "Test parsing weekdays from strings"
  (let ((cases '(("monday" "MON")
                 ("tue" "TUE")
                 ("wednesday" "WED")
                 ("thursday" "THU")
                 ("fri" "FRI")
                 ("saturday" "SAT")
                 ("sun" "SUN"))))
    (dolist (case cases)
      (should (equal (org-todoist--parse-weekday (car case))
                    (cadr case))))))

(ert-deftest org-todoist--test-parse-interval ()
  "Test parsing intervals from strings"
  (let ((cases '(("every other" 2)
                 ("every 3rd" 3)
                 ("every" 1)
                 ("every 5th" 5))))
    (dolist (case cases)
      (should (equal (org-todoist--parse-interval (car case))
                    (cadr case))))))

(ert-deftest org-todoist--test-parse-unit ()
  "Test parsing time units from strings"
  (let ((cases '(("day" day)
                 ("weeks" week)
                 ("month" month)
                 ("years" year))))
    (dolist (case cases)
      (should (equal (org-todoist--parse-unit (car case))
                    (cadr case))))))

(ert-deftest org-todoist--test--add-repeater-basic ()
  "Test basic recurring patterns"
  (let ((ts (org-element-create 'timestamp)))
    ;; Daily patterns with time
    (org-todoist--add-repeater ts "every day at 2pm")
    (should (eq (org-element-property :repeater-unit ts) 'day))
    (should (eq (org-element-property :repeater-value ts) 1))
    (should (eq (org-element-property :repeater-type ts) 'cumulate))
    (should (eq (org-element-property :hour-start ts) 14))
    (should (eq (org-element-property :minute-start ts) 0))
    
    ;; Weekly patterns with weekday
    (org-todoist--add-repeater ts "every friday")
    (should (eq (org-element-property :repeater-unit ts) 'week))
    (should (eq (org-element-property :repeater-value ts) 1))

    (org-todoist--add-repeater ts "every other friday")
    (should (eq (org-element-property :repeater-unit ts) 'week))
    (should (eq (org-element-property :repeater-value ts) 2))
    
    ;; Monthly patterns with restart
    (org-todoist--add-repeater ts "every! 2 months")
    (should (eq (org-element-property :repeater-unit ts) 'month))
    (should (eq (org-element-property :repeater-value ts) 2))
    (should (eq (org-element-property :repeater-type ts) 'restart))))

(ert-deftest org-todoist--test--add-repeater-times ()
  "Test patterns with specific times"
  (let ((ts (org-element-create 'timestamp)))
    ;; Morning
    (org-todoist--add-repeater ts "every morning")
    (should (eq (org-element-property :repeater-unit ts) 'day))
    (should (eq (org-element-property :hour-start ts) 9))
    (should (eq (org-element-property :minute-start ts) 0))
    
    ;; Afternoon
    (org-todoist--add-repeater ts "ev afternoon")
    (should (eq (org-element-property :hour-start ts) 12))
    
    ;; Evening
    (org-todoist--add-repeater ts "every! evening") 
    (should (eq (org-element-property :hour-start ts) 19))
    (should (eq (org-element-property :repeater-type ts) 'restart))))

(ert-deftest org-todoist--test--add-repeater-with-dates ()
  "Test patterns with specific dates"
  (let ((ts (org-element-create 'timestamp)))
    ;; Date with time
    (org-todoist--add-repeater ts "every 2 week on 07/12 at 16:30")
    (should (eq (org-element-property :repeater-unit ts) 'week))
    (should (eq (org-element-property :repeater-value ts) 2))
    (should (eq (org-element-property :month-start ts) 7))
    (should (eq (org-element-property :day-start ts) 12))
    (should (eq (org-element-property :hour-start ts) 16))
    (should (eq (org-element-property :minute-start ts) 30))
    
    ;; Date without time
    (setq ts (org-element-create 'timestamp))
    (org-todoist--add-repeater ts "every 2 week on 07/12")
    (should (eq (org-element-property :repeater-unit ts) 'week))
    (should (eq (org-element-property :repeater-value ts) 2))
    (should (eq (org-element-property :month-start ts) 7))
    (should (eq (org-element-property :day-start ts) 12))
    (should (eq (org-element-property :hour-start ts) nil))
    (should (eq (org-element-property :minute-start ts) nil))))

(ert-deftest org-todoist--test--repeater-regex ()
  (let ((ts (org-element-create 'timestamp))
        (str "every! mon, tues, wed, thurs"))
    (org-todoist--add-repeater ts str)
    (should (not (org-element-property :repeater-type ts)))))

(ert-deftest org-todoist--test--get-prop ()
  "Retrieves the property KEY from the property drawer directly under node.
Returns nil if not present"
  (should-not (org-todoist--get-prop (org-todoist--generate-ast "* Headline") "MYPROP"))
  (should (string-equal-ignore-case "test" (org-todoist--get-prop (org-element-map (org-todoist--generate-ast test-org-str) 'headline #'identity nil t) "MYPROP"))))

(ert-deftest org-todoist--test--add-prop--does-not-exist-works ()
  "Tests if adding a property works both when a property already exists."
  (let* ((ast (org-todoist--generate-ast test-org-str))
         (hl (org-element-map ast 'headline #'identity nil t))
         (drawer (org-todoist--get-property-drawer hl)))
    (should drawer)
    (org-todoist--add-prop drawer "NEWPROP" "VALUE")
    (should (org-todoist--element-equals-str test-org-str-property-added ast))))

;; (ert-deftest org-todoist--test--add-prop-headline ()
;;   "Tests if adding a property works both when a property already exists."
;;   (let* ((ast (org-todoist--generate-sample-ast))
;;          (hl (org-todoist--get-by-id org-todoist--task-type "13" ast))
;;          (drawer (org-todoist--get-property-drawer hl)))
;;     (org-todoist--add-prop hl "NEWPROP" "VALUE")
;;     (should (org-element-map drawer 'node-property
;;               (lambda (prop) (when (org-todoist--is-property prop "NEWPROP") t))))
;;     (should (string-equal (org-todoist--get-node-attribute hl "NEWPROP") "VALUE"))))

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
         (hl (org-element-map ast 'headline #'identity nil t))
         (drawer (org-todoist--get-property-drawer hl)))
    (org-todoist--add-prop drawer "NEWPROP" "NEWVALUE")
    (should (org-todoist--element-equals-str (string-replace "VALUE" "NEWVALUE" test-org-str-property-added) ast))))

;; (ert-deftest org-todoist--test--add-all-properties ()
;;   "Tests adding a properties alist to a node"
;;   (should (string-equal-ignore-case
;;            (org-todoist-org-element-to-string-no-ws
;;             (org-todoist--add-all-properties (org-element-create 'headline '(:title "Headline" :level 1))
;;                                              test-full-date))
;;            (org-todoist--remove-ws test-full-date-headline-text))))
;;            TODO not working due to differences below
;; *Headline
;; :PROPERTIES:
;; :date:2016-12-01
;; :timezone:nil
;; :string:everyday
;; :lang:en
;; :is_recurring:t
;; :END:

;; *Headline
;; :PROPERTIES:
;; :DATE:2016-12-01
;; :TIMEZONE:
;; :STRING:everyday
;; :LANG:en
;; :IS_RECURRING:t
;; : END:


;; (ert-deftest org-todoist--test-add-prop-no-drawer ()
;;   (should (string-equal-ignore-case (org-todoist--remove-ws "* Headline\n:PROPERTIES:\n:PROP:  VALUE\n:END:\n")
;;                                     (org-todoist--remove-ws (org-todoist-org-element-to-string (org-todoist--add-prop (org-todoist--generate-ast "* Headline\n") "PROP" "VALUE"))))))

(ert-deftest org-todoist--test--org-element-to-string ()
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (should (string-equal-ignore-case "* Headline\n" (org-todoist-org-element-to-string (org-todoist--generate-ast "* Headline")))))

(ert-deftest org-todoist--test--org-element-to-string-2 ()
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (should (string-equal-ignore-case (org-todoist--remove-ws test-org-str)
                                    (org-todoist--remove-ws (substring-no-properties (org-element-interpret-data (org-todoist--generate-ast test-org-str)))))))

(ert-deftest org-todoist--test--element-equals-str--expected ()
  (org-todoist--element-equals-str "* Headline\n" (org-todoist--generate-ast "* Headline\n")))

(ert-deftest org-todoist--test--invoke-with-buffer-processes-correctly ()
  "Verifies the helper fn org-todoist--invoke-with-buffer acts on the correct contents"
  (should (string-equal "Headlin" (org-todoist--invoke-with-buffer "* Headline" (lambda () (substring (buffer-string) 2 -1))))))

(defun org-todoist--generate-sample-string ()
  (org-todoist-org-element-to-string (org-todoist--generate-sample-ast)))

(defun file-to-string (file)
  "Read `FILE' as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-todoist-org-element-to-string-no-ws (DATA)
  "Converts DATA (an org-element or tree) to its content string. Note, a \n character is appended if not present."
  (org-todoist--remove-ws (substring-no-properties (org-element-interpret-data DATA))))

(defun org-todoist--element-equals-str (STR DATA)
  "Returns t if the org string from org-element-interpret-data for DATA is STR."
  (string-equal-ignore-case (org-todoist--remove-ws STR) (org-todoist--remove-ws (org-todoist-org-element-to-string DATA))))

(defun org-todoist--invoke-with-buffer (CONTENTS FN)
  "Fills a temp buffer with CONTENTS, applies org-mode, and calls FN."
  (with-temp-buffer (insert CONTENTS) (org-mode) (funcall FN)))

(defun org-todoist--generate-ast (contents)
  (org-todoist--invoke-with-buffer contents #'org-element-parse-buffer))

(defun org-todoist--generate-sample-ast ()
  (org-todoist--generate-ast (file-to-string (expand-file-name "sample.org"))))

(defun org-todoist--test-task () (org-todoist--get-by-id org-todoist--task-type "3" (org-todoist--generate-sample-ast)))
(defun org-todoist--test-subtask () (org-todoist--get-by-id org-todoist--task-type "777" (org-todoist--generate-sample-ast)))
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
