(defun rails-refactoring:controller-names ()
  (mapcar (lambda (name) (remove-postfix name "Controller")) (rails-core:controllers)))

(defun rails-refactoring:current-controller-or-nil ()
  (condition-case nil (rails-core:current-controller) (error nil)))

(defun rails-refactoring:rename-class-definition (from to)
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\(class\\|module\\)[ \t]+" from) nil t)
    (replace-match (concat "\\1 " to) nil nil)
    (error "failed find class definition for renaming")))

(defun rails-refactoring:rename-class-file (from-class from-file to-class to-file)
  (message "rename file from %s to %s" from-file to-file)
  (rename-file (rails-core:file from-file) (rails-core:file to-file))
  (let ((buffer (get-file-buffer (rails-core:file from-file))))
    (when buffer (kill-buffer buffer)))

  (message "change definition from %s to %s" from-class to-class)
  (find-file (rails-core:file to-file))
  (rails-refactoring:rename-class-definition from-class to-class)
  (save-buffer))

(defun rails-refactoring:helper-test-file (controller)
  (format "test/unit/helper/%s" (rails-core:file-by-class (concat controller "HelperTest"))))

(assert (string= (rails-refactoring:helper-test-file "Users") "test/unit/helper/users_helper_test.rb"))

(defun rails-refactoring:rename-controller (from to)
  (interactive (let* ((from (completing-read "Rename controller: "
                                             (rails-refactoring:controller-names) nil t
                                             (rails-refactoring:current-controller-or-nil)))
                      (to (read-string "To: ")))
                 (list from to)))

  (save-some-buffers)

  (unless (rails-core:controller-exist-p from)
    (error "controller '%s' doesn't exists" from))

  ;; ensure no existing file are in the way
  (let ((file (find-if (lambda (file) (and file (file-exists-p (rails-core:file file))))
                       (append (mapcar (lambda (fn) (funcall fn to))
                                       '(rails-core:controller-file rails-core:helper-file rails-core:views-dir))
                               (list (rails-core:layout-file (decamelize to)))))))
    (when file (error "file '%s' already exists" file)))

  (message "refactoring controller class file")
  (rails-refactoring:rename-class-file (rails-core:long-controller-name from)
                                       (rails-core:controller-file from)
                                       (rails-core:long-controller-name  to)
                                       (rails-core:controller-file to))

  (when (file-exists-p (rails-core:file (rails-core:views-dir from)))
    (let ((from-dir (rails-core:views-dir from))
          (to-dir (rails-core:views-dir to)))
      (message "rename view directory from %s to %s" from-dir to-dir)
      (rename-file (rails-core:file from-dir) (rails-core:file to-dir))))

  (when (file-exists-p (rails-core:file (rails-core:functional-test-file from)))
    (message "refactoring functional test class file")
    (rails-refactoring:rename-class-file (concat from "ControllerTest")
                                         (rails-core:functional-test-file from)
                                         (concat to "ControllerTest")
                                         (rails-core:functional-test-file to)))

  (when (file-exists-p (rails-core:file (rails-core:helper-file from)))
    (message "refactoring helper class file")
    (rails-refactoring:rename-class-file (concat from "Helper")
                                         (rails-core:helper-file from)
                                         (concat to "Helper")
                                         (rails-core:helper-file to)))

  (when (file-exists-p (rails-core:file (rails-refactoring:helper-test-file from)))
    (message "refactoring helper test class file")
    (rails-refactoring:rename-class-file (concat from "HelperTest")
                                         (rails-refactoring:helper-test-file from)
                                         (concat to "HelperTest")
                                         (rails-refactoring:helper-test-file to)))

  (error "should rename layout")

  (error "should query replace references to controller class")
  (error "should query replace references to controller in routing")
  (error "should query replace references to helper and includes of class name")
  (error "should query replace render partial / template"))
  