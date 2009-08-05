(defun directory-files-recursive (dirname &optional base)
  "Return a list of names of files in directory named by
DIRNAME. If the directory contains directories these are
traversed recursively.  The returned list of file names are
relative to DIRNAME and only includes regular files.

If BASE is provided, it is interpreted as a subdirectory to
traverse.  This subdirectory is included the returned file
names."
  (apply #'append
         (mapcar (lambda (file)
                   (cond ((file-regular-p (concat dirname "/" file))
                          (list (concat base file)))
                         ((and (file-directory-p (concat dirname "/" file))
                               (not (string-match "^\\." file)))
                          (directory-files-recursive (concat dirname "/" file) (concat base file "/")))))
                 (directory-files dirname))))

(defcustom rails-refactoring-source-extensions '("builder" "erb" "haml" "liquid" "mab" "rake" "rb" "rhtml" "rjs" "rxml" "yml")
  "List of file extensions for refactoring search and replace operations."
  :group 'rails
  :type '(repeat string))

(defun rails-refactoring:source-file-p (name)
  "Test if file has extension from `rails-refactoring-source-extensions'."
  (find-if (lambda (ext) (string-match (concat "\\." ext "$") name))
           rails-refactoring-source-extensions))

(defun rails-refactoring:source-files ()
  "Return a list of all the source files in the current rails
project.  This includes all the files in the 'app', 'config',
'lib' and 'test' directories."
  (apply #'append
         (mapcar (lambda (dirname)
                   (delete-if (lambda (file) (string-match "_flymake.rb" file))
                              (delete-if-not 'rails-refactoring:source-file-p
                                             (directory-files-recursive (rails-core:file dirname) dirname))))
                 '("app/" "config/" "lib/" "test/"))))

(defun rails-refactoring:file (class &optional type)
  "Return file name of CLASS of a given type in the current
rails project.  The TYPE arguments describes the type of
class.

Example: (rails-refactoring:file \"Foo\" :controller)
Returns: \"app/controllers/foo_controller.rb\""

  (cond ((eq type :controller)
         (rails-core:controller-file class))

        ((eq type :functional-test)
         (rails-core:functional-test-file class))

        ((eq type :helper)
         (rails-core:helper-file class))

        ((eql type :helper-test)
         (format "test/unit/helper/%s" (rails-core:file-by-class (concat class "HelperTest"))))
        
        ((eql type :views-dir)
         (rails-core:views-dir class))

        (t (error "not yet implemented type %s" type))))

(defun rails-refactoring:file-exists-p (class &optional type)
  "Return t if file associated with CLASS (and TYPE) exists."
  (file-exists-p (rails-core:file (rails-refactoring:file class type))))

(defun rails-refactoring:rename-class (from to &optional type)
  "Rename class from FROM to TO where TO and FROM and
shortnames like the ones used by `rails-refactoring:file'.  The
file is renamed and the class or module definition is modified."
  (let ((from-file (rails-refactoring:file from type))
        (to-file (rails-refactoring:file to type)))
    (message "rename file from %s to %s" from-file to-file)
    (rename-file (rails-core:file from-file) (rails-core:file to-file))
    (let ((buffer (get-file-buffer (rails-core:file from-file))))
      (when buffer (kill-buffer buffer)))

    (message "change definition from %s to %s" from to)
    (let ((buffer (get-file-buffer (rails-core:file to-file))))
      (when buffer (kill-buffer buffer)))
    (find-file (rails-core:file to-file))
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\(class\\|module\\)[ \t]+" from) nil t)
      (replace-match (concat "\\1 " to) nil nil))
    (save-buffer)))
  
(defun rails-refactoring:query-replace (from to)
  "Replace some occurrences of FROM to TO in all the project source files."
  (interactive "sFrom: \nsTo: ")
  (tags-query-replace from to nil
                      (cons 'list (mapcar #'rails-core:file (rails-refactoring:source-files)))))  

(defun rails-refactoring:rename-layout (from to)
  "Rename all named layouts from FROM to TO."
  (mapc (lambda (from-file)
          (let ((to-file (concat to (substring from-file (length from)))))
            (message "renaming layout from %s to %s" from-file to-file)
            (rename-file (rails-core:file (format "app/views/layouts/%s" from-file))
                         (rails-core:file (format "app/views/layouts/%s" to-file)))))
        (directory-files-recursive (rails-core:file "app/views/layouts") (lambda (file) (string= from (substring file 0 (length from)))))))

(defun rails-refactoring:rename-controller (from to)
  "Rename controller from FROM to TO.  All appropriate files and
directories are renamed and `rails-refactoring:query-replace' is
started to do the rest."
  (interactive (let* ((from (completing-read "Rename controller: "
                                             (mapcar (lambda (name) (remove-postfix name "Controller")) (rails-core:controllers)) nil t
                                             (ignore-errors (rails-core:current-controller))))
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
  (rails-refactoring:rename-class from to :controller)

  (when (rails-refactoring:file-exists-p from :views-dir)
    (let ((from-dir (rails-refactoring:file from :views-dir))
          (to-dir (rails-refactoring:file to :views-dir)))
      (message "rename view directory from %s to %s" from-dir to-dir)
      (rename-file (rails-core:file from-dir) (rails-core:file to-dir))))

  (when (rails-refactoring:file-exists-p from :functional-test)
    (message "refactoring functional test class file")
    (rails-refactoring:rename-class from to :functional-test))

  (when (rails-refactoring:file-exists-p from :helper)
    (message "refactoring helper class file")
    (rails-refactoring:rename-class from to :helper))

  (when (rails-refactoring:file-exists-p from :helper-test)
    (message "refactoring helper test class file")
    (rails-refactoring:rename-class from to :helper-test))

  (rails-refactoring:rename-layout (decamelize from) (decamelize to))

  (ignore-errors (rails-refactoring:query-replace from to))
  (ignore-errors (rails-refactoring:query-replace (decamelize from) (decamelize to)))

  (save-some-buffers))
