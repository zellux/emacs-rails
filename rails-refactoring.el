;;; rails-refactoring.el -- common refactoring operations on rails projects

;; Copyright (C) 2009 by Remco van 't Veer

;; Author: Remco van 't Veer
;; Keywords: ruby rails languages oop refactoring

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(require 'cl)
(require 'rails-core)


;; Customizations

(defcustom rails-refactoring-source-extensions '("builder" "erb" "haml" "liquid" "mab" "rake" "rb" "rhtml" "rjs" "rxml" "yml")
  "List of file extensions for refactoring search and replace operations."
  :group 'rails
  :type '(repeat string))


;; Helper functions

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

(defun rails-refactoring:decamelize (name)
  "Translate Ruby class name to corresponding file name."
  (replace-regexp-in-string "::" "/" (decamelize name)))
(assert (string= "foo_bar/quux" (rails-refactoring:decamelize "FooBar::Quux")))

(defun rails-refactoring:camelize (name)
  "Translate file name into corresponding Ruby class name."
  (replace-regexp-in-string "/" "::"
                            (replace-regexp-in-string "_\\([a-z]\\)" (lambda (match)
                                                                       (upcase (substring match 1)))
                                                      (capitalize name))))
(assert (string= "FooBar::Quux" (rails-refactoring:camelize "foo_bar/quux")))

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

(defun rails-refactoring:classes-alist (&optional source-files)
  "Return a list of all class names and their type in the current
rails project."
  (let ((files (or source-files (rails-refactoring:source-files))))
    (delete-if-not 'identity
                   (mapcar (lambda (file)
                             (cond ((string-match "^app/models/\\(.*\\)\\.rb$" file)
                                    (cons (rails-refactoring:camelize (match-string 1 file)) :model))
                                   ((string-match "^app/controllers/\\(.*\\)\\.rb$" file)
                                    (cons (rails-refactoring:camelize (match-string 1 file)) :controller))
                                   ((string-match "^app/helpers/\\(.*\\)\\.rb$" file)
                                    (cons (rails-refactoring:camelize (match-string 1 file)) :helper))
                                   ((string-match "^lib/\\(.*\\)\\.rb$" file)
                                    (cons (rails-refactoring:camelize (match-string 1 file)) :lib))))
                           files))))
 
(defun rails-refactoring:file (class &optional type)
  "Return file name of CLASS of a given type in the current
rails project.  The TYPE arguments describes the type of
class.

Example: (rails-refactoring:file \"Foo\" :controller)
Returns: \"app/controllers/foo_controller.rb\""

  (cond ((eq type :model)
         (or (rails-core:model-file class)
             (rails-core:file (concat "app/models/" (rails-refactoring:decamelize class) ".rb"))))
        ((eq type :controller)
         (rails-core:controller-file class))
        ((eq type :functional-test)
         (rails-core:functional-test-file class))
        ((eq type :helper)
         (rails-core:helper-file class))
        ((eq type :helper-test)
         (format "test/unit/helper/%s" (rails-core:file-by-class (concat class "HelperTest"))))
        ((eq type :views-dir)
         (rails-core:views-dir class))
        ((eq type :lib)
         (rails-core:lib-file class))
        (t (error "not yet implemented type %s" type))))

(defun rails-refactoring:file-exists-p (class &optional type)
  "Return t if file associated with CLASS (and TYPE) exists."
  (file-exists-p (rails-core:file (rails-refactoring:file class type))))

(defun rails-refactoring:current-class ()
  "Return name of the class in the current buffer or nil.  This
is determine according to the file name associated with this
buffer"
  (caar (rails-refactoring:classes-alist (list (replace-regexp-in-string (rails-project:root) "" (buffer-file-name))))))

(defun rails-refactoring:layouts ()
  "Return list of layouts.  This list not include templating file
extensions."
  (let ((layouts nil))
    (mapc (lambda (file)
            (unless (string-match "^_" file)
              (add-to-list 'layouts (replace-regexp-in-string "\\..*$" "" file))))
          (directory-files-recursive (rails-core:file "app/views/layouts")))
    layouts))

(defun rails-refactoring:current-layout ()
  "Return name of current layout or nil.  The layout name does
not include templating file extension."
  (ignore-errors
    (let ((name (rails-refactoring:decamelize (rails-core:current-controller))))
      (when (find-if (lambda (file) (string-match (concat "^" name) file))
                     (directory-files-recursive (rails-core:file "app/views/layouts")))
        name))))

(defmacro rails-refactoring:disclaim (name)
  `(when (interactive-p)
     (when (not (y-or-n-p (concat "Warning! " ,name " can not be undone! Are you sure you want to continue? ")))
       (error "cancelled"))
     (save-some-buffers)))


;; Refactoring methods

(defun rails-refactoring:rename-class (from to &optional type)
  "Rename class from FROM to TO where TO and FROM and
shortnames like the ones used by `rails-refactoring:file'.  The
file is renamed and the class or module definition is modified."
  (interactive (let ((from (completing-read "Rename class: "
                                            (mapcar 'car (rails-refactoring:classes-alist))
                                            nil t
                                            (rails-refactoring:current-class)))
                     (to (read-string "To: ")))
                 (list from to (cdr (assoc from (rails-refactoring:classes-alist))))))

  (rails-refactoring:disclaim "Rename class")

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
    (save-buffer))

  (when (interactive-p)
    (ignore-errors (rails-refactoring:query-replace from to))
    (save-some-buffers)))

(defun rails-refactoring:query-replace (from to)
  "Replace some occurrences of FROM to TO in all the project source files."
  (interactive "sFrom: \nsTo: ")

  (tags-query-replace from to nil
                      (cons 'list (mapcar #'rails-core:file (rails-refactoring:source-files)))))  

(defun rails-refactoring:rename-layout (from to)
  "Rename all named layouts from FROM to TO."
  (interactive (list (completing-read "From: "
                                      (rails-refactoring:layouts)
                                      nil t
                                      (rails-refactoring:current-layout))
                     (read-string "To: ")))

  (rails-refactoring:disclaim "Rename layout")

  (mapc (lambda (from-file)
          (let ((to-file (concat to (substring from-file (length from)))))
            (message "renaming layout from %s to %s" from-file to-file)
            (rename-file (rails-core:file (format "app/views/layouts/%s" from-file))
                         (rails-core:file (format "app/views/layouts/%s" to-file)))))
        (delete-if-not (lambda (file) (string= from (substring file 0 (length from))))
                       (directory-files-recursive (rails-core:file "app/views/layouts"))))
  (when (interactive-p)
    (ignore-errors (rails-refactoring:query-replace from to))
    (save-some-buffers)))

(defun rails-refactoring:rename-controller (from to)
  "Rename controller from FROM to TO.  All appropriate files and
directories are renamed and `rails-refactoring:query-replace' is
started to do the rest."
  (interactive (list (completing-read "Rename controller: "
                                      (mapcar (lambda (name) (remove-postfix name "Controller")) (rails-core:controllers))
                                      nil t
                                      (ignore-errors (rails-core:current-controller)))
                     (read-string "To: ")))

  (rails-refactoring:disclaim "Rename controller")

  (unless (rails-core:controller-exist-p from)
    (error "controller '%s' doesn't exists" from))

  ;; ensure no existing file are in the way
  (let ((file (find-if (lambda (file) (and file (file-exists-p (rails-core:file file))))
                       (append (mapcar (lambda (fn) (funcall fn to))
                                       '(rails-core:controller-file rails-core:helper-file rails-core:views-dir))
                               (list (rails-core:layout-file (rails-refactoring:decamelize to)))))))
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

  (rails-refactoring:rename-layout (rails-refactoring:decamelize from) (rails-refactoring:decamelize to))

  (when (interactive-p)
    (ignore-errors (rails-refactoring:query-replace from to))
    (ignore-errors (rails-refactoring:query-replace (rails-refactoring:decamelize from) (rails-refactoring:decamelize to)))
    (save-some-buffers)))


;; Tie up in UI

(require 'rails-ui)

(define-keys rails-minor-mode-map
  ((rails-key "\C-c R c") 'rails-refactoring:rename-controller))


(provide 'rails-refactoring)