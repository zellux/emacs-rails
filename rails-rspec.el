;;; rails-test.el --- tests integration with the compile library

;; Copyright (C) 2009 R.W. van 't Veer

;; Authors: R.W. van 't Veer

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

;;; Code:

(defun rails-rspec:run-current-method ()
  "Run spec for the current example."
  (interactive)
  (save-excursion
    (let* ((file (substring (buffer-file-name) (length (rails-project:root))))
           (pattern "\\<it +\\([\"']\\)\\([^\\1]+\\)\\1")
           (example (and (or (looking-at pattern)
                             (re-search-backward pattern nil t)
                             (re-search-forward pattern nil t))
                         (match-string 2))))
      (when example
        (rails-test:run-single-file file (format "-e %s" (shell-quote-argument example)))))))

(provide 'rails-rspec)
