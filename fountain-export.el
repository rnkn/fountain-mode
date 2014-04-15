;;; fountain-export.el --- Export engine for Fountain Mode

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix 'fountain-export-
  :group 'fountain)

;;; customizable variables =============================================

(defcustom fountain-export-buffer-name
  "*Fountain Mode Export*"
  "Buffer name to use when not exporting a file."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-html-header-template
  "<!DOCTYPE html>
<!-- Created by Fountain Mode version ${fountain-version} -->
<html>
<head>
<meta charset=\"$ {charset}\">
<title>$ {title}</title>
<link rel=\"stylesheet\" href=\"${stylesheet}\">
</head>
<body>
$ {content}
</body>
</html>
"
  "HTML header template inserted at beginning of export buffer.
See `fountain-export-format-template'."
  :type 'string
  :group 'fountain-export)

;;; internal funcation =================================================

(defun fountain-export-fontify-buffer ()
  "Ensure the buffer is fontified."
  (save-excursion
    (save-restriction
      (widen)
      (when font-lock-mode
        (let ((font-lock-maximum-decoration t)
              fountain-indent-elements)
          (jit-lock-fontify-now (point-min) (point-max)))))))

(defun fountain-export-format-template (template)
  "Format TEMPLATE according to the following list.

Internal function, will not work outside of
`fountain-export-html'."
  (s-format template 'aget
            `(("fountain-version" . ,fountain-version)
              ("stylesheet" . ,(concat name ".css")))))
  
(defun fountain-export-html ()
  "Convert current buffer to HTML, writing result to new buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fountain-fontify-buffer)
      (let* ((name (if (buffer-file-name)
                       (file-name-base (buffer-file-name))
                     fountain-export-buffer-name))
             (htmlb (generate-new-buffer (concat name ".html")))
             complete)
        (with-current-buffer htmlb
          (with-silent-modifications
            (insert (fountain-export-format-template
                     fountain-export-html-header-template))))
        (switch-to-buffer-other-window htmlb)
        (html-mode)
        (indent-region (point-min) (point-max))
))))

(provide 'fountain-export)
;;; fountain-export.el ends here
