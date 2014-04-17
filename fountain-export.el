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

(defcustom fountain-export-fonts
  '("Courier Prime"
    "Courier Final Draft"
    "Courier Screenplay"
    "Courier"
    "Courier New")
  ""
  :type '(repeat (string :tag "Font"))
  :group 'fountain-export)

(defcustom fountain-export-html-head-template
  "<!DOCTYPE html>
<!-- Created by Emacs ${emacs-version} running Fountain Mode ${fountain-version} -->
<html>
<head>
<link rel=\"stylesheet\" href=\"${cssfile}\">
</head>
"
  "HTML template inserted into export buffer.
See `fountain-export-format-template'."
  :type 'string
  :group 'fountain-export)

;;; internal functions =================================================

(defun fountain-export-fontify-buffer ()
  "Ensure the buffer is fontified."
  (if font-lock-mode
      (let ((font-lock-maximum-decoration t)
            (fontjob (make-progress-reporter "Fontifying..." 0 100))
            (chunk (/ (point-max) 100))
            (n 0))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((end (+ (point) chunk)))
            (jit-lock-fontify-now (point) end)
            (goto-char end)
            (progress-reporter-update fontjob n)
            (setq n (+ n 1))))
        (progress-reporter-done fontjob))))

(defun fountain-export-get-name ()
  ""
  (if (buffer-file-name)
      (file-name-base (buffer-file-name))
    fountain-export-buffer-name))

(defun fountain-export-get-element (start end dest)
  ""
  (let* ((start (point))
         (limit (save-excursion
                  (re-search-forward fountain-blank-regexp nil t)
                  (match-beginning 0)))
         (end (next-single-property-change start 'face nil limit)))
    (when end
      (buffer-substring start end))))

(defun fountain-export-filter (substring)
  "Replace newlines with line-breaks and escape HTML special characters."
  (with-temp-buffer
    (insert substring)
    (format-replace-strings '(("&" . "&amp;")
                              ("<" . "&lt;")
                              (">" . "&gt;")
                              ("\n" . "<br>"))
                            nil (point-min) (point-max))
    (substring-no-properties (buffer-string))))

(defun fountain-export-create-html-element (substring)
  "Return a HTML div with face and substring of SUBSTRING.
Stylesheet class is taken from face, while content is taken from
of SUBSTRING.

If face is `fountain-comment', return nil."
  (let ((class
         (if (get-text-property 0 'face substring)
             (let* ((s (symbol-name (get-text-property 0 'face substring)))
                    (s (s-chop-suffix "-highlight" s))
                    (s (s-chop-prefix "fountain-" s))) s)
           "action"))
        (content
         (fountain-export-filter (substring-no-properties substring))))
    (unless (string= class "comment")
      (format "<div class=\"%s\">%s</div>\n" class content))))

(defun fountain-export-format-template (template)
  "Format TEMPLATE according to the following list.

Internal function, will not work outside of
`fountain-export-html'."
  (s-format template 'aget
            `(("fountain-version" . ,fountain-version)
              ("emacs-version" . ,emacs-version)
              ;; title?
              ;; author?
              ("htmlfile" . ,(concat (fountain-export-get-name) ".html"))
              ("cssfile" . ,(concat (fountain-export-get-name) ".css")))))

(defun fountain-export-pdf-buffer ()
  ""
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      ;; run `fountain-export-pdf-command'
      )))

(defun fountain-export-html-buffer ()
  ""
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fountain-export-html-region (point-min) (point-max)))))

(defun fountain-export-html-region (start end)
  "Convert current buffer to HTML, writing result to new buffer."
  (interactive "r")
  (save-excursion
    (let ((exportjob (make-progress-reporter "Exporting..." 0 100)))
      ;; fontify the entire buffer
      (fountain-export-fontify-buffer)
      ;; FIXME: create the stylesheet
      (let* ((sourcebuf (current-buffer))
             (name (if (buffer-file-name)
                       (file-name-base (buffer-file-name))
                     fountain-export-buffer-name))
             (htmlfile (concat name ".html"))
             (htmlbuf (generate-new-buffer htmlfile)))
        (goto-char (point-min))
        (with-current-buffer htmlbuf
          (with-silent-modifications
            (insert (fountain-export-format-template
                     fountain-export-html-head-template)
                    "<body>\n<div id=\"screenplay\">\n")))
        (while (< (point) (point-max))
          (skip-chars-forward "\n\s\t")
          (let* ((start (point))
                 (limit (save-excursion
                          (re-search-forward "\n\s?\n\\|\\'" nil t)
                          (match-beginning 0)))
                 (end (next-single-property-change start 'face nil limit)))
            (when end
              (let* ((s (buffer-substring start end))
                     (div (fountain-export-create-html-element s)))
                (when div
                  (with-current-buffer htmlbuf
                    (with-silent-modifications
                      (insert div)))))
              (goto-char end)))
          (unless (looking-at ".\\|\\'")
            (forward-char 1))
          (progress-reporter-update
           exportjob
           (truncate (* (/ (float (point)) (buffer-size)) 100))))
        (switch-to-buffer-other-window htmlbuf)
        (let ((htmljob (make-progress-reporter "Preparing HTML..." 0 100)))
          (insert "</div>\n</body>\n<\html>")
          (goto-char (point-min))
          (html-mode)
          (while (< (point) (point-max))
            (indent-according-to-mode)
            (forward-line 1)
            (progress-reporter-update
             htmljob
             (truncate (* (/ (float (point)) (point-max)) 100))))
          (progress-reporter-done htmljob))
        (progress-reporter-done exportjob)))))

(provide 'fountain-export)
;;; fountain-export.el ends here
