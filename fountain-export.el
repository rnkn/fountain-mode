;;; fountain-export.el --- Export engine for Fountain Mode

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp, outlines

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

(defconst fountain-export-format-plist
  '((section
     html ("<div class=\"section\"> id=\"%s\"\n" . "\n</div>"))
    (scene
     html  ("<div class=\"scene\" id=\"%s\">\n" . "\n</div>"))
    (dialog
     html  ("<table class=\"dialog\" data-character=\"%s\">\n<caption class=\"character\">" . "</table>"))
    (scene-heading
     html  ("<h2 class=\"scene-heading\">" . "</h2>")
     latex ("\sceneheading{" . "}")
     fdx   ("<Paragraph Type=\"Scene Heading\">\n<Text>" . "</Text>\n</Paragraph>"))
    (action
     html  ("<p class=\"action\">" . "</p>")
     latex ("" . "")
     fdx   ("<Paragraph Type=\"Action\">\n<Text>" . "</Text>\n</Paragraph>"))
    (character
     html  ("<tr class=\"character\"><td class=\"character\">" . "</td></tr>")
     latex ("\begin{dialog}{" . "}")
     fdx   ("<Paragraph Type=\"Character\">\n<Text>" . "</Text>\n</Paragraph>"))
    (paren
     html  ("<tr class=\"paren\"><td class=\"paren\">" . "</td></tr>")
     latex ("\paren{" . "}"))
    (lines
     html  ("<tr class=\"lines\"><td class=\"lines\">" . "</td></tr>")
     latex ("" . "")
     fdx   ("<Paragraph Type=\"Dialogue\">\n<Text>" . "</Text>\n</Paragraph>"))
    (trans
     html  ("<p class=\"trans\">" . "</p>")
     latex ("\trans{" . "}")
     fdx   ("<Paragraph Type=\"Transition\">\n<Text>" . "</Text>\n</Paragraph>"))
    (section-heading
     html  ("<p class=\"section-heading\">" . "</p>"))
    (synopsis
     html  ("<p class=\"synopsis\">" . "</p>"))
    (note
     html  ("<p class=\"note\">" . "</p>"))
    (comment
     html  ("<p class=\"comment\">" . "</p>"))
    (underline
     html  ("_\\(.+?\\)_" . "<span class=\"underline\">\\1</span>"))
    (bold
     html  ("\\*\\*\\(.+?\\)\\*\\*" . "<strong>\\1</strong>"))
    (italic
     html  ("\\*\\(.+?\\)\\*" . "<em>\\1</em>"))
    (lyric
     html  ("^~\s*\\(.+\\)" . "<i>\\1</i>")))
  "List of strings to format exported elements")

;; (plist-get (cdr (assoc 'scene-heading fountain-export-format-list)) 'html)

(defun fountain-export-fontify-buffer ()
  "If `font-lock-mode' is enables, fontify entire buffer."
  (if font-lock-mode
      (let ((job (make-progress-reporter "Fontifying..." 0 100))
            (chunk (/ (buffer-size) 100))
            (n 0))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((limit (+ (point) chunk)))
              (jit-lock-fontify-now (point) limit)
              (goto-char limit)
              (progress-reporter-update job n)
              (setq n (1+ n))))
          (progress-reporter-done job)))
    (error "Font Lock is not active")))

(defun fountain-export-strip-comments ()
  "Strips buffer of all comments and metadata.
Matches and deletes any text with `fountain-comment',
`fountain-metadata-key' or `fountain-metadata-value' face."
  (save-excursion
    (goto-char (point-min))
    (while (null (eobp))
      (if (memq (face-at-point) '(fountain-comment
                                  fountain-metadata-key
                                  fountain-metadata-value))
          (let ((m (point)))
            (goto-char (next-single-property-change
                        (point) 'face nil (point-max)))
            (delete-region m (point)))
        (goto-char (next-single-property-change
                    (point) 'face nil (point-max)))))))

(defun fountain-export-get-filename (format)
  "If BUFFER is visiting a file, concat file name base and FORMAT.
Otherwise return `fountain-export-buffer'"
  (if (buffer-file-name)
      (concat (file-name-base (buffer-file-name)) "." format)
    (format fountain-export-buffer format)))

(defun fountain-export-get-metadata-value (key) ; FIXME combine with other
  "Like `fountain-get-metadata-value' but filters for HTML."
  (let* ((value (cdr (assoc key fountain-metadata)))
         (s (if (listp value)
                (s-join "\n" value)
              value))
         (s (fountain-export-filter s)))
    s))

(defun fountain-export-create-title-page-element (key)
  "Gets metadata value associated with KEY and creates HTML element."
  (let* ((s (fountain-export-get-metadata-value key))
         (content (fountain-export-line-breaks s)))
    (if (string= key "title")
        (format "<h1>%s</h1>" content)
      (format "<p>%s</p>" content))))

(defun fountain-export-create-html-title-page ()
  "Create title page based on `fountain-export-title-page-template'."
  (if (cdr (assoc "title" fountain-metadata))
      (concat
       "<div id=\"title\">\n"
       (s-format fountain-export-title-page-title-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n<div id=\"left\">\n"
       (s-format fountain-export-title-page-left-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n<div id=\"right\">\n"
       (s-format fountain-export-title-page-right-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n")))

(defun fountain-export-create-style ()
  "Create stylesheet using `fountain-export-style-template'."
  (let* ((page-size fountain-export-page-size)
         (font
          (mapconcat
           (lambda (font) (concat "'" font "'"))
           fountain-export-font ","))
         (scene-bold
          (if fountain-export-bold-scene-headings
              "bold" "normal"))
         (scene-underline
          (if fountain-export-underline-scene-headings
              "underline" "none"))
         (scene-spacing
          (if fountain-export-double-space-scene-headings
              "2em" "1em"))
         (title-bold
          (if fountain-export-bold-title
              "bold" "normal"))
         (title-underline
          (if fountain-export-underline-title
              "underline" "none"))
         (title-upcase
          (if fountain-export-upcase-title
              "uppercase" "none"))
         (dialog-contd (concat "(" fountain-continued-dialog-string ")"))
         (dialog-more fountain-export-more-dialog-string)
         (action-orphans (int-to-string fountain-export-action-orphans))
         (action-widows (int-to-string fountain-export-action-widows))
         (dialog-orphans (int-to-string fountain-export-dialog-orphans))
         (dialog-widows (int-to-string fountain-export-dialog-widows))
         (style-rules (s-format fountain-export-style-template
                          '(lambda (var)
                             (symbol-value (intern var))))))
    (if fountain-export-inline-style
        (concat "<style type=\"text/css\">\n"
                style-rules
                "\n</style>")
      (let ((cssfile (get-buffer-create (fountain-export-get-filename "css")))
            (outputdir (expand-file-name
                        (file-name-directory (buffer-file-name)))))
        (with-current-buffer cssfile
          (erase-buffer)
          (insert
           (format
            "/* Created with Emacs %s running Fountain Mode %s */\n"
            emacs-version fountain-version)
           style-rules)
          (write-file outputdir))
        (concat "<link rel=\"stylesheet\" href=\""
                (buffer-name cssfile)
                "\">")))))

(defun fountain-export-create-html-head ()
  "Create HTML head using `fountain-export-html-head-template'."
  (let ((insert-style (fountain-export-create-style))
        (charset "utf-8")
        (title (or (fountain-export-get-metadata-value "title")
                   (file-name-base (buffer-name))))
        (author (or (fountain-export-get-metadata-value "author")
                    user-full-name)))
    (s-format fountain-export-html-head-template
              '(lambda (var)
                 (symbol-value (intern var))))))

(defun fountain-export-underline (s)
  "Replace underlined text in S with HTML underline span tags."
  (replace-regexp-in-string "_\\(.+?\\)_"
                            "<span class=\"underline\">\\1</span>"
                            s t))

(defun fountain-export-bold (s)
  "Replace bold text in S with HTML strong tags."
  (replace-regexp-in-string "\\*\\*\\(.+?\\)\\*\\*"
                            "<strong>\\1</strong>"
                            s t))

(defun fountain-export-italic (s)
  "Replace italic text in S with HTML emphasis tags."
  (replace-regexp-in-string "\\*\\(.+?\\)\\*"
                            "<em>\\1</em>"
                            s t))

(defun fountain-export-lyrics (s)
  "Replace lyrics in S with HTML italic tags."
  (replace-regexp-in-string "^~\s*\\(.+\\)"
                            "<i>\\1</i>"
                            s t))

(defun fountain-export-line-breaks (s)
  "Replace newlines in S with HTML line breaks."
  (replace-regexp-in-string "\n"
                            "<br>\n"
                            s))

(defun fountain-export-tex-quotes (s)
  "Replace TeX-style quotes in S with \"smart\" quotes."
  (s-replace-all '(("\\`" . "&#96;")
                   ("\\'" . "&apos;")
                   ("``" . "&ldquo;")
                   ("''" . "&rdquo;")
                   ("`" . "&lsquo;")
                   ("'" . "&rsquo;")) s))

(defun fountain-export-sanitize (s)
  "Escape HTML characters in S."
  (s-replace-all '(("&" . "&amp;")
                   ("<" . "&lt;")
                   (">" . "&gt;")) s))

(defun fountain-export-filter (sub-s)   ; FIXME update doc
  (let* ((s (substring-no-properties sub-s))
         (s (fountain-export-sanitize s))
         (s (s-replace-all '(("\\\s" . "&nbsp;")
                             ("^\\\\$" . "<br>\n")
                             ("\\_" . "&#95;")
                             ("\\*" . "&#42;")) s))
         ;; (s (if fountain-export-preserve-line-breaks
         ;;        (fountain-export-line-breaks s)
         ;;      s))
         (s (if fountain-export-convert-quotes
                (fountain-export-tex-quotes s)
              s))
         (s (fountain-export-underline s))
         (s (fountain-export-bold s))
         (s (fountain-export-italic s))
         (s (fountain-export-lyrics s)))
    s))

(defun fountain-export-create-html-dialog-table (content limit)
  (let* ((dialog-contd (concat "(" fountain-continued-dialog-string ")"))
         (character (fountain-export-filter
                     (s-trim (car (s-slice-at dialog-contd content)))))
         (table-start
          (format (concat "<table class=\"dialog\" character=\"%s\">\n"
                          "<caption class=\"character\">\n"
                          "<tr class=\"character\"><td class=\"character\">%s</td></tr>\n")
                  character content))
         (table-body "")
         (table-end "</table>\n"))
    (goto-char (next-single-property-change
                (point) 'fountain-element nil limit))
    (while (< (point) limit)
      (skip-chars-forward "\n")
      (setq table-body
            (concat table-body
                    (fountain-export-create-html-element limit))))
    (concat table-start table-body table-end)))

(defun fountain-export-create-html-element (limit)
  (let* ((index (point))
         (class (or (get-text-property index 'fountain-element)
                    "action"))
         (change (next-single-property-change
                  index 'fountain-element nil limit)))
    (when change
      (let* ((sub-s (buffer-substring index change))
             (content (fountain-export-filter sub-s))
             (element
              (cond ((string= class "character")
                     (fountain-export-create-html-dialog-table content limit))
                    ((member class '("dialog" "paren"))
                     (format "<tr class=\"%s\"><td class=\"%s\">%s</td></tr>\n"
                             class class content))
                    ((string= class "scene-heading")
                     (format "<h2 class=\"%s\">%s</h2>\n"
                             class content))
                    ((format "<p class=\"%s\">%s</p>\n"
                             class content)))))
        (if (string= class "character")
            (goto-char limit)
          (goto-char change))
        element))))

;; (defun fountain-export-parse-buffer (destbuf)
;;   "Find and insert elements into DESTBUF.
;; First, skip forward to next available text, mark point as index,
;; then find the next \"fountain-element\" text property change from
;; index, then pass substring from index to change to
;; `fountain-export-create-html-element', then insert the newly
;; created HTML element to DESTBUF."
;;   (let ((job (make-progress-reporter "Parsing..." 0 100)))
;;     (goto-char (point-min))
;;     (while (null (eobp))
;;       (while (looking-at "\n*\s*\n")
;;         (goto-char (match-end 0)))
;;       (let* ((limit (save-excursion
;;                      (re-search-forward "\n\s*\n\\|\\'" nil t)
;;                      (match-beginning 0)))
;;              (element (fountain-export-create-html-element limit)))
;;         (when element
;;           (with-current-buffer destbuf
;;             (with-silent-modifications
;;               (insert element)))))
;;       (progress-reporter-update
;;        job (truncate (* (/ (float (point)) (buffer-size)) 100))))))

;; (defmacro fountain-export-span (span format s)
;;   (let* ((plist (plist-get (cdr (assoc span fountain-export-format-list)) format))
;;          (match (car plist))
;;          (template (cdr plist)))
;;     `(replace-regexp-in-string ,match ,template s)))

;; (defun fountain-export-string (format s)
;;   (fountain-export-span 'underline format s)
;;   (fountain-export-span 'bold format s)
;;   (fountain-export-span 'italic format s)
;;   (fountain-export-span 'lyric format s))

(defun fountain-export-format-element (element format)
  (let* ((type (car element))
         (content (nth 1 element))
         (plist (nth 2 element)))
    (if (memq type fountain-export-element-set)
        (let* ((template
                (plist-get (cdr (assoc type fountain-export-format-plist))
                           format))
               (prefix (car template))
               (postfix (cdr template)))
          (concat prefix content postfix)))))

(defun fountain-export-get-element ()
  (cond
   ((fountain-section-heading-p)
    (let ((level (funcall outline-level))
          (data (list 'section-heading
                      (list :beg (match-beginning 0)
                            :end (match-end 0))
                      (match-string-no-properties 3)))
          (limit (save-excursion
                   (outline-end-of-subtree)
                   (point))))
      (goto-char (plist-get (nth 1 data)
                            :end))
      (list 'section
            (list :level level)
            (fountain-export-parse limit (list data)))))
   ((fountain-scene-heading-p)
    (let ((data (list 'scene-heading
                      (list :beg (match-beginning 0)
                            :end (match-end 0))
                      (match-string-no-properties 3)))
          (limit (save-excursion
                   (outline-end-of-subtree)
                   (point))))
      (goto-char (plist-get (nth 1 data)
                            :end))
      (list 'scene
            (list :num (ignore))
            (fountain-export-parse limit (list data)))))
   ((fountain-character-p)
    (list 'character
          (list :beg (match-beginning 0)
                :end (match-end 0)
                :name (match-string-no-properties 4)
                :dual (stringp (match-string 5)))
          (match-string-no-properties 3)))
   ((fountain-dialog-p)
    (list 'dialog
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-paren-p)
    (list 'paren
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-trans-p)
    (list 'trans
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-center-p)
    (list 'center
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-synopsis-p)
    (list 'synopsis
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-note-p)
    (list 'note
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   (t
    (let ((beg (car (fountain-get-block-bounds)))
          (end (cdr (fountain-get-block-bounds))))
      (list 'action
            (list :begin beg
                  :end end)
            (s-trim-right (buffer-substring-no-properties
                           (point) end)))))))


  ;; (setq fountain-export-tick (buffer-modified-tick))
  ;; fountain-export-content)

(defun fountain-export (format &optional force)
  (let* (complete
         (source-buf (current-buffer))
         (dest-buf (get-buffer-create
                    (fountain-export-get-filename (symbol-name format)))))
    (unwind-protect
        (progn
          (with-current-buffer dest-buf
            (with-silent-modifications
              (erase-buffer)))
          (if (or force
                  (/= fountain-export-tick (buffer-modified-tick)))
              (save-excursion
                (fountain-read-metadata t)
                (setq fountain-data
                      (fountain-export-parse (point-max)))))
          ;; (let* ((element (pop fountain-export-content))
          ;;        (type (car element))
          ;;        (content (nth 1 element))
          ;;        (plist (nth 2 element)))
          ;; (with-current-buffer dest-buf
          ;;   (insert (fountain-export-format-element element format) ?\n)))
          dest-buf)
      (unless complete
        (kill-buffer dest-buf)))))

(defun fountain-export--html ()
  ;; internal function, don't call externally
  ;; use `fountain-export-buffer-to-html' instead
  ;; first read the metadata
  (fountain-read-metadata)
  (let* ((sourcebuf (current-buffer))
         (destbuf (get-buffer-create
                   (fountain-export-get-filename "html")))
         (head (fountain-export-create-html-head))
         (title-page (fountain-export-create-html-title-page))
         complete)
    (unwind-protect
        (progn
          ;; fontify the accessible buffer
          (fountain-export-fontify-buffer)
          ;; create a temp buffer with source
          (with-temp-buffer
            (insert-buffer-substring sourcebuf)
            ;; strip comments
            (fountain-export-strip-comments)
            ;; insert HTML head
            (with-current-buffer destbuf
              (with-silent-modifications
                (erase-buffer)
                (insert "<!DOCTYPE html>\n<html>\n"
                        head "\n")
                ;; close head and open body
                (insert "<body>\n")
                ;; add the title page maybe
                (if (and title-page
                         fountain-export-include-title-page)
                    (insert "<section id=\"title-page\">\n"
                            title-page
                            "</section>\n"))
                (insert "<section id=\"screenplay\">\n")))
            ;; parse the temp buffer
            (fountain-export-parse-buffer))
          ;; close HTML tags
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert "</section>\n</body>\n</html>")))
          ;; signal completion and return DESTBUF
          (setq complete t)
          destbuf)
      ;; if error occurs, kill the unsaved buffer
      (unless complete
        (kill-buffer destbuf)))))

;;; Commands ===================================================================

(defun fountain-export-default ()
  "Call function defined in `fountain-export-default-command'"
  (interactive)
  (funcall fountain-export-default-command))

(defun fountain-export-buffer-to-html (&optional buffer)
  "Export BUFFER to HTML file, then switch to HTML buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (let ((destbuf (fountain-export--html))
              (outputdir (if (buffer-file-name buffer)
                             (expand-file-name (file-name-directory
                                                (buffer-file-name buffer))))))
          (with-current-buffer destbuf
            (if outputdir
                (write-file outputdir)))
          (if (called-interactively-p 'interactive)
              (switch-to-buffer-other-window destbuf))
          destbuf)))))

(defun fountain-export-buffer-to-pdf-via-html (&optional buffer)
  "Export BUFFER to HTML file, then convert HTML to PDF."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (shell-quote-argument (buffer-file-name
                                      (fountain-export-buffer-to-html
                                       buffer))))
         (command (format fountain-export-pdf-via-html-command file)))
    (async-shell-command command "*Fountain PDF Process*")))

(provide 'fountain-export)
;;; fountain-export.el ends here
