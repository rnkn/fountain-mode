;;; fountain-export.el --- Export engine for Fountain Mode

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 's)
(declare-function 'fountain-get-metadata-value "fountain-mode.el")

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

;;; customizable variables =============================================

(defcustom fountain-export-output-buffer
  "*Fountain Export*"
  "Buffer name to use when not exporting a file."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-pdf-process-buffer
  "*Fountain PDF Process*"
  "Buffer name to use for PDF conversion messages."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-pdf-via-html
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type '(radio (function-item fountain-export-buffer-to-pdf-via-html)
                (function-item fountain-export-buffer-to-html))
  :group 'fountain-export)

(defcustom fountain-export-inline-style t
  "If non-nil, use inline stylesheet.
Otherwise, use an external stylesheet file."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-page-size
  "us-letter"
  "Paper size to use on export."
  :type '(radio (const :tag "US Letter" "us-letter")
                (const :tag "A4" "a4"))
  :group 'fountain-export)

(defcustom fountain-export-font
  '("Courier" "Courier New")
  "List of font names to use when exporting, by priority."
  :type '(repeat (string :tag "Font"))
  :group 'fountain-export)

(defcustom fountain-export-bold-scene-headings nil
  "If non-nil, bold scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-underline-scene-headings nil
  "If non-nil, underline scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-double-space-scene-headings nil
  "If non-nil, double space before scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-prepare-html nil
  "If non-nil, auto-indent HTML elements during export.
This if off by default because it can take a long time for a
minimal benefit."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-convert-quotes nil
  "If non-nil, replace TeX-style quotes with \"smart-quotes\".

\`\`foobar\'\'

will be exported as

&ldquo;foobar&rdquol;"
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-pdf-via-html-command
  "prince %s --verbose"
  "Shell command string to convert HTML file to PDF."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-template
  "<div class=\"title-block\">
<h1>${title}</h1>
<p>${contact}</p>
<p>${author}</p>
<p>${draft}</p>
</div>
<div class=\"contact\">${contact}</div>
<div class=\"date\">${date}</div>"
  "HTML template for created the title-page div."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-style-template
  "@page {
    size: ${page-size};
    margin-top: 1in;
    margin-right: 1in;
    margin-bottom: 0.5in;
    margin-left: 1.5in;
}

#title_page {
    page: title;
}

#screenplay {
    counter-reset: page 1;
    page: screenplay;
    prince-page-group: start;
}

@page screenplay {
    @top-right-corner {
        font-family: ${font};
        font-size: 12pt;
        content: counter(page)\".\";
        vertical-align: bottom;
        padding-bottom: 1em;
    }
}

@page screenplay:first {
    @top-right-corner {
        content: normal;
    }
    @top-left {
        content: normal;
    }
}

h1,h2,h3,h4,h5,h6 {
    font-weight: normal;
    font-size: 12pt;
}

body {
    font-family: ${font};
    font-size: 12pt;
    line-height: 1;
}

em {
    font-style: italic;
}

strong {
    font-weight: bold;
}

span.underline {
    text-decoration: underline;
}

.strikethrough {
    text-line-through-style: solid;
}

.page-break {
    page-break-after: always;
}

h2, h3, h4, h5, h6 {
    prince-bookmark-level: none;
}

#screenplay {
    width: 6in;
    margin: 0 auto;
}

.centered {
    text-align: center;
    margin-left: 0;
    width: 100%;
}

.center {
    text-align: center;
    margin-left: 0;
    width: 100%;
}

p {
    margin-top: 1em;
    margin-bottom: 1em;
    margin-left: 0;
    width: auto;
    orphans: 2;
    widows: 2;
}

.scene-heading {
    font-weight: ${scene-bold};
    text-decoration: ${scene-underline};
    margin-top: ${scene-spacing};
    page-break-after: avoid;
}

.action {
    page-break-inside: avoid;
}

.character {
    margin-bottom: 0;
    margin-left: 2in;
    width: 4in;
    page-break-after: avoid;
}

.paren {
    margin-top: 0;
    margin-bottom: 0;
    margin-left: 1.6in;
    text-indent: -0.6em;
    width: 2in;
    page-break-before: avoid;
    page-break-inside: avoid;
    page-break-after: avoid;
}

.dialog {
    margin-top: 0;
    margin-bottom: 0;
    margin-left: 1in;
    width: 3.5in;
    page-break-before: avoid;
    page-break-inside: avoid;
}

.trans {
    margin-top: 1em;
    margin-bottom: 1em;
    margin-left: 4in;
    width: 2in;
    page-break-before: avoid;
}

.note {
    display: none
}

.section {
    display: none;
}

.synopsis {
    display: none;
}"
  "Style template for exporting to HTML, and PDF via HTML."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-html-head-template
  "<!DOCTYPE html>
<!-- Created with Emacs ${emacs-version} running Fountain Mode ${fountain-version} -->
<html>
<head>
<meta charset=\"${charset}\">
<meta name=\"author\" content=\"${author}\" />
<title>${title}</title>
${insert-style}
</head>"
  "HTML template inserted into export buffer.
Currently, ${charset} will default to UTF-8."
  :type 'string
  :group 'fountain-export)

;;; internal functions =================================================

(defun fountain-export-fontify-buffer ()
  "If `font-lock-mode' is enables, fontify entire buffer."
  (if font-lock-mode
      (let ((font-lock-maximum-decoration t)
            (job (make-progress-reporter "Fontifying..." 0 100))
            (chunk (/ (buffer-size) 100))
            (n 0))
        (font-lock-refresh-defaults)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((limit (+ (point) chunk)))
            (jit-lock-fontify-now (point) limit)
            (goto-char limit)
            (progress-reporter-update job n)
            (setq n (+ n 1))))
        (progress-reporter-done job))
    (error "Font Lock is not active")))

(defun fountain-export-strip-comments ()
  "Strips buffer of all comments and metadata.
Matches and deletes any text with `fountain-comment',
`fountain-metadata-key' or `fountain-metadata-value' face."
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
                  (point) 'face nil (point-max))))))

(defun fountain-export-get-name (ext)
  "If BUFFER is visiting a file, concat file name base and EXT.
Otherwise return `fountain-export-buffer'"
  (if (buffer-file-name)
      (concat (file-name-base (buffer-file-name)) "." ext)
    fountain-export-output-buffer))

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

(defun fountain-export-emphasis (s)
  "Replace italic text in S with HTML emphasis tags."
  (replace-regexp-in-string "\\*\\(.+?\\)\\*"
                            "<em>\\1</em>"
                            s t))

(defun fountain-export-lyrics (s)
  "Replace lyrics in S with HTML italic tags."
  (replace-regexp-in-string "^~\s*\\(.+\\)"
                            "<i>\\1</i>"
                            s t))

(defun fountain-export-filter (s)
  "Escape special characters and replace newlines.
If `fountain-export-convert-quotes' is non-nil, convert quotes to
\"smart quotes\"."
  (let* ((s (s-replace-all '(("&" . "&amp;")
                             ("<" . "&lt;")
                             (">" . "&gt;")
                             ("\s\s" . "&nbsp; ")
                             ("\\\s" . "&nbsp;")
                             ("\\\-" . "&#8209;")
                             ("\\_" . "&#95;")
                             ("\\*" . "&#42;")
                             ("\n" . "<br>")) s))
         (s (if fountain-export-convert-quotes
                (s-replace-all '(("\\`" . "&#96;")
                                 ("\\'" . "&apos;")
                                 ("``" . "&ldquo;")
                                 ("''" . "&rdquo;")
                                 ("`" . "&lsquo;")
                                 ("'" . "&rsquo;")) s)
              s)))
    s))

(defun fountain-export-create-html-element (sub-s)
  "Return an HTML element with face and substring of SUB-S.
Stylesheet class is taken from face, while content is taken from
of SUB-S."
  (let* ((class
          (if (get-text-property 0 'face sub-s)
              (let* ((s (symbol-name (get-text-property 0 'face sub-s)))
                     (s (s-chop-suffix "-highlight" s))
                     (s (s-chop-prefix "fountain-" s))) s)
            "action"))
         (tag (cond ((string= class "scene-heading")
                     "h2")
                    ((string= class "character")
                     "h3")
                    ("p")))
         (content
          (let* ((s (substring-no-properties sub-s))
                 (s (fountain-export-filter s))
                 (s (fountain-export-bold s))
                 (s (fountain-export-emphasis s))
                 (s (fountain-export-lyrics s))
                 (s (fountain-export-underline s)))
            s)))
    (format "<%s class=\"%s\">%s</%s>\n"
            tag class content tag)))

(defun fountain-export-create-html-title-page ()
  "Create the title page using `fountain-export-title-page-template'."
  (s-format fountain-export-title-page-template
            'fountain-get-metadata-value))

(defun fountain-export-create-style ()
  "Create stylesheet using `fountain-export-styles-template'."
  (let* ((page-size fountain-export-page-size)
         (font
          (let (list)
            (dolist (font fountain-export-font (s-join "," list))
              (setq list
                    (append list
                            (list (concat "'" font "'")))))))
         (scene-bold
          (if fountain-export-bold-scene-headings
              "bold" "normal"))
         (scene-underline
          (if fountain-export-underline-scene-headings
              "underline" "none"))
         (scene-spacing
          (if fountain-export-double-space-scene-headings
              "2em" "1em"))
         (style-rules (s-format fountain-export-style-template
                          '(lambda (var)
                             (symbol-value (intern var))))))
    (if fountain-export-inline-style
        (concat "<style type=\"text/css\">\n"
                style-rules
                "\n</style>")
      (let ((cssfile (get-buffer-create (fountain-export-get-name "css")))
            (outputdir (expand-file-name
                        (file-name-directory (buffer-file-name)))))
        (with-current-buffer cssfile
          (erase-buffer)
          (insert style-rules)
          (write-file outputdir))
        (concat "<link rel=\"stylesheet\" href=\""
                (buffer-name cssfile)
                "\">")))))

(defun fountain-export-create-html-head ()
  "Create the HTML head using `fountain-export-html-head-template'."
  (let ((insert-style (fountain-export-create-style))
        (charset "utf-8")
        (title (fountain-get-metadata-value "title"))
        (author (fountain-get-metadata-value "author")))
    (s-format fountain-export-html-head-template
              '(lambda (var)
                 (symbol-value (intern var))))))

(defun fountain-export-parse-buffer (destbuf)
  "Find face changes in current buffer then insert elements into DESTBUF.
First, find the next face property change from point, then pass
substring between point and change to
`fountain-export-create-html-element', then insert the newly
created HTML element to DESTBUF."
  (let ((job (make-progress-reporter "Parsing..." 0 100)))
    (goto-char (point-min))
    (while (null (eobp))
      (skip-chars-forward "\n")
      (let* ((index (point))
             (limit (save-excursion
                      (re-search-forward "\n\s?\n\\|\\'" nil t)
                      (match-beginning 0)))
             (change (next-single-property-change index 'face nil limit)))
        (when change
          (let* ((s (buffer-substring index change))
                 (element (fountain-export-create-html-element s)))
            (when element
              (with-current-buffer destbuf
                (with-silent-modifications
                  (insert element)))))
          (goto-char change)))
      ;; (unless (looking-at ".\\|\\'")
      ;;   (forward-char 1))
      (progress-reporter-update
       job (truncate (* (/ (float (point)) (buffer-size)) 100))))))

(defun fountain-export-prepare-html ()
  ;; internal function, don't call externally
  (sgml-mode)
  (let ((sgml-unclosed-tags '("link" "br"))
        (job (make-progress-reporter "Preparing HTML..." 0 100)))
    (goto-char (point-min))
    (while (null (eobp))
      (indent-according-to-mode)
      (forward-line 1)
      (progress-reporter-update
       job (truncate (* (/ (float (point)) (point-max)) 100))))
    (progress-reporter-done job)))

(defun fountain-export--html ()
  ;; internal function, don't call externally
  ;; use `fountain-export-buffer-to-html' instead
  (let* ((sourcebuf (current-buffer))
         (destbuf (get-buffer-create
                   (fountain-export-get-name "html")))
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
                (insert head "\n")
                ;; close head and open body
                (insert "<body>\n")
                (insert "<div id=\"screenplay\">\n")))
            ;; parse the temp buffer
            (fountain-export-parse-buffer destbuf))
          ;; close HTML tags
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert "</div>\n</body>\n</html>")
              (if fountain-export-prepare-html
                  (fountain-export-prepare-html))))
          ;; signal completion and kill buffers
          (font-lock-refresh-defaults)
          (setq complete t)
          destbuf)
      ;; if errors occur, kill the unsaved buffer
      (unless complete
        (kill-buffer destbuf)))))

;;; Menu Functions =====================================================

(defun fountain-toggle-export-bold-scene-headings ()
  "Toggle `fountain-export-bold-scene-headings'"
  (interactive)
  (setq fountain-export-bold-scene-headings
        (null fountain-export-bold-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-bold-scene-headings
               "bold" "normal")))

(defun fountain-toggle-export-underline-scene-headings ()
  "Toggle `fountain-export-underline-scene-headings'"
  (interactive)
  (setq fountain-export-underline-scene-headings
        (null fountain-export-underline-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-underline-scene-headings
               "underlined" "normal")))

(defun fountain-toggle-export-double-space-scene-headings ()
  "Toggle `fountain-export-double-space-scene-headings'"
  (interactive)
  (setq fountain-export-double-space-scene-headings
        (null fountain-export-double-space-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-double-space-scene-headings
               "double-spaced" "single-spaced")))

;;; Interactive Functions ==============================================

(defun fountain-export-default ()
  "Call the function defined in `fountain-export-default-command'"
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
    (async-shell-command command fountain-export-pdf-process-buffer)))

(provide 'fountain-export)
;;; fountain-export.el ends here
