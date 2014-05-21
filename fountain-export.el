;;; fountain-export.el --- Export engine for Fountain Mode

;; Copyright (C) 2014 Paul Rankin
;; Copyright (C) 2014 Oliver Taylor -- stylesheet adapted from TextPlay
;; <https://github.com/olivertaylor/Textplay>

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

;;; Code:

(require 's)

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

;;; Obsolete Aliases ===================================================

(define-obsolete-variable-alias 'fountain-export-title-page-template
  'fountain-export-title-page-title-template "1.1.0")

;;; Customizable Variables =============================================

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-pdf-via-html
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type '(radio (function-item fountain-export-buffer-to-pdf-via-html)
                (function-item fountain-export-buffer-to-html))
  :group 'fountain-export)

(defcustom fountain-export-include-title-page t
  "Generate a title page on export."
  :type 'boolean
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

(defcustom fountain-export-bold-title nil
  "If non-nil, bold title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-underline-title t
  "If non-nil, underline title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-upcase-title t
  "If non-nil, underline title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-double-space-scene-headings nil
  "If non-nil, double space before scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-prepare-html nil
  "If non-nil, auto-indent HTML elements during export.
Off by default because it can take a long time for minimal
benefit."
  :type 'boolean
  :group 'fountain-export)

;; (defcustom fountain-export-preserve-line-breaks t
;;   "If non-nil, convert all newlines into line breaks.
;; Otherwise, only break paragraphs at explicit line breaks (one or
;; more blank lines)."
;;   :type 'boolean
;;   :group 'fountain-export)

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

(defcustom fountain-export-title-page-title-template
  "${title}
${credit}
${author}"
  "Template for creating title page title block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-left-template
  "${draft}
${date}
${notes}"
  "Template for creating title page left block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-right-template
  "${contact}"
  "Template for creating title page right block."
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

#title-page {
    page: title;
    margin: 0 auto;
    width: 6in;
    clear: both;
    page-break-after: always;
}

#screenplay {
    margin: 0 auto;
    width: 6in;
    clear: both;
    counter-reset: page 1;
    page: screenplay;
    prince-page-group: start;
}

@media print {
    #title {
        margin-top: 3.5in;
        margin-bottom: 4in;
    }
}

#title-page #title {
    text-align: center;
}

#title-page #title img {
    width: 100%;
}

#title-page #left {
    width: 3in;
    float: left;
}

#title-page #right {
    width: 3in;
    float: right;
    text-align: right;
}


#title h1 {
    text-decoration: ${title-underline};
    text-transform: ${title-upcase};
    font-weight: ${title-bold};
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

hr {
    visibility: hidden;
    page-break-after: always;
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

span.strikethrough {
    text-line-through-style: solid;
}

.page-break {
    visibility: hidden;
    page-break-after: always;
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
    white-space: pre-wrap;
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
    margin-left: 4in;
    width: 2in;
    page-break-before: avoid;
}

.note {
    display: none;
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
  "<head>
<meta charset=\"${charset}\">
<meta name=\"author\" content=\"${author}\" />
<meta name=\"generator\" content=\"Emacs ${emacs-version} running Fountain Mode ${fountain-version}\" />
<title>${title}</title>
${insert-style}
</head>"
  "HTML head template inserted into export buffer.
Currently, ${charset} will default to UTF-8."
  :type 'string
  :group 'fountain-export)

;;; Internal Functions =================================================

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
              (setq n (+ n 1))))
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

(defun fountain-export-get-name (ext)
  "If BUFFER is visiting a file, concat file name base and EXT.
Otherwise return `fountain-export-buffer'"
  (if (buffer-file-name)
      (concat (file-name-base (buffer-file-name)) "." ext)
    (format "*Fountain %s Export*" ext)))

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
                             ("\\\s" . "&nbsp;")
                             ("\\_" . "&#95;")
                             ("\\*" . "&#42;")) s))
         (s (if fountain-export-convert-quotes
                (s-replace-all '(("\\`" . "&#96;")
                                 ("\\'" . "&apos;")
                                 ("``" . "&ldquo;")
                                 ("''" . "&rdquo;")
                                 ("`" . "&lsquo;")
                                 ("'" . "&rsquo;")) s)
              s)))
         ;; (s (if fountain-export-preserve-line-breaks
         ;;        (s-replace "\n" "<br>\n" s)
         ;;      s)))
    s))

(defun fountain-export-create-html-element (sub-s)
  "Return HTML element where class is \"fountain-element\" of SUB-S.
CSS class is taken from \"fountain-element\" text property of
SUB-S, while content is taken from SUB-S."
  (let* ((class
          (or (get-text-property 0 'fountain-element sub-s)
              "action"))
         (tag (if (string= class "scene-heading") "h2" "p"))
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

(defun fountain-export-get-metadata-value (key)
  "Like `fountain-get-metadata-value' but filters for HTML."
  (let* ((value (cdr (assoc key fountain-metadata)))
         (s (if (listp value)
                (s-join "\n" value)
              value))
         ;; lexical value no longer has any effect
         (s (let ((fountain-export-preserve-line-breaks t))
              (fountain-export-filter s))))
    s))

(defun fountain-export-create-title-page-element (key)
  "Gets metadata value associated with KEY and creates HTML element."
  (let ((content (fountain-export-get-metadata-value key)))
    (if (string= key "title")
        (format "<h1>%s</h1>" content)
      (format "<p>%s</p>" content))))

(defun fountain-export-create-html-title-page ()
  "Create title page based on `fountain-export-title-page-template'."
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
   "\n</div>\n"))

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

(defun fountain-export-parse-buffer (destbuf)
  "Find and insert elements into DESTBUF.
First, skip forward to next available text, mark point as index,
then find the next \"fountain-element\" text property change from
index, then pass substring from index to change to
`fountain-export-create-html-element', then insert the newly
created HTML element to DESTBUF."
  (let ((job (make-progress-reporter "Parsing..." 0 100)))
    (goto-char (point-min))
    (while (null (eobp))
      (while (looking-at "\n\s?\n?")
        (goto-char (match-end 0)))
      (let* ((index (point))
             (limit (save-excursion
                      (re-search-forward "\n\s?\n\\|\\'" nil t)
                      (match-beginning 0)))
             (change (next-single-property-change
                      index 'fountain-element nil limit)))
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
  ;; first read the metadata
  (fountain-read-metadata)
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
                (insert "<!DOCTYPE html>\n<html>\n"
                        head "\n")
                ;; close head and open body
                (insert "<body>\n")
                (if fountain-export-include-title-page
                    (insert "<div id=\"title-page\">\n"
                            title-page
                            "</div>\n"))
                (insert "<div id=\"screenplay\">\n")))
            ;; parse the temp buffer
            (fountain-export-parse-buffer destbuf))
          ;; close HTML tags
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert "</div>\n</body>\n</html>")
              (if fountain-export-prepare-html
                  (fountain-export-prepare-html))))
          ;; signal completion and return DESTBUF
          (setq complete t)
          destbuf)
      ;; if error occurs, kill the unsaved buffer
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
