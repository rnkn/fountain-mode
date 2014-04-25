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

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-html
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type 'function
  :group 'fountain-export)

(defcustom fountain-export-font
  '("Courier Prime"
    "Courier Final Draft"
    "Courier Screenplay"
    "Courier"
    "Courier New")
  ""
  :type '(repeat (string :tag "Font"))
  :group 'fountain-export)

(defcustom fountain-export-prepare-html nil
  "If non-nil, auto-indent HTML elements during export.
This if off by default to save time, since the HTML isn't meant
to impress anyone."
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
  "prince ${source} --output=${output} --input=html --verbose"
  "Shell command to convert HTML file to PDF."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-stylesheet-template
  "@page {
    size: ${pagesize};
    margin-top: 1in;
    margin-right: 1in;
    margin-bottom: 0.5in;
    margin-left: 1.5in;
}

#title_page {
    page: title;
}

/* This makes the page-counter start on the first page of the screenplay */
#screenplay {
    counter-reset: page 1;
    page: screenplay;
    prince-page-group: start;
}

@page screenplay {
    /* Page Numbers */
    @top-right-corner {
        font-family: ${font};
        font-size: 12pt;
        content: counter(page)\".\";
        vertical-align: bottom;
        padding-bottom: 1em;
    }

    /* Define Header */
    @top-left {
        content: \"\";
        font: italic 10pt Georgia;
        color: #888;
        vertical-align: bottom;
        padding-bottom: 1.3em;
    }

    /* Define Footer */
    @bottom-left {
        content: \"\";
        font: italic 10pt Georgia;
        color: #888;
        vertical-align: top;
        padding-top: 0;
    }
}

/* removes the header and page-numbers from the first page */
@page screenplay: first {
    @top-right-corner { content: normal; }
    @top-left { content: normal; }
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

/* These control where page-breaks can and cannot happen */

.page-break {
    page-break-after: always;
}

/* by default Prince bookmarks all headings, no thanks */
h3, h4, h5, h6 {
    prince-bookmark-level: none;
}

/* -------- COMMON LAYOUT -------- */

#screenplay {
    width: 6in;
    margin: 0 auto;
}

.centered {
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

/* Sluglines and Transitions */

h1,h2,h3,h4,h5,h6 {
    font-weight: normal;
    font-size: 12pt;
    /* margin-top: 1em; */
    /* margin-bottom: 1em; */
    text-transform: uppercase;
}

/* Full Sluglines */

/* h2 { */
/*     width: inherit; */
/*     margin-top: ${scenespacing}em; */
/*     margin-bottom: 12pt; */
/*     margin-left: 0; */
/*     text-decoration: ${sceneunderline}; */
/*     font-weight: ${scenebold}; */
/* } */

.scene-heading {
    font-weight: bold;
    page-break-after: avoid;
}

.forced-scene-heading {
    font-weight: bold;
    page-break-after: avoid;
}

.action {
    page-break-inside: avoid; */
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
  "Stylesheet template for exporting to HTML."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-html-head-template
  "<!DOCTYPE html>
<!-- Created with Emacs ${emacs-version} running Fountain Mode ${fountain-version} -->
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
  "Fontify between START and END."
  (if font-lock-mode
      (let ((font-lock-maximum-decoration t)
            (job (make-progress-reporter "Fontifying... " 0 100))
            (chunk (/ (buffer-size) 100))
            (n 0))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((limit (+ (point) chunk)))
            (jit-lock-fontify-now (point) limit)
            (goto-char limit)
            (progress-reporter-update job n)
            (setq n (+ n 1))))
        (progress-reporter-done job))
    (error "Font Lock is not active")))

(defun fountain-export-get-name (buffer ext)
  "If BUFFER is visiting a file, concat file name base and EXT.
Otherwise return `fountain-export-buffer-name'"
  (if (buffer-file-name buffer)
      (concat (file-name-base (buffer-file-name buffer)) ext)
    fountain-export-buffer-name))

(defun fountain-export-underline (s)
  "Replace underlined text in S with HTML underline span tags."
  (replace-regexp-in-string "_\\(.+\\)_"
                            "<span class=\"underline\">\\1</span>"
                            s t))

(defun fountain-export-bold (s)
  "Replace bold text in S with HTML strong tags."
  (replace-regexp-in-string "\\*\\*\\(.+\\)\\*\\*"
                            "<strong>\\1</strong>"
                            s t))

(defun fountain-export-italic (s)
  "Replace italic text in S with HTML italic tags."
  (replace-regexp-in-string "\\*\\(.+\\)\\*"
                            "<em>\\1</em>"
                            s t))

(defun fountain-export-filter (s)
  "Escape special characters and replace newlines."
  (let* ((s (s-replace-all '(("&" . "&amp;")
                             ("<" . "&lt;")
                             (">" . "&gt;")
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

(defun fountain-export-create-html-element (substring)
  "Return a HTML element with face and substring of SUBSTRING.
Stylesheet class is taken from face, while content is taken from
of SUBSTRING.

If face is `fountain-comment', return nil."
  (let* ((class
          (if (get-text-property 0 'face substring)
              (let* ((s (symbol-name (get-text-property 0 'face substring)))
                     (s (s-chop-suffix "-highlight" s))
                     (s (s-chop-prefix "fountain-" s))) s)
            "action"))
         (tag (cond ((string= class "scene-heading")
                     "h1")
                    ((string= class "forced-scene-heading")
                     "h2")
                    ((string= class "character")
                     "h3")
                    ("p")))
         (content
          (let* ((s (substring-no-properties substring))
                 (s (fountain-export-filter s))
                 (s (fountain-export-bold s))
                 (s (fountain-export-italic s))
                 (s (fountain-export-underline s)))
            s)))
    (unless (string= class "comment")
      (format "<%s class=\"%s\">%s</%s>\n"
              tag class content tag))))

(defun fountain-export-format-template (template sourcebuf)
  "Format TEMPLATE according to the following list.

Internal function, will not work outside of
`fountain-export-html'."
  (s-format template 'aget
            `(("fountain-version" . ,fountain-version)
              ("emacs-version" . ,emacs-version)
              ;; title?
              ;; author?
              ("htmlfile" . ,(fountain-export-get-name
                              sourcebuf ".html"))
              ("cssfile" . ,(fountain-export-get-name
                             sourcebuf ".css")))))

(defun fountain-export-parse-buffer (destbuf)
  "Find face changes from START to END then insert elements into DESTBUF.
First, find the next face property change from point, then pass
substring between point and change to
`fountain-export-create-html-element', then insert the newly
created HTML element to DESTBUF."
  (let ((job (make-progress-reporter "Parsing... " 0 100)))
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward "\n\s\t")
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
      (unless (looking-at ".\\|\\'")
        (forward-char 1))
      (progress-reporter-update
       job (truncate (* (/ (float (point)) (buffer-size)) 100))))))

(defun fountain-export-prepare-html ()
  ;; internal function, don't call externally
  (sgml-mode)
  (let ((sgml-unclosed-tags '("link" "br"))
        (job (make-progress-reporter "Preparing HTML... " 0 100)))
    (goto-char (point-min))
    (while (null (eobp))
      (indent-according-to-mode)
      (forward-line 1)
      (progress-reporter-update
       job (truncate (* (/ (float (point)) (point-max)) 100))))
    (progress-reporter-done job)))

(defun fountain-export-html-1 ()
  ;; internal function, don't call externally
  ;; use `fountain-export-buffer-to-html' instead
  (let* ((job (make-progress-reporter "Exporting... "))
         (sourcebuf (current-buffer))
         (destbuf (get-buffer-create
                   (fountain-export-get-name sourcebuf ".html")))
         complete)
    (unwind-protect
        (progn
          ;; fontify the buffer
          (fountain-export-fontify-buffer)
          ;; FIXME: create the stylesheet
          ;; insert HTML head
          (with-current-buffer destbuf
            (with-silent-modifications
              (erase-buffer)
              (insert (fountain-export-format-template
                       fountain-export-html-head-template sourcebuf)
                      "<body>\n<div id=\"screenplay\">\n")))
          ;; parse the buffer
          (fountain-export-parse-buffer destbuf)
          ;; close HTML tags
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert "</div>\n</body>\n</html>")
              (if fountain-export-prepare-html
                  (fountain-export-prepare-html))))
          ;; signal completion
          (progress-reporter-done job)
          (setq complete t)
          destbuf)
      ;; if errors occur, kill the unsaved buffer
      (unless complete
        (kill-buffer destbuf)))))

;;; Interactive Functions ==============================================

(defun fountain-export-default ()
  "Call the function defined in `fountain-export-default-command'"
  (interactive)
  (funcall fountain-export-default-command))

(defun fountain-export-buffer-to-html (&optional buffer)
  "Export BUFFER to HTML file, then switch to HTML buffer."
  (interactive)                         ; FIXME: add y-or-n-p
  (with-current-buffer
      (or buffer (current-buffer))
    (let ((destbuf (fountain-export-html-1))
          (outputdir (if (buffer-file-name buffer)
                         (expand-file-name (file-name-directory
                                            (buffer-file-name buffer))))))
    (save-excursion
      (save-restriction
        (widen)
        (with-current-buffer destbuf
          (if outputdir
              (write-file outputdir t)))
        (if (called-interactively-p 'interactive)
            (switch-to-buffer-other-window destbuf))
        destbuf)))))

(defun fountain-export-region-to-html (start end)
  "Export the region to HTML file, then switch to HTML buffer."
  (interactive "r")                     ; FIXME: add y-or-n-p
  (save-excursion
    (let ((destbuf (save-restriction
                     (narrow-to-region start end)
                     (fountain-export-html-1)))
          (outputdir (if (buffer-file-name)
                         (expand-file-name (file-name-directory
                                            (buffer-file-name))))))
      (with-current-buffer destbuf
        (if outputdir
            (write-file outputdir t)))
      (switch-to-buffer-other-window destbuf)
      destbuf)))

(defun fountain-export-buffer-to-pdf-via-html (&optional buffer)
  "Export BUFFER to HTML file, then convert HTML to PDF."
  (interactive)                         ; FIXME: add y-or-n-p
  (let* ((buffer (or buffer (current-buffer)))
         (sourcefile (buffer-file-name (fountain-export-buffer-to-html buffer)))
         (filebase (shell-quote-argument (file-name-sans-extension sourcefile)))
         (destfile (concat filebase ".pdf"))
         (logfile (concat filebase ".log"))
         (command
          (s-format fountain-export-pdf-via-html-command 'aget
                    `(("source" . ,(shell-quote-argument sourcefile))
                      ("output" . ,destfile)))))
    (shell-command (concat command " &"))))

(provide 'fountain-export)
;;; fountain-export.el ends here
