;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 1.0.0
;; Package-Requires: ((s "1.9.0"))
;; URL: http://github.com/rnkn/fountain-mode/

;; This file is not part of GNU Emacs.
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fountain Mode is a major mode for GNU Emacs for editing text files in
;; Fountain markup format, a simple markup syntax for writing, editing
;; and sharing screenplays in plain text. Fountain Mode is free
;; software, licensed under the GNU GPL version 3.

;; For more information on Fountain markup format, see
;; <http://fountain.io>

;;; Code:

(require 's)
(require 'thingatpt)
(require 'easymenu)
(require 'fountain-export)

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "http://github.com/rnkn/fountain-mode/"))

(defconst fountain-version
  "1.0.0")

;;; Obsolete Aliases ===================================================

(define-obsolete-variable-alias 'fountain-indent-character-col
  'fountain-align-character "0.12.0")

(define-obsolete-variable-alias 'fountain-indent-dialog-col
  'fountain-align-dialog "0.12.0")

(define-obsolete-variable-alias 'fountain-indent-paren-col
  'fountain-align-paren "0.12.0")

(define-obsolete-variable-alias 'fountain-indent-trans-col
  'fountain-align-trans "0.12.0")

(define-obsolete-variable-alias 'fountain-indent-centered-col
  'fountain-align-centered "0.12.0")

(define-obsolete-variable-alias 'fountain-indent-character
  'fountain-align-character "0.13.0")

(define-obsolete-variable-alias 'fountain-indent-dialog
  'fountain-align-dialog "0.13.0")

(define-obsolete-variable-alias 'fountain-indent-paren
  'fountain-align-paren "0.13.0")

(define-obsolete-variable-alias 'fountain-indent-trans
  'fountain-align-trans "0.13.0")

(define-obsolete-variable-alias 'fountain-indent-centered
  'fountain-align-centered "0.13.0")

;;; Customizable Options ===============================================

(defcustom fountain-mode-hook
  '(turn-on-visual-line-mode)
  "Mode hook for `fountain-mode', run after the mode is turned on."
  :type 'hook
  :group 'fountain)

(defcustom fountain-metadata-template
  "title: ${title}\ncredit: written by\nauthor: ${fullname}\ndraft: first\ndate: ${longtime}\ncontact: ${email}\n"
  "Metadata template to be inserted at beginning of buffer.
See `fountain-insert-template'."
  :type 'string
  :group 'fountain)

(defcustom fountain-scene-heading-prefix-list
  '("INT" "EXT" "I/E" "INT/EXT" "EST")
  "List of scene heading prefixes (case insensitive).

Any scene heading prefix can be followed by a dot and/or a space,
so the following are equivalent:

INT HOUSE - DAY

INT. HOUSE - DAY

INT./EXT. HOUSE - DAY"
  :type '(repeat (string :tag "Prefix"))
  :group 'fountain)

(defcustom fountain-trans-list
  '("TO:" "WITH:" "FADE IN" "FADE OUT" "TO BLACK")
  "List of transition endings (case insensitive).

This list is used to match the endings of transitions,
e.g. \"TO:\" will match both the following:

CUT TO:

DISSOLVE TO:"
  :type '(repeat (string :tag "Transition"))
  :group 'fountain)

(defcustom fountain-add-continued-dialog t
  "If non-nil, mark continued dialog appropriately.

When same character speaks in succession, append
`fountain-continued-dialog-string'."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-continued-dialog-string "CONT'D"
  "String to append when same character speaks in succession.

If `fountain-add-continued-dialog' is non-nil, append this string
to character when speaking in succession.

Parentheses are added automatically, e.g. \"CONT'D\" becomes
\"(CONT'D)\""
  :type 'string
  :group 'fountain)

(defcustom fountain-trim-whitespace nil
  "If non-nil, trim whitespace around elements."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-align-character 20
  "Column integer to which character should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-dialog 10
  "Column integer to which dialog should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-paren 15
  "Column integer to which parenthetical should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-trans 45
  "Column integer to which transitions should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-centered 10
  "Column integer to which centered text should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-elements t
  "If non-nil, elements will be displayed aligned.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-switch-comment-syntax nil
  "\\<fountain-mode-map>If non-nil, use \"//\" as default comment syntax (boneyard).

Two syntaxes are supported:

/* this text is a comment */

// this text is
// also a comment

Both syntax will be recognized as comments. This option changes
the behaviour of the \\[comment-dwim] command.

The default is the former but if you prefer the latter, set this
option to non-nil."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-short-time-format "%x"
  "Format of date and time. See `format-time-string'."
  :type 'string
  :group 'fountain)

(defcustom fountain-long-time-format "%B %-e, %Y"
  "Format of date and time. See `format-time-string'."
  :type 'string
  :group 'fountain)

(defcustom fountain-note-template "${time} - ${fullname}: "
  "Template for inserting notes. See `fountain-insert-template'.

The default \"${time} - ${fullname}: \" will insert something
similar to:

\[\[01/20/14 - Alan Smithee: \]\]"
  :type 'string
  :group 'fountain)

(defcustom fountain-uuid-func
  '(lambda () (shell-command-to-string "uuidgen"))
  "Function for generating a UUID.
The default funcation requires the command line tool \"uuidgen\"."
  :tag "Fountain UUID Function"
  :type 'function
  :group 'fountain)

;;; Variables ==========================================================

(defvar fountain-metadata nil
  "Metadata alist in the form of (KEY . VALUE).")

;;; Element Regular Expressions ========================================

(defconst fountain-blank-regexp
  "\\`\\|^\s?$\\|\\'"
  "Regular expression for matching an empty line.")

(defconst fountain-comment-regexp
  "//.*\\|/\\*\\(.\\|\n\\)*?\\*/"
  "Regular expression for matching comments.")

(defconst fountain-metadata-pair-regexp
  "^\\(.+\\):\s*\\(.+\\)?"
  "Regular expression for matching single-line metadata pairs.")

(defconst fountain-metadata-value-regexp
  "^\s+\\(?2:.+\\)"
  "Regular expression for matching multi-line metadata values.")

(defconst fountain-scene-heading-regexp
  (concat "^\\(?2:"
          (regexp-opt fountain-scene-heading-prefix-list)
          "[.\s\t]+.*\\)")
  "Regular expression for matching scene headings.
Requires `fountain-scene-heading-p' for preceding and succeeding
blank lines.")

(defconst fountain-forced-scene-heading-regexp
  "^\\(\\.\\)\\(\\<.*\\)"
  "Regular expression for matching forced scene headings.
Requires `fountain-scene-heading-p' for preceding and
succeeding blank lines.")

(defconst fountain-paren-regexp
  "^[\s\t]*([^)\n]*)[\s\t]*$"
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or
dialog.")

(defconst fountain-page-break-regexp
  "^[\s\t]*=\\{3,\\}.*"
  "Regular expression for matching page breaks.")

(defconst fountain-note-regexp
  "\\[\\[\\(?:.\n?\\)*]]"
  "Regular expression for matching comments.")

(defconst fountain-section-regexp
  "^\\(#\\{1,5\\}[\s\t]*\\)\\([^#\n].*\\)"
  "Regular expression for matching sections.")

(defconst fountain-synopsis-regexp
  "^\\(=[\s\t]*\\)\\([^=\n].*\\)"
  "Regular expression for matching synopses.")

(defconst fountain-trans-regexp
  (concat "^[\s\t]*\\(?2:[[:upper:]\s]*"
          (regexp-opt fountain-trans-list)
          "\\)$")
  "Regular expression for matching transitions.")

(defconst fountain-forced-trans-regexp
  "^\\([\s\t]*>\s*\\)\\([^<>\n]*\\)$"
  "Regular expression for matching forced transitions.")

(defconst fountain-centered-regexp
  "\\(^[\s\t]*>[\s\t]*\\)\\(.*?\\)\\([\s\t]*<[\s\t]*$\\)"
  "Regular expression for matching centered text.")

;;; Emphasis Regular Expressions =======================================

(defconst fountain-emphasis-delim-regexp
  "[^\\]\\([_*]+\\)"
  "Regular expression for matching emphasis delimiters.")

(defconst fountain-underline-regexp
  "[^\\]_\\(.*?[^\\\n]\\)_"
  "Regular expression for matching underlined text.")

(defconst fountain-italic-regexp
  "[^\\]\\*\\(.*?[^\\\n]\\)\\*"
  "Regular expression for matching italic text.")

(defconst fountain-bold-regexp
  "[^\\]\\*\\{2\\}\\(.*?[^\\\n]\\)\\*\\{2\\}"
  "Regular expression for matching bold text.")

;;; Faces ==============================================================

(defgroup fountain-faces nil
  "Faces used in `fountain-mode'.

There are three levels of Font Lock decoration:

  1 - none, uses default face
  2 - minimal, uses fountain-ELEMENT faces
  3 - maximum, uses fountain-ELEMENT-highlight faces

To switch between these levels of Font Lock decoration, customize
the value of `font-lock-maximum-decoration'. This can be set
indirectly with \\[fountain-set-font-lock-decoration] and saved
with \\[fountain-save-font-lock-decoration]."
  :group 'fountain)

(defface fountain-comment
  '((t (:inherit shadow)))
  "Default face for comments (boneyard)."
  :group 'fountain-faces)

(defface fountain-metadata-key
  '((t (:inherit font-lock-constant-face)))
  "Default face for metadata keys."
  :group 'fountain-faces)

(defface fountain-metadata-value
  '((t (:inherit match)))
  "Default face for metadata keys."
  :group 'fountain-faces)

(defface fountain-page-break
  '((t (:inherit fountain-comment)))
  "Default face for page breaks."
  :group 'fountain-faces)

(defface fountain-scene-heading
  '((t (:weight bold :underline t)))
  "Default face for scene headings."
  :group 'fountain-faces)

(defface fountain-scene-heading-highlight
  '((t (:weight bold :underline t
                :inherit font-lock-function-name-face)))
  "Additional highlighting face for scene headings."
  :group 'fountain-faces)

(defface fountain-paren
  '((t (:inherit default)))
  "Default face for parentheticals."
  :group 'fountain-faces)

(defface fountain-paren-highlight
  '((t (:inherit font-lock-variable-name-face)))
  "Additional highlighting face for parentheticals."
  :group 'fountain-faces)

(defface fountain-centered
  '((t (:inherit default)))
  "Default face for centered text."
  :group 'fountain-faces)

(defface fountain-centered-highlight
  '((t (:inherit default)))
  "Additional highlighting face for centered text."
  :group 'fountain-faces)

(defface fountain-note
  '((t (:inherit fountain-note-highlight)))
  "Default face for notes.")

(defface fountain-note-highlight
  '((t (:inherit font-lock-comment-face)))
  "Additional highlighting face for notes.")

(defface fountain-section
  '((t (:inherit fountain-section-highlight)))
  "Default face for sections."
  :group 'fountain-faces)

(defface fountain-section-highlight
  '((t (:inherit font-lock-builtin-face)))
  "Additional highlighting face for sections."
  :group 'fountain-faces)

(defface fountain-synopsis
  '((t (:inherit fountain-synopsis-highlight)))
  "Default face for synopses."
  :group 'fountain-faces)

(defface fountain-synopsis-highlight
  '((t (:inherit font-lock-type-face)))
  "Default face for synopses."
  :group 'fountain-faces)

(defface fountain-character
  '((t (:inherit default)))
  "Default face for characters."
  :group 'fountain-faces)

(defface fountain-character-highlight
  '((t (:inherit font-lock-keyword-face)))
  "Additional highlighting face for characters."
  :group 'fountain-faces)

(defface fountain-dialog
  '((t (:inherit default)))
  "Default face for dialog."
  :group 'fountain-faces)

(defface fountain-dialog-highlight
  '((t (:inherit font-lock-string-face)))
  "Additional highlighting face for dialog."
  :group 'fountain-faces)

(defface fountain-trans
  '((t (:inherit default)))
  "Default face for transitions."
  :group 'fountain-faces)

(defface fountain-trans-highlight
  '((t (:inherit font-lock-variable-name-face)))
  "Additional highlighting face for transitions."
  :group 'fountain-faces)

;;; Thing Definitions ==================================================

(put 'scene 'forward-op 'fountain-forward-scene)

;;; Internal Functions =================================================

(defun fountain-get-line ()
  "Return the line at point as a string."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun fountain-get-block-bounds ()
  "Return the beginning and end points of block at point."
  (let ((block-beginning
         (save-excursion
           (re-search-backward fountain-blank-regexp nil t)))
        (block-end
         (save-excursion
           (re-search-forward fountain-blank-regexp nil t))))
    (cons block-beginning block-end)))

(defun fountain-strip-comments (start end)
  "Strip comments between START and END and return string."
  (let ((start
         (save-excursion
           (goto-char start)
           ;; using thing-at-point-looking-at is very slow, better to
           ;; use a simpler function.
           ;;
           ;; (if (thing-at-point-looking-at fountain-comment-regexp)
           ;;     (match-beginning 0)
           ;;   start)))
           (if (and (search-forward "*/" end t)
                    (null (search-backward "/*" start t))
                    (search-backward "/*" nil t)
                    (comment-only-p (point) start))
               (point)
             start)))
        (end
         (save-excursion
           (goto-char end)
           ;; (if (thing-at-point-looking-at fountain-comment-regexp)
           ;;     (match-end 0)
           ;;   end))))
           (if (and (search-backward "/*" start t)
                    (null (search-forward "*/" end t))
                    (search-forward "*/" nil t)
                    (comment-only-p (point) end))
               (point)
             end))))
    (replace-regexp-in-string
     fountain-comment-regexp ""
     (buffer-substring-no-properties start end))))

(defun fountain-blank-p ()
  "Return non-nil if point is at a blank line or single space."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      ;; don't modify match-data
      (looking-at-p fountain-blank-regexp))))

(defun fountain-metadata-p ()
  "Return non-nil if point is at metadata."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (or (looking-at fountain-metadata-pair-regexp)
               (looking-at fountain-metadata-value-regexp))
           (or (bobp)
               (save-match-data
                 (forward-line -1)
                 (fountain-metadata-p)))))))

(defun fountain-section-p ()
  "Return non-nil if point is at a section, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-section-regexp))))

(defun fountain-synopsis-p ()
  "Return non-nil if point is at a synopsis, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-synopsis-regexp))))

(defun fountain-note-p ()
  "Return non-nil if point is at a note, nil otherwise."
  (thing-at-point-looking-at fountain-note-regexp))

(defun fountain-comment-p ()
  "Return non-nil if point is at a comment, nil otherwise."
  ;; problems with comment-only-p picking up blank lines as comments
  ;;
  ;; (comment-only-p (line-beginning-position) (line-end-position)))
  (thing-at-point-looking-at fountain-comment-regexp))

(defalias 'fountain-boneyard-p 'fountain-comment-p)

(defun fountain-invisible-p ()
  "Return non-nil if point is at an invisible element.
A line is invisible if it is blank, or consists of a section,
synopsis, note, or is within a comment."
  (or (fountain-blank-p)
      (fountain-section-p)
      (fountain-synopsis-p)
      (fountain-note-p)
      (fountain-comment-p)))

(defun fountain-scene-heading-p ()
  "Return non-nil if point is at a scene heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (or (looking-at fountain-forced-scene-heading-regexp)
               (looking-at fountain-scene-heading-regexp))
           (save-match-data
             (forward-line -1)
             (fountain-invisible-p))))))

(defun fountain-get-character ()
  "Return character if point is at a character, nil otherwise."
  ;; (save-excursion
  ;;   (save-restriction
  ;;     (widen)
  ;;     (when (s-present?
  ;;            (fountain-strip-comments
  ;;             (line-beginning-position) (line-end-position)))
  ;;       (forward-line 0)
  ;;       (unless (looking-at-p fountain-scene-heading-regexp)
  ;;         (let* ((s (fountain-strip-comments
  ;;                    (line-beginning-position) (line-end-position)))
  ;;                (s (s-trim (car (s-slice-at "(\\|\\^" s))))
  ;;                (s (s-presence s)))
  ;;           (when (and s
  ;;                      (or (s-uppercase? s)
  ;;                          (s-starts-with? "@" s))
  ;;                      (save-excursion
  ;;                        (forward-line -1)
  ;;                        (fountain-invisible-p))
  ;;                      (save-excursion
  ;;                        (forward-line 1)
  ;;                        (unless (eobp)
  ;;                          (null (fountain-invisible-p)))))
  ;;             s)))))))
  (if (fountain-character-p)
      (let ((s (match-string-no-properties 0)))
        (s-trim (car (s-slice-at "\\^\\|(" s))))))

(defun fountain-character-p ()
  "Return non-nil if point is at character, nil otherwise."
  (unless (or (fountain-blank-p)
              (fountain-scene-heading-p))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (and (looking-at "[^<>\n]+")
             (save-match-data
               (let* ((s (match-string-no-properties 0))
                      (s (s-trim (car (s-slice-at "(" s)))))
                 (and (or (s-uppercase? s)
                          (s-starts-with? "@" s))
                      (save-excursion
                        (forward-line -1)
                        (fountain-invisible-p))
                      (save-excursion
                        (forward-line 1)
                        (unless (eobp)
                          (null (fountain-invisible-p))))))))))))

(defun fountain-dialog-p ()
  "Return non-nil if point is at dialog."
  (unless (or (fountain-blank-p)
              (fountain-paren-p)
              (fountain-note-p))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (and (looking-at "[^<>\n]+")
             (save-match-data
               (unless (bobp)
                 (forward-line -1)
                 (or (fountain-character-p)
                     (fountain-paren-p)
                     (fountain-dialog-p)))))))))

(defun fountain-paren-p ()
  "Return non-nil if point is at a paranthetical."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-paren-regexp)
           (save-match-data
             (unless (bobp)
               (forward-line -1)
               (or (fountain-character-p)
                   (fountain-dialog-p))))))))

(defun fountain-trans-p ()
  "Return non-nil if point is at a transition."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (let (case-fold-search)
             (or (looking-at fountain-forced-trans-regexp)
                 (looking-at fountain-trans-regexp)))
           (save-match-data
             (save-excursion
               (forward-line -1)
               (or (bobp)
                   (fountain-invisible-p)))
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-invisible-p))))))))

(defun fountain-centered-p ()
  "Return non-nil if point is at centered text."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-centered-regexp))))

(defun fountain-read-metadata ()
  "Read and set `fountain-metadata' alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (setq fountain-metadata nil)
      (while (fountain-metadata-p)
        (let ((key (downcase (match-string-no-properties 1)))
              (value
               (progn
                 (forward-line 1)       ; FIXME: on line ahead
                 (or (match-string-no-properties 2)
                     (let (s)
                       (while (and (fountain-metadata-p)
                                   (null (match-string 1)))
                         (setq s
                               (append s
                                       (list (match-string-no-properties 2))))
                         (forward-line 1))
                       s)))))
          (setq fountain-metadata
                (append fountain-metadata
                        (list (cons key value))))))
      fountain-metadata)))

(defun fountain-get-metadata-value (key)
  "Return the value associated with KEY.
If there are multiple values, join by concatenating with
newlines."
  (let ((value (cdr (assoc key fountain-metadata))))
    (if (listp value)
        (s-join "\n" value)
      value)))

(defun fountain-insert-template (template)
  "Format TEMPLATE according to the following list.

To include an item in a template you must use the full \"${foo}\"
syntax.

    ${title}    Buffer name without extension
    ${longtime} Long date format (defined in `fountain-long-time-format')
    ${time}     Short date format (defined in `fountain-short-time-format')
    ${fullname} User full name (defined in `user-full-name')
    ${nick}     User first name (defined in `user-login-name')
    ${email}    User email (defined in `user-mail-address')
    ${uuid}     Insert a UUID (defined in `fountain-uuid-func')

Optionally, use \"$m\" and \"$p\" to set the `mark' and `point',
respectively, but only use one of each."
  (let ((start (point)))
    (insert (s-format template 'aget
                      `(("title" . ,(file-name-base (buffer-name)))
                        ("longtime" . ,(format-time-string fountain-long-time-format))
                        ("time" . ,(format-time-string fountain-short-time-format))
                        ("fullname" . ,user-full-name)
                        ("nick" . ,(capitalize user-login-name))
                        ("email" . ,user-mail-address)
                        ("uuid" . ,(fountain-uuid)))))
    (let ((end (point)))
      (goto-char start)
      (when (search-forward "$m" end t)
        (replace-match "")
        (push-mark (point) t t))
      (goto-char start)
      (if (search-forward "$p" end t)
          (replace-match "")
        (goto-char end)))))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID."
  (let ((s (downcase (funcall fountain-uuid-func))))
    (car (split-string s "-"))))

;; could combine with `fountain-get-character' as optional N?
(defun fountain-get-previous-character (n)
  "Return Nth previous character within scene, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (while (> n 0)
        (unless (fountain-scene-heading-p)
          (forward-line -1))
        (while (null (or (fountain-character-p)
                         (fountain-scene-heading-p)
                         (bobp)))
          (forward-line -1))
        (setq n (- n 1)))
      (fountain-get-character))))

(defun fountain-trim-whitespace ()
  "Trim whitespace around line."
  (let ((s (s-trim (fountain-get-line)))
        (start (line-beginning-position))
        (end (line-end-position)))
    (insert-before-markers s)
    (delete-region start end)))

(defun fountain-font-lock-extend-region ()
  "Extend region for fontification to text block."
  (eval-when-compile
    (defvar font-lock-beg)
    (defvar font-lock-end))
  (let ((start
         (save-excursion
           (goto-char font-lock-beg)
           (car (fountain-get-block-bounds))))
        (end
         (save-excursion
           (goto-char font-lock-end)
           (cdr (fountain-get-block-bounds))))
        changed)
    (goto-char font-lock-beg)
    (unless (or (bobp)
                (eq start font-lock-beg))
      (setq font-lock-beg start changed t))
    (goto-char font-lock-end)
    (unless (or (eobp)
                (eq end font-lock-end))
      (setq font-lock-end end changed t))
    changed))

;;; Interactive Functions  =============================================

(defun fountain-version ()
  "Return `fountain-mode' version."
  (interactive)
  (message "Fountain Mode %s" fountain-version))

(defun fountain-upcase-line ()
  "Upcase the line."
  ;; not in use, delete?
  (interactive)
  (upcase-region (line-beginning-position) (line-end-position)))

(defun fountain-upcase-line-and-newline ()
  "Upcase the line and insert a newline."
  (interactive)
  (upcase-region (line-beginning-position) (point))
  (newline))

(defun fountain-forward-scene (&optional n)
  "Move forward N scene headings (backward if N is negative)."
  (interactive "^p")
  (let* ((i (or n 1))
         (p (if (< i 0) -1 1)))
    (if (= i 0)
        (progn
          (forward-line 0)
          (while (null (or (eq (point) (buffer-end -1))
                           (fountain-scene-heading-p)))
            (forward-line -1)))
      (while (/= i 0)
        (if (fountain-scene-heading-p)
            (forward-line p))
        (while (null (or (eq (point) (buffer-end p))
                         (fountain-scene-heading-p)))
          (forward-line p))
        (setq i (- i p))))))

(defun fountain-backward-scene (&optional n)
  "Move backward N scene headings (foward if N is negative)."
  (interactive "^p")
  (let ((i (or n 1)))
    (fountain-forward-scene (- i))))

(defun fountain-beginning-of-scene ()
  "Move point to beginning of current scene."
  (interactive "^")
  (fountain-forward-scene 0))

(defun fountain-end-of-scene ()
  "Move point to end of current scene."
  (interactive "^")
  (fountain-forward-scene 1)
  (forward-char -1))

(defun fountain-mark-scene (&optional extend)
  "Put mark at end of this scene, point at beginning."
  (interactive "p")
  ;; (if (or extend
  ;;         (and (region-active-p)
  ;;              (eq last-command this-command)))
  ;;     (progn
  ;;       (fountain-forward-scene 1)
  ;;       (push-mark)
  ;;       (exchange-point-and-mark))
  (push-mark)
  (fountain-forward-scene 0)
  (if (null (fountain-scene-heading-p))
      (progn
        (goto-char (mark))
        (error "Before first scene heading"))
    (push-mark)
    (fountain-forward-scene 1)
    (exchange-point-and-mark)))

(defun fountain-insert-synopsis ()
  "Open line below current scene heading and insert synopsis."
  (interactive)
  (widen)
  (push-mark)
  (forward-line 0)
  (while (null (or (bobp)
                   (fountain-scene-heading-p)
                   (fountain-section-p)))
    (forward-line -1))
  (if (bobp)
      (progn
        (goto-char (mark))
        (error "Before first scene or section heading"))
    (progn
      (forward-line 1)
      (unless (and (fountain-blank-p)
                   (save-excursion
                     (forward-line 1)
                     (fountain-blank-p)))
        (open-line 1))
      (insert "= "))))

(defun fountain-insert-note (&optional arg)
  "Insert a note as per `fountain-note-template'.
If prefixed with \\[universal-argument], only insert note delimiters (\"[[\" \"]]\")."
  (interactive "P")
  (let ((comment-start "[[")
        (comment-end "]]"))
    (if arg
        (comment-dwim nil)
      (unless (fountain-blank-p)
        (re-search-forward fountain-blank-regexp))
      (unless (save-excursion
                (forward-line 1)
                (fountain-blank-p))
        (open-line 1))
      (comment-indent)
      (fountain-insert-template fountain-note-template))))

(defun fountain-insert-metadata ()
  "Insert the metadata template at the beginning of file."
  (interactive)
  (widen)
  (goto-char (point-min))
  (fountain-insert-template fountain-metadata-template))

(defun fountain-continued-dialog-refresh (&optional arg)
  "Add or remove continued dialog on characters speaking in succession.

If `fountain-add-continued-dialog' is non-nil, add
`fountain-continued-dialog-string' on characters speaking in
succession, otherwise remove all occurences.

If region is active, act on region, otherwise act on current
scene.

If prefixed with \\[universal-argument], act on whole
buffer (WARNING: this can be very slow)."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      ;; first expand the region
      (let ((start
             (cond (arg (point-min))
                   ((use-region-p) (region-beginning))
                   ((car (bounds-of-thing-at-point 'scene)))))
            (end
             (cond (arg (point-max))
                   ((use-region-p) (region-end))
                   ((cdr (bounds-of-thing-at-point 'scene)))))
            ;; create continued string
            (s (concat "(" fountain-continued-dialog-string ")")))
        ;; delete all matches in region
        (goto-char start)
        (while (re-search-forward (concat "\s*" s) end t)
          (replace-match ""))
        ;; add string where appropriate
        (when fountain-add-continued-dialog
          (goto-char start)
          (while (< (point) end)
            (when (and (null (s-ends-with? s (fountain-get-line)))
                       (fountain-character-p)
                       (s-equals? (fountain-get-character)
                                  (fountain-get-previous-character 1)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" s)))
            (forward-line 1)))))))

;;; Menu Functions =====================================================

(defun fountain-toggle-comment-syntax ()
  "Toggle `fountain-switch-comment-syntax'"
  (interactive)
  (setq fountain-switch-comment-syntax
        (null fountain-switch-comment-syntax))
  (if fountain-switch-comment-syntax
      (setq comment-start "//" comment-end "")
    (setq comment-start "/*" comment-end "*/"))
  (message "Default comment syntax is now %s"
           (if fountain-switch-comment-syntax
               "\"// COMMENT\"" "\"/* COMMENT */\"")))

(defun fountain-toggle-align-elements ()
  "Toggle `fountain-align-elements'"
  (interactive)
  (setq fountain-align-elements
        (null fountain-align-elements))
  (font-lock-refresh-defaults)
  (message "Elements are now displayed %s"
           (if fountain-align-elements
               "aligned" "non-aligned")))

(defun fountain-toggle-add-continued-dialog ()
  "Toggle `fountain-add-continued-dialog'"
  (interactive)
  (setq fountain-add-continued-dialog
        (null fountain-add-continued-dialog))
  (fountain-continued-dialog-refresh)
  (message "Continued dialog is now %s"
           (if fountain-add-continued-dialog
               "added" "removed")))

(defun fountain-set-font-lock-decoration (level)
  "Set `font-lock-maximum-decoration' for `fountain-mode' to LEVEL."
  (interactive "nMaximum Decoration (1-3): ")
  (let ((n font-lock-maximum-decoration))
    (cond ((or (booleanp n)
               (integerp n))
           (setq font-lock-maximum-decoration
                (list (cons 'fountain-mode level)
                      (cons 't n))))
          ((listp n)
           (if (assoc 'fountain-mode n)
               (setcdr (assoc
                        'fountain-mode font-lock-maximum-decoration)
                       level)
             (add-to-list 'font-lock-maximum-decoration
                          (cons 'fountain-mode level))))
          ((error "Malformed variable `font-lock-maximum-decoration'"))))
  (message "Syntax highlighting is now set at %s"
           (cond ((eq level 1) "none")
                 ((eq level 2) "default")
                 ((eq level 3) "maximum")))
  (font-lock-refresh-defaults))

(defun fountain-save-font-lock-decoration ()
  "Save `font-lock-maximum-decoration' in `custom-file'."
  (interactive)
  (customize-save-variable 'font-lock-maximum-decoration
                           font-lock-maximum-decoration))

(defun fountain-get-font-lock-decoration ()
  "Return the value of `font-lock-maximum-decoration'."
  (cond ((null font-lock-maximum-decoration) 2)
        ((eq font-lock-maximum-decoration t) 3)
        ((integerp font-lock-maximum-decoration)
         font-lock-maximum-decoration)
        ((cdr (assoc 'fountain-mode font-lock-maximum-decoration)))
        ((cdr (assoc 't font-lock-maximum-decoration)) 3)))

;;; Font Lock ==========================================================

(defconst fountain-font-lock-keywords-plist
  `(("note" ,fountain-note-regexp
     ((0 nil)))
    ("scene-heading" fountain-match-scene-heading
     ((0 nil keep)
      (1 fountain-comment t t)))
    ("character" fountain-match-character
     ((0 nil)))
    ("dialog" fountain-match-dialog
     ((0 nil keep)))
    ("paren" fountain-match-paren
     ((0 nil keep)))
    ("trans" fountain-match-trans
     ((0 nil keep)
      (1 fountain-comment t t)))
    ("centered" ,fountain-centered-regexp
     ((0 nil)
      (1 fountain-comment t)
      (3 fountain-comment t)))
    ("section" ,fountain-section-regexp
     ((0 nil)
      (1 fountain-comment t)))
    ("synopsis" ,fountain-synopsis-regexp
     ((0 nil)
      (1 fountain-comment t)))
    ("page-break" ,fountain-page-break-regexp
     ((0 fountain-page-break)))
    ("metadata" fountain-match-metadata
     ((1 fountain-metadata-key nil t)
      (2 fountain-metadata-value nil t)
      (0 fountain-comment keep))))
  "List of face properties to use in creating Font Lock keywords.

Has the format ELEMENT, a string name, MATCHER, a regular
expression or search function, and SUBEXP, a list of: N, the
subexpression to match, FACE, the face to apply, and OVERRIDE, if
t, will allow overriding preexisting faces properties.")

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (let ((list fountain-font-lock-keywords-plist)
        (dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (f list keywords)
      (let* ((element (car f))
             (matcher (nth 1 f))
             (subexp (nth 2 f))
             ;; if we're using max decoration, use highlight faces
             (hl (if (= dec 3) "-highlight"))
             (align (intern (concat "fountain-align-" (car f))))
             ;; if we're using auto-align and the align var is bound,
             ;; set the align properties
             (align-props (if (and fountain-align-elements
                                   (boundp align))
                              `(line-prefix
                                (space :align-to ,align)
                                wrap-prefix
                                (space :align-to ,align))))
             face-props)
        (dolist (f subexp)
          (let* ((n (car f))
                 ;; if we're using no decoration, use nil
                 ;; if face is supplied, use that
                 ;; otherwise use the element string plus highlight
                 (face (cond ((= dec 1) nil)
                             ((nth 1 f))
                             ((intern (concat "fountain-" element hl)))))
                 ;; set the face override
                 (override (nth 2 f))
                 (lax (nth 3 f)))
            (setq face-props
                  (append face-props
                          `((,n '(face ,face ,@align-props)
                                ,override ,lax))))))
        (setq keywords
              (append keywords
                      (list (cons matcher face-props))))))))

(defun fountain-match-element (func limit)
  "If FUNC returns non-nil before LIMIT, return match data."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (if (funcall func)
          (setq match t))
      (forward-line 1))
    match))

;; these could probably be a single function?
(defun fountain-match-scene-heading (limit)
  "Call `fountain-match-element' with `fountain-scene-heading-p'."
  (fountain-match-element 'fountain-scene-heading-p limit))

(defun fountain-match-character (limit)
  "Call `fountain-match-element' with `fountain-character-p'"
  (fountain-match-element 'fountain-character-p limit))

(defun fountain-match-paren (limit)
  "Call `fountain-match-element' with `fountain-paren-p'"
  (fountain-match-element 'fountain-paren-p limit))

(defun fountain-match-dialog (limit)
  "Call `fountain-match-element' with `fountain-dialog-p'"
  (fountain-match-element 'fountain-dialog-p limit))

(defun fountain-match-trans (limit)
  "Call `fountain-match-element' with `fountain-trans-p'"
  (fountain-match-element 'fountain-trans-p limit))

(defun fountain-match-metadata (limit)
  "Call `fountain-match-element' with `fountain-metadata-p'"
  (fountain-match-element 'fountain-metadata-p limit))

;;; Mode Map ===========================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-m") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "M-n") 'fountain-forward-scene)
    (define-key map (kbd "M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-M-n") 'fountain-forward-scene)
    (define-key map (kbd "C-M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-M-a") 'fountain-beginning-of-scene)
    (define-key map (kbd "C-M-e") 'fountain-end-of-scene)
    (define-key map (kbd "C-M-h") 'fountain-mark-scene)
    (define-key map (kbd "C-c C-c") 'fountain-continued-dialog-refresh)
    (define-key map (kbd "C-c C-z") 'fountain-insert-note)
    (define-key map (kbd "C-c C-a") 'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-e C-e") 'fountain-export-default)
    (define-key map (kbd "C-c C-e h") 'fountain-export-buffer-to-html)
    (define-key map (kbd "C-c C-e p") 'fountain-export-buffer-to-pdf-via-html)
    (define-key map (kbd "C-c C-x i") 'fountain-insert-metadata)
    map)
  "Mode map for `fountain-mode'.")

;;; Menu ===============================================================

(easy-menu-define fountain-mode-menu fountain-mode-map
  "Menu for `fountain-mode'."
  '("Fountain"
    ["Insert Metadata" fountain-insert-metadata]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    ["Add/Remove Continued Dialog" fountain-continued-dialog-refresh]
    "---"
    ("Syntax Highlighting"
     ["None" (fountain-set-font-lock-decoration 1)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 1)]
     ["Default" (fountain-set-font-lock-decoration 2)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 2)]
     ["Maximum" (fountain-set-font-lock-decoration 3)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 3)]
     "---"
     ["Save for Future Sessions" fountain-save-font-lock-decoration])
    "---"
    ["Display Elements Auto-Aligned"
     fountain-toggle-align-elements
     :style toggle
     :selected fountain-align-elements]
    ["Add Continued Dialog"
     fountain-toggle-add-continued-dialog
     :style toggle
     :selected fountain-add-continued-dialog]
    ["Switch Default Comment Syntax"
     fountain-toggle-comment-syntax
     :style toggle
     :selected fountain-switch-comment-syntax]
    "---"
    ("Export"
     ["Default" fountain-export-default]
     "---"
     ["Buffer to HTML" fountain-export-buffer-to-html]
     ["Buffer to PDF via HTML" fountain-export-buffer-to-pdf-via-html]
     "---"
     ["Bold Scene Headings"
      fountain-toggle-export-bold-scene-headings
      :style toggle
      :selected fountain-export-bold-scene-headings]
     ["Underline Scene Headings"
      fountain-toggle-export-underline-scene-headings
      :style toggle
      :selected fountain-export-underline-scene-headings]
     ["Double-Space Scene Headings"
      fountain-toggle-export-double-space-scene-headings
      :style toggle
      :selected fountain-export-double-space-scene-headings]
     "---"
     ["Customize Export Group" (customize-group 'fountain-export)])
    "---"
    ("Go To"
     ["Next Scene Heading" fountain-forward-scene]
     ["Previous Scene Heading" fountain-backward-scene])
    "---"
    ["Customize Mode" (customize-group 'fountain)]
    ["Customize Faces" (customize-group 'fountain-faces)]))

;;; Syntax Table =======================================================

(defvar fountain-mode-syntax-table
  (let ((syntax (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" syntax)
    (modify-syntax-entry ?* ". 23b" syntax)
    (modify-syntax-entry ?\n ">" syntax)
    syntax)
  "Syntax table for `fountain-mode'.")

;;; Mode Definition ====================================================

;;;###autoload
(define-derived-mode fountain-mode text-mode "Fountain"
  "Major mode for screenwriting in Fountain markup."
  :group 'fountain
  (set (make-local-variable 'fountain-metadata)
       (fountain-read-metadata))
  (set (make-local-variable 'comment-start)
       (if fountain-switch-comment-syntax "//" "/*"))
  (set (make-local-variable 'comment-end)
       (if fountain-switch-comment-syntax "" "*/"))
  (set (make-local-variable 'font-lock-comment-face)
       'fountain-comment)
  (setq font-lock-defaults '((fountain-create-font-lock-keywords)
                             nil t))
  (setq font-lock-extra-managed-props '(line-prefix wrap-prefix))
  (add-hook 'font-lock-extend-region-functions
            'fountain-font-lock-extend-region t t)
  (add-hook 'after-save-hook
            'fountain-read-metadata))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
