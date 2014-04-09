;;; fountain-mode.el --- Major mode for screenwriting in Fountain plaintext markup

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 0.11.0
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

;;; Group ======================================================================

(defgroup fountain ()
  "Major mode for editing Fountain-formatted text files."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "http://github.com/rnkn/fountain-mode/"))

;;; Customizable Options =======================================================

(defcustom fountain-mode-hook
  '(turn-on-visual-line-mode)
  "Mode hook for Fountain Mode, run after the mode is turned on."
  :type 'hook
  :group 'fountain)

(defcustom fountain-metadata-template
  "title:
credit: written by
author: ${fullname}
draft date: ${longtime}
contact: ${email}"
  "Metadata template to be inserted at beginning of buffer.
See `fountain-format-template'."
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
  '("TO:" "WITH:" "FADE IN:" "FADE OUT" "TO BLACK")
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

(defcustom fountain-indent-character-col 20
  "Column integer to which character should be indented.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-dialog-col 10
  "Column integer to which dialog should be indented.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-paren-col 15
  "Column integer to which parenthetical should be indented.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-trans-col 45
  "Column integer to which transitions should be indented.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-centered-col nil
  "If integer, column to which centered text should be indented.
If nil, indent to center of `window-body-width'.

This option does not affect file contents."
  :type '(choice (const :tag "Center" nil) integer)
  :group 'fountain)

(defcustom fountain-indent-elements t
  "If non-nil, elements will be displayed indented.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-forced-scene-heading-equal nil
  "If non-nil, forced scene headings will be treated as equal.

It is usually preferable to treat forced scene headings as
constituents of the larger scene. If you prefer to treat forced
scene headings as equal to regular scene headings, set this to
non-nil."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-switch-comment-syntax nil
  "If non-nil, use \"//\" as default comment syntax.

Fountain Mode supports two syntax for commenting (boneyard):

/* this text is a comment */

// this text is
// also a comment

The default is the former; if you prefer the latter, set this
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
  "Template for inserting notes. See `fountain-format-template'.

The default \"${time} - ${fullname}: \" will insert something
similar to:

\[\[01/20/14 - Alan Smithee: \]\]"
  :type 'string
  :group 'fountain)

(defcustom fountain-uuid-func
  '(lambda () (shell-command-to-string "uuidgen"))
  "Function for generating a UUID.
Each option has its own requirements:

  uuidgen       command line tool \"uuidgen\"
  uuid.el       Emacs package \"uuid.el\""
  :tag "Fountain UUID Function"
  :type '(radio (function
                 :tag "uuidgen"
                 '(lambda () (shell-command-to-string "uuidgen")))
                (function
                 :tag "uuid.el"
                 uuid-string)
                (function
                 :tag "Custom"))
  :group 'fountain)

;;; Element Regular Expressions ================================================

(defconst fountain-blank-regexp
  "\\`\\|^ ?$\\|\\'"
  "Regular expression for matching an empty line.")

(defconst fountain-comment-regexp
  "//.*\\|/\\*[^*]*\\*/"
  "Regular expression for matching comments.")

(defconst fountain-scene-heading-regexp
  (concat "^\\("
          (regexp-opt fountain-scene-heading-prefix-list)
          "[\\.\s\t]+\\)\\(.*\\)")
  "Regular expression for matching scene headings.
Requires `fountain-scene-heading-p' for preceding and succeeding
blank lines.")

(defconst fountain-forced-scene-heading-regexp
  "^\\.\\<\\(.*\\)"
  "Regular expression for matching forced scene headings.
Requires `fountain-forced-scene-heading-p' for preceding and
succeeding blank lines.")

(defconst fountain-paren-regexp
  "^[\s\t]*([^)]*)[\s\t]*$"
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
  "^#\\{1,5\\}[^#].*"
  "Regular expression for matching sections.")

(defconst fountain-synopsis-regexp
  "^=[^=].*"
  "Regular expression for matching synopses.")

(defconst fountain-trans-regexp
  (concat "^[\s\t]*>[^<\n]*$\\|^[[:upper:]\s]*"
          (regexp-opt fountain-trans-list)
          "\\.?$")
  "Regular expression for matching transitions.")

(defconst fountain-centered-regexp
  "^[\s\t]*\\(>.*<\\)[\s\t]*$"
  "Regular expression for matching centered text.")

;;; Faces ======================================================================

(defgroup fountain-faces nil
  "Faces used in Fountain Mode"
  :group 'fountain)

(defface fountain-scene-heading
  '((t (:weight bold :underline t)))
  "Default face for scene headings."
  :group 'fountain-faces)

(defface fountain-scene-heading-highlight
  '((t (:weight bold :underline t
                :inherit font-lock-function-name-face)))
  "Additional highlighting face for scene headings."
  :group 'fountain-faces)

(defface fountain-forced-scene-heading
  '((t (:weight bold)))
  "Default face for forced scene headings.
Only customize this if `fountain-forced-scene-heading-equal' is
nil."
  :group 'fountain-faces)

(defface fountain-forced-scene-heading-highlight
  '((t (:weight bold :inherit font-lock-function-name-face)))
  "Additional highlighting face for forced scene headings.
Only customize this if `fountain-forced-scene-heading-equal' is
nil."
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

;;; Thing Definitions ==========================================================

(put 'scene 'forward-op 'fountain-forward-scene)

;;; Functions ==================================================================

(defun fountain-get-line ()
  "Return the line at point as a string."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun fountain-get-block-bounds ()
  "Return the beginning and end points of block at point."
  (let ((block-beginning
         (save-excursion
           (re-search-backward fountain-blank-regexp
                               (- (point) 10000) t)))
        (block-end
         (save-excursion
           (re-search-forward fountain-blank-regexp
                              (+ (point) 10000) t))))
    (cons block-beginning block-end)))

(defun fountain-strip-comments (start end)
  "Strip comments between START and END and return string."
  (let ((start
         (save-excursion
           (goto-char start)
           ;; Using thing-at-point-looking-at is very slow, better to
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
    (forward-line 0)
    ;; Do not modify match-data
    (looking-at-p fountain-blank-regexp)))

(defun fountain-section-p ()
  "Return non-nil if point is at a section, nil otherwise."
  (save-excursion
    (forward-line 0)
    (looking-at fountain-section-regexp)))

(defun fountain-synopsis-p ()
  "Return non-nil if point is at a synopsis, nil otherwise."
  (save-excursion
    (forward-line 0)
    (looking-at fountain-synopsis-regexp)))

(defun fountain-note-p ()
  "Return non-nil if point is at a note, nil otherwise."
  (thing-at-point-looking-at fountain-note-regexp))

(defun fountain-comment-p ()
  "Return non-nil if point is at a comment, nil otherwise."
  ;; Problems with comment-only-p picking up blank lines as comments.
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
      (and (or (and fountain-forced-scene-heading-equal
                    (looking-at
                     fountain-forced-scene-heading-regexp))
               (looking-at fountain-scene-heading-regexp))
           (save-match-data
             (forward-line -1)
             (fountain-invisible-p))))))

(defun fountain-forced-scene-heading-p ()
  "Return non-nil if point is at a forced scene heading, nil otherwise.
This function is ignored if `fountain-forced-scene-heading-equal'
is non-nil."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (null fountain-forced-scene-heading-equal)
           (looking-at
            fountain-forced-scene-heading-regexp)
           (save-match-data
             (forward-line -1)
             (fountain-invisible-p))))))

(defun fountain-get-character ()
  "Return character if point is at a character, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (when (s-present?
             (fountain-strip-comments
              (line-beginning-position) (line-end-position)))
        (forward-line 0)
        (unless (looking-at-p fountain-scene-heading-regexp)
          (let* ((s (fountain-strip-comments
                     (line-beginning-position) (line-end-position)))
                 (s (s-presence
                     (s-trim (car (s-slice-at "(\\|\\^" s))))))
            (when (and s
                       (or (s-uppercase? s)
                           (s-starts-with? "@" s))
                       (save-excursion
                         (forward-line -1)
                         (fountain-invisible-p))
                       (save-excursion
                         (forward-line 1)
                         (unless (eobp)
                           (null (fountain-invisible-p)))))
              s)))))))

(defun fountain-dialog-p ()
  "Return non-nil if line at point is dialog."
  (unless (or (fountain-blank-p)
              (fountain-paren-p)
              (fountain-note-p))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (looking-at ".*")
        (save-match-data
          (unless (bobp)
            (forward-line -1)
            (or (fountain-get-character)
                (fountain-paren-p)
                (fountain-dialog-p))))))))

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
               (or (fountain-get-character)
                   (fountain-dialog-p))))))))

(defun fountain-trans-p ()
  "Return non-nil if line at point is a transition."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (let (case-fold-search)
             (looking-at fountain-trans-regexp))
           (save-match-data
             (save-excursion
               (forward-line -1)
               (or (bobp)
                   (fountain-invisible-p)))
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-invisible-p))))))))

(defun fountain-format-template (template)
  "Format TEMPLATE according to the following list.

To include an item in a template you must use the full \"${foo}\"
syntax.

  ${longtime}   Long date format (defined in `fountain-long-time-format')
  ${time}       Short date format (defined in `fountain-short-time-format')
  ${fullname}   User full name (defined in `user-full-name')
  ${nick}       User first name (defined in `user-login-name')
  ${email}      User email (defined in `user-mail-address')
  ${uuid}       Insert a UUID (defined in `fountain-uuid-func')"
  (s-format template 'aget
            `(("longtime" . ,(format-time-string fountain-long-time-format))
              ("time" . ,(format-time-string fountain-short-time-format))
              ("fullname" . ,user-full-name)
              ("nick" . ,(capitalize user-login-name))
              ("email" . ,user-mail-address)
              ("uuid" . ,(fountain-uuid)))))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID."
  (let ((s (downcase (funcall fountain-uuid-func))))
    (car (split-string s "-"))))

(defun fountain-get-previous-character (n)
  "Return Nth previous character within scene, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (while (> n 0)
        (unless (fountain-scene-heading-p)
          (forward-line -1))
        (while (null (or (fountain-get-character)
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

(defun fountain-indent-add (column)
  "Add indentation properties to COLUMN at point."
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'line-prefix `(space :align-to ,column))
    (put-text-property (line-beginning-position) (line-end-position)
                       'wrap-prefix `(space :align-to ,column))))

(defun fountain-indent-refresh (start end)
  "Refresh indentation properties between START and END.
This function is called by `jit-lock-fontify-now'."
  (let ((start
         (progn
           (goto-char start)
           (if (car (fountain-get-block-bounds))
               (car (fountain-get-block-bounds))
             (point))))
        (end
         (progn
           (goto-char end)
           (if (cdr (fountain-get-block-bounds))
               (cdr (fountain-get-block-bounds))
             (point)))))
    (goto-char start)
    (while (< (point) end)
      (if fountain-indent-elements
          (cond ((fountain-get-character)
                 (fountain-indent-add fountain-indent-character-col))
                ((fountain-paren-p)
                 (fountain-indent-add fountain-indent-paren-col))
                ((fountain-dialog-p)
                 (fountain-indent-add fountain-indent-dialog-col))
                ((fountain-trans-p)
                 (fountain-indent-add fountain-indent-trans-col))
                ((thing-at-point-looking-at fountain-centered-regexp)
                 (fountain-indent-add
                  (if fountain-indent-centered-col
                      fountain-indent-centered-col
                    (/ (- (window-body-width)
                          (length (fountain-get-line))) 2))))
                ((fountain-indent-add 0)))
        (fountain-indent-add 0))
      (forward-line 1))))

(defun fountain-clean-exit ()
  "Remove all indenting in buffer."
  (with-silent-modifications
    (save-restriction
      (widen)
      (remove-text-properties (point-min) (point-max)
                              '(line-prefix nil wrap-prefix nil)))))

(defun fountain-lock-extend-region ()
  "Extend region for fontification to text block."
  (eval-when-compile
    (defvar font-lock-beg)
    (defvar font-lock-end))
  (let ((start
         (save-excursion
           (goto-char font-lock-beg)
           (if (car (fountain-get-block-bounds))
               (car (fountain-get-block-bounds)))))
        (end
         (save-excursion
           (goto-char font-lock-end)
           (if (cdr (fountain-get-block-bounds))
               (cdr (fountain-get-block-bounds)))))
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

;;; Interaction ================================================================

(defun fountain-upcase-line ()
  "Upcase the line."
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
  (let ((p (if (< n 0) -1 1)))
    (while (/= n 0)
      (when (fountain-scene-heading-p)
        (forward-line p))
      (while (null (or (eq (point)
                           (buffer-end p))
                       (fountain-scene-heading-p)))
        (forward-line p))
      (setq n (- n p)))))

(defun fountain-backward-scene (&optional n)
  "Move backward N scene headings (foward if N is negative)."
  (interactive "^p")
  (fountain-forward-scene (- n)))

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
        (pop-to-mark-command)
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
      (progn
        (unless (fountain-blank-p)
          (re-search-forward fountain-blank-regexp))
        (unless (save-excursion
                  (forward-line 1)
                  (fountain-blank-p))
          (open-line 1))
        (comment-indent)
        (insert (fountain-format-template fountain-note-template))))))

(defun fountain-insert-metadata ()
  "Insert the metadata template at the beginning of file."
  (interactive)
  (widen)
  (goto-char (point-min))
  (save-excursion
    (insert (fountain-format-template fountain-metadata-template) "\n\n")))

(defun fountain-continued-dialog-refresh (&optional arg)
  "Add or remove continued dialog on successively speaking characters.

If `fountain-add-continued-dialog' is non-nil, add
`fountain-continued-dialog-string' on characters speaking in
succession, otherwise remove all occurences.

If prefixed with \\[universal-argument], act on whole buffer, or
if region is active, act on region, otherwise act on current
scene."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      ;; First expand the region.
      (let ((start
             (cond (arg (point-min))
                   ((use-region-p) (region-beginning))
                   ((car (bounds-of-thing-at-point 'scene)))))
            (end
             (cond (arg (point-max))
                   ((use-region-p) (region-end))
                   ((cdr (bounds-of-thing-at-point 'scene)))))
            (s (concat "(" fountain-continued-dialog-string ")")))
        ;; Delete all matches in region.
        (goto-char start)
        (while (re-search-forward s end t)
          (delete-region (match-beginning 0) (match-end 0)))
        ;; Add string where appropriate.
        (when fountain-add-continued-dialog
          (goto-char start)
          (while (< (point) end)
            (when (and (null (s-ends-with? s (fountain-get-line)))
                       (fountain-get-character)
                       (s-equals? (fountain-get-character)
                                  (fountain-get-previous-character 1)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" s)))
            (forward-line 1)))))))

(defun fountain-toggle-forced-scene-heading-equal ()
  "Toggle `fountain-forced-scene-heading-equal'"
  (interactive)
  (setq fountain-forced-scene-heading-equal
        (null fountain-forced-scene-heading-equal))
  (font-lock-fontify-buffer)
  (message "Forced scene headings are now treated as %s"
           (if fountain-forced-scene-heading-equal
               "equal" "non-equal")))

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

(defun fountain-toggle-indent-elements ()
  "Toggle `fountain-indent-elements'"
  (interactive)
  (setq fountain-indent-elements
        (null fountain-indent-elements))
  (jit-lock-refontify)
  (message "Elements are now displayed %s"
           (if fountain-indent-elements
               "indended" "non-indented")))

(defun fountain-toggle-add-continued-dialog ()
  "Toggle `fountain-add-continued-dialog'"
  (interactive)
  (setq fountain-add-continued-dialog
        (null fountain-add-continued-dialog))
  (fountain-continued-dialog-refresh)
  (message "Continued dialog is now %s"
           (if fountain-indent-elements
               "added" "removed")))

(defun fountain-set-font-lock-decoration (level)
  "Set `font-lock-maximum-decoration' for Fountain Mode to LEVEL."
  (interactive)
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
                 ((eq level 2) "minimal")
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

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords-1 nil
  "Font Lock keywords for no highlighting.")

(defvar fountain-font-lock-keywords-2
  `((fountain-match-scene-heading . 'fountain-scene-heading)
    (fountain-match-forced-scene-heading . 'fountain-forced-scene-heading)
    (fountain-match-dialog . 'fountain-dialog)
    (fountain-match-trans . 'fountain-trans)
    (,fountain-section-regexp . 'fountain-section-highlight)
    (,fountain-synopsis-regexp . 'fountain-synopsis-highlight)
    (,fountain-note-regexp . 'fountain-note-highlight))
  "Font Lock keywords for minimal highlighting.")

(defvar fountain-font-lock-keywords-3
  `((fountain-match-scene-heading . 'fountain-scene-heading-highlight)
    (fountain-match-forced-scene-heading . 'fountain-forced-scene-heading-highlight)
    (fountain-match-dialog . 'fountain-dialog-highlight)
    (fountain-match-trans . 'fountain-trans-highlight)
    (,fountain-section-regexp . 'fountain-section-highlight)
    (,fountain-synopsis-regexp . 'fountain-synopsis-highlight)
    (,fountain-note-regexp . 'fountain-note-highlight))
  "Font Lock keywords for maximum highlighting.")

(defvaralias 'fountain-font-lock-keywords-default
  'fountain-font-lock-keywords-2
  "Default Font Lock keywords.")

(defun fountain-match-element (func limit)
  "If FUNC returns non-nil before LIMIT, return match data."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (when (funcall func)
        (setq match t))
      (forward-line 1))
    match))

(defun fountain-match-scene-heading (limit)
  "Call `fountain-match-element' with `fountain-scene-heading-p'."
  (fountain-match-element 'fountain-scene-heading-p limit))

(defun fountain-match-forced-scene-heading (limit)
  "Call `fountain-match-element' with `fountain-forced-scene-heading-p'."
  (fountain-match-element 'fountain-forced-scene-heading-p limit))

(defun fountain-match-dialog (limit)
  "Call `fountain-match-element' with `fountain-dialog-p'"
  (fountain-match-element 'fountain-dialog-p limit))

(defun fountain-match-trans (limit)
  "Call `fountain-match-element' with `fountain-trans-p'"
  (fountain-match-element 'fountain-trans-p limit))

;;; Mode Map ===================================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "M-n") 'fountain-forward-scene)
    (define-key map (kbd "M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-c C-c") 'fountain-continued-dialog-refresh)
    (define-key map (kbd "C-c C-z") 'fountain-insert-note)
    (define-key map (kbd "C-c C-a") 'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-x i") 'fountain-insert-metadata)
    map)
  "Mode map for `fountain-mode'.")

;;; Menu =======================================================================

(easy-menu-define fountain-mode-menu fountain-mode-map
  "Menu for Fountain Mode."
  '("Fountain"
    ["Insert Metadata" fountain-insert-metadata]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    "---"
    ["Add/Remove Continued Dialog" fountain-continued-dialog-refresh]
    "---"
    ("Syntax Highlighting"
     ["None" (fountain-set-font-lock-decoration 1)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 1)]
     ["Minimal" (fountain-set-font-lock-decoration 2)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 2)]
     ["Maximum" (fountain-set-font-lock-decoration 3)
      :style radio
      :selected (eq (fountain-get-font-lock-decoration) 3)]
     "---"
     ["Save for Future Sessions" fountain-save-font-lock-decoration])
    "---"
    ["Display Elements Indented"
     fountain-toggle-indent-elements
     :style toggle
     :selected fountain-indent-elements]
    ["Add Continued Dialog"
     fountain-toggle-add-continued-dialog
     :style toggle
     :selected fountain-add-continued-dialog]
    ["Treat Forced Scene Headings as Equal"
     fountain-toggle-forced-scene-heading-equal
     :style toggle
     :selected fountain-forced-scene-heading-equal]
    ["Switch Default Comment Syntax"
     fountain-toggle-comment-syntax
     :style toggle
     :selected fountain-switch-comment-syntax]
    "---"
    ("Go To"
     ["Next Scene Heading" fountain-forward-scene]
     ["Previous Scene Heading" fountain-backward-scene])
    "---"
    ["Customize" customize-mode]
    ["Customize Faces" (customize-group 'fountain-faces)]))

;;; Syntax Table ===============================================================

(defvar fountain-mode-syntax-table
  (let ((syntax (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" syntax)
    (modify-syntax-entry ?* ". 23b" syntax)
    (modify-syntax-entry ?\n ">" syntax)
    syntax)
  "Syntax table for `fountain-mode'.")

;;; Mode Definition ============================================================

;;;###autoload
(define-derived-mode fountain-mode text-mode "Fountain"
  "Major mode for editing Fountain-formatted text files.
For more information on the Fountain markup format, visit
<http://fountain.io>."
  :group 'fountain
  (set (make-local-variable 'comment-start)
       (if fountain-switch-comment-syntax "//" "/*"))
  (set (make-local-variable 'comment-end)
       (if fountain-switch-comment-syntax "" "*/"))
  (set (make-local-variable 'font-lock-comment-face) 'shadow)
  (setq font-lock-defaults '((fountain-font-lock-keywords-default
                              fountain-font-lock-keywords-1
                              fountain-font-lock-keywords-2
                              fountain-font-lock-keywords-3) nil t))
  (jit-lock-register 'fountain-indent-refresh)
  (add-hook 'font-lock-extend-region-functions
            'fountain-lock-extend-region t t)
  (add-hook 'change-major-mode-hook
            'fountain-clean-exit nil t))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
