;;; fountain-mode.el --- Major mode for screenwriting in Fountain plaintext markup

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 0.10.3
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
contact: ${email}
"
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
  "Column integer to which character should be indented."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-dialogue-col 10
  "Column integer to which dialogue should be indented."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-paren-col 15
  "Column integer to which parenthetical should be indented."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-trans-col 45
  "Column integer to which transitions should be indented."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-centered-col 20
  "Column integer to which centered text should be indented."
  :type 'integer
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

/* this text is in a boneyard */

// this text is
// also in a boneyard

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

(defcustom fountain-uuid-command "uuidgen"
  "Shell command for generating a UUID."
  :type 'string
  :group 'fountain)

;;; Element Regular Expressions ================================================

(defconst fountain-blank-regexp
  "\\`\\|^ ?$\\|\\'"
  "Regular expression for matching an empty line.")

(defconst fountain-scene-heading-regexp
  (concat "^\\("
          (regexp-opt fountain-scene-heading-prefix-list)
          "[\\.\s\t]+\\)\\(.*\\)")
  "Regular expression for matching scene headings.
Requires `fountain-get-scene-heading' for preceding and succeeding
blank lines.")

(defconst fountain-forced-scene-heading-regexp
  "^\\.\\<\\(.*\\)"
  "Regular expression for matching forced scene headings.
Requires `fountain-get-forced-scene-heading' for preceding and
succeeding blank lines.")

(defconst fountain-paren-regexp
  "^[\s\t]*([^)]*)[\s\t]*$"
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or
dialogue.")

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

(defconst fountain-centered-regexp
  "^[\s\t]*\\(>.*<\\)[\s\t]*$"
  "Regular expression for matching centered text.")

;;; Faces ======================================================================

(defgroup fountain-faces nil
  "Faces used in Fountain Mode"
  :group 'fountain)

(defface fountain-scene-heading-face
  '((t (:weight bold :underline t)))
  "Face for scene headings."
  :group 'fountain-faces)

(defface fountain-forced-scene-heading-face
  '((t (:weight bold)))
  "Face for forced scene headings."
  :group 'fountain-faces)

(defface fountain-note-face
  '((t (:foreground "forest green")))
  "Face for notes.")

(defface fountain-section-face
  '((t (:foreground "dark red")))
  "Face for sections."
  :group 'fountain-faces)

(defface fountain-synopsis-face
  '((t (:foreground "dark cyan")))
  "Face for synopses."
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

(defun fountain-blank-p ()
  "Return non-nil if line at point is a newline or single space."
  (save-excursion
    (forward-line 0)
    (looking-at-p fountain-blank-regexp)))

(defun fountain-boneyard-p ()
  "Return non-nil if line at point is within boneyard."
  (comment-only-p (line-beginning-position) (line-end-position)))

(defun fountain-invisible-p ()
  "Return non-nil if line at point is invisible.
A line is invisible if it is blank, or consists of a comment,
section, synopsis or is within a boneyard."
  (or (fountain-blank-p)
      (fountain-boneyard-p)
      (save-excursion
        (forward-line 0)
        (looking-at-p fountain-section-regexp))
      (save-excursion
        (forward-line 0)
        (looking-at-p fountain-synopsis-regexp))
      (thing-at-point-looking-at fountain-note-regexp)))

(defun fountain-get-scene-heading ()
  "Return scene heading if matches point, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (when (and (or (and fountain-forced-scene-heading-equal
                          (looking-at
                           fountain-forced-scene-heading-regexp))
                     (looking-at fountain-scene-heading-regexp))
                 (save-excursion
                   (forward-line -1)
                   (fountain-invisible-p)))
        (buffer-substring-no-properties
         (match-beginning 0) (match-end 0))))))

(defun fountain-get-forced-scene-heading ()
  "Return forced scene heading if matches point, nil otherwise.
This function is ignored if `fountain-forced-scene-heading-equal'
is non-nil."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (when (and (null fountain-forced-scene-heading-equal)
                 (looking-at
                  fountain-forced-scene-heading-regexp)
                 (save-excursion
                   (forward-line -1)
                   (fountain-invisible-p)))
        (buffer-substring-no-properties
         (match-beginning 0) (match-end 0))))))

(defun fountain-get-character ()
  "Return character if matches line at point, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (let ((s
             (when (s-present? (fountain-get-line))
               (s-presence
                (s-trim (car (s-slice-at "(" (fountain-get-line))))))))
        (unless (looking-at-p fountain-scene-heading-regexp)
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
            s))))))

(defun fountain-dialogue-p ()
  "Return non-nil if line at point is dialogue."
  (unless (or (fountain-blank-p)
              (fountain-paren-p)
              (thing-at-point-looking-at fountain-note-regexp))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (unless (bobp)
          (forward-line -1)
          (or (fountain-get-character)
              (fountain-paren-p)
              (fountain-dialogue-p)))))))

(defun fountain-paren-p ()
  "Return non-nil if line at point is a paranthetical."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at-p fountain-paren-regexp)
           (unless (bobp)
             (forward-line -1)
             (or (fountain-get-character)
                 (fountain-dialogue-p)))))))

(defun fountain-trans-p ()
  "Return non-nil if line at point is a transition."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (unless (looking-at-p fountain-centered-regexp)
        (when (s-present? (fountain-get-line))
          (and (let ((s (s-trim (fountain-get-line))))
                 (or (s-starts-with? ">" s)
                     (s-matches?
                      (concat (regexp-opt fountain-trans-list) "\\.?$")
                      s)))
               (save-excursion
                 (forward-line -1)
                 (or (bobp)
                     (fountain-invisible-p)))
               (save-excursion
                 (forward-line 1)
                 (or (eobp)
                     (fountain-invisible-p)))))))))

(defun fountain-format-template (template)
  "Format TEMPLATE according to the following list.

  ${longtime}   Long date format (defined in `fountain-long-time-format')
  ${time}       Short date format (defined in `fountain-short-time-format')
  ${fullname}   User full name (defined in `user-full-name')
  ${nick}       User first name (defined in `user-login-name')
  ${email}      User email (defined in `user-mail-address')
  ${uuid}       Insert a UUID (defined in `fountain-uuid-command')"
  (s-format template 'aget
            `(("longtime" . ,(format-time-string fountain-long-time-format))
              ("time" . ,(format-time-string fountain-short-time-format))
              ("fullname" . ,user-full-name)
              ("nick" . ,(capitalize user-login-name))
              ("email" . ,user-mail-address)
              ("uuid" . ,(fountain-uuid)))))

(defun fountain-get-previous-character (n)
  "Return Nth previous character within scene, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (while (> n 0)
        (unless (fountain-get-scene-heading)
          (forward-line -1))
        (while (null (or (fountain-get-character)
                         (fountain-get-scene-heading)
                         (bobp)))
          (forward-line -1))
        (setq n (- n 1)))
      (fountain-get-character))))

(defun fountain-continued-dialog-refresh (start end)
  "Refresh continued dialog markers between START and END.

First, delete all matches of `fountain-continued-dialog-string'
between START and END, then, if `fountain-add-continued-dialog'
is non-nil, add `fountain-continued-dialog-string' on characters
speaking in succession."
  (let ((s (concat "(" fountain-continued-dialog-string ")")))
    (goto-char start)
    (while (re-search-forward s end t)
      (delete-region (match-beginning 0) (match-end 0)))
    (when fountain-add-continued-dialog
      (goto-char start)
      (while (< (point) end)
        (when (and (null (s-ends-with? s (fountain-get-line)))
                   (fountain-get-character)
                   (s-equals? (fountain-get-character)
                              (fountain-get-previous-character 1)))
          (re-search-forward "\s*$" (line-end-position) t)
          (replace-match (concat "\s" s)))
        (forward-line 1)))))

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
         (save-excursion
           (goto-char start)
           (if (car (fountain-get-block-bounds))
               (car (fountain-get-block-bounds))
             (point))))
        (end
         (save-excursion
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
                ((fountain-dialogue-p)
                 (fountain-indent-add fountain-indent-dialogue-col))
                ((fountain-trans-p)
                 (fountain-indent-add fountain-indent-trans-col))
                ((thing-at-point-looking-at fountain-centered-regexp)
                 (fountain-indent-add fountain-indent-centered-col))
                ((fountain-indent-add 0)))
        (fountain-indent-add 0))
      (forward-line 1))))

(defun fountain-indent-remove ()
  "Remove all indenting in buffer."
  (with-silent-modifications
    (save-restriction
      (widen)
      (remove-text-properties (point-min) (point-max)
                              '(line-prefix wrap-prefix)))))

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
     (when (fountain-get-scene-heading)
       (forward-line p))
     (while (null (or (eq (point)
                          (buffer-end p))
                      (fountain-get-scene-heading)))
       (forward-line p))
     (setq n (- n p)))))

(defun fountain-backward-scene (&optional n)
  "Move backward N scene headings (foward if N is negative)."
  (interactive "^p")
  (fountain-forward-scene (- n)))

(defun fountain-next-comment ()
  "Find the next comment."
  (interactive)
  (search-forward comment-start))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID."
  (let ((s (downcase (shell-command-to-string fountain-uuid-command))))
    (car (split-string s "-"))))

(defun fountain-insert-synopsis ()
  "Open line below current scene heading and insert synopsis."
  (interactive)
  (widen)
  (push-mark)
  (forward-line 0)
  (while (null (or (bobp)
                   (fountain-get-scene-heading)
                   (looking-at fountain-section-regexp)))
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
    (insert (fountain-format-template fountain-metadata-template) "\n")))

(defun fountain-format-apply (&optional arg)
  "Perform destructive formatting appropriately.

- When point is at scene heading, upcase scene heading.
- Add or remove `fountain-continued-dialog-string' appropriately.

If prefixed with \\[universal-argument], act on whole buffer, or
if region is active, act on region, otherwise act on current
scene."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((start
             (cond (arg (point-min))
                   ((use-region-p) (region-beginning))
                   ((car (bounds-of-thing-at-point 'scene)))))
            (end
             (cond (arg (point-max))
                   ((use-region-p) (region-end))
                   ((cdr (bounds-of-thing-at-point 'scene))))))
        (when (fountain-get-scene-heading)
          (upcase-region (line-beginning-position) (line-end-position)))
        (fountain-continued-dialog-refresh start end)))))

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords
  `((fountain-match-scene-heading . 'fountain-scene-heading-face)
    (fountain-match-forced-scene-heading . 'fountain-forced-scene-heading-face)
    (,fountain-section-regexp . 'fountain-section-face)
    (,fountain-synopsis-regexp . 'fountain-synopsis-face)
    (,fountain-note-regexp . 'fountain-note-face))
  "Font lock highlighting keywords.")

(defun fountain-match-line (func limit)
  "If FUNC matches within LIMIT set match data to line."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (when (funcall func)
        (set-match-data
         (list (set-marker (make-marker) (line-beginning-position))
               (set-marker (make-marker) (line-end-position))))
        (setq match t))
      (forward-line 1))
    match))

(defun fountain-match-scene-heading (limit)
  "Call `fountain-match-line' with `fountain-get-scene-heading'."
  (fountain-match-line 'fountain-get-scene-heading limit))

(defun fountain-match-forced-scene-heading (limit)
  "Call `fountain-match-line' with `fountain-get-forced-scene-heading'."
  (fountain-match-line 'fountain-get-forced-scene-heading limit))

;;; Mode Map ===================================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "M-n") 'fountain-forward-scene)
    (define-key map (kbd "M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-c C-c") 'fountain-format-apply)
    (define-key map (kbd "C-c C-z") 'fountain-insert-note)
    (define-key map (kbd "C-c C-a") 'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-x i") 'fountain-insert-metadata)
    map)
  "Mode map for `fountain-mode'.")

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
       (if fountain-switch-comment-syntax "" "/*"))
  (set (make-local-variable 'font-lock-comment-face) 'shadow)
  (setq font-lock-defaults '(fountain-font-lock-keywords nil t))
  (jit-lock-register 'fountain-indent-refresh)
  (add-hook 'font-lock-extend-region-functions
            'fountain-lock-extend-region t t)
  (add-hook 'change-major-mode-hook
            'fountain-indent-remove nil t))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
