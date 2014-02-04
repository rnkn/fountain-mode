;;; fountain-mode.el --- Major mode for editing Fountain-formatted text files

;; Author: Paul Rankin <paul@tilk.co>
;; Version: 0.9.0
;; Keywords: wp
;; URL: http://github.com/rnkn/fountain-mode/

;; This file is not part of GNU Emacs.

;; Copyright (C) 2014 Paul Rankin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see [http://www.gnu.org/licenses/].

;;; Commentary:

;; Fountain Mode is a major mode for GNU Emacs for editing text files in
;; Fountain markup format, a simple markup syntax for writing, editing
;; and sharing screenplays in plain text. Fountain Mode is free
;; software, licensed under the GNU GPL version 3.

;; For more information on Fountain markup format, see
;; [http://fountain.io]

;;; Code:

;;; Group ======================================================================

(defgroup fountain ()
  "Major mode for editing Fountain-formatted text files."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "http://github.com/rnkn/fountain-mode/"))

;;; Customizable Options =======================================================

(defcustom fountain-metadata-template
  "Title: 
Credit: 
Author: 
Draft date: 
Contact: 
"
  "Metadata template inserted at the beginning of buffer."
  :type 'string
  :group 'fountain)

(defcustom fountain-sceneheading-prefix-list
  '("int." "ext." "i/e." "est.")
  "List of scene heading prefixes (case insensitive).
The default list requires that each scene heading prefix be appended
with a dot, like so:

INT. HOUSE - DAY

If you prefer not to append a dot to your scene heading prefixes, you
can add \"int\", \"ext\", etc. here."
  :type '(repeat (string :tag "Prefix"))
  :group 'fountain)

(defcustom fountain-trans-list
  '("FADE IN:" "TO:" "FADE OUT" "TO BLACK")
  "List of transition endings (case sensitive).
This list is used to match the endings of transitions,
e.g. \"TO:\" will match both the following:

CUT TO:

DISSOLVE TO:"
  :type '(repeat (string :tag "Transition"))
  :group 'fountain)

(defcustom fountain-align-column-character 20
  "Column integer to which character should be aligned."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-column-dialogue 10
  "Column integer to which dialogue should be aligned."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-column-paren 15
  "Column integer to which parenthetical should be aligned."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-column-trans 45
  "Column integer to which transitions should be aligned."
  :type 'integer
  :group 'fountain)

(defcustom fountain-indent-elements t
  "If non-nil, elements will be displayed indented.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-dot-sceneheading-hierarchy t
  "If non-nil, forced scene headings will take a lower hierarchy.
When writing, it is usually preferable to treat forced scene headings
as constituents of the larger scene. If you prefer to treat
forced scene headings like regular scene headings, set this to nil."
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

;;; Element Regular Expressions ================================================

(defconst fountain-line-empty-regexp
  (rx line-start
      (zero-or-one " ")
      line-end)
  "Regular expression for matching an empty line.")

(defvar fountain-sceneheading-regexp
  (rx line-start
      (eval `(or ,@fountain-sceneheading-prefix-list))
      (one-or-more " ")
      (zero-or-more not-newline))
  "Regular expression for matching scene headings.
Requires `fountain-sceneheading-p' for preceding and succeeding blank
lines.")

(defconst fountain-dot-sceneheading-regexp
  (rx line-start
      (group "." word-start)
      (group (zero-or-more not-newline)))
  "Regular expression for matching forced scene headings.")

(defconst fountain-character-regexp
  (rx (group (zero-or-more blank))
      (group (one-or-more (not (any lower "<>\\\n"))))
      (group (zero-or-more blank))
      line-end)
  "Regular expression for matching characters.")

(defvar fountain-paren-regexp
  (rx line-start
      (zero-or-more blank)
      "(" (zero-or-more not-newline) ")"
      (zero-or-more blank)
      line-end)
  "Regular expression for matching parentheticals.")

(defvar fountain-trans-regexp
  (rx (or (and line-start
               (zero-or-more blank)
               ">"
               (group (zero-or-more (any upper blank ":"))
                      line-end))
          (and line-start
               (zero-or-more (any upper blank))
               (eval `(or ,@fountain-trans-list))
               (zero-or-one blank)
               line-end)))
  "Regular expression for matching transitions.
Requires `fountain-trans-p' for preceding and succeeding blank
lines.")

(defconst fountain-page-break-regexp
  (rx line-start
      (group (zero-or-more blank))
      (group (>= 3 "="))
      (group (zero-or-more not-newline)))
  "Regular expression for matching page breaks.")

(defconst fountain-note-regexp
  (rx (group "[[")
      (group (zero-or-more not-newline (zero-or-one "\n")))
      (group "]]"))
  "Regular expression for matching comments.")

(defconst fountain-section-regexp
  (rx line-start
      (group (repeat 1 5 "#") (not (any "#")))
      (group (zero-or-more not-newline)))
  "Regular expression for matching sections.")

(defconst fountain-synopsis-regexp
  (rx line-start
      (group "=" (not (any "=")))
      (group (zero-or-more not-newline)))
  "Regular expression for matching synopses.")

;;; Faces ======================================================================

(require 'font-lock)

(defgroup fountain-faces nil
  "Faces used in Fountain Mode"
  :group 'fountain)

(defvar fountain-sceneheading-face 'fountain-sceneheading-face
  "Face name to use for scene headings.")

(defvar fountain-dot-sceneheading-face
  (if fountain-dot-sceneheading-hierarchy
      'fountain-dot-sceneheading-face
    'fountain-sceneheading-face)
  "Face name to use for forced scene headings.")

(defvar fountain-note-face 'fountain-note-face
  "Face name to use for notes.")

(defvar fountain-section-face 'fountain-section-face
  "Face name to use for sections.")

(defvar fountain-synopsis-face 'fountain-synopsis-face
  "Face name to use for synopses.")

(defface fountain-sceneheading-face
  '((t (:weight bold :underline t)))
  "Face for scene headings."
  :group 'fountain-faces)

(defface fountain-dot-sceneheading-face
  '((t (:weight bold)))
  "Face for forced scene headings."
  :group 'fountain-faces)

(defface fountain-nonprinting-face
  '((t (:foreground "dim gray")))
  "Face for comments."
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

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords
  `((,fountain-sceneheading-regexp . fountain-sceneheading-face)
    (,fountain-dot-sceneheading-regexp . fountain-dot-sceneheading-face)
    (,fountain-section-regexp . fountain-section-face)
    (,fountain-synopsis-regexp . fountain-synopsis-face)
    (,fountain-note-regexp . fountain-note-face))
  "Font lock highlighting keywords.")

;;; Functions ==================================================================

(defun fountain-get-line ()
  "Return the line at point as a string."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun fountain-get-paragraph-bounds ()
  "Return the beginning and end points of paragraph at point."
  (let ((paragraph-beginning
         (save-excursion (forward-paragraph -1) (point)))
        (paragraph-end
         (save-excursion (forward-paragraph 1) (point))))
    (cons paragraph-beginning paragraph-end)))

(defun fountain-trim-whitespace (str)
  "Trim the leading and trailing whitespace of STR."
  (setq str (mapconcat 'identity (split-string str) " ")))

(defun fountain-line-upper-p ()
  "Return non-nil if line at point is uppercase."
  (let ((str (fountain-get-line)))
    (string= (upcase str) str)))

(defun fountain-line-empty-p ()
  "Return non-nil if line at point is a newline or single space."
  (save-excursion
    (forward-line 0)
    (looking-at-p fountain-line-empty-regexp)))

(defun fountain-section-p ()
  "Return non-nil if line at point is a section heading."
  (save-excursion
    (forward-line 0)
    (looking-at-p fountain-section-regexp)))

(defun fountain-synopsis-p ()
  "Return non-nil if line at point is a synopsis."
  (save-excursion
    (forward-line 0)
    (looking-at-p fountain-synopsis-regexp)))

(defun fountain-boneyard-p ()
  "Return non-nil if line at point is within boneyard."
  (comment-only-p (line-beginning-position) (line-end-position)))

(defun fountain-blank-p ()
  "Return non-nil if line at point is considered blank.
A line is blank if it is empty, or consists of a comment,
section, synopsis or is within a boneyard."
  (cond ((fountain-line-empty-p))
        ((fountain-boneyard-p))
        ((fountain-section-p))
        ((fountain-synopsis-p))
        ((fountain-note-p))))

(defun fountain-sceneheading-p ()
  "Return non-nil if line at point is a scene heading."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (or (bobp)
               (save-excursion
                 (forward-line -1)
                 (fountain-blank-p)))
           (save-excursion
             (forward-line 1)
             (or (eobp)
                 (fountain-blank-p)))
           (looking-at-p fountain-sceneheading-regexp)))))

(defun fountain-dot-sceneheading-p ()
  "Return non-nil if line at point is a forced scene heading."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (or (bobp)
               (save-excursion
                 (forward-line -1)
                 (fountain-blank-p)))
           (save-excursion
             (forward-line 1)
             (or (eobp)
                 (fountain-blank-p)))
           (looking-at-p fountain-dot-sceneheading-regexp)))))

(defun fountain-character-p ()
  "Return non-nil if line at point is a character name."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (let ((case-fold-search nil))
             (looking-at-p fountain-character-regexp))
           (or (bobp)
               (save-excursion
                 (forward-line -1)
                 (fountain-line-empty-p)))
           (save-excursion
             (forward-line 1)
             (unless (eobp)
               (not (fountain-blank-p))))))))

(defun fountain-dialogue-p ()
  "Return non-nil if line at point is dialogue."
  (unless (or (fountain-line-empty-p)
              (fountain-paren-p))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (unless (bobp)
          (forward-line -1)
          (or (fountain-character-p)
              (fountain-paren-p)
              (fountain-dialogue-p)))))))

(defun fountain-paren-p ()
  "Return non-nil if line at point is a paranthetical."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at-p fountain-paren-regexp)
           (forward-line -1)
           (unless (bobp)
             (or (fountain-character-p)
                 (fountain-dialogue-p)))))))

(defun fountain-trans-p ()
  "Return non-nil if line at point is a transition."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (let ((case-fold-search nil))
             (looking-at-p fountain-trans-regexp))
           (save-excursion
             (forward-line -1)
             (or (bobp)
                 (fountain-blank-p)))
           (save-excursion
             (forward-line 1)
             (or (eobp)
                 (fountain-blank-p)))))))

(defun fountain-note-p ()
  "Return non-nil if line at point is within a note."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (let* ((marker (point))
             (paragraph-end (cdr (fountain-get-paragraph-bounds)))
             (paragraph-beginning (car (fountain-get-paragraph-bounds)))
             (end (search-forward "]]" paragraph-end t))
             (start (search-backward "[[" paragraph-beginning t)))
        (unless (or (null start)
                    (null end))
          (and (goto-char end)
               (looking-at-p (rx (zero-or-more blank) line-end))
               (goto-char start)
               (forward-line 0)
               (looking-at-p fountain-note-regexp)
               (>= marker start)
               (<= marker end)))))))

(defun fountain-get-character ()
  "Return character (line at point must be character)."
  (fountain-trim-whitespace (car (split-string (fountain-get-line) "("))))

(defun fountain-get-prior-character ()
  "Return the prior character within the scene, nil otherwise."
  (save-excursion
    (save-restriction
      (forward-line -1)
      (while (not (or (fountain-character-p)
                      (fountain-slugline-p)
                      (bobp)))
        (forward-line -1))
      (when (fountain-character-p)
        (fountain-get-character)))))

(defun fountain-same-prior-character ()
  "Return non-nil if character at point is identical to prior character."
  (equal (fountain-get-character) (fountain-get-prior-character)))

(defun fountain-indent-add (column)
  "Add indentation properties to line at point."
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'line-prefix `(space :align-to ,column))
    (put-text-property (line-beginning-position) (line-end-position)
                       'wrap-prefix `(space :align-to ,column))))

(defun fountain-indent-refresh ()
  "Refresh indentation properties at point."
  (cond ((fountain-character-p)
         (fountain-indent-add fountain-align-column-character))
        ((fountain-paren-p)
         (fountain-indent-add fountain-align-column-paren))
        ((fountain-dialogue-p)
         (fountain-indent-add fountain-align-column-dialogue))
        ((fountain-trans-p)
         (fountain-indent-add fountain-align-column-trans))
        ((fountain-indent-add 0))))

(defun fountain-format-refresh (start end length)
  "Refresh format between START and END."
  (save-excursion
    (save-restriction
      (let ((start
             (progn (goto-char start)
                    (car (fountain-get-paragraph-bounds))))
            (end
             (progn (goto-char end)
                    (cdr (fountain-get-paragraph-bounds)))))
        (goto-char start)
        (while (< (point) end)
          (if fountain-indent-elements
              (fountain-indent-refresh)
            (fountain-indent-add 0))
          (forward-line 1))
        (font-lock-fontify-region start end)))))

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

(defun fountain-next-comment ()
  "Find the next comment."
  (interactive)
  (search-forward comment-start))

(defun fountain-note-dwim (&optional arg)
  "Insert a note. If prefixed with ARG, also insert a UUID."
  (interactive "P")
    (let ((comment-start "[[")
          (comment-end "]]"))
      (comment-dwim nil)
      (if arg
          (let ((str (downcase (shell-command-to-string "uuidgen"))))
            (insert (car (split-string str "-")))))))

(defun fountain-metadata-insert ()
  "Insert the metadata template at the beginning of file."
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (insert fountain-metadata-template "\n\n"))))

;;; Mode Map ===================================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "C-c C-z") 'fountain-note-dwim)
    (define-key map (kbd "C-c C-x i") 'fountain-metadata-insert)
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
  (set (make-local-variable 'font-lock-comment-face)
       'fountain-nonprinting-face)
  (setq font-lock-defaults '(fountain-font-lock-keywords nil t))
  (fountain-format-refresh (point-min) (point-max) nil)
  (add-hook 'after-change-functions 'fountain-format-refresh nil t))

(provide 'fountain-mode)

;;; fountain-mode.el ends here
