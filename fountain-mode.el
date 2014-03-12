;;; fountain-mode.el --- Major mode for editing Fountain-formatted text files

;; Author: Paul Rankin <paul@tilk.co>
;; Version: 0.9.2
;; Keywords: wp
;; Package-Requires: ((s "1.9.0"))
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

(require 's)

;;; Group ======================================================================

(defgroup fountain ()
  "Major mode for editing Fountain-formatted text files."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "http://github.com/rnkn/fountain-mode/"))

;;; Customizable Options =======================================================

(defcustom fountain-metadata-template
  "title:
credit: written by
author: ${fullname}
draft date: ${longtime}
contact: ${email}
"
  "Metadata template. See `fountain-format-template'."
  :type 'string
  :group 'fountain)

(eval-and-compile
  (defcustom fountain-scene-heading-prefix-list
    '("int." "ext." "i/e." "est.")
    "List of scene heading prefixes (case insensitive).
The default list requires that each scene heading prefix be appended
with a dot, like so:

INT. HOUSE - DAY

If you prefer not to append a dot to your scene heading prefixes, you
can add \"int\", \"ext\", etc. here."
    :type '(repeat (string :tag "Prefix"))
    :group 'fountain))

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

(defcustom fountain-dot-scene-heading-hierarchy t
  "If non-nil, forced scene headings will take a lower hierarchy.
When writing, it is usually preferable to treat forced scene
headings as constituents of the larger scene. If you prefer to
treat forced scene headings like regular scene headings, set this
to nil."
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

(defcustom fountain-short-time-format
  "%x"
  "Format of date and time. See `format-time-string'."
  :type 'string
  :group 'fountain)

(defcustom fountain-long-time-format
  "%B %-e, %Y"
  "Format of date and time. See `format-time-string'."
  :type 'string
  :group 'fountain)

(defcustom fountain-note-template
  "${time} - ${fullname}: "
  "Template for inserting notes. See `fountain-format-template'.

The default (\"${time} - ${fullname}: \") will insert something
similar too:

\[\[ 01/20/14 - Alan Smithee:  \]\]"
  :type 'string
  :group 'fountain)

;;; Element Regular Expressions ================================================

(defconst fountain-blank-regexp
  (rx line-start
      (zero-or-one " ")
      line-end)
  "Regular expression for matching an empty line.")

(defvar fountain-paren-regexp
  (rx line-start
      (zero-or-more blank)
      "(" (zero-or-more not-newline) ")"
      (zero-or-more blank)
      line-end)
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or
dialogue.")

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

(defgroup fountain-faces nil
  "Faces used in Fountain Mode"
  :group 'fountain)

(defvar fountain-scene-heading-face 'fountain-scene-heading-face
  "Face name to use for scene headings.")

(defvar fountain-dot-scene-heading-face 'fountain-dot-scene-heading-face
  "Face name to use for forced scene headings.")

(defvar fountain-note-face 'fountain-note-face
  "Face name to use for notes.")

(defvar fountain-section-face 'fountain-section-face
  "Face name to use for sections.")

(defvar fountain-synopsis-face 'fountain-synopsis-face
  "Face name to use for synopses.")

(defface fountain-scene-heading-face
  '((t (:weight bold :underline t)))
  "Face for scene headings."
  :group 'fountain-faces)

(defface fountain-dot-scene-heading-face
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

(defun fountain-blank-p ()
  "Return non-nil if line at point is a newline or single space."
  (save-excursion
    (forward-line 0)
    (looking-at-p fountain-blank-regexp)))

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

(defun fountain-get-synopsis ()
  "Return synopsis if matches line at point, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (let ((s (s-presence (fountain-get-line))))
        (when (and s
                   (s-matches? fountain-synopsis-regexp s))
          s)))))

(defun fountain-boneyard-p ()
  "Return non-nil if line at point is within boneyard."
  (comment-only-p (line-beginning-position) (line-end-position)))

(defun fountain-invisible-p ()
  "Return non-nil if line at point is invisible.
A line is invisible if it is blank, or consists of a comment,
section, synopsis or is within a boneyard."
  (cond ((fountain-blank-p))
        ((fountain-boneyard-p))
        ((fountain-section-p))
        ((fountain-synopsis-p))
        ((fountain-note-p))))

(defun fountain-get-scene-heading ()
  "Return scene heading if matches line at point, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (let ((s (s-presence (fountain-get-line))))
        (when (and s
                   (or (s-matches?
                        (concat "^"
                                (regexp-opt fountain-scene-heading-prefix-list)
                                " ") s)
                       (when (null fountain-dot-scene-heading-hierarchy)
                         (s-matches?
                           "^\\.\\<" s)))
                   (or (bobp)
                       (save-excursion
                         (forward-line -1)
                         (fountain-invisible-p)))
                   (save-excursion
                     (forward-line 1)
                     (or (eobp)
                         (fountain-invisible-p))))
          s)))))

(defun fountain-get-dot-scene-heading ()
  "Return forced scene heading if matches line at point, nil otherwise.
This function is ignored unless
`fountain-dot-scene-heading-hierarchy' is non-nil."
  (when fountain-dot-scene-heading-hierarchy
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (let ((s (s-presence (fountain-get-line))))
          (when (and s
                     (s-matches?
                      "^\\.\\<" s)
                     (or (bobp)
                         (save-excursion
                           (forward-line -1)
                           (fountain-invisible-p)))
                     (save-excursion
                       (forward-line 1)
                       (or (eobp)
                           (fountain-invisible-p))))
            s))))))

(defun fountain-get-character ()
  "Return character if matches line at point, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (let ((s
             (when (s-present? (fountain-get-line))
               (s-presence
                (s-trim (car (s-slice-at "(" (fountain-get-line))))))))
        (when (and s
                   (or (s-uppercase? s)
                       (s-starts-with? "@" s))
                   (or (bobp)
                       (save-excursion
                         (forward-line -1)
                         (fountain-blank-p)))
                   (save-excursion
                     (forward-line 1)
                     (unless (eobp)
                       (not (fountain-invisible-p)))))
          s)))))

(defun fountain-dialogue-p ()
  "Return non-nil if line at point is dialogue."
  (unless (or (fountain-blank-p)
              (fountain-paren-p))
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
      (when (s-present? (fountain-get-line))
        (and (let ((s (s-trim (fountain-get-line))))
               (or (unless (s-ends-with? "<" s)
                     (s-starts-with? ">" s))
                   (and (s-uppercase? s)
                        (s-matches? (regexp-opt fountain-trans-list) s))))
             (save-excursion
               (forward-line -1)
               (or (bobp)
                   (fountain-invisible-p)))
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-invisible-p))))))))

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
        (and start end
             (goto-char end)
             (looking-at-p (rx (zero-or-more blank) line-end))
             (goto-char start)
             (forward-line 0)
             (looking-at-p fountain-note-regexp)
             (>= marker start)
             (<= marker end))))))

(defun fountain-get-previous-character (n)
  "Return Nth previous character within scene, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (dotimes (var n (fountain-get-character))
        (unless (fountain-get-scene-heading)
          (forward-line -1)
          (while (not (or (fountain-get-character)
                          (fountain-get-scene-heading)
                          (bobp)))
            (forward-line -1)))))))

(defun fountain-same-previous-character ()
  "Return non-nil if character at point is identical to prior character."
  (equal (fountain-get-character) (fountain-get-previous-character 1)))

(defun fountain-indent-add (column)
  "Add indentation properties to line at point."
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'line-prefix `(space :align-to ,column))
    (put-text-property (line-beginning-position) (line-end-position)
                       'wrap-prefix `(space :align-to ,column))))

(defun fountain-indent-refresh ()
  "Refresh indentation properties at point."
  (cond ((fountain-get-character)
         (fountain-indent-add fountain-align-column-character))
        ((fountain-paren-p)
         (fountain-indent-add fountain-align-column-paren))
        ((fountain-dialogue-p)
         (fountain-indent-add fountain-align-column-dialogue))
        ((fountain-trans-p)
         (fountain-indent-add fountain-align-column-trans))
        ((fountain-indent-add 0))))

(defun fountain-format-refresh (start end)
  "Refresh format between START and END."
  (save-excursion
    (save-restriction
      (widen)
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
          (forward-line 1))))))

(defun fountain-format-remove ()
  "Remove all indenting in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let (fountain-indent-elements)
        (fountain-format-refresh (point-min) (point-max))))))

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
 (if (> n 0)
     (dotimes (var n)
       (when (fountain-get-scene-heading)
         (forward-line 1))
       (while (not (or (eobp)
                       (fountain-get-scene-heading)))
         (forward-line 1)))
   (dotimes (var (* n -1))
     (when (fountain-get-scene-heading)
       (forward-line -1))
     (while (not (or (bobp)
                     (fountain-get-scene-heading)))
       (forward-line -1)))))

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
  (let ((s (downcase (shell-command-to-string "uuidgen"))))
    (car (split-string s "-"))))

(defun fountain-insert-synopsis ()
  "Open line below current scene heading and insert synopsis."
  (interactive)
  (widen)
  (push-mark)
  (while (not (or (bobp)
                  (fountain-get-scene-heading)
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
          (forward-paragraph 1))
        (unless (save-excursion
                  (forward-line 1)
                  (fountain-blank-p))
          (open-line 1))
        (comment-indent)
        (insert (fountain-format-template fountain-note-template))))))

(defun fountain-format-template (template)
  "Format TEMPLATE according to the following list.

  ${longtime}   Long date format (defined in `fountain-long-time-format')
  ${time}       Short date format (defined in `fountain-short-time-format')
  ${fullname}   User full name (defined in `user-full-name')
  ${nick}       User first name (defined in `user-login-name')
  ${email}      User email (defined in `user-mail-address')
  ${uuid}       Insert a UUID (defined in `fountain-uuid-function')"
  (s-format template 'aget
            `(("longtime" . ,(format-time-string fountain-long-time-format))
              ("time" . ,(format-time-string fountain-short-time-format))
              ("fullname" . ,user-full-name)
              ("nick" . ,(capitalize user-login-name))
              ("email" . ,user-mail-address)
              ("uuid" . ,(fountain-uuid)))))

(defun fountain-insert-metadata ()
  "Insert the metadata template at the beginning of file."
  (interactive)
  (widen)
  (goto-char (point-min))
  (save-excursion
    (insert (fountain-format-template fountain-metadata-template) "\n")))

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords
  `((fountain-match-scene-heading . fountain-scene-heading-face)
    (fountain-match-dot-scene-heading . fountain-dot-scene-heading-face)
    (,fountain-section-regexp . fountain-section-face)
    (,fountain-synopsis-regexp . fountain-synopsis-face)
    (,fountain-note-regexp . fountain-note-face))
  "Font lock highlighting keywords.")

(defun fountain-match-line (func limit)
  "If FUNC matches within LIMIT set match data to line."
  (let ((match))
    (while (and (null match)
                (< (point) limit))
      (if (funcall func)
          (progn
            (set-match-data
             (list (set-marker (make-marker) (line-beginning-position))
                   (set-marker (make-marker) (line-end-position))))
            (forward-line 1)
            (setq match t))
        (forward-line 1)))
    match))

(defun fountain-match-scene-heading (limit)
  "Call `fountain-match-line' with `fountain-get-scene-heading'."
  (fountain-match-line 'fountain-get-scene-heading limit))

(defun fountain-match-dot-scene-heading (limit)
  "Call `fountain-match-line' with `fountain-get-dot-scene-heading'."
  (fountain-match-line 'fountain-get-dot-scene-heading limit))

;;; Mode Map ===================================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "M-n") 'fountain-forward-scene)
    (define-key map (kbd "M-p") 'fountain-backward-scene)
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
  (set (make-local-variable 'font-lock-comment-face)
       'fountain-nonprinting-face)
  (setq font-lock-defaults '(fountain-font-lock-keywords nil t))
  (jit-lock-register 'fountain-format-refresh)
  (add-hook 'change-major-mode-hook 'fountain-format-remove nil t))

(provide 'fountain-mode)

;;; fountain-mode.el ends here
