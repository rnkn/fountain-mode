;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 1.2.0
;; Package-Requires: ((s "1.9.0"))
;; URL: https://github.com/rnkn/fountain-mode/

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

;; Fountain Mode
;; =============

;; Fountain Mode aims to be a full-featured screenwriting environment for
;; GNU Emacs using the Fountain markup format. For more information on
;; the Fountain markup format, visit <http://fountain.io>.

;; ![screenshot](https://dl.dropboxusercontent.com/u/94472468/fountain-mode-cdn/screenshot.png)

;; Features
;; --------

;; - support for the Fountain 1.1 specification
;; - exporting to HTML and PDF (requires [Prince][])
;; - include or omit a title page
;; - multiple levels of syntax highlighting for all elements (see below)
;; - auto-indentation: character, parenthetical, dialog, transition and
;;   center text elements (display only, does not modify file contents)
;; - add/remove continued dialog to successively speaking characters
;; - `occur` navigator for section headings, synopses, notes and scene
;;   headings
;; - templates for inserting synopses, notes and metadata
;; - navigate by scene heading
;; - suppoort for emphasis (bold, italic, underlined text)
;; - toggle visibility of emphasis delimiters and escaping characters
;; - standard commenting (boneyard) behaviour
;; - everything is customizable, of course

;; The following features are not yet supported:

;; - dual dialogue (probably won't be supported because it's stupid)

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type `C-h m`. Bugs and feature requests
;; are encouraged on the [Issues][] page, or you can email me
;; directly (email in the source code header).

;; See the [wiki][] for ways to extend Fountain Mode.

;; [prince]: http://www.princexml.com/ "Prince"
;; [issues]: https://github.com/rnkn/fountain-mode/issues/ "Fountain Mode issues"
;; [wiki]: https://github.com/rnkn/fountain-mode/wiki/ "Fountain Mode wiki"

;; Requirements
;; ------------

;; - Emacs 24 (not tested on earlier versions, only tested on Mac OS X
;;   and Linux, not tested on Windows).
;; - [s.el][], the long lost Emacs string manipulation library.
;; - Exporting to PDF requires [Prince][], which is free for personal
;;   use. Prince adds a removable PDF annotation on the first page; if
;;   you don't like it, delete the annotation in a PDF application that
;;   supports editing annotations, or open the PDF and print to PDF,
;;   which will remove all annotations.
;; - to insert UUIDs (useful for using notes as linked bookmarks) you'll
;;   need either `uuidgen` CLT (usually pre-installed on OS X and Linux)
;;   or [uuid.el][] Emacs package.

;; [s.el]: https://github.com/magnars/s.el "s.el"
;; [uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

;; Installation
;; ------------

;; Fountain Mode is available through [MELPA][]

;; Alternately, put `fountain-mode.el` and `s.el` in your `load-path` and
;; add the following line to your `.emacs` or `init.el` file:

;;     (require 'fountain-mode)

;; To load Fountain Mode whenever you open a `.fountain` file, also add the
;; following:

;;     (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

;; [MELPA]: http://melpa.milkbox.net "MELPA"

;; Syntax Highlighting
;; -------------------

;; To change the level of syntax highlighting, customize the value of
;; `font-lock-maximum-decoration`. This can be set indirectly with the
;; menu, or with `M-x fountain-set-font-lock-decoration` and saved with
;; `M-x fountain-save-font-lock-decoration`.

;; History
;; -------

;; See [Releases](https://github.com/rnkn/fountain-mode/releases).

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
  "1.2.0")

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

(define-obsolete-variable-alias 'fountain-align-centered
  'fountain-align-center "1.1.0")

(define-obsolete-face-alias 'fountain-centered
  'fountain-center "1.1.0")

(define-obsolete-face-alias 'fountain-scene-heading-highlight
  'fountain-scene-heading "1.2.0")

(define-obsolete-face-alias 'fountain-note-highlight
  'fountain-note "1.2.0")

(define-obsolete-face-alias 'fountain-section-highlight
  'fountain-section "1.2.0")

(define-obsolete-face-alias 'fountain-synopsis-highlight
  'fountain-synopsis "1.2.0")

(define-obsolete-face-alias 'fountain-center-highlight
  'fountain-center "1.2.0")

(define-obsolete-face-alias 'fountain-character-highlight
  'fountain-character "1.2.0")

(define-obsolete-face-alias 'fountain-paren-highlight
  'fountain-paren "1.2.0")

(define-obsolete-face-alias 'fountain-dialog-highlight
  'fountain-dialog "1.2.0")

(define-obsolete-face-alias 'fountain-trans-highlight
  'fountain-trans "1.2.0")

;;; Customizable Options ===============================================

(defcustom fountain-mode-hook
  '(turn-on-visual-line-mode)
  "Mode hook for `fountain-mode', run after the mode is turned on."
  :type 'hook
  :group 'fountain)

(defcustom fountain-metadata-template
  "title: ${title}\ncredit: written by\nauthor: ${fullname}\ndraft: first\ndate: ${longtime}\ncontact: ${email}\n"
  "\\<fountain-mode-map>Template inserted at beginning of buffer with \\[fountain-insert-metadata].
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
  '("TO:" "WITH:" "FADE IN:" "FADE OUT" "TO BLACK")
  "List of transition endings (case insensitive).
This list is used to match the endings of transitions,
e.g. \"TO:\" will match both the following:

    CUT TO:

    DISSOLVE TO:"
  :type '(repeat (string :tag "Transition"))
  :group 'fountain)

(defcustom fountain-add-continued-dialog t
  "\\<fountain-mode-map>If non-nil, add continued dialog appropriately with \\[fountain-continued-dialog-refresh].
When same character speaks in succession, append
`fountain-continued-dialog-string'."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-continued-dialog-string "CONT'D"
  "String to append to character name speaking in succession.
If `fountain-add-continued-dialog' is non-nil, append this string
to character when speaking in succession.

Parentheses are added automatically, e.g. \"CONT'D\" becomes
\"(CONT'D)\".

WARNING: if you change this variable then call
`fountain-continued-dialog-refresh', strings matching the
previous value will not be recognized. Before changing this
variable, first make sure to set `fountain-add-continued-dialog'
to nil and run `fountain-continued-dialog-refresh', then make the
changes desired."
  :type 'string
  :group 'fountain)

;; (defcustom fountain-trim-whitespace nil
;;   "If non-nil, trim whitespace around elements."
;;   :type 'boolean
;;   :group 'fountain)

(defcustom fountain-align-character 20
  "Column integer to which characters names should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-dialog 10
  "Column integer to which dialog should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-paren 15
  "Column integer to which parentheticals should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-trans 45
  "Column integer to which transitions should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-center 10
  "Column integer to which centered text should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-elements t
  "If non-nil, elements will be displayed auto-aligned.
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
the behaviour of the \\[comment-dwim] command. The default is the
former but if you prefer the latter, set this option to non-nil."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-hide-emphasis-delim nil
  "If non-nil, make emphasis delimiters invisible."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-hide-escapes nil
  "If non-nil, make escaping characters invisible."
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
  "\\<fountain-mode-map>Template for inserting notes with \\[fountain-insert-note].
See `fountain-insert-template'.

The default \"${time} - ${fullname}: \" will insert something
similar to:

\[\[01/20/14 - Alan Smithee: \]\]"
  :type 'string
  :group 'fountain)

(defcustom fountain-uuid-func
  '(lambda () (shell-command-to-string "uuidgen"))
  "Function for generating a UUID.
The default function requires the command line tool \"uuidgen\"."
  :tag "Fountain UUID Function"
  :type 'function
  :group 'fountain)

;;; Variables ==========================================================

(defvar fountain-metadata nil
  "Metadata alist in the form of (KEY . VALUE).
This buffer-local variable is set with `fountain-read-metadata'
upon calling `fountain-mode' or saving a file.")

;;; Element Regular Expressions ========================================

(defconst fountain-blank-regexp
  "\\`\\|^\s?$\\|\\'"
  "Regular expression for matching an empty line.")

(defconst fountain-forced-action-mark-regexp
  "^!"
  "Regular expression for forced action mark.")

(defconst fountain-nbsp-regexp
  (concat "\\(^\\|[^\\]\\)"
          "\\(\\\\\\)\s")
  "Regular expression for non-breaking space.")

(defconst fountain-comment-regexp
  "//.*\\|/\\*\\(.\\|\n\\)*?\\*/"
  "Regular expression for matching comments.")

(defconst fountain-metadata-regexp
  (concat "^\\(.+\\):\s*\\(.+\\)?\\|"
          "^\s+\\(?2:.+\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-metadata-p' for bobp.")

(defconst fountain-scene-heading-regexp
  (concat "^\\(\\.\\)\\(\\<.*\\)\\|"
          "^\\(?2:"
          (regexp-opt fountain-scene-heading-prefix-list)
          "[.\s\t]+.*\\)")
  "Regular expression for matching scene headings.
Requires `fountain-scene-heading-p' for preceding blank line.")

(defconst fountain-paren-regexp
  "^[\s\t]*([^)\n]*)[\s\t]*$"
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or dialog.")

(defconst fountain-page-break-regexp
  "^[\s\t]*=\\{3,\\}.*"
  "Regular expression for matching page breaks.")

(defconst fountain-note-regexp
  "\\[\\[\\(?:.\n?\\)*]]"
  "Regular expression for matching notes.")

(defconst fountain-section-regexp
  "^\\(#\\{1,5\\}[\s\t]*\\)\\([^#\n].*\\)"
  "Regular expression for matching section headings.")

(defconst fountain-synopsis-regexp
  "^\\(=[\s\t]*\\)\\([^=\n].*\\)"
  "Regular expression for matching synopses.")

(defconst fountain-trans-regexp
  (concat "^\\([\s\t]*>\s*\\)\\([^<>\n]*\\)$\\|"
          "^[\s\t]*\\(?2:[[:upper:]\s]*"
          (regexp-opt fountain-trans-list)
          "\\)$")
  "Regular expression for matching transitions.
Requires `fountain-trans-p' for preceding and succeeding blank
lines.")

(defconst fountain-center-regexp
  "\\(^[\s\t]*>[\s\t]*\\)\\(.*?\\)\\([\s\t]*<[\s\t]*$\\)"
  "Regular expression for matching centered text.")

;;; Emphasis Regular Expressions =======================================

(defconst fountain-underline-regexp
  (concat "\\(^\\|[^\\]\\)"
          "\\(_\\)"
          "\\([^\s\t\n_]+?[^\n_]*?\\)"
          "\\(\\2\\)")
  "Regular expression for matching underlined text.")

(defconst fountain-italic-regexp
  (concat "\\(^\\|[^\\\\*]\\)"
          "\\(\\*\\)"
          "\\([^\s\t\n\\*]+?[^\n\\*]*?\\)"
          "\\(\\2\\)")
  "Regular expression for matching italic text.")

(defconst fountain-bold-regexp
  (concat "\\(^\\|[^\\]\\)"
          "\\(\\*\\{2\\}\\)"
          "\\([^\s\t\n\\*]+?[^\n\\*]*?\\)"
          "\\(\\2\\)")
  "Regular expression for matching bold text.")

(defconst fountain-bold-italic-regexp
  (concat "\\(^\\|[^\\\\*]\\)"
          "\\(\\*\\{3\\}\\)"
          "\\([^\s\t\n\\*]+?[^\n\\*]*?\\)"
          "\\(\\2\\)")
  "Regular expression for matching bold-italic text.
Due to the problematic nature of the syntax,
bold-italic-underlined text must be specified with the
bold-italic delimiters together, e.g.

    This text is _***ridiculously important***_.")

(defconst fountain-lyrics-regexp
  (concat "^\\(?2:~\s*\\)"
          "\\(?3:.+\\)")
  "Regular expression for matching lyrics.")

;;; Faces ==============================================================

(defgroup fountain-faces nil
  "Faces used in `fountain-mode'.
There are three levels of Font Lock decoration:

  1. minimum: only highlights comments and escaping characters

  2. default: highlights comments, metadata, scene headings,
     sections, synopses, notes and escaping characters

  3. maximum: highlights comments, metadata keys, metadata
     values, scene headings, sections, synopses, notes,
     characters, parentheticals, dialog, transitions, center text
     and escaping characters

To switch between these levels of Font Lock decoration, customize
the value of `font-lock-maximum-decoration'. This can be set
indirectly with \\[fountain-set-font-lock-decoration] and saved
with \\[fountain-save-font-lock-decoration]."
  :prefix "fountain-"
  :group 'fountain)

(defface fountain-comment
  '((t (:inherit shadow)))
  "Default face for comments (boneyard)."
  :group 'fountain-faces)

(defface fountain-non-printing
  '((t (:inherit fountain-comment)))
  "Default face for emphasis delimiters and escaping characters."
  :group 'fountain-faces)

(defface fountain-metadata-key
  '((t (:inherit font-lock-constant-face)))
  "Default face for metadata keys."
  :group 'fountain-faces)

(defface fountain-metadata-value
  '((t (:inherit match)))
  "Default face for metadata values."
  :group 'fountain-faces)

(defface fountain-page-break
  '((t (:inherit fountain-comment)))
  "Default face for page breaks."
  :group 'fountain-faces)

(defface fountain-scene-heading
  '((t (:inherit font-lock-function-name-face)))
  "Default face for scene headings."
  :group 'fountain-faces)

(defface fountain-paren
  '((t (:inherit font-lock-variable-name-face)))
  "Default face for parentheticals."
  :group 'fountain-faces)

(defface fountain-center
  '((t nil))
  "Default face for centered text."
  :group 'fountain-faces)

(defface fountain-note
  '((t (:inherit font-lock-comment-face)))
  "Default face for notes.")

(defface fountain-section
  '((t (:inherit font-lock-builtin-face)))
  "Default face for section headings."
  :group 'fountain-faces)

(defface fountain-synopsis
  '((t (:inherit font-lock-type-face)))
  "Default face for synopses."
  :group 'fountain-faces)

(defface fountain-character
  '((t (:inherit font-lock-keyword-face)))
  "Default face for characters."
  :group 'fountain-faces)

(defface fountain-dialog
  '((t (:inherit font-lock-string-face)))
  "Default face for dialog."
  :group 'fountain-faces)

(defface fountain-trans
  '((t (:inherit font-lock-variable-name-face)))
  "Default face for transitions."
  :group 'fountain-faces)

;;; Thing Definitions ==================================================

(put 'scene 'forward-op 'fountain-forward-scene)

;;; Internal Functions =================================================

(defun fountain-get-block-bounds ()
  "Return the beginning and end points of block at point."
  (let ((block-beginning
         (save-excursion
           (re-search-backward fountain-blank-regexp nil t)))
        (block-end
         (save-excursion
           (re-search-forward fountain-blank-regexp nil t))))
    (cons block-beginning block-end)))

;; currently unused
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
  "Match metadata if point is at metadata, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-metadata-regexp)
           (or (bobp)
               (save-match-data
                 (forward-line -1)
                 (fountain-metadata-p)))))))

(defun fountain-section-p ()
  "Match section heading if point is at section heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-section-regexp))))

(defun fountain-synopsis-p ()
  "Match synopsis if point is at synopsis, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-synopsis-regexp))))

(defun fountain-note-p ()
  "Match note if point is at a note, nil otherwise."
  (thing-at-point-looking-at fountain-note-regexp))

(defun fountain-comment-p ()
  "Match comment if point is at a comment, nil otherwise."
  ;; problems with comment-only-p picking up blank lines as comments
  ;;
  ;; (comment-only-p (line-beginning-position) (line-end-position)))
  (thing-at-point-looking-at fountain-comment-regexp))
(defalias 'fountain-boneyard-p 'fountain-comment-p)

(defun fountain-invisible-p ()
  "Return non-nil if point is at an invisible element.
A line is invisible if it is blank, or consists of a section
heading, synopsis, note, or is within a comment."
  (or (fountain-blank-p)
      (fountain-section-p)
      (fountain-synopsis-p)
      (fountain-note-p)
      (fountain-comment-p)))

(defun fountain-scene-heading-p ()
  "Match scene heading if point is at a scene heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-scene-heading-regexp)
           (save-match-data
             (forward-line -1)
             (fountain-invisible-p))))))

(defun fountain-character-p ()
  "Match character if point is at character, nil otherwise."
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
                 (and (null (s-starts-with? "!" s))
                      (or (s-uppercase? s)
                          (s-starts-with? "@" s))
                      (save-excursion
                        (forward-line -1)
                        (fountain-invisible-p))
                      (save-excursion
                        (forward-line 1)
                        (unless (eobp)
                          (null (fountain-invisible-p))))))))))))

(defun fountain-dialog-p ()
  "Match dialog if point is at dialog, nil otherwise."
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
  "Match parenthetical if point is at a paranthetical, nil otherwise."
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
  "Match transition if point is at a transition, nil otherwise."
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
                   (fountain-invisible-p))))
           (save-match-data
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-invisible-p))))))))

(defun fountain-center-p ()
  "Match centered text if point is at centered text, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-center-regexp))))

(defun fountain-read-metadata ()
  "Read buffer metadata, set and return `fountain-metadata'."
  (setq fountain-metadata nil)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (fountain-metadata-p)
        (let ((key (downcase (match-string-no-properties 1)))
              (value
               (progn
                 (forward-line 1)       ;FIXME: on line ahead
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
  "Return the value associated with KEY in `fountain-metadata'.
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

Optionally, use \"$m\" and \"$p\" to set the `mark' and `point'
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
    (let ((end (point-marker)))
      (goto-char start)
      (when (search-forward "$m" end t)
        (replace-match "")
        (push-mark (point) t t))
      (goto-char start)
      (if (search-forward "$p" end t)
          (replace-match "")
        (goto-char end)))))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID by calling `fountain-uuid-func'."
  (let ((s (downcase (funcall fountain-uuid-func))))
    (car (split-string s "-"))))

;; make N positive or negative...?
(defun fountain-get-character (&optional n)
  "Return character at point or Nth previous character.
If N is non-nil, return Nth character previous to point. If N is
nil or 0, return character at point, otherwise return nil."
  (let ((i (or n 0)))
    (save-excursion
      (save-restriction
        (widen)
        (while (> i 0)
          (unless (fountain-scene-heading-p)
            (forward-line -1))
          (while (null (or (fountain-character-p)
                           (fountain-scene-heading-p)
                           (bobp)))
            (forward-line -1))
          (setq i (- i 1)))
        (if (fountain-character-p)
            (let ((s (match-string-no-properties 0)))
              (s-trim (car (s-slice-at "\\^\\|(" s)))))))))

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

;; not in use, delete?
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
  "Move forward N scene headings (backward if N is negative).
If N is 0, move to beginning of scene."
  (interactive "^p")
  (let* ((i (or n 1))
         (p (if (<= i 0) -1 1)))
    (if (= i 0)
        (progn
          (forward-line 0)
          (while (null (or (eq (point) (buffer-end p))
                           (fountain-section-p)
                           (fountain-scene-heading-p)))
            (forward-line p)))
      (while (/= i 0)
        (if (or (fountain-scene-heading-p)
                (fountain-section-p))
            (forward-line p))
        (while (null (or (eq (point) (buffer-end p))
                         (fountain-section-p)
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
  (unless (eobp)
    (forward-char -1)))

(defun fountain-mark-scene ()
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
  (if (null (or (fountain-section-p)
                (fountain-scene-heading-p)))
      (progn
        (goto-char (mark))
        (error "Before first scene heading"))
    (push-mark)
    (fountain-forward-scene 1)
    (exchange-point-and-mark)))

(defun fountain-insert-synopsis ()
  "Insert synopsis below scene heading of current scene."
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
  "Insert a note based on `fountain-note-template' underneath current element.
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
  "Insert metadata based on `fountain-metadata-template' at bobp."
  (interactive)
  (widen)
  (goto-char (point-min))
  (fountain-insert-template fountain-metadata-template))

(defun fountain-occur-sections ()
  "Display `occur' buffer searching `fountain-section-regexp'."
  (interactive)
  (occur fountain-section-regexp))

(defun fountain-occur-synopses ()
  "Display `occur' buffer searching `fountain-synopsis-regexp'."
  (interactive)
  (occur fountain-synopsis-regexp))

(defun fountain-occur-notes ()
  "Display `occur' buffer searching `fountain-note-regexp'."
  (interactive)
  (occur fountain-note-regexp))

(defun fountain-occur-scene-headings ()
  "Display `occur' buffer searching `fountain-scene-heading-regexp'."
  (interactive)
  (occur fountain-scene-heading-regexp))

(defun fountain-continued-dialog-refresh (&optional arg)
  "Add or remove continued dialog on characters speaking in succession.
If `fountain-add-continued-dialog' is non-nil, add
`fountain-continued-dialog-string' on characters speaking in
succession, otherwise remove all occurences.

If region is active, act on region, otherwise act on current
scene. If prefixed with \\[universal-argument], act on whole
buffer (this can take a while).

WARNING: if you change `fountain-continued-dialog-string' then
call this function, strings matching the previous value will not
be recognized. Before changing that variable, first make sure to
set `fountain-add-continued-dialog' to nil and run this function,
then make the changes desired."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      ;; first expand the region
      (let ((start (make-marker))
            (end (make-marker))
            ;; create continued string
            (s (concat "(" fountain-continued-dialog-string ")"))
            ;; create progress report
            (job (make-progress-reporter "Refreshing continued dialog...")))
        ;; set START and END markers since buffer contents will change
        (set-marker start
                    (cond (arg (point-min))
                          ((use-region-p) (region-beginning))
                          ((car (bounds-of-thing-at-point 'scene)))))
        (set-marker end
                    (cond (arg (point-max))
                          ((use-region-p) (region-end))
                          ((cdr (bounds-of-thing-at-point 'scene)))))
        ;; delete all matches in region
        (goto-char start)
        (while (re-search-forward (concat "\s*" s) end t)
          (replace-match "")
          (progress-reporter-update job))
        ;; add string where appropriate
        (when fountain-add-continued-dialog
          (goto-char start)
          (while (< (point) end)
            (when (and (null (looking-at-p (concat ".*" s "$")))
                       (fountain-character-p)
                       (s-equals? (fountain-get-character 0)
                                  (fountain-get-character 1)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" s)))
            (forward-line 1)
            (progress-reporter-update job)))
        (progress-reporter-done job)))))

;;; Menu Functions =====================================================

(defun fountain-toggle-comment-syntax ()
  "Toggle `fountain-switch-comment-syntax'."
  (interactive)
  (setq fountain-switch-comment-syntax
        (null fountain-switch-comment-syntax))
  (if fountain-switch-comment-syntax
      (setq comment-start "//" comment-end "")
    (setq comment-start "/*" comment-end "*/"))
  (message "Default comment syntax is now %s"
           (if fountain-switch-comment-syntax
               "\"// COMMENT\"" "\"/* COMMENT */\"")))

(defun fountain-toggle-hide-element (element &optional s)
  "Toggle visibility of fountain-ELEMENT, using S for feedback.
Toggles the value of fountain-hide-ELEMENT, then, if
fountain-hide-ELEMENT is non-nil, adds fountain-ELEMENT to
`buffer-invisibility-spec', otherwise removes it. Returns a
message of \"S are now invisible/visible\"."
  (interactive "sElement: ")            ;FIXME: use autocomplete
  (let* ((option (intern (concat "fountain-hide-" element)))
         (symbol (intern (concat "fountain-" element))))
    (set option
         (null (symbol-value option)))
    (if (symbol-value option)
        (add-to-invisibility-spec symbol)
      (remove-from-invisibility-spec symbol))
    (jit-lock-refontify)
    (if s
        (message "%s are now %s"
                 s (if (symbol-value option)
                       "invisible" "visible")))))

(defun fountain-toggle-align-elements ()
  "Toggle `fountain-align-elements'."
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
  ;; (fountain-continued-dialog-refresh)
  (message "Continued dialog is now %s"
           (if fountain-add-continued-dialog
               "added" "removed")))

(defun fountain-toggle-export-include-title-page ()
  "Toggle `fountain-export-include-title-page'."
  (interactive)
  (setq fountain-export-include-title-page
        (null fountain-export-include-title-page))
  (message "Title page is now %s on export"
           (if fountain-export-include-title-page
               "included" "omitted")))

(defun fountain-set-font-lock-decoration (level)
  "Set `font-lock-maximum-decoration' for `fountain-mode' to LEVEL."
  (interactive "NMaximum Decoration (1-3): ")
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
           (cond ((eq level 1) "minimum")
                 ((eq level 2) "default")
                 ((eq level 3) "maximum")))
  (font-lock-refresh-defaults))

(defun fountain-save-font-lock-decoration ()
  "Save `font-lock-maximum-decoration' in `custom-file'."
  (interactive)
  (customize-save-variable 'font-lock-maximum-decoration
                           font-lock-maximum-decoration))

(defun fountain-save-hidden-elements ()
  "Save hidden element options in `custom-file'."
  (interactive)
  (customize-save-variable 'fountain-hide-emphasis-delim
                           fountain-hide-emphasis-delim)
  (customize-save-variable 'fountain-hide-escapes
                           fountain-hide-escapes))

(defun fountain-get-font-lock-decoration ()
  "Return the value of `font-lock-maximum-decoration'."
  (cond ((null font-lock-maximum-decoration) 2)
        ((eq font-lock-maximum-decoration t) 3)
        ((integerp font-lock-maximum-decoration)
         font-lock-maximum-decoration)
        ((cdr (assoc 'fountain-mode font-lock-maximum-decoration)))
        ((cdr (assoc 't font-lock-maximum-decoration)) 3)))

;;; Font Lock ==========================================================

(defvar fountain-font-lock-keywords-plist
  `(("note" ,fountain-note-regexp
     ((2 0 nil t)))
    ("scene-heading"
     (lambda (limit)
       (fountain-match-element 'fountain-scene-heading-p limit))
     ((2 0 nil nil keep)
      (1 1 fountain-comment fountain-escapes t t)))
    ("character"
     (lambda (limit)
       (fountain-match-element 'fountain-character-p limit))
     ((3 0 nil nil keep)))
    ("dialog"
     (lambda (limit)
       (fountain-match-element 'fountain-dialog-p limit))
     ((3 0 nil nil keep)))
    ("paren"
     (lambda (limit)
       (fountain-match-element 'fountain-paren-p limit))
     ((3 0 nil nil keep)))
    ("trans"
     (lambda (limit)
       (fountain-match-element 'fountain-trans-p limit))
     ((3 0 nil nil keep)
      (1 1 fountain-comment fountain-escapes t t)))
    ("forced-action-mark" ,fountain-forced-action-mark-regexp
     ((1 0 fountain-comment fountain-escapes)))
    ("center" ,fountain-center-regexp
     ((1 1 fountain-comment fountain-escapes)
      (3 2 nil)
      (1 3 fountain-comment fountain-escapes)))
    ("section" ,fountain-section-regexp
     ((2 0 nil t)
      (1 1 fountain-comment fountain-escapes t)))
    ("synopsis" ,fountain-synopsis-regexp
     ((2 0 nil t)
      (1 1 fountain-comment fountain-escapes t)))
    ("page-break" ,fountain-page-break-regexp
     ((2 0 fountain-page-break)))
    ("metadata"
     (lambda (limit)
       (fountain-match-element 'fountain-metadata-p limit))
     ((3 1 fountain-metadata-key t nil t)
      (3 2 fountain-metadata-value t nil t)
      (1 0 fountain-comment t keep)))
    (nil ,fountain-nbsp-regexp
         ((1 2 fountain-non-printing fountain-escapes)))
    (nil ,fountain-underline-regexp
         ((1 2 fountain-non-printing fountain-emphasis-delim)
          (2 3 underline)
          (1 4 fountain-non-printing fountain-emphasis-delim)))
    (nil ,fountain-italic-regexp
         ((1 2 fountain-non-printing fountain-emphasis-delim)
          (2 3 italic)
          (1 4 fountain-non-printing fountain-emphasis-delim)))
    (nil ,fountain-bold-regexp
         ((1 2 fountain-non-printing fountain-emphasis-delim)
          (2 3 bold)
          (1 4 fountain-non-printing fountain-emphasis-delim)))
    (nil ,fountain-bold-italic-regexp
         ((1 2 fountain-non-printing fountain-emphasis-delim)
          (2 3 bold-italic)
          (1 4 fountain-non-printing fountain-emphasis-delim)))
    (nil ,fountain-lyrics-regexp
         ((1 2 fountain-non-printing fountain-emphasis-delim)
          (2 3 italic))))
  "List of face properties to create element Font Lock keywords.
Has the format:

    (ELEMENT MATCHER LIST)

The first element, ELEMENT, is a string naming the element; if
nil, this face is not considered an element. MATCHER is a regular
expression or search function. LIST is a list of lists, each with
the format:

    (LEVEL SUBEXP FACE INVISIBLE OVERRIDE LAXMATCH)

LEVEL is an integer representing the Font Lock decoration level
at which the face is applied. SUBEXP is the subexpression to
match. FACE is either a face name to apply, or nil, which will
generate a face name as \"fountain-ELEMENT\". INVISIBLE, if t,
adds FACE to the \"invisible\" text property. OVERRIDE and
LAXMATCH follow `font-lock-keywords'.")

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (let ((dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (f fountain-font-lock-keywords-plist keywords)
      (let* ((element (car f))
             (matcher (nth 1 f))
             (list (nth 2 f))
             (align (intern (concat "fountain-align-" element)))
             ;; if we're using auto-align and the align var is bound,
             ;; set the align properties
             (align-props (if (and fountain-align-elements
                                   (boundp align))
                              `(line-prefix
                                (space :align-to ,align)
                                wrap-prefix
                                (space :align-to ,align))))
             face-props)
        (dolist (f list)
          (let* ((level (car f))
                 (subexp (nth 1 f))
                 ;; if LEVEL is less or equal to DEC, use either face
                 ;; supplied in PLIST or intern fountain-ELEMENT,
                 ;; otherwise use nil
                 (face (if (<= level dec)
                           (or (nth 2 f)
                               (intern (concat "fountain-" element)))))
                 ;; if INVISIBLE is non-nil, add to INVISIBLE-PROPS
                 (invisible (nth 3 f))
                 (invisible-props
                  (cond ((eq invisible t)
                         `(invisible ,(intern (concat "fountain-" element))))
                        (invisible
                         `(invisible ,invisible))))
                 ;; set the face OVERRIDE and LAXMATCH
                 (override (nth 4 f))
                 (laxmatch (nth 5 f)))
            (setq face-props
                  (append face-props
                          (if element
                              (list `(,subexp '(face ,face
                                                     ,@align-props
                                                     ,@invisible-props
                                                     fountain-element ,element)
                                              ,override ,laxmatch))
                            (list `(,subexp '(face ,face
                                                   ,@invisible-props)
                                            append)))))))
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

;;; Mode Map ===========================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; editing commands
    (define-key map (kbd "C-c C-m") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "<S-return>") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "C-c C-c") 'fountain-continued-dialog-refresh)
    (define-key map (kbd "C-c C-z") 'fountain-insert-note)
    (define-key map (kbd "C-c C-a") 'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-x i") 'fountain-insert-metadata)
    (define-key map (kbd "C-c C-x f") 'fountain-set-font-lock-decoration)
    ;; navigation commands
    (define-key map (kbd "M-n") 'fountain-forward-scene)
    (define-key map (kbd "M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-M-n") 'fountain-forward-scene)
    (define-key map (kbd "C-M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-M-a") 'fountain-beginning-of-scene)
    (define-key map (kbd "C-M-e") 'fountain-end-of-scene)
    (define-key map (kbd "C-M-h") 'fountain-mark-scene)
    ;; exporting commands
    (define-key map (kbd "C-c C-e C-e") 'fountain-export-default)
    (define-key map (kbd "C-c C-e h") 'fountain-export-buffer-to-html)
    (define-key map (kbd "C-c C-e p") 'fountain-export-buffer-to-pdf-via-html)
    ;; view commands
    (define-key map (kbd "C-c C-x \\")
      (lambda ()
        (interactive)
        (fountain-toggle-hide-element "escapes" "Escaping characters")))
    (define-key map (kbd "C-c C-x *")
      (lambda ()
        (interactive)
        (fountain-toggle-hide-element "emphasis-delim" "Emphasis delimiters")))
    (define-key map (kbd "M-s 1") 'fountain-occur-sections)
    (define-key map (kbd "M-s 2") 'fountain-occur-synopses)
    (define-key map (kbd "M-s 3") 'fountain-occur-notes)
    (define-key map (kbd "M-s 4") 'fountain-occur-scene-headings)
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
    ("Export"
     ["Default" fountain-export-default]
     "---"
     ["Buffer to HTML" fountain-export-buffer-to-html]
     ["Buffer to PDF via HTML" fountain-export-buffer-to-pdf-via-html]
     "---"
     ["Include Title Page"
      fountain-toggle-export-include-title-page
      :style toggle
      :selected fountain-export-include-title-page]
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
     ["Customize Exporting" (customize-group 'fountain-export)])
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
    ("Syntax Highlighting"
     ["Minimum" (fountain-set-font-lock-decoration 1)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 1)]
     ["Default" (fountain-set-font-lock-decoration 2)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 2)]
     ["Maximum" (fountain-set-font-lock-decoration 3)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 3)]
     "---"
     ["Save for Future Sessions" fountain-save-font-lock-decoration])
    ("Show/Hide"
     ["Hide Emphasis Delimiters"
      (fountain-toggle-hide-element "emphasis-delim" "Emphasis delimiters")
      :style toggle
      :selected fountain-hide-emphasis-delim
      :keys "C-c C-x *"]
     ["Hide Escaping Characters"
      (fountain-toggle-hide-element "escapes" "Escaping characters")
      :style toggle
      :selected fountain-hide-escapes
      :keys "C-c C-x \\"]
     "---"
     ["Save for Future Sessions" fountain-save-hidden-elements])
    "---"
    ("Navigate"
     ["Next Scene Heading" fountain-forward-scene]
     ["Previous Scene Heading" fountain-backward-scene]
     "---"
     ["Section Heading Navigator" fountain-occur-sections]
     ["Synopsis Navigator" fountain-occur-synopses]
     ["Notes Navigator" fountain-occur-notes]
     ["Scene Heading Navigator" fountain-occur-scene-headings])
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
  (setq font-lock-defaults '(fountain-create-font-lock-keywords
                             nil t))
  (setq font-lock-extra-managed-props
        '(line-prefix wrap-prefix invisible fountain-element))
  (if fountain-hide-emphasis-delim
      (add-to-invisibility-spec 'fountain-emphasis-delim))
  (if fountain-hide-escapes
      (add-to-invisibility-spec 'fountain-escapes))
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec nil))
  (add-hook 'font-lock-extend-region-functions
            'fountain-font-lock-extend-region t t)
  (add-hook 'after-save-hook
            'fountain-read-metadata))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
