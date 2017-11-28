;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2017 Paul Rankin

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp
;; Version: 3.0.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/rnkn/fountain-mode

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

;; Fountain Mode is a complete screenwriting environment for GNU Emacs
;; using the Fountain markup format. For more information on the Fountain markup
;; format, visit <http://fountain.io>.

;; Features
;; --------

;; - Support for Fountain 1.1 specification
;; - WYSIWYG auto-align elements (display only, does not modify file contents)
;;   specific to script format, e.g. screenplay, stageplay or user-defined format
;; - Export to HTML, LaTeX, Final Draft (FDX), or Fountain
;; - Export to standalone document or snippet
;; - Integration with `outline` to fold/cycle visibility of sections and scenes
;; - Integration with `imenu` (sections, scene headings, notes)
;; - Intergration with `auto-insert` for title page metadata
;; - Add/remove automatic continuation string to successively speaking characters
;; - Navigation by section, scene, character name, or page
;; - 3 levels of element syntax highlighting
;; - Automatic loading for `*.fountain` files
;; - Support for both official and legacy commenting (boneyard) syntax
;; - Include or omit a title page
;; - Emphasis (bold, italic, underlined text)
;; - Toggle visibility of emphasis delimiters and syntax characters
;; - Everything is customizable

;; Check out the Nicholl Fellowship sample script exported from Fountain Mode to:

;; - [HTML](https://rawgit.com/rnkn/mcqueen/master/sample/sample.html)
;; - [LaTeX](https://www.sharelatex.com/project/54ed9180966959cb7fdbde8e)
;; - [Final Draft](http://files.paulwrankin.com/fountain-mode/Nicholl%20Fellowship%20sample.fdx)

;; More information on outlining here: <https://github.com/rnkn/fountain-mode/wiki/Outlining>

;; Most common features are accessible from the menu. For a full list of functions
;; and key-bindings, type C-h m.

;; For more, see the [Wiki](https://github.com/rnkn/fountain-mode/wiki).

;; Requirements
;; ------------

;; - Emacs 24.4
;; - LaTeX packages for PDF export: geometry fontspec titling fancyhdr
;;   marginnote ulem xstring oberdiek

;; Installation
;; ------------

;; *For users on OS X with no experience with Emacs, see the
;; [Absolute Beginner's Guide (OS X)][guide].*

;; The latest stable release of Fountain Mode is available via
;; [MELPA-stable](http://stable.melpa.org/#/fountain-mode).

;; Alternately, download the [latest release][], move the files into your
;; `load-path` and add the following line to your `.emacs` or `init.el` file:

;;     (require 'fountain-mode)

;; If you prefer the latest but perhaps unstable version, install via
;; [MELPA][], or clone the repository into your `load-path` and require as
;; above:

;;     git clone https://github.com/rnkn/fountain-mode.git

;; [guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
;; [melpa]: http://melpa.org/#/fountain-mode "MELPA"
;; [melpa-stable]: http://stable.melpa.org/#/fountain-mode "MELPA-stable"
;; [latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

;; Bugs and Feature Requests
;; -------------------------

;; Please raise an issue on [Issues](https://github.com/rnkn/fountain-mode/issues).

;; Roadmap
;; -------

;; See [Milestones](https://github.com/rnkn/fountain-mode/milestones).

;; History
;; -------

;; See [Releases](https://github.com/rnkn/fountain-mode/releases).


;;; Code:

(defconst fountain-version
  "3.0.0")

(defun fountain-version ()
  "Return `fountain-mode' version."
  (interactive)
  (message "Fountain Mode %s" fountain-version))

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "https://github.com/rnkn/fountain-mode"))


;;; Obsolete Warnings

(define-obsolete-variable-alias 'fountain-align-centered
  'fountain-align-center "1.1.0")

(define-obsolete-variable-alias 'fountain-export-title-page-template
  'fountain-export-title-page-title-template "1.1.0")

(define-obsolete-variable-alias 'fountain-hide-escapes
  'fountain-hide-syntax-chars "1.3.0")

(define-obsolete-variable-alias 'fountain-export-inline-style
  'fountain-export-html-use-inline-style "2.0.0")

(define-obsolete-variable-alias 'fountain-export-style-template
  'fountain-export-html-style-template "2.0.0")

(define-obsolete-function-alias 'fountain-toggle-hide-escapes
  'fountain-toggle-hide-syntax-chars "1.3.0")

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

(define-obsolete-face-alias 'fountain-section
  'fountain-section-heading "1.4.1")

(make-obsolete-variable 'fountain-export-title-page-left-template
                        'fountain-export-contact-template "2.0.0")

(make-obsolete-variable 'fountain-export-title-page-right-template
                        'fountain-export-contact-template "2.0.0")

(make-obsolete 'fountain-export-buffer-to-pdf-via-html
               'fountain-export-to-latex "2.0.0")

(make-obsolete-variable 'fountain-export-pdf-via-html-command
                        'fountain-export-shell-command "2.0.0")

(make-obsolete-variable 'fountain-uuid-func
                        "use a third-party package instead." "2.0.0")

(make-obsolete-variable 'fountain-export-bold-scene-headings
                        'fountain-export-scene-heading-format "2.0.0")

(make-obsolete-variable 'fountain-export-underline-scene-headings
                        'fountain-export-scene-heading-format "2.0.0")

(make-obsolete-variable 'fountain-export-double-space-scene-headings
                        'fountain-export-scene-heading-format "2.0.0")

(make-obsolete-variable 'fountain-export-bold-title
                        'fountain-export-title-format "2.0.0")

(make-obsolete-variable 'fountain-export-underline-title
                        'fountain-export-title-format "2.0.0")

(make-obsolete-variable 'fountain-export-upcase-title
                        'fountain-export-title-format "2.0.0")

(make-obsolete-variable 'fountain-export-html-head-template
                        'fountain-export-templates "2.0.0")

(make-obsolete-variable 'fountain-export-html-use-inline-style
                        "use inline style instead." "2.1.0") ; FIXME: make this customizable

(make-obsolete-variable 'fountain-additional-template-replace-functions
                        'fountain-export-format-template "2.1.0")

(make-obsolete 'fountain-insert-metadata
               'auto-insert "2.1.2")

(make-obsolete-variable 'fountain-metadata-template
                        'fountain-metadata-skeleton "2.1.2")

(make-obsolete-variable 'fountain-long-time-format
                        'fountain-time-format "2.1.2")

(define-obsolete-variable-alias 'fountain-short-time-format
  'fountain-time-format "2.1.2")

(make-obsolete-variable 'fountain-export-templates
                        "use individual export templates instead." "2.1.4")

(make-obsolete-variable 'fountain-export-format-replace-alist
                        "use individual export replace alists instead." "2.1.4")

(make-obsolete-variable 'fountain-export-title-format
                        "edit `fountain-export-title-template' instead." "2.1.4")

(define-obsolete-variable-alias 'fountain-trans-list
  'fountain-trans-suffix-list "2.2.2")


;;; Customization

(defcustom fountain-mode-hook
  '(turn-on-visual-line-mode)
  "Mode hook for `fountain-mode', run after the mode is turned on."
  :type 'hook
  :group 'fountain)

(defcustom fountain-scene-heading-prefix-list
  '("INT" "EXT" "EST" "INT/EXT" "I/E")
  "List of scene heading prefixes (case insensitive).
Any scene heading prefix can be followed by a dot and/or a space,
so the following are equivalent:

    INT HOUSE - DAY

    INT. HOUSE - DAY

Call `fountain-mode' again for changes to take effect."
  :type '(repeat (string :tag "Prefix"))
  :group 'fountain)

(defcustom fountain-trans-suffix-list
  '("TO:" "WITH:" "FADE OUT" "TO BLACK")
  "List of transition suffixes (case insensitive).
This list is used to match the endings of transitions,
e.g. `TO:' will match both the following:

    CUT TO:

    DISSOLVE TO:

Call `fountain-mode' again for changes to take effect."
  :type '(repeat (string :tag "Suffix"))
  :group 'fountain)

(defcustom fountain-add-continued-dialog
  t
  "\\<fountain-mode-map>If non-nil, \\[fountain-continued-dialog-refresh] will mark continued dialogue.

When non-nil, append `fountain-continued-dialog-string' to
successively speaking characters with `fountain-continued-dialog-refresh'.

When nil, remove `fountain-continued-dialog-string' with
`fountain-continued-dialog-refresh'."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-continued-dialog-string
  "(CONT'D)"
  "String to append to character name speaking in succession.
If `fountain-add-continued-dialog' is non-nil, append this string
to character when speaking in succession.

WARNING: if you change this variable then call
`fountain-continued-dialog-refresh', strings matching the
previous value will not be recognized. Before changing this
variable, first make sure to set `fountain-add-continued-dialog'
to nil and run `fountain-continued-dialog-refresh', then make the
changes desired."
  :type 'string
  :group 'fountain)

(defcustom fountain-block-limit
  10000
  "Integer to limit fontification block in characters.
Used by `fountain-font-lock-extend-region'.

Sometimes `font-lock-mode' can hang if asked for fontify a very
large block of unbroken text. If you experience performance
issues, consider reducing this value."
  :type 'integer
  :group 'fountain)

(defcustom fountain-switch-comment-syntax
  nil
  "\\<fountain-mode-map>If non-nil, use `//' as default comment syntax (boneyard).
Two syntaxes are supported:

    /* this text is a comment */

    // this text is
    // also a comment

Both syntax will be recognized as comments. This option changes
the behavior of \\[comment-dwim]. The default is the former but
if you prefer the latter, set this option to non-nil."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-hide-emphasis-delim
  nil
  "If non-nil, make emphasis delimiters invisible."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-hide-syntax-chars
  nil
  "If non-nil, make syntax characters invisible."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-time-format
  "%F"
  "Format of date and time. See `format-time-string'."
  :type 'string
  :group 'fountain)

(defcustom fountain-note-template
  " {{time}} - {{fullname}}: "
  "\\<fountain-mode-map>Template for inserting notes with \\[fountain-insert-note].
To include an item in a template you must use the full {{KEY}}
syntax.

    {{title}}    Buffer name without extension
    {{time}}     Short date format (defined in `fountain-time-format')
    {{fullname}} User full name (defined in `user-full-name')
    {{nick}}     User first name (defined in `user-login-name')
    {{email}}    User email (defined in `user-mail-address')

The default {{time}} - {{fullname}}: will insert something like:

\[\[ 2014-20-01 - Alan Smithee: \]\]"
  :type 'string
  :group 'fountain)


;;; Aligning

(defgroup fountain-align ()
  "Options for element alignment.

For each Fountain element this group contains a variable that can
be an integer representing align column for that element for all
formats, or a list where each element takes the form:

    (FORMAT INT)

Where FORMAT is a string and INT is the align column for that
format.

To disable element alignment, see `fountain-align-element'."
  :prefix "fountain-align-"
  :group 'fountain)

(defcustom fountain-align-elements
  t
  "If non-nil, elements will be displayed auto-aligned.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain-align)

(defcustom fountain-align-section-heading
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which section headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-scene-heading
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which scene headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-synopsis
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which synopses should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-action
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 20))
  "Column integer to which action should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-character
  '(("screenplay" 20)
    ("teleplay" 20)
    ("stageplay" 30))
  "Column integer to which characters names should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-dialog
  '(("screenplay" 10)
    ("teleplay" 10)
    ("stageplay" 0))
  "Column integer to which dialog should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-paren
  '(("screenplay" 15)
    ("teleplay" 15)
    ("stageplay" 20))
  "Column integer to which parentheticals should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-trans
  '(("screenplay" 45)
    ("teleplay" 45)
    ("stageplay" 30))
  "Column integer to which transitions should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-center
  '(("screenplay" 20)
    ("teleplay" 20)
    ("stageplay" 20))
  "Column integer to which centered text should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-display-scene-numbers-in-margin
  nil
  "If non-nil, display scene numbers in the right margin.

If nil, do not change scene number display.

This option does affect file contents."
  :type 'boolean
  :group 'fountain-align)

(define-obsolete-variable-alias 'fountain-align-scene-number
  'fountain-display-scene-numbers-in-margin "2.3.0")

(defun fountain-get-align (element)
  "Return ELEMENT align integer based on buffer format."
  (if (integerp element) element
    (let ((format (or (plist-get (fountain-read-metadata)
                                 'format)
                      "screenplay")))
      (cadr (or (assoc format element)
                (car element))))))


;;; Autoinsert

(require 'autoinsert)

(defvar fountain-metadata-skeleton
  '(nil
    "title: " (skeleton-read "Title: " (file-name-base (buffer-name))) | -7 "\n"
    "credit: " (skeleton-read "Credit: " "written by") | -9 "\n"
    "author: " (skeleton-read "Author: " user-full-name) | -9 "\n"
    "format: " (skeleton-read "Script format: " "screenplay") | -9 "\n"
    "source: " (skeleton-read "Source: ") | -9 "\n"
    "date: " (skeleton-read "Date: " (format-time-string fountain-time-format)) | -7 "\n"
    "contact:\n" ("Contact details, %s: " "    " str | -4 "\n") | -9))

(define-auto-insert '(fountain-mode . "Fountain metadata skeleton")
  fountain-metadata-skeleton)


;;; Regular Expressions

(defvar fountain-scene-heading-regexp
  nil
  "Regular expression for matching scene headings.
Set with `fountain-init-scene-heading-regexp'.

    Group 1: match trimmed whitespace
    Group 2: match leading . (for forced element)
    Group 3: match scene heading without scene number (export group)
    Group 4: match space before scene number
    Group 5: match first # delimiter
    Group 6: match scene number
    Group 7: match last # delimiter

Requires `fountain-match-scene-heading' for preceding blank line.")

(defvar fountain-scene-number-regexp
  "\\(?4:[\s\t]+\\)\\(?5:#\\)\\(?6:[a-z0-9\\.-]+\\)\\(?7:#\\)"
  "Regular expression for matching scene numbers.

    Group 4: match space before scene number
    Group 5: match first # delimiter
    Group 6: match scene number
    Group 7: match last # delimiter")

(defvar fountain-trans-regexp
  nil
  "Regular expression for matching transitions.

    Group 1: match trimmed whitespace
    Group 2: match forced transition mark
    Group 3: match transition (export group)

Set with `fountain-init-trans-regexp'. Requires
`fountain-match-trans' for preceding and succeeding blank lines.")

(defconst fountain-blank-regexp
  "^\s?$"
  "Regular expression for matching an empty line.")

(defconst fountain-action-regexp
  "^\\(!\\)?\\(.*\\)[\s\t]*$"
  "Regular expression for forced action.

    Group 1: match forced action mark
    Group 2: match trimmed whitespace (export group)")

(defconst fountain-nbsp-regexp
  "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:\\\\\\)\s\\)"
  "Regular expression for non-breaking space.")

(defconst fountain-comment-regexp
  (concat "\\(?://[\s\t]*\\(?:.*\\)\\)"
          "\\|"
          "\\(?:\\(?:/\\*\\)[\s\t]*\\(?:\\(?:.\\|\n\\)*?\\)[\s\t]*\\*/\\)")
  "Regular expression for matching comments.")

(defconst fountain-metadata-regexp
  (concat "^\\(?1:\\(?2:[^:\n]+\\):[\s\t]*\\(?3:.+\\)?\\)"
          "\\|"
          "^[\s\t]+\\(?1:\\(?3:.+\\)\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-match-metadata' for `bobp'.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*\\(?1:\\(?:"
          "\\(?2:@\\)\\(?3:\\(?4:[^<>\n]+?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\|"
          "\\(?3:\\(?4:[^a-z<>\n]*?[A-Z][^a-z<>\n]*?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\)[\s\t]*\\(?5:\\^\\)?\\)[\s\t]*$")
  "Regular expression for matching character names.

    Group 1: match trimmed whitespace
    Group 2: match leading @ (for forced element)
    Group 3: match character name and parenthetical (export group)
    Group 4: match character name only
    Group 5: match trailing ^ (for dual dialog)

Requires `fountain-match-character' for preceding blank line.")

(defconst fountain-dialog-regexp
  (concat "^\\(\s\s\\)$"
          "\\|"
          "^[\s\t]*\\(?1:[^<>\n]+?\\)[\s\t]*$")
  "Regular expression for matching dialogue.

    Group 1: match trimmed whitespace

Requires `fountain-match-dialog' for preceding character,
parenthetical or dialogue.")

(defconst fountain-paren-regexp
  (concat "^[\s\t]*\\(([^)\n]*)\\)[\s\t]*$")
  "Regular expression for matching parentheticals.

    Group 1: match trimmed whitespace (export group)

Requires `fountain-match-paren' for preceding character or
dialogue.")

(defconst fountain-page-break-regexp
  "^[\s\t]*\\(=\\{3,\\}\\)[\s\t]*\\([a-z0-9\\.-]+\\)?.*$"
  "Regular expression for matching page breaks.

    Group 1: leading ===
    Group 2: forced page number (export group)")

(defconst fountain-end-regexp
  "^[\s\t]*\\(=\\{3,\\}\\)[\s\t]*\\(end\\)\\>.*$"
  "Regular expression for matching script end break.

    Group 1: leading ===
    Group 2: end")

(defconst fountain-note-regexp
  "\\(\\[\\[[\s\t]*\\(\\(?:.\n?\\)*?\\)[\s\t]*]]\\)"
  "Regular expression for matching notes.

    Group 1: note including [[ ]] delimiters
    Group 2: note (export group)")

(defconst fountain-section-heading-regexp
  "^\\(?1:\\(?2:#\\{1,5\\}\\)[\s\t]*\\(?3:[^#\n].*?\\)\\)[\s\t]*$"
  "Regular expression for matching section headings.

    Group 1: match trimmed whitespace
    Group 2: match leading #'s
    Group 3: match heading (export group)")

(defconst fountain-synopsis-regexp
  "^\\(\\(=[\s\t]*\\)\\([^=\n].+?\\)\\)[\s\t]*$"
  "Regular expression for matching synopses.

    Group 1: match trimmed whitespace
    Group 2: leading =
    Group 3: synopsis (export group)")

(defconst fountain-center-regexp
  "^[\s\t]*\\(?1:\\(?2:>[\s\t]*\\)\\(?3:.*?\\)\\(?4:[\s\t]*<\\)\\)[\s\t]*$"
  "Regular expression for matching centered text.

    Group 1: match trimmed whitespace
    Group 2: match leading > and whitespace
    Group 3: match center text (export group)
    Group 4: match trailing whitespace and <")

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

(defconst fountain-template-key-regexp
  "{{\\([^{}\n]+?\\)}}"
  "Regular expression key for making template replacements.")


;;; Faces

(defgroup fountain-faces ()
  "Faces used in `fountain-mode'.
There are three levels of `font-lock-mode' decoration:

    1 (minimum):
        Comments
        Syntax Characters

    2 (default):
        Comments
        Syntax Characters
        Metadata
        Scene Headings
        Section Headings
        Synopses
        Notes

    3 (maximum):
        Comments
        Syntax Characters
        Metadata Keys
        Metadata Values
        Section Headings
        Scene Headings
        Synopses
        Notes
        Character Names
        Parentheticals
        Dialog
        Transitions
        Center Text

To switch between these levels, customize the value of
`font-lock-maximum-decoration'. This can be set with
\\[fountain-set-font-lock-decoration]."
  :prefix "fountain-"
  :link '(info-link "(emacs)Font Lock")
  :group 'fountain)

(defface fountain-action
  '((t nil))
  "Default face for action."
  :group 'fountain-faces)

(defface fountain-comment
  '((t (:inherit shadow)))
  "Default face for comments (boneyard)."
  :group 'fountain-faces)

(defface fountain-non-printing
  '((t (:inherit fountain-comment)))
  "Default face for emphasis delimiters and syntax characters."
  :group 'fountain-faces)

(defface fountain-metadata-key
  '((t (:inherit font-lock-constant-face)))
  "Default face for metadata keys."
  :group 'fountain-faces)

(defface fountain-metadata-value
  '((t (:inherit font-lock-keyword-face)))
  "Default face for metadata values."
  :group 'fountain-faces)

(defface fountain-page-break
  '((t (:inherit font-lock-constant-face)))
  "Default face for page breaks."
  :group 'fountain-faces)

(defface fountain-page-number
  '((t (:inherit font-lock-warning-face)))
  "Default face for page numbers."
  :group 'fountain-faces)

(defface fountain-scene-heading
  '((t (:inherit font-lock-function-name-face)))
  "Default face for scene headings."
  :group 'fountain-faces)

(defface fountain-paren
  '((t (:inherit font-lock-builtin-face)))
  "Default face for parentheticals."
  :group 'fountain-faces)

(defface fountain-center
  '((t nil))
  "Default face for centered text."
  :group 'fountain-faces)

(defface fountain-note
  '((t (:inherit font-lock-comment-face)))
  "Default face for notes."
  :group 'fountain-faces)

(defface fountain-section-heading
  '((t (:inherit font-lock-keyword-face)))
  "Default face for section headings."
  :group 'fountain-faces)

(defface fountain-synopsis
  '((t (:inherit font-lock-type-face)))
  "Default face for synopses."
  :group 'fountain-faces)

(defface fountain-character
  '((t (:inherit font-lock-variable-name-face)))
  "Default face for characters."
  :group 'fountain-faces)

(defface fountain-dialog
  '((t (:inherit font-lock-string-face)))
  "Default face for dialog."
  :group 'fountain-faces)

(defface fountain-trans
  '((t (:inherit font-lock-builtin-face)))
  "Default face for transitions."
  :group 'fountain-faces)

(defface fountain-include
  '((t (:inherit font-lock-warning-face)))
  "Default face for file inclusions."
  :group 'fountain-faces)


;;; Initializing

(defun fountain-init-scene-heading-regexp ()
  "Initialize scene heading regular expression.
Uses `fountain-scene-heading-prefix-list' to create non-forced
scene heading regular expression."
  (setq fountain-scene-heading-regexp
        (concat
         ;; First match forced scene heading.
         "^\\(?1:\\(?2:\\.\\)\\(?3:\\<.*?\\)"
         "\\(?:" fountain-scene-number-regexp "\\)?"
         "\\)[\s\t]*$"
         ;; Or match omitted scene.
         "\\|"
         "^\\(?1:\\(?3:OMIT\\(?:TED\\)?\\)"
         "\\(?:" fountain-scene-number-regexp "\\)?"
         "\\)[\s\t]*$"
         ;; Or match regular scene heading.
         "\\|"
         "^\\(?1:\\(?3:"
         (regexp-opt fountain-scene-heading-prefix-list)
         "[.\s\t].*?\\)"
         "\\(?:" fountain-scene-number-regexp "\\)?"
         "\\)[\s\t]*$")))

(defun fountain-init-trans-regexp ()
  "Initialize transition regular expression.
Uses `fountain-trans-suffix-list' to create non-forced tranistion
regular expression."
  (setq fountain-trans-regexp
        (concat
         ;; First match forced transition.
         "^[\s\t]*\\(?1:\\(?2:>[\s\t]*\\)\\(?3:[^<>\n]*?\\)\\)[\s\t]*$"
         ;; Or match regular transition.
         "\\|"
         "^[\s\t]*\\(?1:\\(?3:[[:upper:]\s\t]*"
         (upcase (regexp-opt fountain-trans-suffix-list))
         "\\)\\)[\s\t]*$")))

(defun fountain-init-outline-regexp ()
  "Initialize `outline-regexp'."
  (setq-local outline-regexp
              (concat fountain-end-regexp
                      "\\|"
                      fountain-section-heading-regexp
                      "\\|"
                      fountain-scene-heading-regexp)))

(defun fountain-init-imenu-generic-expression () ; FIXME: allow user customize
  "Initialize `imenu-generic-expression'."
  (setq imenu-generic-expression
        (list
         (list "Notes" fountain-note-regexp 2)
         (list "Scene Headings" fountain-scene-heading-regexp 3)
         (list "Sections" fountain-section-heading-regexp 1))))

(defun fountain-init-comment-syntax ()
  "Set comment syntax according to `fountain-switch-comment-syntax'."
  (setq-local comment-start
              (if fountain-switch-comment-syntax "//" "/*"))
  (setq-local comment-end
              (if fountain-switch-comment-syntax "" "*/")))

(defun fountain-init-vars ()
  "Initialize important variables.
These are required for functions to operate with temporary buffers."
  (fountain-init-scene-heading-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-comment-syntax)
  (setq-local comment-use-syntax t)
  (setq-local page-delimiter fountain-page-break-regexp)
  (setq-local outline-level #'fountain-outline-level)
  (setq-local require-final-newline mode-require-final-newline))


;;; Emacs Bugs

(defcustom fountain-patch-emacs-bugs
  t
  "If non-nil, attempt to patch known bugs in Emacs.
See function `fountain-patch-emacs-bugs'."
  :type 'boolean
  :group 'fountain)

(defun fountain-patch-emacs-bugs ()
  "Attempt to patch known bugs in Emacs.

Disable `show-paren-mode' in local buffers.
See <https://debbugs.gnu.org/29381>

In Emacs versions prior to 26, adds advice to override
`outline-invisible-p' to return non-nil only if the character
after POS or point has invisible text property eq to 'outline.
See <http://debbugs.gnu.org/24073>."
  ;; `show-paren-mode' is erroneously created as a global minor mode, treating
  ;; all text like code. This is not what sane people want. Make it buffer-local
  ;; and set to nil.
  (unless (assq 'show-paren-mode (buffer-local-variables (current-buffer)))
    (setq-local show-paren-mode nil)
    (message "fountain-mode: `show-paren-mode' disabled in current buffer"))
  ;; In Emacs version prior to 26, `outline-invisible-p' returns non-nil for ANY
  ;; invisible property of text at point:
  ;;
  ;; (get-char-property (or pos (point)) 'invisible))
  ;;
  ;; We want to only return non-nil if property is 'outline
  (unless (or (advice-member-p 'fountain-outline-invisible-p 'outline-invisible-p)
              (<= 26 emacs-major-version))
    (advice-add 'outline-invisible-p :override 'fountain-outline-invisible-p)
    ;; Because `outline-invisible-p' is an inline function, we need to
    ;; reevaluate those functions that called the original bugged version.
    ;; This is impossible for users who have installed Emacs without
    ;; uncompiled source, so we need to demote errors.
    (with-demoted-errors "Error: %S"
      (dolist (fun '(outline-back-to-heading
                     outline-on-heading-p
                     outline-next-visible-heading))
        (let ((source (find-function-noselect fun)))
          (with-current-buffer (car source)
            (goto-char (cdr source))
            (eval (read (current-buffer))))))
      (message "fountain-mode: Function `outline-invisible-p' has been patched"))))


;;; Element Matching

(defun fountain-blank-p ()
  "Return non-nil if point is at a blank line."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      ;; don't modify match-data
      (looking-at-p fountain-blank-regexp))))

(defun fountain-tachyon-p ()
  "Return non-nil if point is at a non-interfering element.
These include blank lines, section headings, synopses, notes, and
comments."
  (or (fountain-blank-p)
      (fountain-match-comment)
      (fountain-match-section-heading) ; FIXME: what about stageplays?
      (fountain-match-synopsis)
      (fountain-match-note)))

(defun fountain-match-metadata ()
  "Match metadata if point is at metadata, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-metadata-regexp)
           (or (bobp)
               (save-match-data
                 (forward-line -1)
                 (fountain-match-metadata)))))))

(defun fountain-match-page-break ()
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-page-break-regexp))))

(defun fountain-match-section-heading ()
  "Match section heading if point is at section heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-section-heading-regexp))))

(defun fountain-match-synopsis ()
  "Match synopsis if point is at synopsis, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-synopsis-regexp))))

(defun fountain-match-note ()
  "Match note if point is at a note, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (or (looking-at fountain-note-regexp)
          (let ((x (point)))
            (when (re-search-backward fountain-blank-regexp nil t)
              (goto-char (match-end 0))
              (skip-chars-forward "\n\r\s\t"))
            (and (looking-at fountain-note-regexp)
                 (< x (match-end 0))))))))

(defun fountain-match-comment ()            ; FIXME: does not see "//" comments
  "Match comment if point is at a comment, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (if (eq (char-before) ?*) (forward-char -1))
      (let ((x (point))
            beg end)
        (search-forward "*/" nil t)
        (setq end (point-marker))
        (if (and (forward-comment -1)
                 (setq beg (point-marker))
                 (<= beg x end))
            (progn (set-match-data (list beg end) t)
                   t))))))

(defalias 'fountain-match-boneyard 'fountain-match-comment)

(defun fountain-match-scene-heading ()
  "Match scene heading if point is at a scene heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-scene-heading-regexp)
           (save-match-data
             (forward-line -1)
             (or (bobp)
                 (fountain-tachyon-p)))))))

(defun fountain-match-character ()
  "Match character if point is at character, nil otherwise."
  (unless (fountain-match-scene-heading)
    (save-excursion
      (forward-line 0)
      (and (not (and (looking-at fountain-action-regexp)
                     (match-string 1)))
           (let ((case-fold-search nil))
             (looking-at fountain-character-regexp))
           (save-match-data
             (save-restriction
               (widen)
               (and (save-excursion
                      (forward-line -1)
                      (fountain-tachyon-p))
                    (save-excursion
                      (forward-line 1)
                      (unless (eobp)
                        (not (fountain-tachyon-p)))))))))))

(defun fountain-match-dialog ()
  "Match dialog if point is at dialog, nil otherwise."
  (unless (or (fountain-blank-p)
              (fountain-match-paren)
              (fountain-match-note))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (and (looking-at fountain-dialog-regexp)
             (save-match-data
               (unless (bobp)
                 (forward-line -1)
                 (or (fountain-match-character)
                     (fountain-match-paren)
                     (fountain-match-dialog)))))))))

(defun fountain-match-paren ()
  "Match parenthetical if point is at a paranthetical, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (and (looking-at fountain-paren-regexp)
           (save-match-data
             (unless (bobp)
               (forward-line -1)
               (or (fountain-match-character)
                   (fountain-match-dialog))))))))

(defun fountain-match-trans ()
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
                   (fountain-tachyon-p))))
           (save-match-data
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-tachyon-p))))))))

(defun fountain-match-center ()
  "Match centered text if point is at centered text, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-center-regexp))))

(defun fountain-match-action ()
  "Match action text if point is at action, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (or (and (looking-at fountain-action-regexp)
               (match-string 1))
          (and (not (or (fountain-blank-p)
                        (fountain-match-comment)
                        (fountain-match-metadata)
                        (fountain-match-section-heading)
                        (fountain-match-scene-heading)
                        (fountain-match-character)
                        (fountain-match-dialog)
                        (fountain-match-paren)
                        (fountain-match-trans)
                        (fountain-match-center)
                        (fountain-match-synopsis)
                        (fountain-match-note)))
               (looking-at fountain-action-regexp))))))

(defun fountain-get-element ()
  "Return element at point as a symbol."
  (cond
   ((fountain-match-metadata) 'metadata)
   ((fountain-match-section-heading) 'section-heading)
   ((fountain-match-scene-heading) 'scene-heading)
   ((fountain-match-character) 'character)
   ((fountain-match-dialog) 'lines)
   ((fountain-match-paren) 'paren)
   ((fountain-match-trans) 'trans)
   ((fountain-match-center) 'center)
   ((fountain-match-synopsis) 'synopsis)
   ((fountain-match-note) 'note)
   ((fountain-match-page-break) 'page-break)
   (t 'action)))


;;; Inclusions

(defvar fountain-include-regexp
  "^[\s\t]*{{[\s\t]*\\(?1:[^:\n]+:\\)?[\s\t]*\\(?2:.+?\\)[\s\t]*}}[\s\t]*")

(defun fountain-match-include ()
  "Match inclusion if point is at inclusion, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-include-regexp))))

(defun fountain-include-find-file (&optional arg)
  "\\<fountain-mode-map>Find included file at point.

If called with ARG (\\[universal-argument] \\[fountain-include-find-file]), find file in same window,
otherwise find file in other window.

If called noninteractively, find file without selecting window."
  (interactive "P")
  (if (and (fountain-match-include)
           (save-match-data
             (string-match "include:" (match-string 1))))
      (let ((filename (expand-file-name (match-string 2))))
        (if (called-interactively-p)
            (if arg
                (find-file filename)
              (find-file-other-window filename))
          (find-file-noselect filename)))))


;;; Parsing

(require 'subr-x)

(defun fountain-get-character (&optional n limit)
  "Return Nth next character (or Nth previous if N is negative).
If N is non-nil, return Nth next character or Nth previous
character if N is negative, otherwise return nil. If N is nil or
0, return character at point, otherwise return nil.

If LIMIT is 'scene, halt at next scene heading. If LIMIT is
'dialog, halt at next non-dialog element."
  (let ((n (or n 0)))
    (save-excursion
      (save-restriction
        (widen)
        (fountain-forward-character n limit)
        (if (fountain-match-character)
            (match-string-no-properties 4))))))

(defun fountain-read-metadata ()
  "Read metadata of current buffer and return as a property list.

Key string is slugified using `fountain-slugify', and interned.
Value string remains a string. e.g.

    Draft date: 2015-12-25 -> (draft-date \"2015-12-25\")"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (list)
        (while (fountain-match-metadata)
          (let ((key (match-string 2))
                (value (match-string-no-properties 3)))
            (forward-line 1)
            (while (and (fountain-match-metadata)
                        (null (match-string 2)))
              (setq value
                    (concat value (if value "\n")
                            (match-string-no-properties 3)))
              (forward-line 1))
            (setq list
                  (append list (list (intern (fountain-slugify key))
                                     value)))))
        list))))

(defun fountain-count-lines (beg end &optional element)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char beg)
      (let ((x (point))
            (line-count 0)
            (line-width
             (cdr (symbol-value
                   (plist-get (alist-get (or element (fountain-element))
                                         fountain-elements)
                              :fill)))))
        (while (< (point) end)
          (while (< (point) (+ x line-width))
            (forward-word 1))
          (if (< (+ x line-width) (point))
              (forward-word -1))
          (setq x (point)
                line-count (1+ line-count)))
        line-count))))

(defun fountain-dual-dialog (&optional x)
  "Non-nil if point or X is within dual dialogue.
Returns \"right\" if within right-side dual dialogue, \"left\" if
within left-side dual dialogue, and nil otherwise."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (if x (goto-char x))
        (cond ((progn (fountain-forward-character 0 'dialog)
                      (and (fountain-match-character)
                           (stringp (match-string 5))))
               'right)
              ((progn (fountain-forward-character 1 'dialog)
                      (and (fountain-match-character)
                           (stringp (match-string 5))))
               'left))))))

(defun fountain-starts-new-page (&optional limit) ; FIXME: implement LIMIT
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (forward-line 0)
        (skip-chars-backward "\n\r\s\t")
        (fountain-match-page-break)))))

(defun fountain-parse-goto-end-of-subtree (x)
  (save-excursion
    (goto-char x)
    (outline-end-of-subtree)
    (unless (eobp)
      (forward-char 1))
    (let ((end (point)))
      (goto-char x)
      (if (and (re-search-forward fountain-include-regexp end t)
               (save-match-data
                 (string-match "include:" (match-string 1))))
          (match-beginning 0)
        end))))

(defun fountain-parse-section (match-data &optional export-elements job)
  "Return an element list for matched section heading."
  (set-match-data match-data)
  (let* ((beg (match-beginning 0))
         (starts-new-page (fountain-starts-new-page))
         (section-heading
          (list 'section-heading
                (list 'begin (match-beginning 0)
                      'end (match-end 0)
                      'level (save-excursion
                               (goto-char (match-beginning 0))
                               (funcall outline-level))
                      'export t)
                (match-string-no-properties 3)))
         (end (fountain-parse-goto-end-of-subtree beg))
         content)
    (goto-char (plist-get (nth 1 section-heading) 'end))
    (setq content (fountain-parse-region (point) end export-elements job))
    (list 'section
          (list 'begin beg
                'end end
                'starts-new-page starts-new-page
                'export t)
          (cons section-heading content))))

(defun fountain-parse-scene (match-data &optional export-elements job)
  "Return an element list for matched scene heading at point.
Includes child elements."
  (set-match-data match-data)
  (let* ((beg (match-beginning 0))
         (starts-new-page (fountain-starts-new-page))
         (scene-number
          (save-excursion
            (save-match-data
              (goto-char (match-beginning 0))
              (fountain-scene-number-to-string
               (fountain-get-scene-number 0)))))
         (scene-heading
          (list 'scene-heading
                (list 'begin (match-beginning 0)
                      'end (match-end 0)
                      'scene-number scene-number
                      'forced (stringp (match-string 2))
                      'export t
                      'starts-new-page starts-new-page)
                (match-string-no-properties 3)))
         (end (fountain-parse-goto-end-of-subtree beg))
         content)
    (goto-char (plist-get (nth 1 scene-heading) 'end))
    (setq content (fountain-parse-region (point) end export-elements job))
    (list 'scene
          (list 'begin beg
                'end end
                'scene-number scene-number
                'starts-new-page starts-new-page
                'export t)
          (cons scene-heading content))))

(defun fountain-parse-dialog (match-data &optional export export-elements job)
  (set-match-data match-data)
  (let* ((beg (match-beginning 0))
         (starts-new-page (fountain-starts-new-page))
         (dual (fountain-dual-dialog))
         (character
          (list 'character
                (list 'begin (match-beginning 0)
                      'end (match-end 0)
                      'forced (stringp (match-string 2))
                      'export (if export t)
                      'starts-new-page (unless (eq dual 'left) starts-new-page))
                (match-string-no-properties 3)))
         (end
          (save-excursion
            (fountain-forward-character 1 'dialog)
            (skip-chars-backward "\n\r\s\t")
            (point)))
         first-dialog)
    (goto-char (plist-get (nth 1 character) 'end))
    ;; Parse the first dialogue tree, which may be the only dialogue tree.
    (setq first-dialog
          (list 'dialog
                (list 'begin beg
                      'end end
                      'dual dual
                      'export t)
                (cons character (fountain-parse-region (point) end export-elements job))))
    ;; If at the first (left) character of dual dialogue, parse a dual-dialogue
    ;; tree, containing dialogue trees.
    (if (eq dual 'left)
        ;; Find the end of the dual-dialogue.
        (let ((end
               (save-excursion
                 (while (fountain-dual-dialog)
                   (fountain-forward-character 1 'dialog))
                 (skip-chars-backward "\n\r\s\t")
                 (point))))
          ;; Return the dual-dialogue tree.
          (list 'dual-dialog
                (list 'begin beg
                      'end end
                      'starts-new-page starts-new-page
                      'export (if export t))
                ;; Add the first dialogue block to the head of the dual-dialogue
                ;; tree.
                (cons first-dialog
                      ;; Parse the containing region.
                      (fountain-parse-region (plist-get (nth 1 first-dialog) 'end)
                                             end export-elements job))))
      ;; Otherwise, return the first dialogue tree.
      first-dialog)))

(defun fountain-parse-lines (match-data &optional export)
  "Return an element list for matched dialogue."
  (set-match-data match-data)
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (list 'lines
          (list 'begin beg
                'end end
                'export (if export t))
          (match-string-no-properties 1))))

(defun fountain-parse-paren (match-data &optional export)
  "Return an element list for matched parenthetical."
  (set-match-data match-data)
  (list 'paren
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (if export t))
        (match-string-no-properties 1)))

(defun fountain-parse-trans (match-data &optional export)
  "Return an element list for matched transition."
  (set-match-data match-data)
  (list 'trans
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'forced (stringp (match-string 2))
              'export (if export t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 3)))

(defun fountain-parse-center (match-data &optional export)
  "Return an element list for matched center text."
  (list 'center
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (if export t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 3)))

(defun fountain-parse-page-break (match-data &optional export)
  "Return an element list for matched page break."
  (set-match-data match-data)
  (list 'page-break
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (if export t))
        (match-string-no-properties 2)))

(defun fountain-parse-synopsis (match-data &optional export)
  "Return an element list for matched synopsis."
  (set-match-data match-data)
  (list 'synopsis
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (if export t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 3)))

(defun fountain-parse-note (match-data &optional export)
  "Return an element list for matched note."
  (set-match-data match-data)
  (list 'note
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (if export t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 2)))

(defun fountain-parse-include (match-data &optional export)
  "Return an element list for matched inclusions."
  (set-match-data match-data)
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        content)
    (goto-char beg)
    (with-current-buffer (fountain-include-find-file nil)
      (setq content (fountain-parse-region (point-min) (point-max))))
    (list 'include
          (list 'begin beg
                'end end
                'export (if export t))
          content)))

(defun fountain-parse-action (match-data &optional export)
  "Return an element list for matched action."
  (set-match-data match-data)
  (let ((beg (match-beginning 0))
        (end
         (save-excursion
           (save-match-data
             (goto-char (match-beginning 0))
             (re-search-forward fountain-blank-regexp nil 'move)
             (skip-chars-backward "\n\r\s\t")
             (point))))
        string)
    (setq string (buffer-substring-no-properties (match-beginning 2) end)
          string (replace-regexp-in-string "^!" "" string))
    (list 'action
          (list 'begin beg
                'end end
                'forced (stringp (match-string 1))
                'export (if export t)
                'starts-new-page (fountain-starts-new-page))
          string)))

(defun fountain-parse-element (&optional export-elements job)
  "Call appropropriate element parsing function for matched element at point."
  (cond
   ((fountain-match-section-heading)
    (fountain-parse-section
     (match-data) export-elements job))
   ((fountain-match-scene-heading)
    (fountain-parse-scene
     (match-data) export-elements job))
   ((fountain-match-character)
    (fountain-parse-dialog
     (match-data) (or (memq 'character export-elements)
                      (memq 'lines export-elements)
                      (memq 'paren export-elements))
     export-elements job))
   ((fountain-match-dialog)
    (fountain-parse-lines
     (match-data) (memq 'lines export-elements)))
   ((fountain-match-paren)
    (fountain-parse-paren
     (match-data) (memq 'paren export-elements)))
   ((fountain-match-trans)
    (fountain-parse-trans
     (match-data) (memq 'trans export-elements)))
   ((fountain-match-center)
    (fountain-parse-center
     (match-data) (memq 'center export-elements)))
   ((fountain-match-synopsis)
    (fountain-parse-synopsis
     (match-data) (memq 'synopsis export-elements)))
   ((fountain-match-note)
    (fountain-parse-note
     (match-data) (memq 'note export-elements)))
   ((fountain-match-include)
    (fountain-parse-include
     (match-data) (memq 'include export-elements)))
   ((fountain-match-page-break)
    (fountain-parse-page-break
     (match-data) (memq 'page-break export-elements)))
   (t
    (fountain-match-action)
    (fountain-parse-action
     (match-data) (memq 'action export-elements)))))

(defun fountain-parse-region (beg end &optional export-elements job) ; FIXME: needs temp buffer model
  "Return a list of parsed element lists in region between BEG and END.

Use list EXPORT-ELEMENTS to determine exported elements, or
create new list.

Ignores blank lines, comments and metadata. Calls
`fountain-parse-element' and adds element list to list, then
moves to property value \"end\" of element."
  (unless export-elements
    (setq export-elements
          (cdr (or (assoc-string
                    (or (plist-get (fountain-read-metadata) 'format)
                        "screenplay")
                    fountain-export-include-elements)
                   (car fountain-export-include-elements)))))
  (goto-char beg)
  (let (list job-working)
    (if job
        (setq job-working t)
      (setq job (make-progress-reporter "Parsing...")))
    (while (< (point) (min end (point-max)))
      (while (or (looking-at "\n*\s?\n")
                 (fountain-match-metadata)
                 (fountain-match-comment))
        (goto-char (match-end 0)))
      (if (< (point) end)
          (let ((element (fountain-parse-element export-elements job)))
            (push element list)
            (goto-char (plist-get (nth 1 element) 'end))))
      (progress-reporter-update job))
    (unless job-working (progress-reporter-done job))
    (reverse list)))


;;; Exporting

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

(defcustom fountain-export-include-elements
  '(("screenplay" title-page scene-heading action character lines paren trans center page-break include)
    ("teleplay" title-page section-heading scene-heading action character lines paren trans center page-break include)
    ("stageplay" title-page section-heading scene-heading action character lines paren trans center page-break include))
  "Association list of elements to include when exporting.
Note that comments (boneyard) are never included."
  :type '(alist :key-type (string :tag "Format")
                :value-type (set :tag "Elements"
                                 (const :tag "Title Page" title-page)
                                 (const :tag "Section Headings" section-heading)
                                 (const :tag "Scene Headings" scene-heading)
                                 (const :tag "Action" action)
                                 (const :tag "Character Names" character)
                                 (const :tag "Dialogue" lines)
                                 (const :tag "Parentheticals" paren)
                                 (const :tag "Transitions" trans)
                                 (const :tag "Center Text" center)
                                 (const :tag "Page Breaks" page-break)
                                 (const :tag "Synopses" synopsis)
                                 (const :tag "Notes" note)
                                 (const :tag "Included Files" include)))
  :group 'fountain-export)

(define-obsolete-variable-alias 'fountain-export-include-elements-alist
  'fountain-export-include-elements "3.0.0")

(defcustom fountain-export-standalone
  t
  "If non-nil, export a standalone document.

A standalone document is formatted with the export format's
document template in `fountain-export-templates'.

If nil, export snippet, which only formats each element. This is
useful when exporting parts of a script for inclusion in another
document."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-buffer-name
  "*Fountain %s Export*"
  "Name of export buffer when source buffer is not visiting a file.
Passed to `format' with export format as single variable."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-ps
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type '(radio (function-item fountain-export-buffer-to-ps)
                (function-item fountain-export-buffer-to-html)
                (function-item fountain-export-buffer-to-fdx)
                (function-item fountain-export-buffer-to-fountain)
                (function-item fountain-export-buffer-to-txt)
                (function-item fountain-export-shell-command))
  :group 'fountain-export)

(make-obsolete-variable 'fountain-export-include-title-page
  'fountain-export-include-elements "3.0.0")

(defcustom fountain-export-page-size
  'letter
  "Paper size to use on export."
  :type '(radio (const :tag "US Letter" 'letter)
                (const :tag "A4" 'a4))
  :group 'fountain-export)

(defcustom fountain-export-font
  "Courier"
  "Font to use when exporting."
  :type '(string :tag "Font")
  :group 'fountain-export)

(defcustom fountain-export-contact-align-right
  nil
  "If non-nil, align title page contact block on the right."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-scene-heading-format
  '(double-space)
  "List of format options applied when exporting scene headings.
Options are: bold, double-space, underline."
  :type '(set (const :tag "Bold" bold)
              (const :tag "Double-spaced" double-space)
              (const :tag "Underlined" underline))
  :group 'fountain-export)

(defcustom fountain-export-more-dialog-string
  "(MORE)"
  "String to append to dialog when breaking across pages."
  :type 'string
  :group 'fountain-export)

;; (defcustom fountain-export-convert-quotes
;;   nil
;;   "If non-nil, replace TeX-style quotes with \"smart-quotes\".

;;     \`\`HAL\'\'

;; will be exported as

;;     “HAL”"
;;   :type 'boolean
;;   :group 'fountain-export)

(defcustom fountain-export-shell-command
  "afterwriting --source %s --pdf --overwrite"
  "Shell command string to convert Fountain source to ouput.
`%s' will be substituted with `buffer-file-name'"
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-template
  "\
_{{title}}_

{{credit}}

{{author}}"
  "Template for creating title page title block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-contact-template
  "{{contact}}"
  "Template for creating title page left block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-use-title-as-filename
  nil
  "If non-nil, use title metadata as export filename.

This is useful if you are exporting to Fountain and need to
specify a different filename."
  :type 'boolean
  :group 'fountain-export)

(defvar fountain-export-formats
  '((html
     :tag "HTML"
     :ext ".html"
     :template fountain-export-html-template
     :replace fountain-export-html-replacements
     :hook fountain-export-html-hook)
    (fdx
     :tag "Final Draft"
     :ext ".fdx"
     :template fountain-export-fdx-template
     :hook fountain-export-fdx-hook)
    (fountain
     :tag "Fountain"
     :ext ".fountain"
     :template fountain-export-fountain-template
     :hook fountain-export-fountain-hook)
    (txt
     :tag "plaintext"
     :ext ".txt"
     :fill t
     :template fountain-export-txt-template
     :hook fountain-export-txt-hook)
    (ps
     :tag "PostScript"
     :ext ".ps"
     :fill t
     :hook fountain-export-ps-hook))
  "Association list of export formats and their properties.

Takes the form:

    (FORMAT KEYWORD PROPERTY)")

(defvar fountain-elements
  '((section-heading
     :tag "Section Heading"
     :fill fountain-fill-section-heading)
    (scene-heading
     :tag "Scene Heading"
     :fill fountain-fill-scene-heading)
    (action
     :tag "Action"
     :fill fountain-fill-action)
    (character
     :tag "Character Name"
     :fill fountain-fill-character)
    (lines
     :tag "Dialogue"
     :fill fountain-fill-dialog)
    (paren
     :tag "Parenthetical"
     :fill fountain-fill-paren)
    (trans
     :tag: "Transition"
     :fill fountain-fill-trans)
    (center
     :tag "Center Text"
     :fill fountain-fill-action)
    (page-break
     :tage "Page Break")
    (synopsis
     :tag "Synopsis"
     :fill fountain-fill-action)
    (note
     :tag "Note"
     :fill fountain-fill-action)
    (note
     :tag "Note"
     :fill fountain-fill-note))
  "Association list of Fountain elements and their properties.
Includes references to various functions and variables.

Takes the form:

    (ELEMENT KEYWORD PROPERTY)")


(define-widget 'fountain-element-list-type 'lazy
  "Customize widget for Fountain templates."
  :offset 4
  :type '(list
          (group (const :tag "Document" document)
                 (choice string (const nil)))
          (group (const :tag "Section" section)
                 (choice string (const nil)))
          (group (const :tag "Section Heading" section-heading)
                 (choice string (const nil)))
          (group (const :tag "Scene" scene)
                 (choice string (const nil)))
          (group (const :tag "Scene Heading" scene-heading)
                 (choice string (const nil)))
          (group (const :tag "Dual Dialogue" dual-dialog)
                 (choice string (const nil)))
          (group (const :tag "Dialogue" dialog)
                 (choice string (const nil)))
          (group (const :tag "Character" character)
                 (choice string (const nil)))
          (group (const :tag "Parenthetical" paren)
                 (choice string (const nil)))
          (group (const :tag "Lines" lines)
                 (choice string (const nil)))
          (group (const :tag "Transition" trans)
                 (choice string (const nil)))
          (group (const :tag "Action" action)
                 (choice string (const nil)))
          (group (const :tag "Page Break" page-break)
                 (choice string (const nil)))
          (group (const :tag "Synopsis" synopsis)
                 (choice string (const nil)))
          (group (const :tag "Note" note)
                 (choice string (const nil)))
          (group (const :tag "Center Text" center)
                 (choice string (const nil)))
          (group (const :tag "Included Files" include)
                 (choice string (const nil)))))

(defun fountain-export-include-element-p (elt)
  "Returns non-nil if exporting ELT current format."
  (memq elt (cdr (assoc-string
                  (or (plist-get (fountain-read-metadata)
                                 'format)
                      "screenplay")
                  fountain-export-include-elements))))

(defun fountain-export-get-filename (format &optional buffer)
  "If buffer is visiting a file, concat file name base and FORMAT.
Otherwise return `fountain-export-buffer' formatted with export
format tag."
  (let* ((alist (alist-get format fountain-export-formats))
         (tag (plist-get alist :tag))
         (ext (plist-get alist :ext)))
    (with-current-buffer (or buffer (current-buffer))
      (cond (fountain-export-use-title-as-filename
             (concat (plist-get (fountain-read-metadata) 'title) ext))
            ((buffer-file-name)
             (concat (file-name-base (buffer-file-name)) ext))
            (t
             (format fountain-export-buffer-name tag))))))

(require 'subr-x)

(defun fountain-slugify (string)
  "Convert STRING to one suitable for slugs.

STRING is downcased, non-alphanumeric characters are removed, and
whitespace is converted to dashes. e.g.

    Hello Wayne's World 2! -> hello-wanyes-world-2"
  (string-join
   (split-string
    (downcase
     (replace-regexp-in-string "[^\.\n\s\t-_[:alnum:]]" "" string))
    "[^[:alnum:]]+" t)
   "-"))

(defun fountain-export-format-string (string format)
  "Replace matches in STRING for FORMAT alist in `fountain-export-format-replace-alist'."
  (let ((alist (symbol-value
                (plist-get (alist-get format fountain-export-formats)
                           :replace))))
  (dolist (var alist string)
    (setq string (replace-regexp-in-string
                  (car var) (cadr var) string t)))))

(defconst fountain-export-conditional-replacements
  '((fdx
     (starts-new-page
      (t (t "Yes") (nil "No"))))
    (fountain
     (forced
      (scene-heading (t "."))
      (character (t "@"))
      (trans (t "> "))
      (action (t "!")))
     (dual-dialog
      (character (right " ^")))
     (level
      (section-heading (1 "#")
                       (2 "##")
                       (3 "###")
                       (4 "####")
                       (5 "#####"))))))

(defconst fountain-export-replacements
  '((ps
     (title-template fountain-export-ps-title-template))
    (fdx
     (title-template fountain-export-fdx-title-template))
    (html
     (emacs-version emacs-version)
     (fountain-version fountain-version)
     (page-size (symbol-name fountain-export-page-size))
     (font fountain-export-font)
     (title-template fountain-export-html-title-template)
     (scene-heading-spacing
      (if (memq 'double-space fountain-export-scene-heading-format)
          "2em" "1em"))
     (more fountain-export-more-dialog-string)
     (contd fountain-continued-dialog-string))))

(defun fountain-export-element (element-list format)
  "Return a formatted string from ELEMENT-LIST according to FORMAT.

Break ELEMENT-LIST into ELEMENT, PLIST and CONTENT.

If CONTENT is a string and PLIST property 'export is non-nil:

Check if ELEMENT corresponds to a TEMPLATE in
`fountain-export-templates'

If PLIST property `export' is non-nil, check if ELEMENT
corresponds to a TEMPLATE in `fountain-export-templates' for
FORMAT. If so, replace matches of `fountain-template-key-regexp'
in the following order:

    1. {{content}} is replaced with CONTENT.
    2. If KEY corresponds to a string property in PLIST then {{KEY}} is
       replaced with that string.
    3. If KEY corresponds with remaining replacement conditions then {{KEY}} is
       replaced with that string.
    4. If none of the above, {{KEY}} is replaced with an empty string."
  ;; Break ELEMENT-LIST into ELEMENT, PLIST and CONTENT. Find the FORMAT-PLIST.
  (let ((element (car element-list))
        (plist (nth 1 element-list))
        (content (nth 2 element-list))
        (format-plist (alist-get format fountain-export-formats))
        template-alist template string)
    (cond
     ;; If CONTENT is a string, and included in export, format STRING.
     ((and (stringp content)
           (plist-get plist 'export))
      (setq string (fountain-export-format-string content format))
      (when (plist-get format-plist :fill)
        (with-temp-buffer
          (insert string)
          (let ((var (symbol-value
                      (plist-get (alist-get element fountain-elements) :fill))))
            (setq left-margin (car var)
                  fill-column (+ left-margin (cdr var)))
            ;; Replace emphasis syntax with face text propoerties (before performing fill).
            ;;
            ;; (dolist (var '((fountain-italic-regexp italic)
            ;;                (fountain-bold-regexp bold)
            ;;                (fountain-underline-regexp underline)))
            ;;   (while (re-search-forward (symbol-value (car var)) nil t)
            ;;     (put-text-property (match-beginning 3) (match-end 3) 'face (cadr var))
            ;;     (delete-region (match-beginning 4) (match-end 4))
            ;;     (delete-region (match-beginning 2) (match-end 2))))
            ;;
            ;; Fill the buffer and return it as a string.
            (fill-region (point-min) (point-max)))
          (setq string (buffer-string)))))
     ;; If CONTENT is a list, work through the list binding each
     ;; CHILD-ELEMENT-LIST and recursively calling this function.
     ((listp content)
      (dolist (child-element-list content)
        (setq string
              (concat string
                      (fountain-export-element child-element-list format)))))
     ;; Otherwise, CONTENT is either malformed, or not exported, then set and
     ;; empty string.
     (t
      (setq string "")))
    ;; Set the export template.
    (setq template-alist (symbol-value
                          (plist-get format-plist :template))
          element-template (car (alist-get element template-alist)))
    (cond
     ;; If there is a TEMPLATE-ALIST and an ELEMENT-TEMPLATE, replace template
     ;; keys in that template.
     ((and template-alist element-template)
      (while (string-match fountain-template-key-regexp element-template)
        (setq element-template
              (replace-regexp-in-string
               fountain-template-key-regexp
               (lambda (match)
                 ;; Find KEY and corresponding VALUE.
                 (let* ((key (match-string 1 match))
                        (value (plist-get plist (intern key))))
                   (cond
                    ;; If KEY is "content", replace with STRING.
                    ((string= key "content")
                     string)
                    ;; If KEY is "slugify", replace with slugified STRING.
                    ((string= key "slugify")
                     (save-match-data
                       (fountain-slugify string)))
                    ;; If KEY's VALUE is a string, format and replace
                    ;; with VALUE.
                    ((stringp value)
                     (fountain-export-format-string value format))
                    ;; If KEY's VALUE is not a string, but still in
                    ;; PLIST, attempt conditional replacement based on
                    ;; VALUE.
                    ((memq key plist)
                     (let* ((format-alist
                             (alist-get format fountain-export-conditional-replacements))
                            (key-alist
                             (alist-get (intern key) format-alist))
                            (type-alist
                             (or (alist-get element key-alist)
                                 (alist-get t key-alist))))
                       ;; If KEY's VALUE corresponds to a replacement
                       ;; string, insert that string, otherwise insert an
                       ;; empty string.
                       (or (car (alist-get value type-alist)) "")))
                    ;; Otherwise, attempt expression replacements.
                    (t
                     (let* ((format-alist
                             (alist-get format fountain-export-replacements))
                            (replacement
                             (car (alist-get (intern key) format-alist))))
                       ;; If a replacement expression is found, evaluate it
                       ;; for a string, otherwise insert an empty string.
                       (if replacement (eval replacement) ""))))))
               element-template t t)))
      (setq string element-template))
     ;; If there's no ELEMENT-TEMPLATE for element in TEMPLATE-ALIST, discard
     ;; STRING.
     (template-alist
      (setq string "")))
    ;; Return the string.
    (or string "")))

(defun fountain-export-region (beg end format &optional snippet)
  "Return an export string of region between BEG and END in FORMAT.
If SNIPPET, do not include a document template wrapper.

Save current outline visibility level, then show all. Then read
file metadata. Then calculate elements included in export from
assocation list in `fountain-export-include-elements'
corresponding to FORMAT. Then parse the region into an element tree.

If exporting a standalone document, call
`fountain-export-format-element' with tree, FORMAT and list of
included elements, otherwise walk the element tree calling
`fountain-export-format-element' and concatenate the resulting
strings."
  (let ((job (make-progress-reporter "Exporting..."))
        tree string)
    ;; Search for script end point and reset END.
    (save-excursion
      (goto-char beg)
      (if (re-search-forward fountain-end-regexp end t)
          (setq end (match-beginning 0))))
    ;; Parse the region to TREE.
    (save-excursion
      (setq tree (fountain-parse-region beg end)))
    ;; If exporting a standalone document, list TREE inside a document element.
    (unless (or snippet (not fountain-export-standalone))
      (setq tree
            (list (list 'document
                        (append
                         (list 'begin beg
                               'end end
                               'export t)
                               (fountain-read-metadata))
                        tree))))
    ;; Walk through TREE, concatenating exported elements to STRING.
    (while tree
      (setq string
            (concat string (fountain-export-element (pop tree) format)))
      (progress-reporter-update job))
    (progress-reporter-done job)
    ;; Return exported STRING.
    string))

(defun fountain-export-buffer (format &optional snippet buffer)
  "Export current buffer or BUFFER to export format FORMAT.

If destination buffer is not empty, ask to overwrite or generate
a new buffer. If destination buffer is the same as source buffer,
generate a new buffer.

Switch to destination buffer if complete without errors,
otherwise kill destination buffer."
  ;; If called interactively, present export format options.
  (interactive
   (list (intern
          (completing-read "Export format: "
                           (mapcar #'car fountain-export-formats) nil t))
         (car current-prefix-arg)))
  (setq buffer (or buffer (current-buffer)))
  (let ((dest-buffer (get-buffer-create
                      (fountain-export-get-filename format buffer)))
        (hook (plist-get (alist-get format fountain-export-formats)
                         :hook))
        string complete)
    (unwind-protect
        (with-current-buffer buffer
          ;; If DEST-BUFFER is not empty, check if it is the current buffer, or
          ;; if not, if the user does not wish to overwrite.
          (when (< 0 (buffer-size dest-buffer))
            (if (or (eq (current-buffer) dest-buffer)
                    (not (y-or-n-p (format "Buffer `%s' is not empty; overwrite? "
                                           dest-buffer))))
                ;; If so, generate a new buffer.
                (progn
                  (setq dest-buffer
                        (generate-new-buffer (buffer-name dest-buffer)))
                  (message "Using new buffer `%s'" dest-buffer))))
          ;; Export the region to STRING.
          (setq string
                (fountain-export-region (point-min) (point-max) format snippet))
          ;; Insert STRING into DEST-BUFFER.
          (with-current-buffer dest-buffer
            (with-silent-modifications
              (erase-buffer)
              (insert string)))
          ;; Switch to DEST-BUFFER and save.
          (switch-to-buffer dest-buffer)
          (write-file (buffer-name) t)
          ;; Set COMPLETE flag and run hooks.
          (setq complete t)
          (run-hooks hook))
      ;; If export failed, kill DEST-BUFFER.
      (unless complete
        (kill-buffer dest-buffer)))))

(defun fountain-export-default ()
  "Call function defined in `fountain-export-default-command'."
  (interactive)
  (funcall fountain-export-default-command))

(defun fountain-export-shell-command (&optional buffer)
  "Call shell command defined in variable `fountain-export-shell-command'.
Command acts on current buffer or BUFFER."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (if file
        (async-shell-command            ; FIXME use start-process
         (format fountain-export-shell-command (shell-quote-argument file))
         "*Fountain Export Process*")
      (user-error "Buffer `%s' is not visiting a file" buffer))))


;;; -> plaintext

(defcustom fountain-export-txt-template
  '((document "{{content}}")
    (section "{{content}}")
    (section-heading "{{content}}\n\n")
    (scene "{{content}}")
    (scene-heading "{{content}}\n\n")
    (dual-dialog "{{content}}\n")
    (dialog "{{content}}\n")
    (character "{{content}}\n")
    (paren "{{content}}\n")
    (lines "{{content}}\n")
    (trans "{{content}}\n\n")
    (action "{{content}}\n\n")
    (page-break "\n\n")
    (synopsis "{{content}}\n\n")
    (note "[ note: {{content}} ]\n\n")
    (center "{{content}}")
    (include "{{content}}"))
  "Association list of element templates for exporting to Fountain.
Takes the form:

    ((ELEMENT TEMPLATE) ...)

ELEMENT is the Fountain element, a symbol (see below). TEMPLATE
is the template with which to format the format string. If
TEMPLATE is nil, the format string is passed as is without
formatting. An empty string discards the format string and passes
the empty string.

Fountain ELEMENTs:

    document            wrapper template for all content, see
                        `fountain-export-standalone'
    section             section string, including child elements
    section-heading     section heading string, excluding syntax chars
    scene               scene string, including child elements
    scene-heading       scene heading string, excluing syntax chars
    dialog              dialogue string, including child elements
    dual-dialog         dual dialogue string, including child elements
    character           character string, excluding syntax chars
    paren               parenthetical string
    lines               dialogue lines, up to end of dialogue block or
                        next parenthetical
    trans               transition string, excluding syntax chars
    action              action string
    page-break          page break, including forced page number
    synopsis            synopsis string, excluding syntax chars
    note                note string, excluding syntax chars
    center              center text string, excluding syntax chars
    include             inclusion string

The format of TEMPLATE can include replacement keys in the form
{{KEY}}. Each TEMPLATE should include the {{content}} key.

If TEMPLATE is nil, the string is discarded."
  :type 'fountain-element-list-type
  :group 'fountain-export)

(defcustom fountain-export-txt-hook
  nil
  "Hook run with export buffer on sucessful export to plaintext."
  :type 'hook
  :group 'fountain-export)

(defun fountain-export-buffer-to-txt ()
  "Convenience function for exporting buffer to plaintext."
  (interactive)
  (fountain-export-buffer 'txt))


;;; -> PostScript

(defcustom fountain-export-ps-hook
  nil
  "Hook run with export buffer on sucessful export to PostScript."
  :type 'hook
  :group 'fountain-export)

(defun fountain-export-buffer-to-ps ()
  "Convenience function for exporting buffer to PostScript."
  (interactive)
  (fountain-export-buffer 'ps))


;;; -> HTML

(defcustom fountain-export-html-template
  '((document "\
<head>
<meta charset=\"utf-8\">
<meta name=\"author\" content=\"{{author}}\" />
<meta name=\"generator\" content=\"Emacs {{emacs-version}} running {{fountain-version}}\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=no\">
<title>{{title}}</title>
<style type=\"text/css\">
@page screenplay, screenplay-title {
  size: {{page-size}};
  margin-top: 1in;
  margin-right: 1in;
  margin-bottom: 0.75in;
  margin-left: 1.5in;
}
@page screenplay {
  @top-right-corner {
    font-family: {{font}};
    font-size: 12pt;
    content: counter(page) \".\";
    vertical-align: bottom;
    padding-bottom: 1em;
  }
}
@page screenplay:first {
  @top-right-corner {
    content: normal;
  }
}
.screenplay {
  page: screenplay;
  counter-reset: page;
  font-family: {{font}};
  font-size: 12pt;
  line-height: 1;
  max-width: 6in;
  margin: 1em auto;
  -webkit-text-size-adjust: none;
}
.screenplay .title-page {
  display: {{include-title-page}};
  page: screenplay-title;
  page-break-after: always;
  margin-top: 0;
  margin-right: auto;
  margin-bottom: 1em;
  margin-left: auto;
}
.screenplay .title-page .title {
  text-align: center;
}
@media print {
  .screenplay .title-page .title {
    margin-top: 3.5in;
    margin-bottom: 4in;
  }
}
.screenplay .title-page .title h1 {
  text-transform: {{title-upcase}};
  text-decoration: {{title-underline}};
}
.screenplay h1, .screenplay h2, .screenplay h3, .screenplay h4, .screenplay h5, .screenplay h6 {
  font-weight: inherit;
  font-size: inherit;
}
.screenplay a {
  color: inherit;
  text-decoration: none;
}
.screenplay hr {
  page-break-after: always;
}
@media print {
  .screenplay hr {
    visibility: hidden;
  }
}
.screenplay mark {
  background-color: yellow;
}
.screenplay mark:before {
  content: '*';
  width: 0.5in;
  position: absolute;
  right: 0;
}
.screenplay del:before {
  content: '*';
  width: 0.5in;
  position: absolute;
  right: 0;
}
.screenplay .section-heading {
  display: block;
  text-align: center;
  text-decoration: underline;
}
.screenplay .scene-heading {
  margin-top: {{scene-heading-spacing}};
  margin-bottom: 0;
  clear: both;
  page-break-after: avoid;
}
.screenplay .action {
  margin-top: 1em;
  margin-bottom: 0;
  clear: both;
  white-space: pre-wrap;
  orphans: 2;
  widows: 2;
}
.screenplay .character {
  max-width: 4in;
  margin-top: 1em;
  margin-left: 33%;
  margin-bottom: 0;
  clear: both;
}
.screenplay .character.dual-left {
  width: 33%;
  margin-left: 0;
  padding-left: 15%;
  float: left;
  clear: left;
}
.screenplay .character.dual-right {
  width: 33%;
  margin-left: 50%;
  padding-left: 15%;
  clear: right;
}
.screenplay .dialog {
  max-width: 3.5in;
  margin-top: 0;
  margin-bottom: 0;
  margin-left: 17%;
  clear: both;
  white-space: pre-wrap;
  orphans: 2;
  widows: 2;
}
.screenplay .dialog.dual-left {
  width: 48%;
  margin-left: 0;
  float: left;
  clear: left;
}
.screenplay .dialog.dual-right {
  width: 48%;
  margin-left: 50%;
  clear: right;
}
.screenplay .paren {
  max-width: 2in;
  margin-top: 0;
  margin-bottom: 0;
  margin-left: 27%;
  text-indent: -0.6em;
  page-break-inside: avoid;
  page-break-after: avoid;
}
.screenplay .paren.dual-left {
  width: 38%;
  margin-left: 0;
  padding-left: 10%;
  float: left;
  clear: left;
}
.screenplay .paren.dual-right {
  width: 38%;
  margin-left: 50%;
  padding-left: 10%;
  clear: right;
}
.screenplay .trans {
  max-width: 2in;
  margin-top: 1em;
  margin-bottom: 1em;
  margin-left: 63%;
  clear: both;
  page-break-before: avoid;
}
.screenplay .note {
  display: block;
  font-size: 11pt;
  font-family: \"Comic Sans MS\", \"Helvetica\", \"Marker Felt\";
  line-height: 1.5;
  background-color: lightgoldenrodyellow;
  padding: 1em;
}
.screenplay .synopsis {
  display: block;
  margin-top: 0;
  color: grey;
  font-style: italic;
}
.screenplay .center {
  text-align: center;
  width: 100%;
  white-space: pre-wrap;
}
.screenplay .underline {
  text-decoration: underline;
}
.screenplay .menu {
  display: none;
  position: fixed;
  top: 0;
  right: 0;
  color: white;
  background-color: rgba(0, 0, 0, 0.25);
  cursor: pointer;
}
</style>
</head>
<body>
<section class=\"screenplay\">
<section class=\"title-page\">
<p class=\"title\">{{title-template}}</p>
<p class=\"contact\">{{contact-template}}</p>
</section>
{{content}}\
<div class=\"menu\">Aa</div>
</section>
</body>")
    (section "<section class=\"section\">\n{{content}}</section>\n")
    (section-heading "<a href=\"#{{slugify}}\"><p class=\"section-heading\" id=\"{{slugify}}\">{{content}}</p></a>\n")
    (scene "<section class=\"scene\">\n{{content}}</section>\n")
    (scene-heading "<a href=\"#{{scene-number}}\"><p class=\"scene-heading\" id=\"{{scene-number}}\">{{content}}</p></a>\n")
    (dual-dialog "<div class=\"dual-dialog\">\n{{content}}</div>\n")
    (dialog "<div class=\"dialog\">\n{{content}}</div>\n")
    (character "<p class=\"character\">{{content}}</p>\n")
    (paren "<p class=\"paren\">{{content}}</p>\n")
    (lines "<p class=\"lines\">{{content}}</p>\n")
    (trans "<p class=\"trans\">{{content}}</p>\n")
    (action "<p class=\"action\">{{content}}</p>\n")
    (page-break "<a href=\"#p{{content}}\"><hr id=\"{{content}}\">\n<p class=\"page-number\">{{content}}</p></a>")
    (synopsis "<p class=\"synopsis\">{{content}}</p>\n")
    (note "<p class=\"note\">{{content}}</p>\n")
    (center "<p class=\"center\">{{content}}</p>\n")
    (include "{{content}}"))
  "Association list of element templates for exporting to HTML.
Takes the form:

    ((ELEMENT TEMPLATE) ...)

ELEMENT is the Fountain element, a symbol (see below). TEMPLATE
is the template with which to format the format string. If
TEMPLATE is nil, the format string is passed as is without
formatting. An empty string discards the format string and passes
the empty string.

Fountain ELEMENTs:

    document            wrapper template for all content, see
                        `fountain-export-standalone'
    section             section string, including child elements
    section-heading     section heading string, excluding syntax chars
    scene               scene string, including child elements
    scene-heading       scene heading string, excluing syntax chars
    dialog              dialogue string, including child elements
    dual-dialog         dual dialogue string, including child elements
    character           character string, excluding syntax chars
    paren               parenthetical string
    lines               dialogue lines, up to end of dialogue block or
                        next parenthetical
    trans               transition string, excluding syntax chars
    action              action string
    page-break          page break, including forced page number
    synopsis            synopsis string, excluding syntax chars
    note                note string, excluding syntax chars
    center              center text string, excluding syntax chars

The format of TEMPLATE can include replacement keys in the form
`{{KEY}}'. Each TEMPLATE should include the {{content}} key. See
`fountain-export-format-template' for how replacement strings are
calculated."
    :type 'fountain-element-list-type
    :group 'fountain-export)

(defvar fountain-export-html-replacements
  (backquote
   ((,fountain-comment-regexp "")       ; FIXME: separate strip-comments fun?
    ("&" "&amp;")
    ("<" "&lt;")
    (">" "&gt;")
    ("\\\\\s" "&nbsp;")
    ("^\\\\$" "<br>")
    ("\\\\_" "&#95;")
    ("\\\\\\*" "&#42;")
    ("\\\\`" "&#96;")
    ("\\\\'" "&apos;")
    ("\"\\(.+?\\)\"" "&ldquo;\\1&rdquo;")
    ("``" "&ldquo;")
    ("''" "&rdquo;")
    ("`" "&lsquo;")
    ("'" "&rsquo;")
    ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "<strong><em>\\1</em></strong>")
    ("\\*\\*\\(.+?\\)\\*\\*" "<strong>\\1</strong>")
    ("\\*\\(.+?\\)\\*" "<em>\\1</em>")
    ("^~\s*\\(.+?\\)$" "<i>\\1</i>")
    ("_\\(.+?\\)_" "<span class=\"underline\">\\1</span>")
    ("\n\n+" "<br><br>")
    ("\n" "<br>")))
  "Association list of regular expression export replacements.
Replacements are made in sequential order. The sequence is
important: first, characters that are special in the export
format are sanitized, then escaped characters are converted to
character codes, then format replacement is made.")

(defcustom fountain-export-html-hook
  nil
  "Hook run with export buffer on sucessful export to HTML."
  :type 'hook
  :group 'fountain-export)

(defun fountain-export-buffer-to-html ()
  "Convenience function for exporting buffer to HTML."
  (interactive)
  (fountain-export-buffer 'html))


;;; -> FDX

(defcustom fountain-export-fdx-template
  '((document "\
<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>
<FinalDraft DocumentType=\"Script\" Template=\"No\" Version=\"1\">
<Content>
{{content}}\
</Content>
<TitlePage>
<Content>
<Paragraph Alignment=\"Center\">
<Text>{{title}}</Text>
</Paragraph>
<Paragraph Alignment=\"Center\">
<Text></Text>
</Paragraph>
<Paragraph Alignment=\"Center\">
<Text>{{credit}}</Text>
</Paragraph>
<Paragraph Alignment=\"Center\">
<Text></Text>
</Paragraph>
<Paragraph Alignment=\"Center\">
<Text>{{author}}</Text>
</Paragraph>
<Paragraph Alignment=\"Left\">
<Text>{{contact-template}}</Text>
</Paragraph>
</Content>
</TitlePage>
</FinalDraft>")
    (section "{{content}}")
    (section-heading nil)
    (scene "{{content}}")
    (scene-heading "<Paragraph Number=\"{{scene-number}}\" Type=\"Scene Heading\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (dual-dialog "<Paragraph StartsNewPage=\"{{starts-new-page}}\">\n<DualDialogue>\n{{content}}</DualDialogue>\n</DualDialogue>\n")
    (dialog "{{content}}")
    (character "<Paragraph Type=\"Character\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (paren "<Paragraph Type=\"Parenthetical\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (lines "<Paragraph Type=\"Dialogue\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (trans "<Paragraph Type=\"Transition\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (action "<Paragraph Type=\"Action\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (page-break nil)
    (synopsis nil)
    (note nil)
    (center "<Paragraph Alignment=\"Center\" Type=\"Action\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n")
    (include "{{content}}"))
  "Association list of element templates for exporting to Final Draft.
Takes the form:

    ((ELEMENT TEMPLATE) ...)

ELEMENT is the Fountain element, a symbol (see below). TEMPLATE
is the template with which to format a string.

Fountain ELEMENTs:

    document            wrapper template for all content, see
                        `fountain-export-standalone'
    section             section string, including child elements
    section-heading     section heading string, excluding syntax chars
    scene               scene string, including child elements
    scene-heading       scene heading string, excluing syntax chars
    dialog              dialogue string, including child elements
    dual-dialog         dual dialogue string, including child elements
    character           character string, excluding syntax chars
    paren               parenthetical string
    lines               dialogue lines, up to end of dialogue block or
                        next parenthetical
    trans               transition string, excluding syntax chars
    action              action string
    page-break          page break, including forced page number
    synopsis            synopsis string, excluding syntax chars
    note                note string, excluding syntax chars
    center              center text string, excluding syntax chars
    include             inclusion string

The format of TEMPLATE can include replacement keys in the form
{{KEY}}. Each TEMPLATE should include the {{content}} key.

If TEMPLATE is nil, the string is discarded."
  :type 'fountain-element-list-type
  :group 'fountain-export)

(defcustom fountain-export-fdx-hook
  nil
  "Hook run with export buffer on sucessful export to Final Draft."
  :type 'hook
  :group 'fountain-export)

(defun fountain-export-buffer-to-fdx ()
  "Convenience function for exporting buffer to Final Draft."
  (interactive)
  (fountain-export-buffer 'fdx))


;;; -> Fountain

(defcustom fountain-export-fountain-template
  '((document "\
title: {{title}}
credit: {{credit}}
author: {{author}}
date: {{date}}
contact:
  {{contact-template}}


{{content}}")
    (section "{{content}}")
    (section-heading "{{level}}{{content}}\n\n")
    (scene "{{content}}")
    (scene-heading "{{forced}}{{content}}\n\n")
    (dual-dialog "{{content}}\n")
    (dialog "{{content}}\n")
    (character "{{forced}}{{content}}{{dual-dialog}}\n")
    (paren "{{content}}\n")
    (lines "{{content}}\n")
    (trans "{{forced}}{{content}}\n\n")
    (action "{{forced}}{{content}}\n\n")
    (page-break "==={{content}}\n\n")
    (synopsis "= {{content}}\n\n")
    (note "[[ {{content}} ]]\n\n")
    (center "> {{content}} <")
    (include "{{content}}"))
  "Association list of element templates for exporting to Fountain.
Takes the form:

    ((ELEMENT TEMPLATE) ...)

ELEMENT is the Fountain element, a symbol (see below). TEMPLATE
is the template with which to format the format string. If
TEMPLATE is nil, the format string is passed as is without
formatting. An empty string discards the format string and passes
the empty string.

Fountain ELEMENTs:

    document            wrapper template for all content, see
                        `fountain-export-standalone'
    section             section string, including child elements
    section-heading     section heading string, excluding syntax chars
    scene               scene string, including child elements
    scene-heading       scene heading string, excluing syntax chars
    dialog              dialogue string, including child elements
    dual-dialog         dual dialogue string, including child elements
    character           character string, excluding syntax chars
    paren               parenthetical string
    lines               dialogue lines, up to end of dialogue block or
                        next parenthetical
    trans               transition string, excluding syntax chars
    action              action string
    page-break          page break, including forced page number
    synopsis            synopsis string, excluding syntax chars
    note                note string, excluding syntax chars
    center              center text string, excluding syntax chars
    include             inclusion string

The format of TEMPLATE can include replacement keys in the form
{{KEY}}. Each TEMPLATE should include the {{content}} key.

If TEMPLATE is nil, the string is discarded."
  :type 'fountain-element-list-type
  :group 'fountain-export)

(defcustom fountain-export-fountain-hook
  nil
  "Hook run with export buffer on sucessful export to Fountain."
  :type 'hook
  :group 'fountain-export)

(defun fountain-export-buffer-to-fountain ()
  "Convenience function for exporting buffer to Fountain."
  (interactive)
  (fountain-export-buffer 'fountain))


;;; Outlining

(require 'outline)

(defvar-local fountain-outline-cycle
  0
  "Integer representing global outline cycling status.

    0: Show all
    1: Show level 1 section headings
    2: Show level 2 section headings
    3: Show level 3 section headings
    4: Show level 4 section headings
    5: Show level 5 section headings
    6: Show scene headings

Used by `fountain-outline-cycle'.")

(defvar-local fountain-outline-cycle-subtree
  0
  "Integer representing subtree outline cycling status.
Used by `fountain-outline-cycle'.")

(defcustom fountain-outline-startup-level
  0
  "Outline level to show when visiting a file.

This can be set on a per-file basis by including in metadata:

\tstartup-level: N"
  :type '(choice (const :tag "Show all" 0)
                 (const :tag "Show top-level" 1)
                 (const :tag "Show scene headings" 6)
                 (integer :tag "Custom level"))
  :group 'fountain)

(defcustom fountain-outline-custom-level
  nil
  "Additional section headings to include in outline cycling."
  :type '(choice (const :tag "Only top-level" nil)
                 (const :tag "Include level 2" 2)
                 (const :tag "Include level 3" 3)
                 (const :tag "Include level 4" 4)
                 (const :tag "Include level 5" 5))
  :group 'fountain)

(defalias 'fountain-outline-next 'outline-next-visible-heading)
(defalias 'fountain-outline-previous 'outline-previous-visible-heading)
(defalias 'fountain-outline-forward 'outline-forward-same-level)
(defalias 'fountain-outline-backward 'outline-backward-same-level)
(defalias 'fountain-outline-up 'outline-up-heading)
(defalias 'fountain-outline-mark 'outline-mark-subtree)

(when (version< emacs-version "25")
  (defalias 'outline-show-all 'show-all)
  (defalias 'outline-show-entry 'show-entry)
  (defalias 'outline-show-subtree 'show-subtree)
  (defalias 'outline-show-children 'show-children)
  (defalias 'outline-hide-subtree 'hide-subtree)
  (defalias 'outline-hide-sublevels 'hide-sublevels))

(defun fountain-outline-invisible-p (&optional pos)
  "Non-nil if the character after POS has outline invisible property.
If POS is nil, use `point' instead."
  (eq (get-char-property (or pos (point)) 'invisible) 'outline))

(defun fountain-outline-shift-down (&optional n)
  "Move the current subtree down past N headings of same level."
  (interactive "p")
  (outline-back-to-heading)
  (let* (hanging-line
         (move-fun
          (if (< 0 n)
              'outline-get-next-sibling
            'outline-get-last-sibling))
         (end-point-fun
          (lambda ()
            (outline-end-of-subtree)
            ;; Add newline if none at eof.
            (if (and (eobp)
                     (/= (char-before) ?\n))
                (insert-char ?\n))
            ;; Temporary newline if only 1 at eof
            (when (and (eobp)
                       (save-excursion
                         (forward-line -1)
                         (not (fountain-blank-p))))
              (insert-char ?\n)
              (setq hanging-line t))
            ;; Avoid eobp signal.
            (unless (eobp)
              (forward-char 1))
            (point)))
         (beg (point))
         (folded
          (save-match-data
            (outline-end-of-heading)
            (outline-invisible-p)))
         (end
          (save-match-data
            (funcall end-point-fun)))
         (insert-point (make-marker))
         (i (abs n)))
    (goto-char beg)
    (while (< 0 i)
      (or (funcall move-fun)
          (progn (goto-char beg)
                 (message "Cannot shift past higher level")))
      (setq i (1- i)))
    (if (< 0 n)
        (funcall end-point-fun))
    (set-marker insert-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char insert-point)
    (if folded
        (outline-hide-subtree))
    ;; Remove temporary newline.
    (if hanging-line
        (save-excursion
          (goto-char (point-max))
          (delete-char -1)))
    (set-marker insert-point nil)))

(defun fountain-outline-shift-up (&optional n)
  "Move the current subtree up past N headings of same level."
  (interactive "p")
  (fountain-outline-shift-down (- n)))

(defun fountain-outline-hide-level (n &optional silent)
  "Set outline visibilty to outline level N.
Display a message unless SILENT."
  (cond ((= n 0)
         (outline-show-all)
         (unless silent (message "Showing all")))
        ((= n 6)
         (outline-hide-sublevels n)
         (unless silent (message "Showing scene headings")))
        (t
         (outline-hide-sublevels n)
         (unless silent (message "Showing level %s headings" n))))
  (setq fountain-outline-cycle n))

(defun fountain-outline-cycle (&optional arg) ; FIXME: document
  "\\<fountain-mode-map>Cycle outline visibility depending on ARG.

    \\[fountain-outline-cycle]				If ARG is nil, cycle outline visibility of current
                    subtree and its children
    \\[universal-argument] \\[fountain-outline-cycle]			If ARG is 4, cycle outline visibility of buffer
                    (same as \\[fountain-outline-cycle-global])
    \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]		If ARG is 16, show all
    \\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]	If ARG is 64, show outline visibility set in
                    `fountain-outline-custom-level'"
  (interactive "p")
  (let ((custom-level
         (if fountain-outline-custom-level
             (save-excursion
               (goto-char (point-min))
               (let (found)
                 (while (and (not found)
                             (outline-next-heading))
                   (if (= (funcall outline-level)
                          fountain-outline-custom-level)
                       (setq found t)))
                 (if found fountain-outline-custom-level)))))
        (highest-level
         (save-excursion
           (goto-char (point-max))
           (outline-back-to-heading t)
           (let ((level (funcall outline-level)))
             (while (and (not (bobp))
                         (< 1 level))
               (outline-up-heading 1 t)
               (unless (bobp)
                 (setq level (funcall outline-level))))
             level))))
    (cond ((eq arg 4)
           (cond
            ((and (= fountain-outline-cycle 1) custom-level)
             (fountain-outline-hide-level custom-level))
            ((< 0 fountain-outline-cycle 6)
             (fountain-outline-hide-level 6))
            ((= fountain-outline-cycle 6)
             (fountain-outline-hide-level 0))
            ((= highest-level 6)
             (fountain-outline-hide-level 6))
            (t
             (fountain-outline-hide-level highest-level))))
          ((eq arg 16)
           (outline-show-all)
           (message "Showing all")
           (setq fountain-outline-cycle 0))
          ((and (eq arg 64) custom-level)
           (fountain-outline-hide-level custom-level))
          (t
           (save-excursion
             (outline-back-to-heading)
             (let ((eoh
                    (save-excursion
                      (outline-end-of-heading)
                      (point)))
                   (eos
                    (save-excursion
                      (outline-end-of-subtree)
                      (point)))
                   (eol
                    (save-excursion
                      (forward-line 1)
                      (while (and (not (eobp))
                                  (get-char-property (1- (point)) 'invisible))
                        (forward-line 1))
                      (point)))
                   (children
                    (save-excursion
                      (outline-back-to-heading)
                      (let ((level (funcall outline-level)))
                        (outline-next-heading)
                        (and (outline-on-heading-p t)
                             (< level (funcall outline-level)))))))
               (cond
                ((= eos eoh)
                 (message "Empty heading")
                 (setq fountain-outline-cycle-subtree 0))
                ((and (<= eos eol)
                      children)
                 (outline-show-entry)
                 (outline-show-children)
                 (message "Showing headings")
                 (setq fountain-outline-cycle-subtree 2))
                ((or (<= eos eol)
                     (= fountain-outline-cycle-subtree 2))
                 (outline-show-subtree)
                 (message "Showing contents")
                 (setq fountain-outline-cycle-subtree 3))
                (t
                 (outline-hide-subtree)
                 (message "Hiding contents")
                 (setq fountain-outline-cycle-subtree 1)))))))))

(defun fountain-outline-cycle-global ()
  "Globally cycle outline visibility.

Calls `fountain-outline-cycle' with argument 4 to cycle buffer
outline visibility through the following states:

    1: Top-level section headings
    2: Value of `fountain-outline-custom-level'
    3: All section headings and scene headings
    4: Everything"
  (interactive)
  (fountain-outline-cycle 4))

(defun fountain-outline-level ()
  "Return the heading's nesting level in the outline.
Assumes that point is at the beginning of a heading and match
data reflects `outline-regexp'."
  (cond ((string-match fountain-end-regexp (match-string 0))
         1)
        ((string-prefix-p "#" (match-string 0))
         (string-width (match-string 2)))
        (t 6)))


;;; Navigation

(defun fountain-forward-scene (&optional n)
  "Move forward N scene headings (backward if N is negative).
If N is 0, move to beginning of scene."
  (interactive "^p")
  (unless n (setq n 1))
  (let* ((p (if (<= n 0) -1 1))
         (move-fun
          (lambda ()
            (while (not (or (eq (point) (buffer-end p))
                            (fountain-match-scene-heading)))
              (forward-line p)))))
    (if (/= n 0)
        (while (/= n 0)
          (if (fountain-match-scene-heading)
              (forward-line p))
          (funcall move-fun)
          (setq n (- n p)))
      (forward-line 0)
      (funcall move-fun))))

(defun fountain-backward-scene (&optional n)
  "Move backward N scene headings (foward if N is negative)."
  (interactive "^p")
  (or n (setq n 1))
  (fountain-forward-scene (- n)))

(defun fountain-beginning-of-scene ()   ; FIXME: needed?
  "Move point to beginning of current scene."
  (interactive "^")
  (fountain-forward-scene 0))

(defun fountain-end-of-scene ()         ; FIXME: needed?
  "Move point to end of current scene."
  (interactive "^")
  (fountain-forward-scene 1)
  (unless (eobp)
    (forward-char -1)))

(defun fountain-mark-scene ()           ; FIXME: extending region
  "Put mark at end of this scene, point at beginning."
  (interactive)
  ;; (if (or extend
  ;;         (and (region-active-p)
  ;;              (eq last-command this-command)))
  ;;     (progn
  ;;       (fountain-forward-scene 1)
  ;;       (push-mark)
  ;;       (exchange-point-and-mark))
  (push-mark)
  (fountain-forward-scene 0)
  (if (not (or (fountain-match-section-heading)
               (fountain-match-scene-heading)))
      (progn
        (goto-char (mark))
        (user-error "Before first scene heading"))
    (push-mark)
    (fountain-forward-scene 1)
    (exchange-point-and-mark)))

(defun fountain-goto-scene (n)
  "Move point to Nth scene in current buffer.

Ignores revised scene numbers scenes.

    10  = 10
    10B = 10
    A10 =  9"
  (interactive "NGoto scene: ")
  (goto-char (point-min))
  (let ((scene (if (fountain-match-scene-heading)
                   (car (fountain-scene-number-to-list (match-string 6)))
                 0)))
    (while (and (< scene n)
                (< (point) (point-max)))
      (fountain-forward-scene 1)
      (if (fountain-match-scene-heading)
          (setq scene (or (car (fountain-scene-number-to-list (match-string 6)))
                          (1+ scene)))))))

(defun fountain-forward-character (&optional n limit)
  "Goto Nth next character (or Nth previous is N is negative).
If LIMIT is 'dialog, halt at end of dialog. If LIMIT is 'scene,
halt at end of scene."
  (interactive "^p")
  (unless n (setq n 1))
  (let* ((p (if (<= n 0) -1 1))
         (move-fun
          (lambda ()
            (while (cond ((eq limit 'dialog)
                          (and (not (= (point) (buffer-end p)))
                               (or (fountain-match-dialog)
                                   (fountain-match-paren)
                                   (fountain-tachyon-p))))
                         ((eq limit 'scene)
                          (not (or (= (point) (buffer-end p))
                                   (fountain-match-character)
                                   (fountain-match-scene-heading))))
                         ((not (or (= (point) (buffer-end p))
                                   (fountain-match-character)))))
              (forward-line p)))))
    (if (/= n 0)
        (while (/= n 0)
          (if (fountain-match-character)
              (forward-line p))
          (funcall move-fun)
          (setq n (- n p)))
      (forward-line 0)
      (funcall move-fun))))

(defun fountain-backward-character (&optional n)
  "Move backward N character (foward if N is negative)."
  (interactive "^p")
  (setq n (or n 1))
  (fountain-forward-character (- n)))


;;; Endnotes

(defgroup fountain-endnotes ()
  "Options for displaying endnotes.

Fountain endnotes are kept at the end of a script following an
endotes page break, defined as three or more \"=\" and the word
\"end\" (case-insensitive).

    === end [===]

The endnotes section is a good place to keep extensive notes or
scenes you want to move out of the script, but still wish to
reference. Endnotes are not exported.

WARNING: if using other Fountain apps, check to make sure they
support endnotes."
  :group 'fountain)

(defcustom fountain-endnotes-buffer-name
  "%s<endnotes>"
  "Name of buffer in which to display file endnotes.
`%s' is replaced with `buffer-name'.

To hide this buffer from the buffer list, prefix with a space."
  :type 'string
  :group 'fountain-endnotes)

(defcustom fountain-endnotes-select-window
  nil
  "If non-nil, switch to endnotes window upon displaying it."
  :type 'boolean
  :group 'fountain-endnotes)

(defcustom fountain-endnotes-window-side
  'right
  "Preferred side of frame to display endnotes window."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'fountain-endnotes)

(defcustom fountain-endnotes-window-size
  '(0.3 0.25)
  "Height and width of the endnotes window as a fraction of root window."
  :type '(list (float :tag "Height")
               (float :tag "Width"))
  :group 'fountain-endnotes)

;; (defcustom fountain-endnotes-display-function
;;   'display-buffer-pop-up-window
;;   "Buffer display function used to display endnotes."
;;   :type '(radio (const :tag "Pop-up new window" display-buffer-pop-up-window)
;;                 (const :tag "Pop-up new frame" display-buffer-pop-up-frame)
;;                 (const :tag "Show in same window" display-buffer-same-window))
;;   :group 'fountain-endnotes)

(defun fountain-show-or-hide-endnotes ()
  "Pop up a window containing endnotes of current buffer.

Display a window containing an indirect clone of the current
buffer, narrowed to the first endnotes page break to the end of
buffer.

The window displayed is a special \"side\" window, which will
persist even when calling \\[delete-other-windows]."
  (interactive)
  (set-buffer (or (buffer-base-buffer) (current-buffer)))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((beg (if (re-search-forward fountain-end-regexp nil t)
                     (point)))
            (src (current-buffer))
            (buf (format fountain-endnotes-buffer-name (buffer-name))))
        (if beg
            (if (get-buffer-window buf (selected-frame))
                (delete-windows-on buf (selected-frame))
              (display-buffer-in-side-window
               (or (get-buffer buf)
                   (make-indirect-buffer src buf t))
               (list (cons 'inhibit-same-window t)
                     (cons 'side fountain-endnotes-window-side)
                     (cons 'window-height (car fountain-endnotes-window-size))
                     (cons 'window-width (cadr fountain-endnotes-window-size))))
              (with-current-buffer buf
                (narrow-to-region (1+ beg) (point-max)))
              (if fountain-endnotes-select-window
                  (select-window (get-buffer-window buf (selected-frame))))
              (message "Showing `%s' endnotes; %s to hide" src
                       (key-description (where-is-internal this-command
                                                           overriding-local-map t))))
          (user-error "Buffer `%s' does not contain endnotes" (buffer-name)))))))


;;; Editing

(defcustom fountain-auto-upcase-scene-headings
  t
  "If non-nil, automatically upcase lines matching `fountain-scene-heading-regexp'."
  :type 'boolean
  :group 'fountain)

(defun fountain-auto-upcase ()
  (if (and fountain-auto-upcase-scene-headings
           (fountain-match-scene-heading))
      (upcase-region (line-beginning-position)
                     (or (match-end 3)
                         (point)))))

(defun fountain-upcase-line (&optional arg)
  "Upcase the line.
If prefixed with ARG, insert `.' at beginning of line to force
a scene heading."
  (interactive "P")
  (if arg
      (save-excursion
        (forward-line 0)
        (insert ".")))
  (upcase-region (line-beginning-position) (line-end-position)))

(defun fountain-upcase-line-and-newline (&optional arg)
  "Upcase the line and insert a newline.
If prefixed with ARG, insert `.' at beginning of line to force
a scene heading."
  (interactive "P")
  (if arg
      (unless (fountain-match-scene-heading)
        (save-excursion
          (forward-line 0)
          (insert "."))))
  (upcase-region (line-beginning-position) (point))
  (newline))

(defun fountain-delete-comments-in-region (start end)
  "Delete comments in region between START and END."
  (save-excursion
    (goto-char start)
    (if (and (search-forward "*/" end t)
             (not (search-backward "/*" start t))
             (search-backward "/*" nil t))
        (setq start (point)))
    (goto-char end)
    (if (and (search-backward "/*" start t)
             (not (search-forward "*/" end t))
             (search-forward "*/" nil t))
        (setq end (point))))
  (goto-char start)
  (while (re-search-forward fountain-comment-regexp end t)
    (goto-char (match-beginning 0))
    (skip-chars-backward "\s\t")
    (delete-region (point) (match-end 0))))

(defun fountain-insert-alternate-character ()
  "Insert second-last character within the scene, and newline."
  (interactive)
  (let* ((n -1)
         (character-1 (fountain-get-character n 'scene))
         (character-2 character-1))
    (while (and (stringp character-1)
                (string= character-1 character-2))
      (setq n (1- n)
            character-2 (fountain-get-character n 'scene)))
    (if character-2
        (let ((x (save-excursion
                   (skip-chars-backward "\s\n\t")
                   (point))))
          (delete-region x (point))
          (newline 2)
          (insert character-2))
      (message "No alternate character within scene"))
    (newline)))

(defun fountain-insert-synopsis ()
  "Insert synopsis below scene heading of current scene."
  (interactive)
  (widen)
  (when (outline-back-to-heading)
    (forward-line 1)
    (or (bolp) (newline))
    (unless (and (fountain-blank-p)
                 (save-excursion
                   (forward-line 1)
                   (fountain-blank-p)))
      (save-excursion
        (newline)))
    (insert "= ")
    (if (outline-invisible-p) (fountain-outline-cycle))))

(defun fountain-insert-note (&optional arg)
  "Insert a note based on `fountain-note-template' underneath current element.
If region is active and it is appropriate to act on, only
surround region with note delimiters (`[[ ]]'). If prefixed with
ARG (\\[universal-argument]), only insert note delimiters."
  (interactive "P")
  (let ((comment-start "[[")
        (comment-end "]]"))
    (if (or arg (use-region-p))
        (comment-dwim nil)
      (unless (fountain-blank-p)
        (re-search-forward "^[\s\t]*$" nil 'move))
      (unless (save-excursion
                (forward-line 1)
                (fountain-blank-p))
        (save-excursion
          (newline)))
      (comment-indent)
      (insert
       (replace-regexp-in-string
        fountain-template-key-regexp
        (lambda (match)
          (let ((key (match-string 1 match)))
            (cdr
             (assoc-string key (list (cons 'title (file-name-base (buffer-name)))
                                     (cons 'time (format-time-string fountain-time-format))
                                     (cons 'fullname user-full-name)
                                     (cons 'nick (capitalize user-login-name))
                                     (cons 'email user-mail-address))))))
        fountain-note-template)))))

(defun fountain-continued-dialog-refresh (&optional arg)
  "Add or remove continued dialog on characters speaking in succession.
If `fountain-add-continued-dialog' is non-nil, add
`fountain-continued-dialog-string' on characters speaking in
succession, otherwise remove all occurences.

If region is active, act on region, otherwise act on current
scene. If prefixed with ARG (\\[universal-argument]), act on
whole buffer (this can take a while).

WARNING: if you change `fountain-continued-dialog-string' then
call this function, strings matching the previous value will not
be recognized. Before changing that variable, first make sure to
set `fountain-add-continued-dialog' to nil and run this function,
then make the changes desired."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (make-marker))
            (end (make-marker))
            (job (make-progress-reporter "Refreshing continued dialog...")))
        ;; Set START and END markers since buffer contents will change.
        (set-marker start
                    (cond (arg (point-min))
                          ((use-region-p)
                           (region-beginning))
                          (t
                           (fountain-forward-scene 0)
                           (point))))
        (set-marker end
                    (cond (arg (point-max))
                          ((use-region-p)
                           (region-end))
                          (t
                           (fountain-forward-scene 1)
                           (point))))
        ;; Delete all matches in region.
        ;; FIXME: should this check character elements?
        (goto-char start)
        (while (re-search-forward
                (concat "\s*" fountain-continued-dialog-string) end t)
          (replace-match "")
          (progress-reporter-update job))
        ;; When FOUNTAIN-ADD-CONTINUED-DIALOG, add string where appropriate.
        (when fountain-add-continued-dialog
          (goto-char start)
          (while (< (point) end)
            (when (and (not (looking-at-p
                             (concat ".*" fountain-continued-dialog-string "$")))
                       (fountain-match-character)
                       (string= (fountain-get-character 0)
                                (fountain-get-character -1 'scene)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" fountain-continued-dialog-string)))
            (forward-line 1)
            (progress-reporter-update job)))
        (set-marker start nil)
        (set-marker end nil)
        (progress-reporter-done job)))))


;;; Scene Numbers

(defcustom fountain-prefix-revised-scene-numbers
  nil
  "If non-nil, prefix revision letters to new scene numbers.

If nil, when inserting new scene headings after numbering
existing scene headings, revised scene numbers work as follows:

    10
    10A <- new scene
    11

If non-nil, revised scene numbers work as follows:

    10
    A11 <- new scene
    11

WARNING: Using conflicting revised scene numbers in the same
script may result in errors in output."
  :type 'boolean
  :group 'fountain)

(defun fountain-scene-number-to-list (string)
  "Read scene number STRING and return a list.

If `fountain-prefix-revised-scene-numbers' is non-nil:

    \"10\" -> (10)
    \"AA10\" -> (9 1 1)

Or if nil:

    \"10\" -> (10)
    \"10AA\" -> (10 1 1)"
  (let (number revision)
    (when (stringp string)
      (if fountain-prefix-revised-scene-numbers
          (when (string-match "\\([a-z]*\\)[\\.-]*\\([0-9]+\\)[\\.-]*" string)
            (setq number (string-to-number (match-string 2 string))
                  revision (match-string 1 string))
            (unless (string-empty-p revision) (setq number (1- number))))
        (when (string-match "\\([0-9]+\\)[\\.-]*\\([a-z]*\\)[\\.-]*" string)
          (setq number (string-to-number (match-string-no-properties 1 string))
                revision (match-string-no-properties 2 string))))
      (setq revision (mapcar #'(lambda (n) (- (upcase n) 64)) revision))
      (cons number revision))))

(defun fountain-scene-number-to-string (list)
  "Read scene number LIST and return a string.

If `fountain-prefix-revised-scene-numbers' is non-nil:

    (10) -> \"10\"
    (9 1 1) -> \"AA10\"

Or, if nil:

    (10) -> \"10\"
    (9 1 1) -> \"9AA\""
  (when (listp list)
    (let ((number (car list))
          (revision (mapconcat #'(lambda (char) (char-to-string (+ char 64)))
                               (cdr list) nil)))
      (if fountain-prefix-revised-scene-numbers
          (progn
            (unless (string-empty-p revision) (setq number (1+ number)))
            (concat revision (number-to-string number)))
        (concat (number-to-string number) revision)))))

(defun fountain-get-scene-number (&optional n)
  "Return the scene number of the Nth next scene as a list.
Return Nth previous if N is negative."
  (unless n (setq n 0))
  (save-excursion
    (save-restriction
      (widen)
      ;; Make sure we're at a scene heading.
      (fountain-forward-scene 0)
      ;; Go to the Nth scene.
      (unless (= n 0) (fountain-forward-scene n))
      ;; Unless we're at a scene heading now, raise a user error.
      (unless (fountain-match-scene-heading)
        (user-error "Before first scene heading"))
      (let ((x (point))
            (err-order "Scene `%s' seems to be out of order")
            found)
        ;; First, check if there are any scene numbers already. If not we can
        ;; save a lot of work.
        (save-match-data
          (goto-char (point-min))
          (while (not (or found (eobp)))
            (if (and (re-search-forward fountain-scene-heading-regexp nil 'move)
                     (match-string 6))
                (setq found t))))
        (if found
            ;; There are scene numbers, so this scene number needs to be
            ;; calculated relative to those.
            (let ((current-scene (fountain-scene-number-to-list (match-string 6)))
                  last-scene next-scene)
              ;; Check if scene heading is already numbered and if there is a
              ;; NEXT-SCENE. No previousscene number can be greater or equal to
              ;; this.
              (goto-char x)
              (while (not (or next-scene (eobp)))
                (fountain-forward-scene 1)
                (if (fountain-match-scene-heading)
                    (setq next-scene (fountain-scene-number-to-list (match-string 6)))))
              (cond
               ;; If there's both a NEXT-SCENE and CURRENT-SCENE, but NEXT-SCENE
               ;; is less or equal to CURRENT-SCENE, scene numbers are out of
               ;; order.
               ((and current-scene next-scene
                     (version-list-<= next-scene current-scene))
                (user-error err-order (fountain-scene-number-to-string current-scene)))
               ;; Otherwise, if there is a CURRENT-SCENE and either no
               ;; NEXT-SCENE or there is and it's greater then CURRENT-SCENE,
               ;; just return CURRENT-SCENE.
               (current-scene)
               (t
                ;; There is no CURRENT-SCENE yet, so go to the first scene
                ;; heading and if it's already numberd set it to that, or just
                ;; (list 1).
                (goto-char (point-min))
                (unless (fountain-match-scene-heading)
                  (fountain-forward-scene 1))
                (if (<= (point) x)
                    (setq current-scene
                          (or (fountain-scene-number-to-list (match-string 6))
                              (list 1))))
                ;; While before point X, go forward through each scene heading,
                ;; setting LAST-SCENE to CURRENT-SCENE and CURRENT-SCENE to an
                ;; incement of (car LAST-SCENE).
                (while (< (point) x (point-max))
                  (fountain-forward-scene 1)
                  (when (fountain-match-scene-heading)
                    (setq last-scene current-scene
                          current-scene (or (fountain-scene-number-to-list (match-string 6))
                                            (list (1+ (car last-scene)))))
                    ;; However, this might make CURRENT-SCENE greater or equal
                    ;; to NEXT-SCENE (a problem), so if there is a NEXT-SCENE,
                    ;; and NEXT-SCENE is less or equal to CURRENT-SCENE:
                    ;;
                    ;; 1. pop (car LAST-SCENE), which should always be less than
                    ;;    NEXT-SCENE as N
                    ;; 2. set CURRENT-SCENE to (list TMP-SCENE (1+ N))
                    ;; 3. set TMP-SCENE to (list TMP-SCENE n)
                    ;;
                    ;; Loop through this so that the last (or only) element of
                    ;; CURRENT-SCENE is incremented by 1, and TMP-SCENE is
                    ;; appended with N or 1. e.g.
                    ;;
                    ;;    CURRENT-SCENE (4 2) -> (4 3)
                    ;;    TMP-SCENE (4 2) -> (4 2 1)
                    ;;
                    ;; Return CURRENT-SCENE.
                    (let (n tmp-scene)
                      (while (and next-scene (version-list-<= next-scene current-scene))
                        (setq n (pop last-scene)
                              current-scene (append tmp-scene (list (1+ (or n 0))))
                              tmp-scene (append tmp-scene (list (or n 1))))
                        (if (version-list-<= next-scene tmp-scene)
                            (user-error err-order (fountain-scene-number-to-string current-scene)))))))
                current-scene)))
          ;; Otherwise there were no scene numbers, so we can just count
          ;; the scenes.
          (goto-char (point-min))
          (unless (fountain-match-scene-heading)
            (fountain-forward-scene 1))
          (let ((current-scene 1))
            (while (< (point) x)
              (fountain-forward-scene 1)
              (if (fountain-match-scene-heading)
                  (setq current-scene (1+ current-scene))))
            (list current-scene)))))))

(defun fountain-remove-scene-numbers ()
  "Remove scene numbers from scene headings in current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let (buffer-invisibility-spec)
        (goto-char (point-min))
        (unless (fountain-match-scene-heading)
          (fountain-forward-scene 1))
        (while (and (fountain-match-scene-heading)
                    (< (point) (point-max)))
          (if (match-string 6)
              (delete-region (match-beginning 4)
                             (match-end 7)))
          (fountain-forward-scene 1))))))

(defun fountain-add-scene-numbers ()
  "Add scene numbers to scene headings in current buffer.

Adding scene numbers to scene headings after numbering existing
scene headings will use a prefix or suffix letter, depending on
the value of `fountain-prefix-revised-scene-numbers':

    10
    10A <- new scene
    10B <- new scene
    11

If further scene headings are inserted:

    10
    10A
    10AA <- new scene
    10B
    11

In this example, you can't automatically number a new scene
between 10 and 10A (which might be numbered as 10aA). Instead,
add these scene numbers manually. Note that if
`fountain-auto-upcase-scene-headings' is non-nil you will need to
insert the scene number delimiters (\"##\") first, to protect the
scene number from being auto-upcased."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((job (make-progress-reporter "Adding scene numbers..."))
            buffer-invisibility-spec)
        (goto-char (point-min))
        (unless (fountain-match-scene-heading)
          (fountain-forward-scene 1))
        (while (and (fountain-match-scene-heading)
                    (< (point) (point-max)))
          (unless (match-string 6)
            (end-of-line)
            (delete-horizontal-space t)
            (insert "\s#" (fountain-scene-number-to-string (fountain-get-scene-number)) "#"))
          (fountain-forward-scene 1)
          (progress-reporter-update job))
        (progress-reporter-done job)))))


;;; Font Lock

(defvar fountain-font-lock-keywords-plist
  (backquote
   (;; Section Headings
    (,fountain-section-heading-regexp
     ((:level 2 :subexp 0 :face fountain-section-heading
              :invisible section-heading)
      (:level 2 :subexp 2 :face fountain-non-printing
              :override t))
     fountain-align-scene-heading)
    ;; Scene Headings
    ((lambda (limit)
       (fountain-match-element 'fountain-match-scene-heading limit))
     ((:level 2 :subexp 0 :face fountain-scene-heading
              :invisible scene-heading)
      (:level 2 :subexp 2 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override append
              :laxmatch t)
      (:level 2 :subexp 4
              :laxmatch t)
      (:level 2 :subexp 5 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override append
              :laxmatch t)
      (:level 2 :subexp 6
              :override append
              :laxmatch t)
      (:level 2 :subexp 7 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override append
              :laxmatch t))
     fountain-align-scene-heading)
    ;; Character
    ((lambda (limit)
       (fountain-match-element 'fountain-match-character limit))
     ((:level 3 :subexp 0 :face fountain-character
              :invisible character)
      (:level 3 :subexp 2
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)
      (:level 3 :subexp 5 :face highlight
              :override append
              :laxmatch t))
     fountain-align-character)
    ;; Parenthetical
    ((lambda (limit)
       (fountain-match-element 'fountain-match-paren limit))
     ((:level 3 :subexp 0 :face fountain-paren
              :invisible paren))
     fountain-align-paren)
    ;; Dialog
    ((lambda (limit)
       (fountain-match-element 'fountain-match-dialog limit))
     ((:level 3 :subexp 0 :face fountain-dialog
              :invisible dialog))
     fountain-align-dialog)
    ;; Transition
    ((lambda (limit)
       (fountain-match-element 'fountain-match-trans limit))
     ((:level 3 :subexp 0 :face fountain-trans
              :invisible trans)
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t))
     fountain-align-trans)
    ;; Center text
    (,fountain-center-regexp
     ((:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)
      (:level 3 :subexp 3
              :invisible center)
      (:level 2 :subexp 4 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t))
     fountain-align-center)
    ;; Page-break
    (,fountain-page-break-regexp
     ((:level 2 :subexp 0 :face fountain-page-break
              :invisible page-break)
      (:level 2 :subexp 2 :face fountain-page-number
              :override t
              :laxmatch t)))
    ;; Synopses
    (,fountain-synopsis-regexp
     ((:level 2 :subexp 0 :face fountain-synopsis
              :invisible synopsis)
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t))
     fountain-align-synopsis)
    ;; Notes
    (,fountain-note-regexp
     ((:level 2 :subexp 0 :face fountain-note
              :invisible note)))
    ;; Inclusions
    (,fountain-include-regexp
     ((:level 2 :subexp 0 :face fountain-include
              :invisible include)))
    ;; Metedata
    ((lambda (limit)
       (fountain-match-element 'fountain-match-metadata limit))
     ((:level 2 :subexp 0 :face fountain-metadata-key
              :invisible metadata
              :laxmatch t)
      (:level 2 :subexp 3 :face fountain-metadata-value
              :override t
              :laxmatch t)))
    ;; Action
    ((lambda (limit)
       (fountain-match-element 'fountain-match-action limit))
     ((:level 1 :subexp 0 :face fountain-action
              :invisible action)
      (:level 1 :subexp 1 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t))
     fountain-align-action)
    ;; Non-breaking space
    (,fountain-nbsp-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override append)))
    ;; Underline text
    (,fountain-underline-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face underline
              :override append)
      (:level 1 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Italic text
    (,fountain-italic-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face italic
              :override append)
      (:level 1 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Bold text
    (,fountain-bold-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face bold
              :override append)
      (:level 1 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Bold-Italic text
    (,fountain-bold-italic-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face bold-italic
              :override append)
      (:level 1 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Lyrics
    (,fountain-lyrics-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face italic
              :override append)))))
  "List of face properties to create element Font Lock keywords.
Takes the format:

    (ELEMENT MATCHER SUB-PLIST)

The first element, ELEMENT, is a string naming the element; if
nil, this face is not considered an element. MATCHER is a regular
expression or search function. SUB-PLIST is a list of plists,
assigning the following keywords:

    :level      integer representing level of `font-lock-maximum-decoration'
                at which face is applied
    :subexp     subexpression to match
    :face       face name to apply
    :invisible  if t, adds :face property to invisible text property
    :override   as per `font-lock-keywords'
    :laxmatch   as per `font-lock-keywords'")

(defun fountain-get-font-lock-decoration ()
  "Return the value of `font-lock-maximum-decoration' for `fountain-mode'."
  (let ((n (if (listp font-lock-maximum-decoration)
               (cdr (or (assq 'fountain-mode font-lock-maximum-decoration)
                        (assq 't font-lock-maximum-decoration)))
             font-lock-maximum-decoration)))
    (cond ((null n) 2)
          ((eq n t) 3)
          ((integerp n) n)
          (t 2))))

(defun fountain-set-font-lock-decoration (n)
  "Set `font-lock-maximum-decoration' for `fountain-mode' to N."
  (interactive "NMaximum decoration (1-3): ")
  (if (and (integerp n)
           (<= 1 n 3))
      (let ((level (cond ((= n 1) 1)
                         ((= n 2) nil)
                         ((= n 3) t))))
        (cond ((listp font-lock-maximum-decoration)
               (setq font-lock-maximum-decoration
                     (assq-delete-all 'fountain-mode font-lock-maximum-decoration))
               (customize-set-variable 'font-lock-maximum-decoration
                                       (cons (cons 'fountain-mode level)
                                             font-lock-maximum-decoration)))
              ((or (booleanp font-lock-maximum-decoration)
                   (integerp font-lock-maximum-decoration))
               (customize-set-variable 'font-lock-maximum-decoration
                                       (list (cons 'fountain-mode level)
                                             (cons 't font-lock-maximum-decoration)))))
        (message "Syntax highlighting is now: %s"
                 (cond ((= n 1) "minimum")
                       ((= n 2) "default")
                       ((= n 3) "maximum")))
        (font-lock-refresh-defaults)
        (font-lock-ensure (save-excursion
                            (goto-char (point-min))
                            (re-search-forward fountain-end-regexp nil 'move)
                            (point))
                          (point-max)))
    (user-error "Decoration must be an integer 1-3")))

(defun fountain-font-lock-extend-region ()
  "Extend region for fontification to text block."
  (defvar font-lock-beg nil)
  (defvar font-lock-end nil)
  (let ((beg
         (save-excursion
           (goto-char font-lock-beg)
           (re-search-backward "^[\s\t]*$"
                               (- (point) fountain-block-limit) 'move)
           (point)))
        (end
         (save-excursion
           (goto-char font-lock-end)
           (re-search-forward "^[\s\t]*$"
                              (+ (point) fountain-block-limit) 'move)
           (point)))
        changed)
    (goto-char font-lock-beg)
    (unless (or (bobp)
                (eq beg font-lock-beg))
      (setq font-lock-beg beg changed t))
    (goto-char font-lock-end)
    (unless (or (eobp)
                (eq end font-lock-end))
      (setq font-lock-end end changed t))
    changed))

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (fountain-init-vars)
  (let ((dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (var fountain-font-lock-keywords-plist keywords)
      (let ((matcher (car var))
            (plist-list (nth 1 var))
            (align (fountain-get-align (symbol-value (nth 2 var))))
            align-props facespec)
        (if (and align fountain-align-elements)
            (setq align-props (backquote (line-prefix (space :align-to ,align)
                                          wrap-prefix (space :align-to ,align)))))
        (dolist (var plist-list)
          (let ((subexp (plist-get var :subexp))
                (face (if (<= (plist-get var :level) dec)
                          (plist-get var :face)))
                (invisible (plist-get var :invisible))
                invisible-props)
            (if invisible
                (setq invisible-props (list 'invisible invisible)))
            (setq facespec
                  (append facespec
                          (list (backquote (,subexp '(face ,face
                                                           ,@align-props
                                                           ,@invisible-props)
                                                    ,(plist-get var :override)
                                                    ,(plist-get var :laxmatch))))))))
        (setq keywords
              (append keywords
                      (list (cons matcher facespec))))))))

(defun fountain-match-element (fun limit)
  "If FUN returns non-nil before LIMIT, return non-nil."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (if (funcall fun)
          (setq match t))
      (forward-line 1))
    match))

(defun fountain-redisplay-scene-numbers (start end)
  (goto-char start)
  (while (< (point) end)
    (if (fountain-match-scene-heading)
        (if (and fountain-display-scene-numbers-in-margin
                 (match-string 6))
            (put-text-property (match-beginning 6) (match-end 6)
                               'display
                               (list '(margin right-margin) (match-string-no-properties 6)))
          (remove-text-properties (match-beginning 0) (match-end 0) '(display))))
    (forward-line 1)))


;;; Key Bindings

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; editing commands
    (define-key map (kbd "C-c RET") #'fountain-upcase-line-and-newline)
    (define-key map (kbd "<S-return>") #'fountain-upcase-line-and-newline)
    (define-key map (kbd "C-c C-c") #'fountain-upcase-line)
    (define-key map (kbd "C-c C-d") #'fountain-continued-dialog-refresh)
    (define-key map (kbd "C-c C-z") #'fountain-insert-note)
    (define-key map (kbd "C-c C-a") #'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-x i") #'auto-insert)
    (define-key map (kbd "C-c C-x #") #'fountain-add-scene-numbers)
    (define-key map (kbd "C-c C-x _") #'fountain-remove-scene-numbers)
    (define-key map (kbd "C-c C-x f") #'fountain-set-font-lock-decoration)
    (define-key map (kbd "C-c C-o") #'fountain-include-find-file)
    ;; navigation commands
    (define-key map (kbd "C-M-n") #'fountain-forward-scene)
    (define-key map (kbd "C-M-p") #'fountain-backward-scene)
    (define-key map (kbd "C-M-a") #'fountain-beginning-of-scene)
    (define-key map (kbd "C-M-e") #'fountain-end-of-scene)
    (define-key map (kbd "C-M-h") #'fountain-mark-scene)
    (define-key map (kbd "M-g s") #'fountain-goto-scene)
    (define-key map (kbd "M-n") #'fountain-forward-character)
    (define-key map (kbd "M-p") #'fountain-backward-character)
    ;; outline commands
    (define-key map (kbd "C-c C-n") #'fountain-outline-next)
    (define-key map (kbd "C-c C-p") #'fountain-outline-previous)
    (define-key map (kbd "C-c C-f") #'fountain-outline-forward)
    (define-key map (kbd "C-c C-b") #'fountain-outline-backward)
    (define-key map (kbd "C-c C-u") #'fountain-outline-up)
    (define-key map (kbd "C-c C-^") #'fountain-outline-shift-up)
    (define-key map (kbd "C-c C-v") #'fountain-outline-shift-down)
    (define-key map (kbd "C-c C-SPC") #'fountain-outline-mark)
    (define-key map (kbd "TAB") #'fountain-outline-cycle)
    (define-key map (kbd "<backtab>") #'fountain-outline-cycle-global)
    (define-key map (kbd "S-TAB") #'fountain-outline-cycle-global)
    ;; endnotes
    (define-key map (kbd "M-s e") #'fountain-show-or-hide-endnotes)
    ;; exporting commands
    (define-key map (kbd "C-c C-e C-e") #'fountain-export-default)
    (define-key map (kbd "C-c C-e h") #'fountain-export-buffer-to-html)
    (define-key map (kbd "C-c C-e d") #'fountain-export-buffer-to-fdx)
    (define-key map (kbd "C-c C-e f") #'fountain-export-buffer-to-fountain)
    (define-key map (kbd "C-c C-e s") #'fountain-export-shell-command)
    ;; view commands
    (define-key map (kbd "C-c C-x !") #'fountain-toggle-hide-syntax-chars) ; FIXME ??
    (define-key map (kbd "C-c C-x *") #'fountain-toggle-hide-emphasis-delim) ; FIXME ??
    map)
  "Mode map for `fountain-mode'.")


;;; Settings

(defun fountain-toggle-custom-variable (var &optional elt)
  "Toggle variable VAR using `customize'.

If VAR's custom type is boolean, toggle the value of VAR,
otherwise, if ELT is provided, toggle the presence of ELT in VAR."
  (cond ((eq (get var 'custom-type) 'boolean)
         (customize-set-variable var (not (symbol-value var))))
        ((and elt
              (listp (eval (car (get var 'standard-value)))))
         (if (memq elt (symbol-value var))
             (customize-set-variable var (delq elt (symbol-value var)))
           (customize-set-variable var (cons elt (symbol-value var))))))
  (font-lock-refresh-defaults)
  (message "%s is now: %s"
           (custom-unlispify-tag-name var)
           (symbol-value var)))

(defun fountain-toggle-export-title-page ()
  "Toggle title-page in `fountain-export-include-elements'."
  (interactive)
  (let* ((var fountain-export-include-elements)
         (format (or (plist-get (fountain-read-metadata)
                                'format)
                     "screenplay"))
         (list (cdr (assoc-string format var))))
    (setq list
          (if (memq 'title-page list)
              (delq 'title-page list)
            (cons 'title-page list)))
    (setq var (delq (assoc-string format var) var))
    (customize-set-variable 'fountain-export-include-elements
                            (cons (cons format list) var))
    (message "Title page export is now %s"
             (if (memq 'title-page list) "enabled" "disabled"))))

(defun fountain-toggle-hide-element (element)
  "Toggle visibility of fountain-ELEMENT, using S for feedback.
Toggles the value of fountain-hide-ELEMENT, then, if
fountain-hide-ELEMENT is non-nil, adds fountain-ELEMENT to
`buffer-invisibility-spec', otherwise removes it."
  (let* ((option (intern (concat "fountain-hide-" element)))
         (symbol (intern (concat "fountain-" element))))
    (customize-set-variable option
                            (not (symbol-value option)))
    (if (symbol-value option)
        (add-to-invisibility-spec symbol)
      (remove-from-invisibility-spec symbol))
    (font-lock-refresh-defaults)
    (message "%s are now: %s"
             (custom-unlispify-tag-name symbol)
             (if (symbol-value option)
                 "invisible" "visible"))))

(defun fountain-toggle-hide-emphasis-delim ()
  "Toggle `fountain-hide-emphasis-delim'."
  (interactive)
  (fountain-toggle-hide-element "emphasis-delim"))

(defun fountain-toggle-hide-syntax-chars ()
  "Toggle `fountain-hide-syntax-chars'."
  (interactive)
  (fountain-toggle-hide-element "syntax-chars"))

(defun fountain-toggle-comment-syntax ()
  "Toggle `fountain-switch-comment-syntax'."
  (interactive)
  (customize-set-variable 'fountain-switch-comment-syntax
                          (not fountain-switch-comment-syntax))
  (fountain-init-comment-syntax)
  (message "Fountain Default Comment Syntax is now: %s"
           (if fountain-switch-comment-syntax
               "\"// COMMENT\"" "\"/* COMMENT */\"")))

(defun fountain-save-options ()
  "Save `fountain-mode' options with `customize'."
  (interactive)
  (let (unsaved)
    (dolist (option '(fountain-align-elements
                      fountain-add-continued-dialog
                      fountain-hide-emphasis-delim
                      fountain-hide-syntax-chars
                      fountain-display-scene-numbers-in-margin
                      fountain-export-scene-heading-format
                      font-lock-maximum-decoration))
      (if (customize-mark-to-save option)
          (setq unsaved t)))
    (if unsaved (custom-save-all))))


;;; Menu

(require 'easymenu)

(easy-menu-define fountain-mode-menu fountain-mode-map
  "Menu for `fountain-mode'."
  '("Fountain"
    ("Navigation"
     ["Go to Scene Heading..." fountain-goto-scene]
     "---"
     ["Next Scene Heading" fountain-forward-scene]
     ["Previous Scene Heading" fountain-backward-scene]
     "---"
     ["Next Character" fountain-forward-character]
     ["Previous Character" fountain-backward-character])
    ("Outlining"
     ["Cycle Scene/Section Visibility" fountain-outline-cycle]
     ["Cycle Global Visibility" fountain-outline-cycle-global]
     "---"
     ["Up Heading" fountain-outline-up]
     ["Next Heading" fountain-outline-next]
     ["Previous Heading" fountain-outline-previous]
     ["Forward Heading" fountain-outline-forward]
     ["Backward Heading" fountain-outline-backward]
     "---"
     ["Mark Section/Scene" fountain-outline-mark]
     ["Shift Section/Scene Up" fountain-outline-shift-up]
     ["Shift Section/Scene Down" fountain-outline-shift-down])
    ("Locking"
     ["Add Scene Numbers" fountain-add-scene-numbers]
     ["Remove Scene Numbers" fountain-remove-scene-numbers])
    "---"
    ["Insert Metadata" auto-insert]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    ["Add/Remove Continued Dialog" fountain-continued-dialog-refresh]
    "---"
    ("Exporting"
     ["Default" fountain-export-default]
     "---"
     ["Buffer to HTML" fountain-export-buffer-to-html]
     ["Buffer to Final Draft" fountain-export-buffer-to-fdx]
     ["Buffer to Fountain" fountain-export-buffer-to-fountain]
     "---"
     ["Run Shell Command" fountain-export-shell-command]
     "---"
     ["US Letter Page Size" (customize-set-variable 'fountain-export-page-size 'letter)
      :style radio
      :selected (eq fountain-export-page-size 'letter)]
     ["A4 Page Size" (customize-set-variable 'fountain-export-page-size 'a4)
      :style radio
      :selected (eq fountain-export-page-size 'a4)]
     "---"
     ["Include Title Page" fountain-toggle-export-title-page
      :style toggle
      :selected (fountain-export-include-element-p 'title-page)]
     ["Bold Scene Headings"
      (fountain-toggle-custom-variable
       'fountain-export-scene-heading-format 'bold)
      :style toggle
      :selected (memq 'bold fountain-export-scene-heading-format)]
     ["Double-Space Scene Headings"
      (fountain-toggle-custom-variable
       'fountain-export-scene-heading-format 'double-space)
      :style toggle
      :selected (memq 'double-space fountain-export-scene-heading-format)]
     ["Underline Scene Headings"
      (fountain-toggle-custom-variable
       'fountain-export-scene-heading-format 'underline)
      :style toggle
      :selected (memq 'underline fountain-export-scene-heading-format)]
     "---"
     ["Customize Export"
      (customize-group 'fountain-export)])
    "---"
    ["Display Elements Auto-Aligned"
     (fountain-toggle-custom-variable
      'fountain-align-elements)
     :style toggle
     :selected fountain-align-elements]
    ["Display Scene Numbers in Margin"
     (fountain-toggle-custom-variable
      'fountain-display-scene-numbers-in-margin)
     :style toggle
     :selected fountain-display-scene-numbers-in-margin]
    ["Auto-Upcase Scene Headings"
     (fountain-toggle-custom-variable
      'fountain-auto-upcase-scene-headings)
     :style toggle
     :selected fountain-auto-upcase-scene-headings]
    ["Add Continued Dialog"
     (fountain-toggle-custom-variable
      'fountain-add-continued-dialog)
     :style toggle
     :selected fountain-add-continued-dialog]
    "---"
    ("Syntax Highlighting"
     ["Minimum"
      (fountain-set-font-lock-decoration 1)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 1)]
     ["Default"
      (fountain-set-font-lock-decoration 2)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 2)]
     ["Maximum"
      (fountain-set-font-lock-decoration 3)
      :style radio
      :selected (= (fountain-get-font-lock-decoration) 3)])
    ("Show/Hide"
     ["Hide Emphasis Delimiters" fountain-toggle-hide-emphasis-delim
      :style toggle
      :selected fountain-hide-emphasis-delim]
     ["Hide Syntax Characters" fountain-toggle-hide-syntax-chars
      :style toggle
      :selected fountain-hide-syntax-chars])
    "---"
    ["Save Options" fountain-save-options]
    ["Customize Mode" (customize-group 'fountain)]
    ["Customize Faces" (customize-group 'fountain-faces)]))


;;; Syntax Table

(defvar fountain-mode-syntax-table
  (let ((syntax (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" syntax)
    (modify-syntax-entry ?* ". 23b" syntax)
    (modify-syntax-entry ?\n ">" syntax)
    syntax)
  "Syntax table for `fountain-mode'.")


;;; Mode Definition

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

;;;###autoload
(define-derived-mode fountain-mode text-mode "Fountain"
  "Major mode for screenwriting in Fountain markup."
  :group 'fountain
  (fountain-init-vars)
  (fountain-init-imenu-generic-expression)
  (setq font-lock-defaults
        '(fountain-create-font-lock-keywords nil t))
  (add-to-invisibility-spec (cons 'outline t))
  (if fountain-hide-emphasis-delim
      (add-to-invisibility-spec 'fountain-emphasis-delim))
  (if fountain-hide-syntax-chars
      (add-to-invisibility-spec 'fountain-syntax-chars))
  (setq-local font-lock-comment-face 'fountain-comment)
  (setq-local font-lock-extra-managed-props
              '(line-prefix wrap-prefix invisible))
  (let ((n (plist-get (fountain-read-metadata) 'startup-level)))
    (if (stringp n)
        (setq-local fountain-outline-startup-level
                    (min (string-to-number n) 6))))
  (add-hook 'post-self-insert-hook
            #'fountain-auto-upcase t t)
  (add-hook 'font-lock-extend-region-functions
            #'fountain-font-lock-extend-region t t)
  (if fountain-patch-emacs-bugs (fountain-patch-emacs-bugs))
  (jit-lock-register #'fountain-redisplay-scene-numbers t)
  (fountain-outline-hide-level fountain-outline-startup-level t))

(provide 'fountain-mode)

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:

;;; fountain-mode.el ends here
