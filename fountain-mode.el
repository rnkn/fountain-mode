;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2019 Free Software Foundation, Inc.
;; Copyright (c) 2019 Paul W. Rankin

;; Author: Paul W. Rankin <pwr@sdf.org>
;; Keywords: wp, text
;; Version: 2.7.4
;; Package-Requires: ((emacs "24.5"))
;; URL: https://fountain-mode.org
;; git: https://github.com/rnkn/fountain-mode

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

;; # Fountain Mode #

;; Fountain Mode is a scriptwriting program for GNU Emacs using the
;; Fountain plain text markup format.

;; For more information on the fountain markup format, visit
;; <https://fountain.io>.

;; Screenshot: <https://f002.backblazeb2.com/file/pwr-share/fountain-mode.png>

;; ## Features ##

;; - Support for Fountain 1.1 specification
;; - WYSIWYG auto-align elements (display only, does not modify file
;;   contents) specific to script format, e.g. screenplay, stageplay or
;;   user-defined format
;; - Navigation by section, scene, character name, or page
;; - 3 levels of syntax highlighting
;; - Integration with outline to fold/cycle visibility of sections and
;;   scenes
;; - Integration with imenu (sections, scene headings, notes)
;; - Intergration with auto-insert for title page metadata
;; - Traditional TAB auto-completion writing style
;; - Automatically add/remove character "(CONT'D)"
;; - Export to plain text, HTML, LaTeX, Final Draft (FDX), or Fountain
;; - Export to standalone document or snippet
;; - Emphasis (bold, italic, underlined text)
;; - Include external files with {{ include: FILENAME }}
;; - Optionally display scene numbers in the right margin
;; - Intelligent insertion of a page breaks
;; - Automatic loading for *.fountain files
;; - Include or omit a title page
;; - Toggle visibility of emphasis delimiters and syntax characters
;; - Everything is customizable

;; Check out the Nicholl Fellowship sample script exported from Fountain
;; Mode to the following formats:

;; - plain text: <https://f002.backblazeb2.com/file/pwr-share/Nicholl_Fellowship_sample.txt>
;; - HTML: <https://f002.backblazeb2.com/file/pwr-share/fountain-export.html>
;; - Final Draft: <https://f002.backblazeb2.com/file/pwr-share/fountain-export.fdx>
;; - LaTeX: <https://www.overleaf.com/project/54ed9180966959cb7fdbde8e>

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type C-h m.

;; ## Requirements ##

;; - Emacs 24.5
;; - LaTeX packages for PDF export: geometry fontspec titling fancyhdr
;;   marginnote ulem xstring oberdiek

;; ## Installation ##

;; The latest stable release of Fountain Mode is available via
;; [MELPA-stable] and can be installed with:

;;     M-x package-install RET fountain-mode RET

;; Alternately, download the [latest release], move this file into your
;; load-path and add to your .emacs/init.el file:

;;     (require 'fountain-mode)

;; If you prefer the latest but perhaps unstable version, install via
;; [MELPA], or clone the repository into your load-path and require as
;; above:

;;     git clone https://github.com/rnkn/fountain-mode.git

;; Users of Debian >=10 or Ubuntu >=18.04 can install Fountain Mode with:

;;     sudo apt install elpa-fountain-mode

;; [melpa]: https://melpa.org/#/fountain-mode "MELPA"
;; [melpa-stable]: https://stable.melpa.org/#/fountain-mode "MELPA-stable"
;; [latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

;; ## History ##

;; See: <https://github.com/rnkn/fountain-mode/releases>

;; ## Bugs and Feature Requests ##

;; To report bugs either use <https://github.com/rnkn/fountain-mode/issues>
;; or send an email to <help@fountain-mode.org>.


;;; Code:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

(eval-when-compile
  (require 'lisp-mnt)
  (defconst fountain-version
    (lm-version load-file-name)))

(defun fountain-version ()
  "Return `fountain-mode' version."
  (interactive)
  (message "Fountain Mode %s" fountain-version))

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'text)


;;; Obsolete Warnings

(define-obsolete-variable-alias 'fountain-align-centered
  'fountain-align-center "fountain-mode-1.1.0")

(define-obsolete-variable-alias 'fountain-hide-escapes
  'fountain-hide-syntax-chars "fountain-mode-1.3.0")

(make-obsolete-variable 'fountain-export-inline-style
                        "use inline style instead."
                        "fountain-mode-2.1.0")

(define-obsolete-variable-alias 'fountain-export-style-template
  'fountain-export-html-stylesheet "fountain-mode-2.4.0")

(define-obsolete-face-alias 'fountain-centered
  'fountain-center "fountain-mode-1.1.0")

(define-obsolete-face-alias 'fountain-scene-heading-highlight
  'fountain-scene-heading "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-note-highlight
  'fountain-note "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-section-highlight
  'fountain-section "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-synopsis-highlight
  'fountain-synopsis "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-center-highlight
  'fountain-center "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-character-highlight
  'fountain-character "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-paren-highlight
  'fountain-paren "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-dialog-highlight
  'fountain-dialog "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-trans-highlight
  'fountain-trans "fountain-mode-1.2.0")

(define-obsolete-face-alias 'fountain-section
  'fountain-section-heading "fountain-mode-1.4.1")

(make-obsolete-variable 'fountain-export-title-page-left-template
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-title-page-right-template
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(make-obsolete 'fountain-export-buffer-to-pdf-via-html
               'fountain-export-buffer-to-latex "fountain-mode-2.5.0")

(make-obsolete-variable 'fountain-export-pdf-via-html-command
                        'fountain-export-shell-command
                        "fountain-mode-2.0.0")

(make-obsolete-variable 'fountain-uuid-func
                        "use a third-party package instead."
                        "fountain-mode-2.0.0")

(make-obsolete-variable 'fountain-export-bold-scene-headings
                        'fountain-export-scene-heading-format
                        "fountain-mode-2.0.0")

(make-obsolete-variable 'fountain-export-underline-scene-headings
                        'fountain-export-scene-heading-format
                        "fountain-mode-2.0.0")

(make-obsolete-variable 'fountain-export-double-space-scene-headings
                        'fountain-export-scene-heading-format
                        "fountain-mode-2.0.0")

(make-obsolete-variable 'fountain-export-bold-title
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-underline-title
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-upcase-title
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-html-head-template
                        'fountain-export-html-template
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-html-use-inline-style
                        "use inline style instead."
                        "fountain-mode-2.1.0")

(make-obsolete-variable 'fountain-additional-template-replace-functions
                        "see `fountain-export-formats'."
                        "fountain-mode-2.4.0")

(make-obsolete 'fountain-insert-metadata
               'auto-insert "fountain-mode-2.1.2")

(make-obsolete-variable 'fountain-metadata-template
                        'fountain-metadata-skeleton
                        "fountain-mode-2.1.2")

(make-obsolete-variable 'fountain-long-time-format
                        'fountain-time-format
                        "fountain-mode-2.1.2")

(define-obsolete-variable-alias 'fountain-short-time-format
  'fountain-time-format "fountain-mode-2.1.2")

(make-obsolete-variable 'fountain-export-templates
                        "use individual export templates instead."
                        "fountain-mode-2.1.4")

(define-obsolete-variable-alias 'fountain-align-scene-number
  'fountain-display-scene-numbers-in-margin "fountain-mode-2.3.0")

(make-obsolete-variable 'fountain-export-format-replace-alist
                        "see `fountain-export-formats'."
                        "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-export-title-format
                        "edit individual export templates instead."
                        "fountain-mode-2.4.0")

(define-obsolete-variable-alias 'fountain-trans-list
  'fountain-trans-suffix-list "fountain-mode-2.2.2")

(make-obsolete-variable 'fountain-switch-comment-syntax
                        "use the standard comment syntax instead."
                        "fountain-mode-2.4.0")

(define-obsolete-variable-alias 'fountain-export-include-elements-alist
  'fountain-export-include-elements "fountain-mode-2.4.0")

(define-obsolete-variable-alias 'fountain-export-standalone
  'fountain-export-make-standalone "fountain-mode-2.4.0")

(define-obsolete-variable-alias 'fountain-export-buffer-name
  'fountain-export-tmp-buffer-name "fountain-mode-2.4.0")

(make-obsolete-variable 'fountain-outline-startup-level
                        'fountain-outline-custom-level
                        "fountain-mode-2.5.4")

(make-obsolete-variable 'fountain-endnotes-buffer
                        "use a third-party package instead."
                        "fountain-mode-2.6.0")

(make-obsolete-variable 'fountain-endnotes-window-side
                        "use a third-party package instead."
                        "fountain-mode-2.6.0")

(make-obsolete-variable 'fountain-endnotes-window-size
                        "use a third-party package instead."
                        "fountain-mode-2.6.0")

(make-obsolete-variable 'fountain-tab-command
                        "edit `fountain-mode-map' instead."
                        "fountain-mode-2.7.0")


;;; Customization

(defun fountain--set-and-refresh-all-font-lock (symbol value)
  "Set SYMBOL to VALUE and refresh defaults.

Cycle buffers and call `font-lock-refresh-defaults' when
`fountain-mode' is active."
  (set-default symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'fountain-mode)
        (font-lock-refresh-defaults)))))

(defcustom fountain-mode-hook
  '(turn-on-visual-line-mode fountain-outline-hide-custom-level)
  "Mode hook for `fountain-mode', run after the mode is turned on."
  :type 'hook
  :options '(turn-on-visual-line-mode
             fountain-outline-hide-custom-level
             fountain-completion-update
             turn-on-flyspell))

(defcustom fountain-script-format "screenplay"
  "Default script format for editing and exporting.

Can be overridden in metadata with, e.g.

    format: teleplay"
  :type 'string
  :safe 'string)

(defcustom fountain-add-continued-dialog
  t
  "\\<fountain-mode-map>If non-nil, \\[fountain-continued-dialog-refresh] will mark continued dialogue.

When non-nil, append `fountain-continued-dialog-string' to
successively speaking characters with `fountain-continued-dialog-refresh'.

When nil, remove `fountain-continued-dialog-string' with
`fountain-continued-dialog-refresh'."
  :type 'boolean
  :safe 'booleanp)

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
  :safe 'stringp)

(defcustom fountain-hide-emphasis-delim
  nil
  "If non-nil, make emphasis delimiters invisible."
  :type 'boolean
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (if fountain-hide-emphasis-delim
                   (add-to-invisibility-spec 'fountain-emphasis-delim)
                 (remove-from-invisibility-spec 'fountain-emphasis-delim))
               (font-lock-refresh-defaults))))))

(defcustom fountain-hide-syntax-chars
  nil
  "If non-nil, make syntax characters invisible."
  :type 'boolean
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (if fountain-hide-syntax-chars
                   (add-to-invisibility-spec 'fountain-syntax-chars)
                 (remove-from-invisibility-spec 'fountain-syntax-chars))
               (font-lock-refresh-defaults))))))

;; FIXME: fountain-mode shouldn't be formatting time, better to farm
;; this to something builtin.
(defcustom fountain-time-format
  "%F"
  "Format of date and time used when inserting `{{time}}'.
See `format-time-string'."
  :type 'string
  :safe 'stringp)

(defcustom fountain-note-template
  " {{time}} - {{fullname}}: "
  "\\<fountain-mode-map>Template for inserting notes with \\[fountain-insert-note].
To include an item in a template you must use the full {{KEY}}
syntax.

    {{title}}    Buffer name without extension
    {{time}}     Short date format (defined in option `fountain-time-format')
    {{fullname}} User full name (defined in option `user-full-name')
    {{nick}}     User first name (defined in option `user-login-name')
    {{email}}    User email (defined in option `user-mail-address')

The default {{time}} - {{fullname}}: will insert something like:

    [[ 2017-12-31 - Alan Smithee: ]]"
  :type 'string
  :safe 'stringp)


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
  :safe 'booleanp
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-section-heading
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which section headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-scene-heading
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which scene headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-synopsis
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 30))
  "Column integer to which synopses should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-action
  '(("screenplay" 0)
    ("teleplay" 0)
    ("stageplay" 20))
  "Column integer to which action should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-character
  '(("screenplay" 20)
    ("teleplay" 20)
    ("stageplay" 30))
  "Column integer to which characters names should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-dialog
  '(("screenplay" 10)
    ("teleplay" 10)
    ("stageplay" 0))
  "Column integer to which dialog should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-paren
  '(("screenplay" 15)
    ("teleplay" 15)
    ("stageplay" 20))
  "Column integer to which parentheticals should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-trans
  '(("screenplay" 45)
    ("teleplay" 45)
    ("stageplay" 30))
  "Column integer to which transitions should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-align-center
  '(("screenplay" 20)
    ("teleplay" 20)
    ("stageplay" 20))
  "Column integer to which centered text should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-display-scene-numbers-in-margin
  nil
  "If non-nil, display scene numbers in the right margin.

If nil, do not change scene number display.

This option does affect file contents."
  :type 'boolean
  :safe 'booleanp
  :set #'fountain--set-and-refresh-all-font-lock)

;; FIXME: a cleaner way would be:
;;   (fountain-get-align 'character) -> 20
(defun fountain-get-align (option)
  "Return OPTION align integer based on script format.
e.g.

    (fountain-get-align fountain-align-character) -> 20"
  (if (integerp option)
      option
    (cadr (or (assoc (or (plist-get (fountain-read-metadata)
                                    'format)
                         fountain-script-format)
                     option)
              (car option)))))


;;; Autoinsert

(require 'autoinsert)

(defvar fountain-metadata-skeleton
  '(nil
    "title: " (skeleton-read "Title: " (file-name-base (buffer-name))) | -7 "\n"
    "credit: " (skeleton-read "Credit: " "written by") | -9 "\n"
    "author: " (skeleton-read "Author: " user-full-name) | -9 "\n"
    "format: " (skeleton-read "Script format: " fountain-script-format) | -9 "\n"
    "source: " (skeleton-read "Source: ") | -9 "\n"
    "date: " (skeleton-read "Date: " (format-time-string fountain-time-format)) | -7 "\n"
    "contact:\n" ("Contact details, %s: " "    " str | -4 "\n") | -9))

(define-auto-insert '(fountain-mode . "Fountain metadata skeleton")
  fountain-metadata-skeleton)


;;; Regular Expressions

(defvar fountain-scene-heading-regexp
  nil
  "Regular expression for matching scene headings.

    Group 1: match leading . for forced scene heading
    Group 2: match whole scene heading without scene number
    Group 3: match INT/EXT
    Group 4: match location
    Group 5: match suffix separator
    Group 6: match suffix
    Group 7: match space between scene heading and scene number
    Group 8: match first # delimiter
    Group 9: match scene number
    Group 10: match last # delimiter

Requires `fountain-match-scene-heading' for preceding blank line.")

(defcustom fountain-scene-heading-suffix-sep
  " - "
  "String separating scene heading location from suffix.

WARNING: If you change this any existing scene headings will no
longer be parsed correctly."
  :type 'string
  :safe 'string
  :set #'fountain--set-and-refresh-all-font-lock)

(defcustom fountain-scene-heading-suffix-list
  '("DAY" "NIGHT" "CONTINUOUS" "LATER" "MOMENTS LATER")
  "List of scene heading suffixes (case insensitive).

These are only used for auto-completion. Any scene headings can
have whatever suffix you like.

Separated from scene heading locations with
`fountain-scene-heading-suffix-sep'."
  :type '(repeat (string :tag "Suffix"))
  :set #'fountain--set-and-refresh-all-font-lock)

(defvar fountain-trans-regexp
  nil
  "Regular expression for matching transitions.

    Group 1: match forced transition mark
    Group 2: match transition

Set with `fountain-init-trans-regexp'. Requires
`fountain-match-trans' for preceding and succeeding blank lines.")

(defconst fountain-action-regexp
  "^\\(!\\)?\\(.*\\)[\s\t]*$"
  "Regular expression for forced action.

    Group 1: match forced action mark
    Group 2: match trimmed whitespace (export group)")

(defconst fountain-comment-regexp
  (concat "\\(?://[\s\t]*\\(?:.*\\)\\)"
          "\\|"
          "\\(?:\\(?:/\\*\\)[\s\t]*\\(?:\\(?:.\\|\n\\)*?\\)[\s\t]*\\*/\\)")
  "Regular expression for matching comments.")

(defconst fountain-metadata-regexp
  (concat "^\\(?1:\\(?2:[^:\n]+\\):[\s\t]*\\(?3:.+\\)?\\)[\s\t]*"
          "\\|"
          "^[\s\t]+\\(?1:\\(?3:.+\\)\\)[\s\t]*")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-match-metadata' for `bobp'.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*\\(?1:\\(?:"
          "\\(?2:@\\)\\(?3:\\(?4:[^<>\n]+?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\|"
          "\\(?3:\\(?4:[^!#a-z<>\n]*?[A-Z][^a-z<>\n]*?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
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

(defconst fountain-note-regexp
  "\\(\\[\\[[\s\t]*\\(\\(?:.\n?\\)*?\\)[\s\t]*]]\\)"
  "Regular expression for matching notes.

    Group 1: note including [[ ]] delimiters
    Group 2: note (export group)")

(defconst fountain-section-heading-regexp
  "^\\(?1:#\\{1,5\\}\\)[\s\t]*\\(?2:[^#\n].*?\\)[\s\t]*$"
  "Regular expression for matching section headings.

    Group 1: match leading #'s
    Group 2: match heading")

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
          "\\([^\n\r\s\t\\*]+?[^\n\\*]*?\\)"
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


;;; Faces

(defgroup fountain-faces ()
  "\\<fountain-mode-map>Faces used in `fountain-mode'.
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
  :link '(info-link "(emacs) Font Lock")
  :group 'fountain)

(defface fountain
  '((t nil))
  "Default base-level face for `fountain-mode' buffers.")

(defface fountain-action
  '((t nil))
  "Default face for action.")

(defface fountain-comment
  '((t (:inherit shadow)))
  "Default face for comments (boneyard).")

(defface fountain-non-printing
  '((t (:inherit fountain-comment)))
  "Default face for emphasis delimiters and syntax characters.")

(defface fountain-metadata-key
  '((t (:inherit font-lock-constant-face)))
  "Default face for metadata keys.")

(defface fountain-metadata-value
  '((t (:inherit font-lock-keyword-face)))
  "Default face for metadata values.")

(defface fountain-page-break
  '((t (:inherit font-lock-constant-face)))
  "Default face for page breaks.")

(defface fountain-page-number
  '((t (:inherit font-lock-warning-face)))
  "Default face for page numbers.")

(defface fountain-scene-heading
  '((t (:inherit font-lock-function-name-face)))
  "Default face for scene headings.")

(defface fountain-paren
  '((t (:inherit font-lock-builtin-face)))
  "Default face for parentheticals.")

(defface fountain-center
  '((t nil))
  "Default face for centered text.")

(defface fountain-note
  '((t (:inherit font-lock-comment-face)))
  "Default face for notes.")

(defface fountain-section-heading
  '((t (:inherit font-lock-keyword-face)))
  "Default face for section headings.")

(defface fountain-synopsis
  '((t (:inherit font-lock-type-face)))
  "Default face for synopses.")

(defface fountain-character
  '((t (:inherit font-lock-variable-name-face)))
  "Default face for characters.")

(defface fountain-dialog
  '((t (:inherit font-lock-string-face)))
  "Default face for dialog.")

(defface fountain-trans
  '((t (:inherit font-lock-builtin-face)))
  "Default face for transitions.")

(defface fountain-template
  '((t (:inherit font-lock-preprocessor-face)))
  "Default face for template keys.")


;;; Initializing

(defcustom fountain-scene-heading-prefix-list
  '("INT" "EXT" "EST" "INT./EXT." "INT/EXT" "I/E")
  "List of scene heading prefixes (case insensitive).
Any scene heading prefix can be followed by a dot and/or a space,
so the following are equivalent:

    INT HOUSE - DAY

    INT. HOUSE - DAY"
  :type '(repeat (string :tag "Prefix"))
  :group 'fountain
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Don't call fountain-init-*' while in the middle of
         ;; loading this file!
         (when (featurep 'fountain-mode)
           (fountain-init-scene-heading-regexp)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'fountain-mode)
                 (fountain-init-outline-regexp)
                 (font-lock-refresh-defaults)))))))

(defcustom fountain-trans-suffix-list
  '("TO:" "WITH:" "FADE OUT" "TO BLACK")
  "List of transition suffixes (case insensitive).
This list is used to match the endings of transitions,
e.g. `TO:' will match both the following:

    CUT TO:

    DISSOLVE TO:"
  :type '(repeat (string :tag "Suffix"))
  :group 'fountain
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Don't call fountain-*' while in the middle of
         ;; loading this file!
         (when (featurep 'fountain-mode)
           (fountain-init-trans-regexp)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'fountain-mode)
                 (font-lock-refresh-defaults)))))))

(defun fountain-init-scene-heading-regexp ()
  "Initialize scene heading regular expression.
Uses `fountain-scene-heading-prefix-list' to create non-forced
scene heading regular expression."
  (setq fountain-scene-heading-regexp
        (concat
         "^\\(?:"
         ;; Group 1: match leading . (for forced scene heading)
         "\\(?1:\\.\\)"
         ;; Group 2: match scene heading without scene number
         "\\(?2:\\<"
         ;; Group 4: match location
         "\\(?4:.+?\\)"
         ;; Group 5: match suffix separator
         "\\(?:\\(?5:" fountain-scene-heading-suffix-sep "\\)"
         ;; Group 6: match suffix
         "\\(?6:.+\\)?\\)?"
         "\\)\\|"
         ;; Group 2: match scene heading without scene number
         "^\\(?2:"
         ;; Group 3: match INT/EXT
         "\\(?3:" (regexp-opt fountain-scene-heading-prefix-list) ".?\s+\\)"
         ;; Group 4: match location
         "\\(?4:.+?\\)?"
         ;; Group 5: match suffix separator
         "\\(?:\\(?5:" fountain-scene-heading-suffix-sep "\\)"
         ;; Group 6: match suffix
         "\\(?6:.+?\\)?\\)?"
         "\\)\\)"
         ;;; Match scene number
         "\\(?:"
         ;; Group 7: match space between scene heading and scene number
         "\\(?7:\s+\\)"
         ;; Group 8: match first # delimiter
         "\\(?8:#\\)"
         ;; Group 9: match scene number
         "\\(?9:[0-9a-z\\.-]+\\)"
         ;; Group 10: match last # delimiter
         "\\(?10:#\\)\\)?"
         "\s*$")))

(defun fountain-init-trans-regexp ()
  "Initialize transition regular expression.
Uses `fountain-trans-suffix-list' to create non-forced tranistion
regular expression."
  (setq fountain-trans-regexp
        (concat
         "^\\(?:[\s\t]*"
         ;; Group 1: match forced transition mark
         "\\(?1:>\\)[\s\t]*"
         ;; Group 2: match transition
         "\\(?2:[^<>\n]*?\\)"
         "\\|"
         ;; Group 2: match transition
         "\\(?2:[[:upper:]\s\t]*"
         (upcase (regexp-opt fountain-trans-suffix-list))
         "\\)"
         "\\)[\s\t]*$")))

(defun fountain-init-outline-regexp ()
  "Initialize `outline-regexp'."
  (setq-local outline-regexp
              (concat fountain-section-heading-regexp
                      "\\|"
                      fountain-scene-heading-regexp)))

(defun fountain-init-imenu-generic-expression ()
  "Initialize `imenu-generic-expression'."
  ;; FIXME: each of these should be a boolean user option to allow the
  ;; user to choose which appear in the imenu list.
  (setq imenu-generic-expression
        (list
         (list "Notes" fountain-note-regexp 2)
         (list "Scene Headings" fountain-scene-heading-regexp 2)
         (list "Sections" fountain-section-heading-regexp 0))))

(defun fountain-init-vars ()
  "Initialize important variables.
Needs to be called for every Fountain buffer because some
variatbles are required for functions to operate with temporary
buffers."
  (fountain-init-scene-heading-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-imenu-generic-expression)
  (modify-syntax-entry (string-to-char "/") ". 14" nil)
  (modify-syntax-entry (string-to-char "*") ". 23" nil)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-comment-face 'fountain-comment)
  (setq-local page-delimiter fountain-page-break-regexp)
  (setq-local outline-level #'fountain-outline-level)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local completion-cycle-threshold t)
  (setq-local completion-at-point-functions
              '(fountain-completion-at-point))
  (setq-local font-lock-extra-managed-props
              '(line-prefix wrap-prefix invisible))
  (setq font-lock-multiline 'undecided)
  (setq font-lock-defaults
        '(fountain-create-font-lock-keywords nil t))
  (add-to-invisibility-spec (cons 'outline t))
  (when fountain-hide-emphasis-delim
    (add-to-invisibility-spec 'fountain-emphasis-delim))
  (when fountain-hide-syntax-chars
    (add-to-invisibility-spec 'fountain-syntax-chars)))


;;; Emacs Bugs

(defcustom fountain-patch-emacs-bugs
  t
  "If non-nil, attempt to patch known bugs in Emacs.
See function `fountain-patch-emacs-bugs'."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain)

(defun fountain-patch-emacs-bugs ()
  "Attempt to patch known bugs in Emacs.

In Emacs versions prior to 26, adds advice to override
`outline-invisible-p' to return non-nil only if the character
after POS or point has invisible text property eq to 'outline.
See <http://debbugs.gnu.org/24073>."
  ;; In Emacs version prior to 26, `outline-invisible-p' returns non-nil for ANY
  ;; invisible property of text at point. We want to only return non-nil if
  ;; property is 'outline
  (unless (or (advice-member-p 'fountain-outline-invisible-p 'outline-invisible-p)
              (<= 26 emacs-major-version))
    (advice-add 'outline-invisible-p :override #'fountain-outline-invisible-p)
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
            (eval (read (current-buffer)) lexical-binding))))
      (message "fountain-mode: Function `outline-invisible-p' has been patched"))))


;;; Element Matching

(defun fountain-blank-before-p ()
  "Return non-nil if preceding line is blank or a comment."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (bobp)
          (progn (forward-line -1)
                 (or (and (bolp) (eolp))
                     (progn (end-of-line)
                            (forward-comment -1))))))))

(defun fountain-blank-after-p ()
  "Return non-nil if following line is blank or a comment."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line)
      (or (eobp)
          (and (bolp) (eolp))
          (forward-comment 1)))))

(defun fountain-match-metadata ()
  "Match metadata if point is at metadata, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (and (looking-at fountain-metadata-regexp)
           (or (bobp)
               (save-match-data
                 (forward-line -1)
                 (fountain-match-metadata)))))))

(defun fountain-match-page-break ()
  "Match page break if point is at page break, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (looking-at fountain-page-break-regexp))))

(defun fountain-match-section-heading ()
  "Match section heading if point is at section heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (looking-at fountain-section-heading-regexp))))

(defun fountain-match-synopsis ()
  "Match synopsis if point is at synopsis, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (looking-at fountain-synopsis-regexp))))

(defun fountain-match-note ()
  "Match note if point is at a note, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (looking-at fountain-note-regexp)
          (let ((x (point)))
            (and (re-search-backward "\\[\\[" nil t)
                 (looking-at fountain-note-regexp)
                 (< x (match-end 0))))))))

(defun fountain-match-scene-heading ()
  "Match scene heading if point is at a scene heading, nil otherwise."
  (save-excursion
    (save-restriction
      ;; Widen the restriction to ensure the previous line really is
      ;; blank.
      (widen)
      (beginning-of-line)
      (and (looking-at fountain-scene-heading-regexp)
           (fountain-blank-before-p)))))

(defun fountain-match-character ()
  "Match character if point is at character, nil otherwise."
  (unless (fountain-match-scene-heading)
    (save-excursion
      (beginning-of-line)
      (and (let ((case-fold-search nil))
             (looking-at fountain-character-regexp))
           (save-match-data
             (save-restriction
               (widen)
               (and (fountain-blank-before-p)
                    (save-excursion
                      (forward-line)
                      (unless (eobp)
                        (not (and (bolp) (eolp))))))))))))

(defun fountain-match-dialog ()
  "Match dialog if point is at dialog, nil otherwise."
  (unless (or (and (bolp) (eolp))
              (save-excursion (and (forward-comment 1) (eolp)))
              (fountain-match-paren)
              (fountain-match-note))
    (save-excursion
      (save-restriction
        (widen)
        (beginning-of-line)
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
      (beginning-of-line)
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
      (beginning-of-line)
      (and (let (case-fold-search)
             (looking-at fountain-trans-regexp))
           (fountain-blank-before-p)
           (save-match-data
             (fountain-blank-after-p))))))

(defun fountain-match-center ()
  "Match centered text if point is at centered text, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (looking-at fountain-center-regexp))))

(defun fountain-match-action ()
  "Match action text if point is at action, nil otherwise.
Assumes that all other element matching has been done."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (and (looking-at fountain-action-regexp)
               (match-string 1))
          (and (not (or (and (bolp) (eolp))
                        (fountain-match-section-heading)
                        (fountain-match-scene-heading)
                        (fountain-match-template)
                        (fountain-match-page-break)
                        (fountain-match-character)
                        (fountain-match-dialog)
                        (fountain-match-paren)
                        (fountain-match-trans)
                        (fountain-match-center)
                        (fountain-match-synopsis)
                        (fountain-match-metadata)
                        (fountain-match-note)))
               (looking-at fountain-action-regexp))))))

(defun fountain-get-element ()
  "Return element at point as a symbol."
  (cond
   ((and (bolp) (eolp)) nil)
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
   (t (looking-at fountain-action-regexp) 'action)))


;;; Auto-completion

(defvar-local fountain-completion-locations
  nil
  "List of scene locations in the current buffer.")

(defvar-local fountain-completion-characters
  nil
  "List of characters in the current buffer.
Each element is a cons (NAME . OCCUR) where NAME is a string, and
OCCUR is an integer representing the character's number of
occurrences. ")

(defcustom fountain-completion-additional-characters
  nil
  "List of additional characters to offer for completion.
Case insensitive, all character names will be made uppercase.

This is more useful when working with multiple files and set with
`add-dir-local-variable'."
  :type '(repeat (string :tag "Character"))
  :safe '(lambda (value)
           (and (listp value)
                (seq-every-p 'stringp value))))

(defcustom fountain-completion-additional-locations
  nil
  "List of additional locations to offer for completion.
Case insensitive, all locations will be made uppercase.

This is more useful when working with multiple files and set with
`add-dir-local-variable'."
  :type '(repeat (string :tag "Location"))
  :safe '(lambda (value)
           (and (listp value)
                (seq-every-p 'stringp value))))

(defun fountain-completion-get-characters ()
  "Return a list of characters for completion.

First, return second-last speaking character, followed by each
previously speaking character within scene. After that, return
characters from `fountain-completion-additional-characters' then
`fountain-completion-characters'.

n.b. `fountain-completion-additional-characters' are offered as
candidates ahead of `fountain-completion-characters' because
these need to be manually set, and so are considered more
important."
  (let (scene-characters
        alt-character
        contd-character
        rest-characters)
    (save-excursion
      (save-restriction
        (widen)
        (fountain-forward-character 0 'scene)
        (while (not (or (bobp) (fountain-match-scene-heading)))
          (when (fountain-match-character)
            (let ((character (match-string-no-properties 4)))
              (unless (member character scene-characters)
                (push (list character) scene-characters))))
          (fountain-forward-character -1 'scene))))
    (setq scene-characters (reverse scene-characters)
          alt-character (cadr scene-characters)
          contd-character (car scene-characters)
          rest-characters (cddr scene-characters)
          scene-characters nil)
    (when rest-characters
      (setq scene-characters rest-characters))
    (when contd-character
      (setq scene-characters
            (cons contd-character scene-characters)))
    (when alt-character
      (setq scene-characters
            (cons alt-character scene-characters)))
    (append scene-characters
            (mapcar 'upcase fountain-completion-additional-characters)
            fountain-completion-characters)))

(defun fountain-completion-at-point ()
  "\\<fountain-mode-map>Return completion table for entity at point.
Trigger completion with \\[fountain-dwim].

1. If point is at a scene heading and matches
`fountain-scene-heading-suffix-sep', offer completion candidates
from `fountain-scene-heading-suffix-list'.

2. If point is at a line matching
`fountain-scene-heading-prefix-list', offer completion candidates
from `fountain-completion-locations' and
`fountain-completion-additional-locations'.

3. If point is at beginning of line with a preceding blank line,
offer completion candidates from `fountain-completion-characters'
and `fountain-completion-additional-characters'. For more
information of character completion sorting, see
`fountain-completion-get-characters'.

Added to `completion-at-point-functions'."
  (cond ((and (fountain-match-scene-heading)
              (match-string 5))
         ;; Return scene heading suffix completion
         (list (match-end 5)
               (point)
               (completion-table-case-fold
                fountain-scene-heading-suffix-list)))
        ((and (fountain-match-scene-heading)
              (match-string 3))
         ;; Return scene location completion
         (list (match-end 3)
               (point)
               (completion-table-case-fold
                (append
                 (mapcar 'upcase fountain-completion-additional-locations)
                 fountain-completion-locations))))
        ((and (fountain-match-scene-heading)
              (match-string 1))
         ;; Return scene location completion (forced)
         (list (match-end 1)
               (point)
               (completion-table-case-fold
                (append
                 (mapcar 'upcase fountain-completion-additional-locations)
                 fountain-completion-locations))))
        ((and (eolp)
              (fountain-blank-before-p))
         ;; Return character completion
         (list (line-beginning-position)
               (point)
               (completion-table-case-fold
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      (list 'metadata
                            (cons 'display-sort-function 'identity)
                            (cons 'cycle-sort-function 'identity))
                    (complete-with-action
                     action (fountain-completion-get-characters)
                     string pred))))))))

(defun fountain-completion-update ()
  "Update completion candidates for current buffer.

While `fountain-completion-locations' are left unsorted for
`completion-at-point' to perform sorting,
`fountain-completion-characters' are sorted by number of lines.
For more information on character completion sorting, see
`fountain-completion-get-characters'.

Add to `fountain-mode-hook' to have completion upon load."
  (interactive)
  (setq fountain-completion-locations nil
        fountain-completion-characters nil)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (fountain-match-scene-heading)
          (let ((location (match-string-no-properties 4)))
            (unless (member location fountain-completion-locations)
              (push location fountain-completion-locations))))
        (fountain-forward-scene 1))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (fountain-match-character)
          (let ((character (match-string-no-properties 4))
                candidate lines)
            (setq candidate (assoc-string character
                                          fountain-completion-characters)
                  lines (cdr candidate))
            (if (null lines)
                (push (cons character 1) fountain-completion-characters)
              (cl-incf (cdr candidate)))))
        (fountain-forward-character 1))
      (setq fountain-completion-characters
            (sort fountain-completion-characters
                  #'(lambda (a b) (< (cdr b) (cdr a)))))))
  (message "Completion candidates updated"))


;;; Pages

(defgroup fountain-pages ()
  "Options for calculating page length."
  :group 'fountain
  :prefix "fountain-pages-")

(defcustom fountain-pages-max-lines
  '((letter . 55) (a4 . 60))
  "Integer representing maximum number of lines on a page.

WARNING: if you change this option after locking pages in a
script, you may get incorrect output."
  :type '(choice integer
                 (list (cons (const :tag "US Letter" letter) integer)
                       (cons (const :tag "A4" a4) integer))))

(defcustom fountain-pages-ignore-narrowing
  nil
  "Non-nil if counting pages should ignore buffer narrowing."
  :type 'boolean
  :safe 'booleanp)

(defvar fountain-elements
  '((section-heading
     :tag "Section Heading"
     :matcher fountain-section-heading-regexp
     :parser fountain-parse-section
     :fill fountain-fill-section-heading)
    (scene-heading
     :tag "Scene Heading"
     :parser fountain-parse-scene
     :fill fountain-fill-scene-heading)
    (action
     :tag "Action"
     :parser fountain-parse-action
     :fill fountain-fill-action)
    (character
     :tag "Character Name"
     :parser fountain-parse-dialog
     :fill fountain-fill-character)
    (lines
     :tag "Dialogue"
     :parser fountain-parse-lines
     :fill fountain-fill-dialog)
    (paren
     :tag "Parenthetical"
     :parser fountain-parse-paren
     :fill fountain-fill-paren)
    (trans
     :tag: "Transition"
     :parser fountain-parse-trans
     :fill fountain-fill-trans)
    (center
     :tag "Center Text"
     :matcher fountain-center-regexp
     :parser fountain-parse-center
     :fill fountain-fill-action)
    (page-break
     :tage "Page Break"
     :parser fountain-parse-page-break
     :matcher fountain-page-break-regexp)
    (synopsis
     :tag "Synopsis"
     :parser fountain-parse-synopsis
     :fill fountain-fill-action)
    (note
     :tag "Note"
     :parser fountain-parse-note
     :fill fountain-fill-note))
  "Association list of Fountain elements and their properties.
Includes references to various functions and variables.

Takes the form:

    (ELEMENT KEYWORD PROPERTY)")

(defvar fountain-export-page-size)
(defvar fountain-export-more-dialog-string)

(defun fountain-goto-page-break-point ()
  "Move point to appropriate place to break a page.
This is usually before point, but may be after if only skipping
over whitespace."
  ;; FIXME: currently does not account for elements not included in
  ;; `fountain-export-include-elements' for current format. This will
  ;; throw page off page counting in many cases.
  (when (looking-at "\n[\n\s\t]*\n") (goto-char (match-end 0)))
  (let ((element (fountain-get-element)))
    (cond
     ;; If we're are a section heading, scene heading or character, we
     ;; can safely break before.
     ((memq element '(section-heading scene-heading character))
      (beginning-of-line))
     ;; If we're at a parenthetical, check if the previous line is a
     ;; character. and if so call recursively on that element.
     ((eq element 'paren)
      (beginning-of-line)
      (let ((x (point)))
        (backward-char)
        (if (fountain-match-character)
            (progn
              (beginning-of-line)
              (fountain-goto-page-break-point))
          ;; Otherwise parenthetical is mid-dialogue, so get character
          ;; name and break at this element.
          (goto-char x))))
     ;; If we're at dialogue, skip over spaces then go to the
     ;; beginning of the current sentence.
     ((eq element 'lines)
      (skip-chars-forward "\s\t")
      (if (not (looking-back (sentence-end)
                             (save-excursion
                               (fountain-forward-character 0)
                               (point))))
          (forward-sentence -1)
        ;; This may move to character element, or back within
        ;; dialogue. If previous line is a character or parenthetical,
        ;; call recursively on that element. Otherwise, get character
        ;; name and break page here.
        (let ((x (point)))
          (backward-char)
          (if (or (fountain-match-character)
                  (fountain-match-paren))
              (fountain-goto-page-break-point)
            (goto-char x)))))
     ;; If we're at a transition or center text, skip backwards to
     ;; previous element and call recursively on that element.
     ((memq element '(trans center))
      (skip-chars-backward "\n\r\s\t")
      (beginning-of-line)
      (fountain-goto-page-break-point))
     ;; If we're at action, skip over spaces then go to the beginning
     ;; of the current sentence.
     ((eq element 'action)
      (skip-chars-forward "\s\t")
      (unless (or (bolp)
                  (looking-back (sentence-end) nil))
        (forward-sentence -1))
      ;; Then, try to skip back to the previous element. If it is a
      ;; scene heading, call recursively on that element. Otherwise,
      ;; break page here.
      (let ((x (point)))
        (skip-chars-backward "\n\r\s\t")
        (beginning-of-line)
        (if (fountain-match-scene-heading)
            (fountain-goto-page-break-point)
          (goto-char x)))))))

(defun fountain-forward-page (&optional n export-elements)
  "Move point forward by approximately N pages.

Moves forward from point, which is unlikely to correspond to
final exported pages and so probably should not be used
interactively.

To considerably speed up this function, supply EXPORT-ELEMENTS
with `fountain-get-export-elements'."
  (let ((skip-whitespace-fun
         (lambda ()
           (when (looking-at "[\n\s\t]*\n") (goto-char (match-end 0))))))
    (unless n (setq n 1))
    (while (< 0 n)
      ;; Pages don't begin with blank space, so skip over any at point.
      (funcall skip-whitespace-fun)
      ;; If we're at a page break, move to its end and skip over
      ;; whitespace.
      (when (fountain-match-page-break)
        (goto-char (match-end 0))
        (funcall skip-whitespace-fun))
      ;; Start counting lines.
      (let ((line-count 0))
        ;; Begin the main loop, which only halts if we reach the end
        ;; of buffer, a forced page break, or after the maximum lines
        ;; in a page.
        (while (and (< line-count (cdr (assq fountain-export-page-size
                                             fountain-pages-max-lines)))
                    (not (eobp))
                    (not (fountain-match-page-break)))
          (cond
           ;; If we're at the end of a line (but not also the
           ;; beginning, i.e. not a blank line) then move forward a
           ;; line and increment line-count.
           ((and (eolp) (not (bolp)))
            (forward-line)
            (setq line-count (1+ line-count)))
           ;; If we're looking at newline, skip over it and any
           ;; whitespace and increment line-count.
           ((looking-at "[\n\s\t]*\n")
            (goto-char (match-end 0))
            (setq line-count (1+ line-count)))
           ;; We are at an element. Find what kind of element. If it
           ;; is not included in export, skip over without
           ;; incrementing line-count (implement with block bounds).
           ;; Get the line width.
           (t
            (let ((element (fountain-get-element)))
              (if (memq element (or export-elements
                                    (fountain-get-export-elements)))
                  (progn
                    (fountain-move-to-fill-width element)
                    (setq line-count (1+ line-count)))
                ;; Element is not exported, so skip it without
                ;; incrementing line-count.
                (end-of-line)
                (funcall skip-whitespace-fun)))))))
      ;; We are not at the furthest point in a page. Skip over any
      ;; remaining whitespace, then go back to page-break point.
      (skip-chars-forward "\n\s\t")
      (fountain-goto-page-break-point)
      (setq n (1- n)))))

(defun fountain-move-to-fill-width (element)
  "Move point to column of ELEMENT fill limit suitable for breaking line.
Skip over comments."
  (let ((fill-width
         (cdr (symbol-value
               (plist-get (cdr (assq element fountain-elements))
                          :fill)))))
    (let ((i 0))
      (while (and (< i fill-width) (not (eolp)))
        (cond ((= (syntax-class (syntax-after (point))) 0)
               (forward-char 1)
               (setq i (1+ i)))
              ((forward-comment 1))
              (t
               (forward-char 1)
               (setq i (1+ i))))))
    (skip-chars-forward "\s\t")
    (when (eolp) (forward-line))
    (unless (bolp) (fill-move-to-break-point (line-beginning-position)))))

(defun fountain-insert-page-break (&optional string)
  "Insert a page break at appropriate place preceding point.
STRING is an optional page number string to force the page
number."
  (interactive "sPage number (RET for none): ")
  ;; Save a marker where we are.
  (let ((x (point-marker))
        (page-break
         (concat "===" (when (< 0 (string-width string))
                         (concat "\s" string "\s==="))))
        element)
    ;; Move point to appropriate place to break page.
    (fountain-goto-page-break-point)
    (setq element (fountain-get-element))
    ;; At this point, element can only be: section-heading,
    ;; scene-heading, character, action, paren or lines. Only paren and
    ;; lines require special treatment.
    (if (memq element '(lines paren))
        (let ((name (fountain-get-character -1)))
          (insert (concat
                   fountain-export-more-dialog-string "\n\n"
                   page-break "\n\n"
                   name "\s" fountain-continued-dialog-string "\n")))
      ;; Otherwise, insert the page break where we are. If the preceding
      ;; element is a page break, only replace the page number,
      ;; otherwise, insert the page break.
      (if (save-excursion
            (save-restriction
              (widen)
              (skip-chars-backward "\n\r\s\t")
              (fountain-match-page-break)))
          (replace-match page-break t t)
        (delete-horizontal-space)
        (unless (bolp) (insert "\n\n"))
        (insert-before-markers page-break "\n\n")))
    ;; Return to where we were.
    (goto-char x)))

(defun fountain-get-page-count ()
  "Return a cons of the current page number and the total pages."
  (let ((x (point))
        (total 0)
        (current 0)
        (end (point-max))
        (export-elements (fountain-get-export-elements))
        found)
    (save-excursion
      (save-restriction
        (when fountain-pages-ignore-narrowing (widen))
        (goto-char (point-min))
        (while (< (point) end)
          (fountain-forward-page 1 export-elements)
          (setq total (1+ total))
          (when (and (not found) (<= x (point)))
            (setq current total found t)))
        (cons current total)))))

(defun fountain-count-pages ()
  "Message the current page of total pages in current buffer.
n.b. This is an approximate calculation."
  (interactive)
  (let ((pages (fountain-get-page-count)))
    (message "Page %d of %d" (car pages) (cdr pages))))


;;; Templating

(defconst fountain-template-regexp
  "{{[\s\t\n]*\\(?1:[.-a-z0-9]+\\)\\(?::[\s\t\n]*\\(?2:[^{}]+?\\)\\)?[\s\t\n]*}}"
  "Regular expression for matching template keys.")

(defun fountain-match-template ()
  "Match template key if point is at template, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (or (looking-at fountain-template-regexp)
          (let ((x (point)))
            (and (re-search-backward "{{" nil t)
                 (looking-at fountain-template-regexp)
                 (< x (match-end 0))))))))

(defun fountain-find-included-file (&optional no-select)
  "Find included file at point.

Optional argument NO-SELECT will find file without selecting
window."
  (interactive)
  (when (and (fountain-match-template)
             (string-match-p "include" (match-string 1)))
    (let ((file (expand-file-name (match-string 2))))
      (if no-select
          (find-file-noselect file)
        (find-file file)))))

(defun fountain-include-replace-in-region (start end &optional delete)
  "Replace inclusions between START and END with their file contents.

If optional argument DELETE is non-nil (if prefix with \\[universal-argument]
when called interactively), delete instead.

If specified file is missing or otherwise not readable, replace
with empty string."
  (interactive "*r\nP")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (while (re-search-forward fountain-template-regexp end t)
        (when (string-match-p "include" (match-string 1))
          (if delete
              (delete-region (match-beginning 0) (match-end 0))
            (replace-match
              (let ((file (match-string 2)))
                (if (file-readable-p file)
                    (save-match-data
                      (with-current-buffer
                          (find-file-noselect (expand-file-name (match-string 2)))
                        (save-restriction
                          (widen)
                          (buffer-substring-no-properties (point-min) (point-max)))))
                  ""))
              t t))))
      (set-marker end nil))))


;;; Parsing

(defun fountain-get-character (&optional n limit)
  "Return Nth next character (or Nth previous if N is negative).

If N is non-nil, return Nth next character or Nth previous
character if N is negative, otherwise return nil. If N is nil or
0, return character at point, otherwise return nil.

If LIMIT is 'scene, halt at next scene heading. If LIMIT is
'dialog, halt at next non-dialog element."
  (unless n (setq n 0))
  (save-excursion
    (save-restriction
      (widen)
      (fountain-forward-character n limit)
      (when (fountain-match-character)
        (match-string-no-properties 4)))))

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
        (while (and (bolp)
                    (fountain-match-metadata))
          (let ((key (match-string-no-properties 2))
                (value (match-string-no-properties 3)))
            (forward-line)
            (while (and (fountain-match-metadata)
                        (null (match-string 2)))
              (setq value
                    (concat value (when value "\n")
                            (match-string-no-properties 3)))
              (forward-line))
            (setq list
                  (append list (list (intern (fountain-slugify key))
                                     value)))))
        list))))

(defun fountain-dual-dialog (&optional pos)
  "Non-nil if point or POS is within dual dialogue.
Returns \"right\" if within right-side dual dialogue, \"left\" if
within left-side dual dialogue, and nil otherwise."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (when pos (goto-char pos))
        (cond ((progn (fountain-forward-character 0 'dialog)
                      (and (fountain-match-character)
                           (stringp (match-string 5))))
               'right)
              ((progn (fountain-forward-character 1 'dialog)
                      (and (fountain-match-character)
                           (stringp (match-string 5))))
               'left))))))

;; FIXME: implement LIMIT
(defun fountain-starts-new-page ()
  "Return non-nil if current element follows a page break."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (beginning-of-line)
        (skip-chars-backward "\n\r\s\t")
        (fountain-match-page-break)))))

(defun fountain-parse-section (match-data &optional export-elements job)
  "Return an element list for matched section heading.

Use MATCH-DATA for match data. When \"section-heading\" included
in list argument EXPORT-ELEMENTS, parse element for export.

Update JOB."
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
                      'export (when (memq 'section-heading export-elements) t))
                (match-string-no-properties 3)))
         (end (save-excursion (outline-end-of-subtree) (point)))
         content)
    (goto-char (plist-get (nth 1 section-heading) 'end))
    (setq content
          (fountain-parse-region (point) end export-elements job))
    (list 'section
          (list 'begin beg
                'end end
                'starts-new-page starts-new-page
                'export t)
          (cons section-heading content))))

(defun fountain-parse-scene (match-data &optional export-elements job)
  "Return an element list for matched scene heading at point.

Use MATCH-DATA for match data. When \"scene-heading\" included
in list argument EXPORT-ELEMENTS, parse element for export.

Includes child elements. Update JOB."
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
                      'forced (stringp (match-string 1))
                      'export (when (memq 'scene-heading export-elements) t)
                      'starts-new-page starts-new-page)
                (match-string-no-properties 2)))
         (end (save-excursion (outline-end-of-subtree) (point)))
         content)
    (goto-char (plist-get (nth 1 scene-heading) 'end))
    (setq content
          (fountain-parse-region (point) end export-elements job))
    (list 'scene
          (list 'begin beg
                'end end
                'scene-number scene-number
                'starts-new-page starts-new-page
                'export t)
          (cons scene-heading content))))

(defun fountain-parse-dialog (match-data &optional export-elements job)
  "Return an element list for matched dialog.

Use MATCH-DATA for match data. When \"character\", \"lines\" or
\"paren\" included in list argument EXPORT-ELEMENTS, parse
element for export.

Update JOB."
  (set-match-data match-data)
  (let* ((beg (match-beginning 0))
         (starts-new-page (fountain-starts-new-page))
         (dual (fountain-dual-dialog))
         (character
          (list 'character
                (list 'begin (match-beginning 0)
                      'end (match-end 0)
                      'forced (stringp (match-string 2))
                      'export (when (memq 'character export-elements) t)
                      'starts-new-page (unless (eq dual 'left) starts-new-page))
                (match-string-no-properties 3)))
         (end
          (save-excursion
            (fountain-forward-character 1 'dialog)
            (skip-chars-backward "\n\r\s\t")
            (point)))
         first-dialog)
    (goto-char (plist-get (nth 1 character) 'end))
    ;; Parse the first dialogue tree, which may be the only dialogue
    ;; tree.
    (setq first-dialog
          (list 'dialog
                (list 'begin beg
                      'end end
                      'dual dual
                      'export (when (or (memq 'character export-elements)
                                        (memq 'lines export-elements)
                                        (memq 'paren export-elements))
                                t))
                (cons character
                      (fountain-parse-region (point) end export-elements job))))
    ;; If at the first (left) character of dual dialogue, parse a
    ;; dual-dialogue tree, containing dialogue trees.
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
                      'export (when (or (memq 'character export-elements)
                                        (memq 'lines export-elements)
                                        (memq 'paren export-elements))
                                t))
                ;; Add the first dialogue block to the head of the
                ;; dual-dialogue tree.
                (cons first-dialog
                      ;; Parse the containing region.
                      (fountain-parse-region
                       (plist-get (nth 1 first-dialog) 'end)
                       end export-elements job))))
      ;; Otherwise, return the first dialogue tree.
      first-dialog)))

(defun fountain-parse-lines (match-data &optional export-elements _job)
  "Return an element list for matched dialogue.

Use MATCH-DATA for match data. When \"lines\" included in list
argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (list 'lines
          (list 'begin beg
                'end end
                'export (when (memq 'lines export-elements) t))
          (match-string-no-properties 1))))

(defun fountain-parse-paren (match-data &optional export-elements _job)
  "Return an element list for matched parenthetical.

Use MATCH-DATA for match data. When \"paren\" included in list
argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'paren
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (when (memq 'paren export-elements) t))
        (match-string-no-properties 1)))

(defun fountain-parse-trans (match-data &optional export-elements _job)
  "Return an element list for matched transition.

Use MATCH-DATA for match data. When \"trans\" included in list
argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'trans
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'forced (stringp (match-string 1))
              'export (when (memq 'trans export-elements) t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 2)))

(defun fountain-parse-center (match-data &optional export-elements _job)
  "Return an element list for matched center text.

Use MATCH-DATA for match data. When \"center\" included in list
argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'center
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (when (memq 'center export-elements) t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 3)))

(defun fountain-parse-page-break (match-data &optional export-elements _job)
  "Return an element list for matched page break.

Use MATCH-DATA for match data. When \"page-break\" included in
list argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'page-break
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (when (memq 'page-break export-elements) t))
        (match-string-no-properties 2)))

(defun fountain-parse-synopsis (match-data &optional export-elements _job)
  "Return an element list for matched synopsis.

Use MATCH-DATA for match data. When \"synopsis\" included
in list argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'synopsis
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (when (memq 'synopsis export-elements) t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 3)))

(defun fountain-parse-note (match-data &optional export-elements _job)
  "Return an element list for matched note.

Use MATCH-DATA for match data. When \"note\" included
in list argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (list 'note
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'export (when (memq 'note export-elements) t)
              'starts-new-page (fountain-starts-new-page))
        (match-string-no-properties 2)))

(defun fountain-parse-action (match-data &optional export-elements _job)
  "Return an element list for matched action.

Use MATCH-DATA for match data. When \"action\" included
in list argument EXPORT-ELEMENTS, parse element for export."
  (set-match-data match-data)
  (let ((bounds (fountain-get-block-bounds))
        begin end string)
    (setq begin (car bounds))
    (save-excursion
      (goto-char (cdr bounds))
      (skip-chars-backward "\n\s\t")
      (setq end (point)))
    (setq string (buffer-substring-no-properties begin end)
          string (replace-regexp-in-string "^!" "" string))
    (list 'action
          (list 'begin begin
                'end end
                'forced (stringp (match-string 1))
                'export (when (memq 'action export-elements) t)
                'starts-new-page (fountain-starts-new-page))
          string)))

(defun fountain-parse-element (&optional export-elements job)
  "Call appropropriate parsing function for matched element at point.

Passes EXPORT-ELEMENTS for efficiency. Update JOB."
  (let ((parser (plist-get (cdr (assq (fountain-get-element)
                                      fountain-elements))
                           :parser)))
    (when parser (funcall parser (match-data) export-elements job))))

(defun fountain-parse-region (start end export-elements job)
  "Return a list of parsed element lists in region between START and END.
Elements are parsed for export if included in list argument EXPORT-ELEMENTS.

Update JOB as we go."
  (goto-char start)
  (setq end (min end (point-max)))
  (let (list)
    (while (< (point) end)
      (skip-chars-forward "\n\r\s\t")
      (beginning-of-line)
      (when (< (point) end)
        (let ((element (fountain-parse-element export-elements job)))
          (push element list)
          (goto-char (plist-get (nth 1 element) 'end))))
      (when job (progress-reporter-update job)))
    (reverse list)))

(defun fountain-prep-and-parse-region (start end)
  "Prepare and parse region between START and END."
  (let ((buffer (current-buffer))
        (export-elements (fountain-get-export-elements))
        (job (make-progress-reporter "Parsing...")))
    (prog1
        (with-temp-buffer
          (fountain-init-vars)
          (insert-buffer-substring buffer start end)
          (fountain-include-replace-in-region
           (point-min) (point-max) (not (memq 'include export-elements)))
          (fountain-delete-comments-in-region (point-min) (point-max))
          ;; Delete metadata.
          (goto-char (point-min))
          (while (fountain-match-metadata)
            (forward-line))
          (delete-region (point-min) (point))
          (fountain-parse-region (point-min) (point-max) export-elements job))
      (progress-reporter-done job))))


;;; Filling

(defgroup fountain-fill ()
  "Options for filling elements.

Filling elements is used in exporting to plaintext and
PostScript, and in calculating page length for page locking."
  :prefix "fountain-fill-"
  :group 'fountain-export)

(defcustom fountain-fill-section-heading
  '(0 . 61)
  "Cons cell of integers for indenting and filling section headings.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-scene-heading
  '(0 . 61)
  "Cons cell of integers for indenting and filling scene headings.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-action
  '(0 . 61)
  "Cons cell of integers for indenting and filling action.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-character
  '(20 . 38)
  "Cons cell of integers for indenting and filling character.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-paren
  '(15 . 26)
  "Cons cell of integers for indenting and filling parenthetical.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-dialog
  '(10 . 35)
  "Cons cell of integers for indenting and filling dialogue.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-trans
  '(42 . 16)
  "Cons cell of integers for indenting and filling transition.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-synopsis
  '(0 . 61)
  "Cons cell of integers for indenting and filling synopses.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-note
  '(0 . 61)
  "Cons cell of integers for indenting and filling notes.
The car sets `left-margin' and cdr `fill-column'."
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))


;;; Exporting

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

(defcustom fountain-export-include-elements
  '(("screenplay" scene-heading action character lines paren trans center page-break include)
    ("teleplay" section-heading scene-heading action character lines paren trans center page-break include)
    ("stageplay" section-heading scene-heading action character lines paren trans center page-break include))
  "Association list of elements to include when exporting.
Note that comments (boneyard) are never included."
  :type '(alist :key-type (string :tag "Format")
                :value-type (set :tag "Elements"
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
                                 (const :tag "Included Files" include))))

(defcustom fountain-export-make-standalone
  t
  "If non-nil, export a standalone document.

A standalone document is formatted with the export format's
document template in `fountain-export-templates'.

If nil, export snippet, which only formats each element. This is
useful when exporting parts of a script for inclusion in another
document."
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-export-tmp-buffer-name
  "*Fountain %s Export*"
  "Name of export buffer when source buffer is not visiting a file.
Passed to `format' with export format as single variable."
  :type 'string
  :safe 'stringp)

(define-obsolete-variable-alias 'fountain-export-default-command
  'fountain-export-default-function "fountain-mode-2.7.0")
(defcustom fountain-export-default-function
  #'fountain-export-buffer-to-latex
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type '(radio (function-item fountain-export-buffer-to-latex)
                (function-item fountain-export-buffer-to-html)
                (function-item fountain-export-buffer-to-fdx)
                (function-item fountain-export-buffer-to-fountain)
                (function-item fountain-export-buffer-to-txt)
                (function-item fountain-export-shell-command)))

(defcustom fountain-export-page-size
  'letter
  "Paper size to use on export."
  :type '(radio (const :tag "US Letter" letter)
                (const :tag "A4" a4)))

(defcustom fountain-export-font
  "Courier"
  "Font to use when exporting."
  :type '(string :tag "Font")
  :safe 'stringp)

(defcustom fountain-export-include-title-page
  t
  "If non-nil, include a title page in export."
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-export-contact-align-right
  nil
  "If non-nil, align title page contact block on the right."
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-export-number-first-page
  nil
  "If non-nil, add a page number to the first page.

Traditionally, screenplays omit a page number on the first page."
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-export-include-scene-numbers
  nil
  "If non-nil, include scene numbers in export."
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-export-scene-heading-format
  '(double-space)
  "List of format options applied when exporting scene headings.
Options are: bold, double-space, underline."
  :type '(set (const :tag "Bold" bold)
              (const :tag "Double-spaced" double-space)
              (const :tag "Underlined" underline)))

(defcustom fountain-export-more-dialog-string
  "(MORE)"
  "String to append to dialog when breaking across pages."
  :type 'string
  :safe 'stringp)

(defcustom fountain-export-shell-command
  "afterwriting --source %s --pdf --overwrite"
  "Shell command string to convert Fountain source to ouput.
`%s' will be substituted with `buffer-file-name'"
  :type 'string)

(defcustom fountain-export-use-title-as-filename
  nil
  "If non-nil, use title metadata as export filename.

This is useful if you are exporting to Fountain and need to
specify a different filename."
  :type 'boolean
  :safe 'booleanp)

(defvar fountain-export-formats
  '((html
     :tag "HTML"
     :ext ".html"
     :template fountain-export-html-template
     ;; FIXME: this should use the regexp vars
     :string-replace (("&" "&amp;")
                      ("<" "&lt;")
                      (">" "&gt;")
                      ("\\\\\\*" "&#42;")
                      ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "<strong><em>\\1</em></strong>")
                      ("\\*\\*\\(.+?\\)\\*\\*" "<strong>\\1</strong>")
                      ("\\*\\(.+?\\)\\*" "<em>\\1</em>")
                      ("^~\s*\\(.+?\\)$" "<i>\\1</i>")
                      ("_\\(.+?\\)_" "<span class=\"underline\">\\1</span>")
                      ("\n\n+" "<br><br>")
                      ("\n" "<br>"))
     :eval-replace ((stylesheet fountain-export-html-stylesheet)
                    (font fountain-export-font)
                    (scene-heading-spacing
                     (if (memq 'double-space fountain-export-scene-heading-format)
                         "2em" "1em"))
                    (title-page
                     (when fountain-export-include-title-page
                       fountain-export-html-title-page-template)))
     :hook fountain-export-html-hook)
    (tex
     :tag "LaTeX"
     :ext ".tex"
     :template fountain-export-tex-template
     :string-replace (("%" "\\\\%")
                      ("&" "\\\\&")
                      ("#" "\\\\#")
                      ("\\$" "\\\\$")
                      ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "\\\\textbf{\\\\emph{\\1}}")
                      ("\\*\\*\\(.+?\\)\\*\\*" "\\\\textbf{\\1}")
                      ("\\*\\(.+?\\)\\*" "\\\\emph{\\1}")
                      ("^~\s*\\(.+?\\)$\\*\\*" "\\\\textit{\\1}")
                      ("_\\(.+?\\)_" "\\\\uline{\\1}")
                      ("\n[\s\t]*\n+" "\\\\par")
                      ("\n" "\\\\protecting{\\\\\\\\}"))
     :eval-replace ((font fountain-export-font)
                    (scene-heading-spacing
                     (if (memq 'double-space fountain-export-scene-heading-format)
                         "true" "false"))
                    (scene-heading-underline
                     (if (memq 'underline fountain-export-scene-heading-format)
                         "true" "false"))
                    (scene-heading-bold
                     (if (memq 'bold fountain-export-scene-heading-format)
                         "true" "false"))
                    (title-page
                     (when fountain-export-include-title-page
                       fountain-export-tex-title-page-template))
                    (title-contact-align
                     (if fountain-export-contact-align-right
                         "true" "false"))
                    (number-first-page
                     (if fountain-export-number-first-page
                         "true" "false"))
                    (include-scene-numbers
                     (if fountain-export-include-scene-numbers
                         "true" "false")))
     :hook fountain-export-tex-hook)
    (fdx
     :tag "Final Draft"
     :ext ".fdx"
     :template fountain-export-fdx-template
     :string-replace (("&" "&#38;")
                      ("<" "&#60;")
                      (">" "&#62;")
                      ("\"" "&#34;")
                      ("'" "&#39;")
                      ("\\\\_" "&#96;")
                      ("\\\\\\*" "&#42;")
                      ("_\\*\\*\\*\\(.+?\\)\\*\\*\\*_" "</Text><Text Style=\"Bold+Underline+Italic\">\\1</Text><Text>")
                      ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "</Text><Text Style=\"Bold+Italic\">\\1</Text><Text>")
                      ("_\\*\\*\\(.+?\\)\\*\\*_" "</Text><Text Style=\"Bold+Underline\">\\1</Text><Text>")
                      ("_\\*\\(.+?\\)\\*_" "</Text><Text Style=\"Underline+Italic\">\\1</Text><Text>")
                      ("\\*\\*\\(.+?\\)\\*\\*" "</Text><Text Style=\"Bold\">\\1</Text><Text>")
                      ("\\*\\(.+?\\)\\*" "</Text><Text Style=\"Italic\">\\1</Text><Text>")
                      ("^~\s*\\(.+?\\)$" "</Text><Text Style=\"Italic\">\\1</Text><Text>")
                      ("_\\(.+?\\)_" "</Text><Text Style=\"Underline\">\\1</Text><Text>")
                      ("\n\n+" "\n\n"))
     :cond-replace ((t
                     (starts-new-page
                      (t "Yes") (nil "No"))))
     :eval-replace ((title-page
                     (when fountain-export-include-title-page
                       fountain-export-fdx-title-page-template)))
     :hook fountain-export-fdx-hook)
    (fountain
     :tag "Fountain"
     :ext ".fountain"
     :template fountain-export-fountain-template
     :cond-replace ((scene-heading
                     (forced (t ".")))
                    (character
                     (dual (right " ^"))
                     (forced (t "@")))
                    (trans
                     (forced (t "> ")))
                    (action
                     (forced (t "!")))
                    (section-heading
                     (level (1 "#")
                            (2 "##")
                            (3 "###")
                            (4 "####")
                            (5 "#####"))))
     :eval-replace ((title-page
                     (when fountain-export-include-title-page
                       fountain-export-fountain-title-page-template))
                    (scene-heading-spacing
                     (when (memq 'double-space fountain-export-scene-heading-format)
                       "\n")))
     :hook fountain-export-fountain-hook)
    (txt
     :tag "plaintext"
     :ext ".txt"
     :fill t
     :template fountain-export-txt-template
     :eval-replace ((scene-heading-spacing
                     (when (memq 'double-space fountain-export-scene-heading-format)
                       "\n"))
                    (title-page
                     (when fountain-export-include-title-page
                       fountain-export-txt-title-page-template)))
     :hook fountain-export-txt-hook))
  "Association list of export formats and their properties.
Takes the form:

    ((FORMAT KEYWORD PROPERTY)
      ...)")

(defmacro define-fountain-export-template-docstring (format)
  "Define docstring for export format FORMAT."
  (let ((tag (plist-get (cdr (assq format fountain-export-formats))
                        :tag)))
    (format
     "Association list of element templates for exporting to %s.
Takes the form:

    ((ELEMENT TEMPLATE)
      ...)

ELEMENT is the Fountain element, a symbol (see below). TEMPLATE
is the template with which to format the format string. If
TEMPLATE is nil, the format string is discarded.

Fountain ELEMENTs:

    document            wrapper template for all content, see
                        `fountain-export-make-standalone'
    section             string of section, including child elements
    section-heading     string of section heading, excluding syntax chars
    scene               string of scene, including child elements
    scene-heading       string of scene heading, excluing syntax chars
    dual-dialog         string of dual-dialogue block, including child dialog
                        block elements
    dialog              string of dialogue block, including child elements
    character           string of character name, excluding syntax chars
    paren               string of parenthetical
    lines               string of dialogue lines, up to end of dialogue block or
                        next parenthetical
    trans               string of transition, excluding syntax chars
    action              string of action block
    synopsis            string of synopsis, excluding syntax chars
    note                string of note, excluding syntax chars
    center              string of center text, excluding syntax chars

Each TEMPLATE should include the replacement key `{{content}}'.
Templates may use any metadata keys (e.g. `{{title}}', `{{author}}',
etc.) as well as keys defined in `fountain-export-formats'." tag)))

;; The %s template also uses the following keys:
;; %s" tag tag
;;     (mapconcat #'(lambda (var)
;;                    (concat "    {{" (symbol-name (car var)) "}}"))
;;                (plist-get (cdr (assq format fountain-export-formats))
;;                           :eval-replace)
;;                "\n"))))

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
                 (choice string (const nil)))))

(defun fountain-get-export-elements (&optional format)
  "Return list of elements exported in current script format FORMAT."
  (cdr (or (assoc-string
            (or format
                (plist-get (fountain-read-metadata) 'format)
                fountain-script-format)
            fountain-export-include-elements)
           (car fountain-export-include-elements))))

(defun fountain-export-get-filename (format &optional buffer)
  "Return appropriate filename for export.
If current buffer or BUFFER is visiting a file, concat file name
base and FORMAT. Otherwise return option `fountain-export-buffer'
formatted with export format FORMAT tag."
  (let* ((alist (cdr (assq format fountain-export-formats)))
         (tag (plist-get alist :tag))
         (ext (plist-get alist :ext)))
    (with-current-buffer (or buffer (current-buffer))
      (cond (fountain-export-use-title-as-filename
             (concat (plist-get (fountain-read-metadata) 'title) ext))
            (buffer-file-name
             (concat (file-name-base buffer-file-name) ext))
            (t
             (format fountain-export-tmp-buffer-name tag))))))

(defun fountain-slugify (string)
  "Convert STRING to one suitable for slugs.

STRING is downcased, non-alphanumeric characters are removed, and
whitespace is converted to dashes. e.g.

    Hello Wayne's World 2! -> hello-wanyes-world-2"
  (save-match-data
    (string-join
     (split-string
      (downcase
       (replace-regexp-in-string "[^\n\s\t[:alnum:]]" "" string))
      "[^[:alnum:]]+" t)
     "-")))

(defun fountain-export-fill-string (string element-type)
  "Fill STRING according to fill margins of ELEMENT-TYPE.
Return filled string."
  (with-temp-buffer
    (insert string)
    (let (adaptive-fill-mode
          (fill-margins (symbol-value
                 (plist-get (cdr (assq element-type fountain-elements))
                            :fill))))
      (setq left-margin (car fill-margins)
            fill-column (+ left-margin (cdr fill-margins)))
      ;; Replace emphasis syntax with face text propoerties (before
      ;; performing fill).
      (dolist (face '((fountain-italic-regexp . italic)
                      (fountain-bold-regexp . bold)
                      (fountain-underline-regexp . underline)))
        (goto-char (point-min))
        (while (re-search-forward (symbol-value (car face)) nil t)
          (put-text-property (match-beginning 3) (match-end 3) 'face (cdr face))
          (delete-region (match-beginning 4) (match-end 4))
          (delete-region (match-beginning 2) (match-end 2))))
      ;; Fill the buffer and return it as a string.
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun fountain-export-replace-in-string (string format)
  "Perform export replacements in STRING according to FORMAT."
  (let ((replace-alist
         (plist-get (cdr (assq format fountain-export-formats))
                    :string-replace)))
    (dolist (replacement replace-alist string)
      (setq string (replace-regexp-in-string
                    (car replacement) (cadr replacement) string t nil)))))

(defun fountain-export-get-cond-replacement (format element key value)
  "Return conditional replacements in export format FORMAT.
Get conditional replacement alist from `fountain-export-formats'
for FORMAT. Get alist associated with ELEMENT or t, then alist
associated with KEY, and value associated with VALUE."
  (let ((replace-alist
         (plist-get (cdr (assq format fountain-export-formats))
                    :cond-replace)))
    (car
     (cdr (assq value
                (cdr (assq key
                           (or (cdr (assq element replace-alist))
                               (cdr (assq t replace-alist))))))))))

(defun fountain-export-get-eval-replacement (key format)
  "Return evaluated replacements for KEY in export format FORMAT."
  (let ((replacement
         (car (cdr (assq key
                         (plist-get (cdr (assq format
                                               fountain-export-formats))
                                    :eval-replace)))))
        string)
    (unwind-protect
        (setq string (eval replacement t))
      (when (stringp string) string))))

;; FIXME: this function is too log and ugly
(defun fountain-export-element (element-list format)
  "Return a formatted string from ELEMENT-LIST according to FORMAT.

Break ELEMENT-LIST into ELEMENT, PLIST and CONTENT.

If PLIST property \"export\" is non-nil, proceed, otherwise
return an empty string.

If CONTENT is a string, format with `fountain-export-replace-in-string'
and if format it filled, fill with `fountain-export-fill-string'.

If CONTENT is a list, recursively call this function on each
element of the list.

Check if ELEMENT corresponds to a template in
`fountain-export-templates' and set ELEMENT-TEMPLATE. If so,
replace matches of `fountain-template-regexp' in the
following order:

1. {{content}} is replaced with CONTENT.

2. If {{KEY}} corresponds to a string property in PLIST, it is
   replaced with that string.

3. If {{KEY}} corresponds to the value of the key of ELEMENT of
   FORMAT in `fountain-export-conditional-replacements', it is
   replaced with that string.

4. If {{KEY}} corresponds with a cdr of FORMAT in
   `fountain-export-replacements', it is evaluated using `eval'
   and replaced with that string.

5. If none of the above, {{KEY}} is replaced with an empty
   string."
  ;; Break ELEMENT-LIST into ELEMENT, PLIST and CONTENT.
  (let ((element (car element-list))
        (plist (nth 1 element-list))
        (content (nth 2 element-list)))
    ;; First, element must be included for export. Check if export
    ;; property is non-nil.
    (if (plist-get plist 'export)
        ;; Set the ELEMENT-FORMAT-PLIST. STRING will return the final
        ;; exported string.
        (let ((export-format-plist (cdr (assq format fountain-export-formats)))
              format-template element-template string)
          (cond
           ;; If CONTENT is nil, set STRING as an empty string.
           ((not content)
            (setq string ""))
           ;; If CONTENT is a string, format CONTENT and set as STRING.
           ((stringp content)
            (setq string (fountain-export-replace-in-string content format))
            ;; If the format is filled, fill STRING in temporary buffer
            (when (plist-get export-format-plist :fill)
              (setq string (fountain-export-fill-string string element))))
           ;; If CONTENT is a list, work through the list setting each
           ;; element as CHILD-ELEMENT-LIST and recursively calling this
           ;; function.
           ((listp content)
            (dolist (child-element-list content)
              (setq string
                    (concat string
                            (fountain-export-element child-element-list format)))))
           ;; Otherwise, CONTENT is either not exported or malformed,
           ;; then set an empty string.
           (t
            (setq string "")))
          ;; Set the FORMAT-TEMPLATE, which is the big alist of template
          ;; strings for each element. From this, get the
          ;; ELEMENT-TEMPLATE.
          (setq format-template (symbol-value (plist-get export-format-plist :template))
                element-template (car (cdr (assq element format-template))))
          (cond
           ;; If there is a FORMAT-TEMPLATE and an ELEMENT-TEMPLATE,
           ;; replace template keys in that template.
           ((and format-template element-template)
            (while (string-match fountain-template-regexp element-template)
              (setq element-template
                    (replace-regexp-in-string
                     fountain-template-regexp
                     (lambda (match)
                       ;; Find KEY and corresponding VALUE in PLIST.
                       (let* ((key (match-string 1 match))
                              (value (plist-get plist (intern key))))
                         (cond
                          ;; If KEY is "content", replace with STRING.
                          ((string= key "content")
                           string)
                          ;; If KEY is "slugify", replace with slugified
                          ;; STRING.
                          ((string= key "slugify")
                           (fountain-slugify string))
                          ;; If KEY's VALUE is a string, format and
                          ;; replace with VALUE.
                          ((stringp value)
                           (fountain-export-replace-in-string value format))
                          ;; If KEY's VALUE is not a string but still
                          ;; non-nil attempt conditional replacement
                          ;; based on KEY's VALUE.
                          ;;
                          ;; FIXME: the following two functions are
                          ;; ugly/messy. Some work has already been done
                          ;; to combine these into just
                          ;; `fountain-export-get-eval-replacement' by
                          ;; using template {{KEYS}} in the export
                          ;; templates.
                          (value
                           (fountain-export-get-cond-replacement
                            format element (intern key) value))
                          ;; Otherwise, attempt expression
                          ;; replacements.
                          ((fountain-export-get-eval-replacement
                            (intern key) format))
                          (t ""))))
                     element-template t t)))
            (setq string element-template))
           ;; If there's no ELEMENT-TEMPLATE for element in
           ;; FORMAT-TEMPLATE, set an empty string
           (format-template
            (setq string "")))
          ;; Return the string.
          (or string ""))
      ;; Element is not exported, return an empty string.
      "")))

(defun fountain-export-region (start end format &optional snippet)
  "Return an export string of region between START and END in FORMAT.
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
    ;; Parse the region to TREE.
    (save-excursion
      (setq tree (fountain-prep-and-parse-region start end)))
    ;; If exporting a standalone document, list TREE inside a document
    ;; element.
    (unless (or snippet (not fountain-export-make-standalone))
      (setq tree
            (list (list 'document
                        (append
                         (list 'begin start
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
Pass argument SNIPPET to `fountain-export-region'.

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
  (unless buffer (setq buffer (current-buffer)))
  (let ((dest-buffer (get-buffer-create
                      (fountain-export-get-filename format buffer)))
        (hook (plist-get (cdr (assq format fountain-export-formats))
                         :hook))
        string complete)
    (unwind-protect
        (with-current-buffer buffer
          ;; If DEST-BUFFER is not empty, check if it is the current
          ;; buffer, or if not, if the user does not wish to overwrite.
          (when (< 0 (buffer-size dest-buffer))
            (when (or (eq (current-buffer) dest-buffer)
                      (not (y-or-n-p
                            (format "Buffer `%s' is not empty; overwrite? "
                                    dest-buffer))))
              ;; If so, generate a new buffer.
              (setq dest-buffer
                    (generate-new-buffer (buffer-name dest-buffer)))
              (message "Using new buffer `%s'" dest-buffer)))
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
  "Call function defined in `fountain-export-default-function'."
  (interactive)
  (funcall fountain-export-default-function))

(defun fountain-export-shell-command (&optional buffer)
  "Call shell command defined in variable `fountain-export-shell-command'.
Command acts on current buffer or BUFFER."
  ;; FIXME: better to call ‘start-process’ directly, since it offers
  ;; more control and does not impose the use of a shell (with its need
  ;; to quote arguments).
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (if file
        (async-shell-command
         (format fountain-export-shell-command (shell-quote-argument file))
         "*Fountain Export Process*")
      (user-error "Buffer `%s' is not visiting a file" buffer))))


;;; -> plaintext

(defgroup fountain-plaintext-export ()
  "Options for exporting Fountain files to plaintext."
  :prefix "fountain-export-txt-"
  :group 'fountain-export)

(defcustom fountain-export-txt-title-page-template
  "\
{{title}}\n
{{credit}}\n
{{author}}\n\n
{{contact}}
{{date}}\n\n"
  "Template for plaintext title page."
  :type 'string)

(defcustom fountain-export-txt-template
  '((document "{{title-page}}{{content}}")
    (section "{{content}}")
    (section-heading "{{content}}\n\n")
    (scene "{{content}}")
    (scene-heading "{{scene-heading-spacing}}{{content}}\n\n")
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
    (center "{{content}}\n\n"))
  (define-fountain-export-template-docstring 'txt)
  :type 'fountain-element-list-type)

(defcustom fountain-export-txt-hook
  nil
  "Hook run with export buffer on sucessful export to plaintext."
  :type 'hook)

(defun fountain-export-buffer-to-txt ()
  "Convenience function for exporting buffer to plaintext."
  (interactive)
  (fountain-export-buffer 'txt))


;;; -> PostScript

;; FIXME: Exporting to PostScript requires some further work before it
;; can be implemented. Exporting to PostScript is essentially the
;; existing export to plain text functions, with the following
;; additions:

;; When calling `fountain-prep-and-parse-region' for exporting to
;; PostScript, the preparation phase should add a while-loop that begins
;; at point-min and calls `fountain-forward-page' to move through the
;; temporary buffer, inserting manual page-breaks by calling
;; `fountain-insert-page-break' with each iteration. However the
;; function `fountain-goto-page-break-point' needs to be improved to
;; test for existing page-breaks immediately before or after point, to
;; prevent inserting multiple consecutive page-breaks creating blank
;; pages. This should be its own function, e.g.
;; `fountain-paginate-buffer'.

;; Once page-breaks have been inserted, `fountain-prep-and-parse-region'
;; will return a lisp data tree of the buffer with appropriate
;; page-breaks. Page-breaks need to then be inserted as linefeed (^L)
;; characters in the destination buffer, to signal a page-break to the
;; PostScript typesetter.

;; Additionally, a user option for headers and footer formatting is
;; required, which should include the page number at the right-hand-side
;; of the header. It is important that if the header or footer
;; encroaches into the space of the page, then `fountain-forward-page'
;; reacts accordingly. (However it would be more reasonable to limit the
;; header and footer each to a single line.)

;; Finally, the variables `ps-paper-type', `ps-left-margin',
;; `ps-top-margin', `ps-font-size', `ps-print-color-p' and
;; `ps-print-header' need to be set accordingly before calling
;; `ps-print-buffer'. (See below.)

;; This may seem like an odd way to calculate page length, but if
;; implemented well, should allow for a script to have "locked pages" by
;; calling the aforementioned `fountain-paginate-buffer'. For more
;; information on locking pages in a script, see:
;; https://en.wikipedia.org/wiki/Shooting_script#Preserving_scene_and_page_numbers

;; (defgroup fountain-postscript-export ()
;;   "Options for exporting Fountain files to PostScript."
;;   :prefix 'fountain-export-ps-
;;   :group 'fountain-export)

;; (defcustom fountain-export-ps-top-margin
;;   1.0
;;   "Float representing top page margin in inches.

;; There is no corresponding bottom margin option, as page length
;; is calculated using `fountain-pages-max-lines'."
;;   :type 'float
;;   :group 'fountain-export)

;; (defcustom fountain-export-ps-left-margin
;;   1.5
;;   "Float representing left page margin in inches.

;; There is no corresponding right margin option, as text width
;; is calculated using `fountain-fill'."
;;   :type 'float
;;   :group 'fountain-export)

;; (defcustom fountain-export-ps-hook
;;   nil
;;   "Hook run with export buffer on sucessful export to PostScript."
;;   :type 'hook
;;   :group 'fountain-export)

;; (defun fountain-export-buffer-to-ps ()
;;   "Convenience function for exporting buffer to PostScript."
;;   (interactive)
;;   (let ((ps-paper-type fountain-export-page-size)
;;         (ps-left-margin (* fountain-export-left-margin 72))
;;         (ps-top-margin (* fountain-export-top-margin 72))
;;         (ps-font-size 12)
;;         ps-print-color-p
;;         ps-print-header)
;;     (ps-print-buffer (fountain-export-get-filename 'ps))))
;; (fountain-export-buffer 'ps))


;;; -> HTML

(defgroup fountain-html-export ()
  "Options for export Fountain files to HTML."
  :prefix "foutnain-export-html-"
  :group 'fountain-export)

(defcustom fountain-export-html-title-page-template
  "<section class=\"title-page\">
<div class=\"title\">
<h1>{{title}}</h1>
<p>{{credit}}</p>
<p>{{author}}</p>
</div>
<p class=\"contact\">{{contact}}</p>
</section>"
  "Template for HTML title page."
  :type 'string)

(defcustom fountain-export-html-template
  '((document "\
<head>
<meta charset=\"utf-8\">
<meta name=\"author\" content=\"{{author}}\" />
<meta name=\"generator\" content=\"Emacs {{emacs-version}} running Fountain Mode {{fountain-version}}\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=no\">
<title>{{title}}</title>
<style type=\"text/css\">
{{stylesheet}}
</style>
</head>
<body>
<section class=\"screenplay\">
{{title-page}}
{{content}}\
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
    (center "<p class=\"center\">{{content}}</p>\n"))
  (define-fountain-export-template-docstring 'html)
  :type 'fountain-element-list-type)

(defcustom fountain-export-html-stylesheet
  "\
.screenplay {
  font-family: {{font}};
  font-size: 12pt;
  line-height: 1;
  max-width: 6in;
  margin: 1em auto;
  -webkit-text-size-adjust: none;
}
.screenplay h1, h2, h3, h4, h5, h6 {
  font-weight: inherit;
  font-size: inherit;
}
.screenplay a {
  color: inherit;
  text-decoration: none;
}
.screenplay .underline {
  text-decoration: underline;
}
.screenplay .title-page {
  margin: 0 auto 1em;
}
.screenplay .title-page .title {
  text-align: center;
}
.screenplay .title-page .title h1 {
  text-transform: uppercase;
  text-decoration: underline;
}
.screenplay .section-heading {
  text-align: center;
}
.screenplay .section-heading:hover {
  background-color: lightyellow;
}
.screenplay .scene {
  margin-top: {{scene-heading-spacing}};
}
.screenplay .scene-heading {
  margin-bottom: 0;
}
.screenplay .scene-heading:hover {
  background-color: lightyellow;
}
.screenplay .action {
  margin: 1em 0;
  white-space: pre-wrap;
}
.screenplay .dialog {
  width: 75%;
  max-width: 4in;
  margin-top: 1em;
  margin-bottom: 1em;
  margin-left: 17%;
}
.screenplay .dialog .character {
  margin-top: 0;
  margin-bottom: 0;
  margin-left: 25%;
}
.screenplay .dialog .lines {
  max-width: 3.5in;
  margin-top: 0;
  margin-bottom: 0;
  white-space: pre-wrap;
}
.screenplay .dialog .paren {
  max-width: 2in;
  margin-top: 0;
  margin-bottom: 0;
  margin-left: 15%;
  text-indent: -0.6em;
  page-break-inside: avoid;
  page-break-after: avoid;
}
.screenplay .dual-dialog {
  width: 100%;
  margin: 1em 0;
}
.screenplay .dual-dialog .dialog {
  max-width: 50%;
  margin-top: 0;
  margin-left: 0;
  margin-right: 0;
  float: left;
  clear: none;
}
.screenplay .dual-dialog .dialog .lines {
  width: 95%;
}
.screenplay .trans {
  max-width: 2in;
  margin-left: 63%;
  clear: both;
  page-break-before: avoid;
}
.screenplay .note {
  display: block;
  font-size: 11pt;
  font-family: \"Comic Sans MS\", \"Marker Felt\", \"sans-serif\";
  line-height: 1.5;
  background-color: lightgoldenrodyellow;
  padding: 1em;
}
.screenplay .synopsis {
  margin-top: 0;
  color: grey;
  font-style: italic;
}
.screenplay .center {
  text-align: center;
  white-space: pre-wrap;
}"
  "Stylesheet for HTML export.

Screenplay content is wrapped in class \"screenplay\", which
means all screenplay elements require the \".screenplay\" class
parent."
  :type 'string
  :link '(url-link "https://github.com/rnkn/mcqueen"))

(defcustom fountain-export-html-title-template
  "<div class=\"title\">{{title-template}}</div>
<h1>{{title}}</h1>
<p>{{credit}}</p>
<p>{{author}}</p>
<p class=\"contact\">{{contact-template}}</p>
"
  "HTML template for title page export."
  :type 'string)

(defcustom fountain-export-html-hook
  nil
  "Hook run with export buffer on sucessful export to HTML."
  :type 'hook)

(defun fountain-export-buffer-to-html ()
  "Convenience function for exporting buffer to HTML."
  (interactive)
  (fountain-export-buffer 'html))


;;; -> LaTeX

(defgroup fountain-latex-export ()
  "Options for exporting Fountain files to LaTeX."
  :prefix "fountain-export-tex-"
  :group 'fountain-export)

(defcustom fountain-export-tex-title-page-template
  "\
\\title{{{title}}}
\\author{{{author}}}
\\date{{{date}}}
\\newcommand{\\credit}{{{credit}}}
\\newcommand{\\contact}{{{contact}}}

\\thispagestyle{empty}
\\vspace*{3in}

\\begin{center}
  \\uline{\\begin{MakeUppercase}\\thetitle\\end{MakeUppercase}}\\par
  \\credit\\par
  \\theauthor\\par
\\end{center}

\\vspace{3in}
\\iftoggle{contactalignright}{%
  \\begin{flushright}
    \\contact
  \\end{flushright}
}{%
  \\contact
}
\\clearpage"
  "Template for LaTeX title page."
  :type 'string)

(defcustom fountain-export-tex-template
  '((document "\
\\documentclass[12pt,{{page-size}}]{article}

% Conditionals
\\usepackage{etoolbox}
\\newtoggle{contactalignright}
\\newtoggle{doublespacesceneheadings}
\\newtoggle{underlinesceneheadings}
\\newtoggle{boldsceneheadings}
\\newtoggle{includescenenumbers}
\\newtoggle{numberfirstpage}

\\settoggle{contactalignright}{{{title-contact-align}}}
\\settoggle{doublespacesceneheadings}{{{scene-heading-spacing}}}
\\settoggle{underlinesceneheadings}{{{scene-heading-underline}}}
\\settoggle{boldsceneheadings}{{{scene-heading-bold}}}
\\settoggle{includescenenumbers}{{{include-scene-numbers}}}
\\settoggle{numberfirstpage}{{{number-first-page}}}

% Page Layout Settings
\\usepackage[left=1.5in,right=1in,top=1in,bottom=0.75in]{geometry}

% Font Settings
\\usepackage{fontspec}
\\setmonofont{{{font}}}
\\renewcommand{\\familydefault}{\\ttdefault}

% Text Settings
\\setlength{\\baselineskip}{12pt plus 0pt minus 0pt}
\\setlength{\\parskip}{12pt plus 0pt minus 0pt}
\\setlength{\\topskip}{0pt plus 0pt minus 0pt}
\\setlength{\\headheight}{\\baselineskip}
\\setlength{\\headsep}{\\baselineskip}
\\linespread{0.85}
\\hyphenpenalty=10000
\\widowpenalty=10000
\\clubpenalty=10000
\\frenchspacing
\\raggedright

% Underlining
\\usepackage[normalem]{ulem}
\\renewcommand{\\ULthickness}{1pt}

% Header & Footer Settings
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\fancyhead[R]{\\thepage.}
\\renewcommand{\\headrulewidth}{0pt}

% Margin Settings
\\usepackage{marginnote}
\\renewcommand*{\\raggedleftmarginnote}{\\hspace{0.2in}}

% Section Headings
\\newcommand{\\sectionheading}[1]{%
  \\begin{center}
    \\uline{#1}
  \\end{center}
}

% Scene Headings
\\newcommand*{\\sceneheading}[2][]{%
  \\def\\thesceneheading{#2}
  \\iftoggle{doublespacesceneheadings}{%
    \\vspace{\\parskip}
  }{}
  \\iftoggle{boldsceneheadings}{%
    \\let\\BFtmp\\thesceneheading
    \\renewcommand{\\thesceneheading}{\\textbf{\\BFtmp}}
  }{}
  \\iftoggle{underlinesceneheadings}{%
    \\let\\ULtmp\\thesceneheading
    \\renewcommand{\\thesceneheading}{\\uline{\\ULtmp}}
  }{}
  \\thesceneheading\\nopagebreak[4]%
  \\iftoggle{includescenenumbers}{%
    \\normalmarginpar\\marginnote{#1}\\reversemarginpar\\marginnote{#1}%
  }{}
}

% Dialogue
\\usepackage{xstring}
\\newcommand{\\contd}{{{contd}}}
\\newcommand{\\more}{{{more}}}
\\newlength{\\characterindent}
\\newlength{\\characterwidth}
\\newlength{\\dialogindent}
\\newlength{\\dialogwidth}
\\setlength{\\characterindent}{1in}
\\setlength{\\characterwidth}{4in}
\\setlength{\\dialogindent}{1in}
\\setlength{\\dialogwidth}{3.5in}
\\newcommand*{\\character}[1]{%
  \\hspace*{\\characterindent}\\parbox[t]{\\characterwidth}{#1}%
}
\\newenvironment{dialog}[1]{%
  \\setlength{\\parskip}{0pt}
  \\begin{list}{}{%
      \\setlength{\\topsep}{0pt}
      \\setlength{\\partopsep}{0pt}
      \\setlength{\\parsep}{0pt}
      \\setlength{\\leftmargin}{\\dialogindent}
      \\setlength{\\rightmargin}{\\dimexpr\\linewidth-\\leftmargin-\\dialogwidth}
    }%
  \\item\\character{#1}\\mark{#1}\\nopagebreak[4]%
  }{%
    \\mark{\\empty}\\end{list}%
}
\\newcommand*{\\paren}[1]{%
  \\par%
  \\hspace*{0.5in}\\parbox[t]{2in}{%
    \\hangindent=0.1in\\hangafter=1#1}\\par\\nopagebreak[4]
  \\vspace{2pt}%
}

% Transitions
\\newlength{\\transindent}
\\newlength{\\transwidth}
\\setlength{\\transindent}{4in}
\\setlength{\\transwidth}{2in}
\\newcommand*{\\trans}[1]{%
  \\nopagebreak[4]\\hspace*{\\transindent}\\parbox[t]{\\transwidth}{#1}
}

% Center Text
\\newcommand{\\centertext}[1]{%
  \\setlength{\\topsep}{0pt}
  \\begin{center}#1\\end{center}
}

% Page Breaking Settings
\\usepackage{atbegshi}
\\AtBeginShipout{%
  \\if\\botmark\\empty
  \\else
  \\hspace*{\\dialogindent}\\character{\\StrDel[1]{\\botmark}{\\contd}\\space\\contd}%
  \\fi%
}

% Title Page
\\usepackage{titling}

% Document
\\begin{document}

{{title-page}}

\\setcounter{page}{1}
\\iftoggle{numberfirstpage}{}{\\thispagestyle{empty}}
{{content}}\
\\end{document}

% Local Variables:
% tex-command: \"xelatex\"
% TeX-engine: xetex
% End:")
    (section "{{content}}")
    (section-heading "\\sectionheading{{{content}}}\n\n")
    (scene "{{content}}")
    (scene-heading "\\sceneheading{{{content}}}\n\n")
    (dual-dialog "{{content}}")
    (dialog "\\begin{dialog}{{content}}\\end{dialog}\n\n")
    (character "{{{content}}}\n")
    (paren "\\paren{{{content}}}\n")
    (lines "{{content}}\n")
    (trans "\\trans{{{content}}}\n\n")
    (action "{{content}}\n\n")
    (page-break "\\clearpage\n\n")
    (synopsis nil)
    (note nil)
    (center "\\centertext{{{content}}}\n\n"))
  (define-fountain-export-template-docstring 'tex)
  :type 'fountain-element-list-type)

(defcustom fountain-export-tex-hook
  nil
  "Hook run with export buffer on sucessful export to LaTeX."
  :type 'hook)

(defun fountain-export-buffer-to-latex ()
  "Convenience function for exporting buffer to LaTeX."
  (interactive)
  (fountain-export-buffer 'tex))


;;; -> FDX

(defgroup fountain-final-draft-export ()
  "Options for exporting Fountain files to Final Draft."
  :prefix "fountain-export-fdx-"
  :group 'fountain-export)

(defcustom fountain-export-fdx-title-page-template
  "\
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
<Text>{{contact}}</Text>
</Paragraph>
</Content>
</TitlePage>"
  "Template for Final Draft title page."
  :type 'string)

(defcustom fountain-export-fdx-template
  '((document "\
<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>
<FinalDraft DocumentType=\"Script\" Template=\"No\" Version=\"1\">
<Content>
{{content}}\
</Content>
{{title-page}}
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
    (center "<Paragraph Alignment=\"Center\" Type=\"Action\" StartsNewPage=\"{{starts-new-page}}\">\n<Text>{{content}}</Text>\n</Paragraph>\n"))
  (define-fountain-export-template-docstring 'fdx)
  :type 'fountain-element-list-type)

(defcustom fountain-export-fdx-hook
  nil
  "Hook run with export buffer on sucessful export to Final Draft."
  :type 'hook)

(defun fountain-export-buffer-to-fdx ()
  "Convenience function for exporting buffer to Final Draft."
  (interactive)
  (fountain-export-buffer 'fdx))


;;; -> Fountain

(defgroup fountain-fountain-export ()
  "Options for exporting Fountain files to Fountain."
  :prefix "fountain-export-fountain-"
  :group 'fountain-export)

(defcustom fountain-export-fountain-title-page-template
  "\
title: {{title}}
credit: {{credit}}
author: {{author}}
date: {{date}}
contact: {{contact}}"
  "Template for Fountain title page.
This just adds the current metadata to the exported file."
  :type 'string)

(defcustom fountain-export-fountain-template
  '((document "\
{{title-page}}

{{content}}")
    (section "{{content}}")
    (section-heading "{{level}} {{content}}\n\n")
    (scene "{{content}}")
    (scene-heading "{{scene-heading-spacing}}{{forced}}{{content}}\n\n")
    (dual-dialog "{{content}}\n")
    (dialog "{{content}}\n")
    (character "{{forced}}{{content}}{{dual-dialog}}\n")
    (paren "{{content}}\n")
    (lines "{{content}}\n")
    (trans "{{forced}}{{content}}\n\n")
    (action "{{forced}}{{content}}\n\n")
    (page-break "=== {{content}} ===\n\n")
    (synopsis "= {{content}}\n\n")
    (note "[[ {{content}} ]]\n\n")
    (center "> {{content}} <"))
  (define-fountain-export-template-docstring 'fountain)
  :type 'fountain-element-list-type)

(defcustom fountain-export-fountain-hook
  nil
  "Hook run with export buffer on sucessful export to Fountain."
  :type 'hook)

(defun fountain-export-buffer-to-fountain ()
  "Convenience function for exporting buffer to Fountain."
  (interactive)
  (fountain-export-buffer 'fountain))


;;; Outlining

(require 'outline)

(defvar-local fountain--outline-cycle
  0
  "Internal local integer representing global outline cycling status.

    0: Show all
    1: Show level 1 section headings
    2: Show level 2 section headings
    3: Show level 3 section headings
    4: Show level 4 section headings
    5: Show level 5 section headings
    6: Show scene headings

Used by `fountain-outline-cycle'.")

(defvar-local fountain--outline-cycle-subtree
  0
  "Internal local integer representing subtree outline cycling status.

Used by `fountain-outline-cycle'.")

(defcustom fountain-outline-custom-level
  nil
  "Additional section headings to include in outline cycling."
  :type '(choice (const :tag "Only top-level" nil)
                 (const :tag "Level 2" 2)
                 (const :tag "Level 3" 3)
                 (const :tag "Level 4" 4)
                 (const :tag "Level 5" 5))
  :group 'fountain)

(defcustom fountain-shift-all-elements
  t
  "\\<fountain-mode-map>Non-nil if \\[fountain-shift-up] and \\[fountain-shift-down] should operate on all elements.
Otherwise, only operate on section and scene headings."
  :type 'boolean
  :safe 'boolean
  :group 'fountain)

(defalias 'fountain-outline-next 'outline-next-visible-heading)
(defalias 'fountain-outline-previous 'outline-previous-visible-heading)
(defalias 'fountain-outline-forward 'outline-forward-same-level)
(defalias 'fountain-outline-backward 'outline-backward-same-level)
(defalias 'fountain-outline-up 'outline-up-heading)
(defalias 'fountain-outline-mark 'outline-mark-subtree)
(defalias 'fountain-outline-show-all 'outline-show-all)

(when (< emacs-major-version 25)
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

(defun fountain-get-block-bounds ()
  "Return the beginning and end bounds of current element block."
  (let ((element (fountain-get-element))
        begin end)
    (when element
      (save-excursion
        (save-restriction
          (widen)
          (cond ((memq element '(section-heading scene-heading))
                 (setq begin (match-beginning 0))
                 (outline-end-of-subtree)
                 (skip-chars-forward "\n\s\t")
                 (setq end (point)))
                ((memq element '(character paren lines))
                 (fountain-forward-character 0)
                 (setq begin (line-beginning-position))
                 (while (not (or (eobp)
                                 (and (bolp) (eolp))
                                 (fountain-match-note)))
                   (forward-line))
                 (skip-chars-forward "\n\s\t")
                 (setq end (point)))
                ((memq element '(trans center synopsis note page-break))
                 (setq begin (match-beginning 0))
                 (goto-char (match-end 0))
                 (skip-chars-forward "\n\s\t")
                 (setq end (point)))
                ((eq element 'action)
                 (save-excursion
                   (if (fountain-blank-before-p)
                       (setq begin (line-beginning-position))
                     (backward-char)
                     (while (and (eq (fountain-get-element) 'action)
                                 (not (bobp)))
                       (forward-line -1))
                     (skip-chars-forward "\n\s\t")
                     (beginning-of-line)
                     (setq begin (point))))
                 (forward-line)
                 (unless (eobp)
                   (while (and (eq (fountain-get-element) 'action)
                               (not (eobp)))
                     (forward-line))
                   (skip-chars-forward "\n\s\t")
                   (beginning-of-line))
                 (setq end (point))))))
      (cons begin end))))

(defun fountain-insert-hanging-line-maybe ()
  "Insert a empty newline if needed.
Return non-nil if empty newline was inserted."
  (let (hanging-line)
    (when (and (eobp) (/= (char-before) ?\n))
      (insert "\n"))
    (when (and (eobp) (not (fountain-blank-before-p)))
      (insert "\n")
      (setq hanging-line t))
    (unless (eobp)
      (forward-char 1))
    hanging-line))

(defun fountain-shift-down (&optional n)
  "Move the current element down past N elements of the same level."
  (interactive "p")
  (unless n (setq n 1))
  (if (outline-on-heading-p)
      (fountain-outline-shift-down n)
    (when fountain-shift-all-elements
      (let ((forward (< 0 n))
            hanging-line)
        (when (and (bolp) (eolp))
          (funcall (if forward #'skip-chars-forward #'skip-chars-backward)
                   "\n\s\t"))
        (save-excursion
          (save-restriction
            (widen)
            (let ((block-bounds (fountain-get-block-bounds))
                  outline-begin outline-end next-block-bounds)
              (unless (and (car block-bounds)
                           (cdr block-bounds))
                (user-error "Not at a moveable element"))
              (save-excursion
                (when (not forward)
                  (goto-char (cdr block-bounds))
                  (when (setq hanging-line (fountain-insert-hanging-line-maybe))
                    (setcdr block-bounds (point)))
                  (goto-char (car block-bounds)))
                (outline-previous-heading)
                (setq outline-begin (point))
                (outline-next-heading)
                (setq outline-end (point)))
              (if forward
                  (goto-char (cdr block-bounds))
                (goto-char (car block-bounds))
                (backward-char)
                (skip-chars-backward "\n\s\t"))
              (setq next-block-bounds (fountain-get-block-bounds))
              (unless (and (car next-block-bounds)
                           (cdr next-block-bounds))
                (user-error "Cannot shift element any further"))
              (when forward
                (goto-char (cdr next-block-bounds))
                (when (setq hanging-line (fountain-insert-hanging-line-maybe))
                  (setcdr next-block-bounds (point))))
              (unless (< outline-begin (car next-block-bounds) outline-end)
                (user-error "Cannot shift past higher level"))
              (goto-char (if forward (car block-bounds) (cdr block-bounds)))
              (insert-before-markers
               (delete-and-extract-region (car next-block-bounds)
                                          (cdr next-block-bounds))))
            (when hanging-line
              (goto-char (point-max))
              (delete-char -1))))))))

(defun fountain-shift-up (&optional n)
  "Move the current element up past N elements of the same level."
  (interactive "p")
  (unless n (setq n 1))
  (fountain-shift-down (- n)))

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
            (setq hanging-line (fountain-insert-hanging-line-maybe))
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
    (when (< 0 n) (funcall end-point-fun))
    (set-marker insert-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char insert-point)
    (when folded (outline-hide-subtree))
    (when hanging-line
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
  (setq fountain--outline-cycle n))

(defun fountain-outline-hide-custom-level ()
  "Set the outline visibilty to `fountain-outline-custom-level'."
  (when fountain-outline-custom-level
    (fountain-outline-hide-level fountain-outline-custom-level t)))

;; FIXME: document
(defun fountain-outline-cycle (&optional arg)
  "\\<fountain-mode-map>Cycle outline visibility depending on ARG.

    1. If ARG is nil, cycle outline visibility of current subtree and
       its children (\\[fountain-outline-cycle]).
    2. If ARG is 4, cycle outline visibility of buffer (\\[universal-argument] \\[fountain-outline-cycle],
       same as \\[fountain-outline-cycle-global]).
    3. If ARG is 16, show all (\\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]).
    4. If ARG is 64, show outline visibility set in
       `fountain-outline-custom-level' (\\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle])."
  (interactive "p")
  (let ((custom-level
         (when fountain-outline-custom-level
           (save-excursion
             (goto-char (point-min))
             (let (found)
               (while (and (not found)
                           (outline-next-heading))
                 (when (= (funcall outline-level) fountain-outline-custom-level)
                   (setq found t)))
               (when found fountain-outline-custom-level)))))
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
            ((and (= fountain--outline-cycle 1) custom-level)
             (fountain-outline-hide-level custom-level))
            ((< 0 fountain--outline-cycle 6)
             (fountain-outline-hide-level 6))
            ((= fountain--outline-cycle 6)
             (fountain-outline-hide-level 0))
            ((= highest-level 6)
             (fountain-outline-hide-level 6))
            (t
             (fountain-outline-hide-level highest-level))))
          ((eq arg 16)
           (outline-show-all)
           (message "Showing all")
           (setq fountain--outline-cycle 0))
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
                      (forward-line)
                      (while (and (not (eobp))
                                  (get-char-property (1- (point)) 'invisible))
                        (forward-line))
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
                 (setq fountain--outline-cycle-subtree 0))
                ((and (<= eos eol)
                      children)
                 (outline-show-entry)
                 (outline-show-children)
                 (message "Showing headings")
                 (setq fountain--outline-cycle-subtree 2))
                ((or (<= eos eol)
                     (= fountain--outline-cycle-subtree 2))
                 (outline-show-subtree)
                 (message "Showing contents")
                 (setq fountain--outline-cycle-subtree 3))
                (t
                 (outline-hide-subtree)
                 (message "Hiding contents")
                 (setq fountain--outline-cycle-subtree 1)))))))))

(defun fountain-outline-cycle-global ()
  "Globally cycle outline visibility.

Calls `fountain-outline-cycle' with argument 4 to cycle buffer
outline visibility through the following states:

    1. Top-level section headings
    2. Value of `fountain-outline-custom-level'
    3. All section headings and scene headings
    4. Everything"
  (interactive)
  (fountain-outline-cycle 4))

(defun fountain-outline-level ()
  "Return the heading's nesting level in the outline.
Assumes that point is at the beginning of a heading and match
data reflects `outline-regexp'."
  (if (string-prefix-p "#" (match-string 0))
      (string-width (match-string 1))
    6))

(defcustom fountain-pop-up-indirect-windows
  nil
  "Non-nil if opening indirect buffers should make a new window."
  :type 'boolean
  :group 'fountain)

(defun fountain-outline-to-indirect-buffer ()
  "Clone section/scene at point to indirect buffer.

Set `fountain-pop-up-indirect-windows' to control how indirect
buffer windows are opened."
  (interactive)
  (let ((pop-up-windows fountain-pop-up-indirect-windows)
        (base-buffer (buffer-name (buffer-base-buffer)))
        beg end heading-name target-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (outline-back-to-heading t)
        (setq beg (point))
        (when (or (fountain-match-section-heading)
                  (fountain-match-scene-heading))
          (setq heading-name (match-string-no-properties 2)
                target-buffer (concat base-buffer "-" heading-name))
          (outline-end-of-subtree)
          (setq end (point)))))
    (if (and (get-buffer target-buffer)
             (with-current-buffer target-buffer
               (goto-char beg)
               (and (or (fountain-match-section-heading)
                        (fountain-match-scene-heading))
                    (string= heading-name (match-string-no-properties 2)))))
        (pop-to-buffer target-buffer)
      (clone-indirect-buffer target-buffer t)
      (outline-show-all))
    (narrow-to-region beg end)))


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
          (when (fountain-match-scene-heading) (forward-line p))
          (funcall move-fun)
          (setq n (- n p)))
      (beginning-of-line)
      (funcall move-fun))))

(defun fountain-backward-scene (&optional n)
  "Move backward N scene headings (foward if N is negative)."
  (interactive "^p")
  (unless n (setq n 1))
  (fountain-forward-scene (- n)))

;; FIXME: needed?
(defun fountain-beginning-of-scene ()
  "Move point to beginning of current scene."
  (interactive "^")
  (fountain-forward-scene 0))

;; FIXME: needed?
(defun fountain-end-of-scene ()
  "Move point to end of current scene."
  (interactive "^")
  (fountain-forward-scene 1)
  (unless (eobp)
    (backward-char)))

;; FIXME: extending region
(defun fountain-mark-scene ()
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
  (interactive "NGo to scene: ")
  (push-mark)
  (goto-char (point-min))
  (let ((scene (if (fountain-match-scene-heading)
                   (car (fountain-scene-number-to-list (match-string 8)))
                 0)))
    (while (and (< scene n)
                (< (point) (point-max)))
      (fountain-forward-scene 1)
      (when (fountain-match-scene-heading)
        (setq scene (or (car (fountain-scene-number-to-list (match-string 8)))
                        (1+ scene)))))))

(defun fountain-goto-page (n)
  "Move point to Nth appropropriate page in current buffer."
  (interactive "NGo to page: ")
  (push-mark)
  (goto-char (point-min))
  (fountain-forward-page n (fountain-get-export-elements)))

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
                               (or (and (bolp) (eolp))
                                   (forward-comment p)
                                   (fountain-match-dialog)
                                   (fountain-match-paren))))
                         ((eq limit 'scene)
                          (not (or (= (point) (buffer-end p))
                                   (fountain-match-character)
                                   (fountain-match-scene-heading))))
                         ((not (or (= (point) (buffer-end p))
                                   (fountain-match-character)))))
              (forward-line p)))))
    (if (/= n 0)
        (while (/= n 0)
          (when (fountain-match-character) (forward-line p))
          (funcall move-fun)
          (setq n (- n p)))
      (beginning-of-line)
      (funcall move-fun))))

(defun fountain-backward-character (&optional n)
  "Move backward N character (foward if N is negative)."
  (interactive "^p")
  (unless n (setq n 1))
  (fountain-forward-character (- n)))


;;; Editing

(defcustom fountain-auto-upcase-scene-headings
  t
  "If non-nil, automatically upcase lines matching `fountain-scene-heading-regexp'."
  :type 'boolean
  :group 'fountain)

(defun fountain-auto-upcase ()
  "Upcase all or part of the current line contextually.

If `fountain-auto-upcase-scene-headings' is non-nil and point is
at a scene heading, activate auto upcasing for beginning of line
to scene number or point."
  (when (and fountain-auto-upcase-scene-headings
             (fountain-match-scene-heading))
    (upcase-region (line-beginning-position) (or (match-end 2) (point)))))

(defun fountain-dwim (&optional arg)
  "Call a command based on context (Do What I Mean).

1. If prefixed with ARG, call `fountain-outline-cycle' and pass
   ARG.

2. If point is at the beginning of a scene heading or section
   heading call `fountain-outline-cycle'.

3. Otherwise, call `complation-at-point'."
  (interactive "p")
  (cond ((and arg (< 1 arg))
         (fountain-outline-cycle arg))
        ((eolp)
         (completion-at-point))
        ((or (fountain-match-section-heading)
             (fountain-match-scene-heading)
             (eq (get-char-property (point) 'invisible) 'outline))
         (fountain-outline-cycle))))

(defun fountain-upcase-line (&optional arg)
  "Upcase the line.
If prefixed with ARG, insert `.' at beginning of line to force
a scene heading."
  (interactive "P")
  (when arg (save-excursion (beginning-of-line) (insert ".")))
  (upcase-region (line-beginning-position) (line-end-position)))

(defun fountain-upcase-line-and-newline (&optional arg)
  "Upcase the line and insert a newline.
If prefixed with ARG, insert `.' at beginning of line to force
a scene heading."
  (interactive "P")
  (when (and arg (not (fountain-match-scene-heading)))
    (save-excursion
      (beginning-of-line)
      (insert ".")))
  (upcase-region (line-beginning-position) (point))
  (insert "\n"))

(defun fountain-delete-comments-in-region (start end)
  "Delete comments in region between START and END."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (let ((x (point)))
        (if (forward-comment 1)
            (delete-region x (point))
          (unless (eobp) (forward-char 1)))))))

(defun fountain-insert-synopsis ()
  "Insert synopsis below scene heading of current scene."
  (interactive)
  (widen)
  (when (outline-back-to-heading)
    (forward-line)
    (or (bolp) (newline))
    (unless (and (bolp) (eolp)
                 (fountain-blank-after-p))
      (save-excursion
        (newline)))
    (insert "= ")
    (when (outline-invisible-p) (fountain-outline-cycle))))

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
      (unless (and (bolp) (eolp))
        (re-search-forward "^[\s\t]*$" nil 'move))
      (unless (fountain-blank-after-p)
        (save-excursion
          (newline)))
      (comment-indent)
      (insert
       (replace-regexp-in-string
        fountain-template-regexp
        (lambda (match)
          (let ((key (match-string 1 match)))
            (cdr
             ;; FIXME: rather than hard-code limited options, these
             ;; could work better if reusing the key-value replacement
             ;; code from `fountain-export-element'.
             (assoc-string key (list (cons 'title (file-name-base (buffer-name)))
                                     (cons 'time (format-time-string fountain-time-format))
                                     (cons 'fullname user-full-name)
                                     (cons 'nick (capitalize user-login-name))
                                     (cons 'email user-mail-address))))))
        fountain-note-template)))))

(defun fountain-continued-dialog-refresh ()
  "Add or remove continued dialog in buffer.

If `fountain-add-continued-dialog' is non-nil, add
`fountain-continued-dialog-string' on characters speaking in
succession, otherwise remove all occurences.

If `fountain-continued-dialog-string' has changed, also attempt
to remove previous string first."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((job (make-progress-reporter "Refreshing continued dialog..."))
            (backup (car (get 'fountain-continued-dialog-string
                              'backup-value)))
            (replace-fun
             (lambda (string job)
               (goto-char (point-min))
               (while (re-search-forward
                       (concat "\s*" string) nil t)
                 (let ((inhibit-changing-match-data t))
                   (when (fountain-match-character)
                     (delete-region (match-beginning 0) (match-end 0))))
                 (progress-reporter-update job))))
            case-fold-search)
        (when (string= fountain-continued-dialog-string backup)
          (setq backup (eval (car (get 'fountain-continued-dialog-string
                                       'standard-value))
                             t)))
        ;; Delete all matches of backup string.
        (when (stringp backup) (funcall replace-fun backup job))
        ;; Delete all matches of current string.
        (funcall replace-fun fountain-continued-dialog-string job)
        ;; When `fountain-add-continued-dialog', add string where
        ;; appropriate.
        (when fountain-add-continued-dialog
          (goto-char (point-min))
          (while (< (point) (point-max))
            (when (and (not (looking-at-p
                             (concat ".*" fountain-continued-dialog-string "$")))
                       (fountain-match-character)
                       (string= (fountain-get-character 0)
                                (fountain-get-character -1 'scene)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" fountain-continued-dialog-string)))
            (forward-line)
            (progress-reporter-update job)))
        (progress-reporter-done job)))))


;;; Scene Numbers

(defcustom fountain-prefix-revised-scene-numbers
  nil
  "If non-nil, new scene numbers get prefixed revision characters.

If nil, when inserting new scene headings after numbering
existing scene headings, revised scene number format works as
follows:

    10
    10A <- new scene
    11

If non-nil, revised scene number format works as follows:

    10
    A11 <- new scene
    11

WARNING: Using conflicting revised scene number format in the
same script may result in errors in output."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-scene-numbers)

(defcustom fountain-scene-number-first-revision
  ?A
  "Character to start revised scene numbers."
  :type 'character
  :safe 'characterp
  :group 'fountain-scene-numbers)

(defcustom fountain-scene-number-separator
  nil
  "Character to separate scene numbers."
  :type '(choice (const nil)
                 (character ?-))
  :safe '(lambda (value)
           (or (null value)
               (characterp value)))
  :group 'fountain-scene-numbers)

(defun fountain-scene-number-to-list (string)
  "Read scene number STRING and return a list.

If `fountain-prefix-revised-scene-numbers' is non-nil:

    \"10\" -> (10)
    \"AA10\" -> (9 1 1)

Or if nil:

    \"10\" -> (10)
    \"10AA\" -> (10 1 1)"
  ;; FIXME: does not account for user option
  ;; `fountain-scene-number-separator' or
  ;; `fountain-scene-number-first-revision'.
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

(defun fountain-scene-number-to-string (scene-num-list)
  "Read scene number SCENE-NUM-LIST and return a string.

If `fountain-prefix-revised-scene-numbers' is non-nil:

    (10) -> \"10\"
    (9 1 2) -> \"AB10\"

Or, if nil:

    (10) -> \"10\"
    (9 1 2) -> \"9AB\""
  (let ((number (car scene-num-list))
        separator revision)
    (when (< 1 (length scene-num-list))
      (setq separator
            (if fountain-scene-number-separator
                (char-to-string fountain-scene-number-separator)
              "")
            revision
            (mapconcat #'(lambda (char)
                           (char-to-string
                            (+ (1- char) fountain-scene-number-first-revision)))
                       (cdr scene-num-list) separator)))
    (if fountain-prefix-revised-scene-numbers
        (progn
          (unless (string-empty-p revision) (setq number (1+ number)))
          (concat revision separator (number-to-string number)))
      (concat (number-to-string number) separator revision))))

(defun fountain-get-scene-number (&optional n)
  "Return the scene number of the Nth next scene as a list.
Return Nth previous if N is negative.

Scene numbers will not be accurate if buffer contains directives
to include external files."
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
        ;; First, check if there are any scene numbers already. If not
        ;; we can save a lot of work.
        ;; FIXME: this is just extra work since we're doing for each
        ;; scene heading
        (save-match-data
          (goto-char (point-min))
          (while (not (or found (eobp)))
            (when (and (re-search-forward fountain-scene-heading-regexp nil 'move)
                       (match-string 8))
              (setq found t))))
        (if found
            ;; There are scene numbers, so this scene number needs to be
            ;; calculated relative to those.
            (let ((current-scene (fountain-scene-number-to-list (match-string 8)))
                  last-scene next-scene)
              ;; Check if scene heading is already numbered and if there
              ;; is a NEXT-SCENE. No previousscene number can be greater
              ;; or equal to this.
              (goto-char x)
              (while (not (or next-scene (eobp)))
                (fountain-forward-scene 1)
                (when (fountain-match-scene-heading)
                  (setq next-scene (fountain-scene-number-to-list (match-string 8)))))
              (cond
               ;; If there's both a NEXT-SCENE and CURRENT-SCENE, but
               ;; NEXT-SCENE is less or equal to CURRENT-SCENE, scene
               ;; numbers are out of order.
               ((and current-scene next-scene
                     (version-list-<= next-scene current-scene))
                (user-error err-order (fountain-scene-number-to-string current-scene)))
               ;; Otherwise, if there is a CURRENT-SCENE and either no
               ;; NEXT-SCENE or there is and it's greater then
               ;; CURRENT-SCENE, just return CURRENT-SCENE.
               (current-scene)
               (t
                ;; There is no CURRENT-SCENE yet, so go to the first
                ;; scene heading and if it's already numberd set it to
                ;; that, or just (list 1).
                (goto-char (point-min))
                (unless (fountain-match-scene-heading)
                  (fountain-forward-scene 1))
                (when (<= (point) x)
                  (setq current-scene
                        (or (fountain-scene-number-to-list (match-string 8))
                            (list 1))))
                ;; While before point X, go forward through each scene
                ;; heading, setting LAST-SCENE to CURRENT-SCENE and
                ;; CURRENT-SCENE to an incement of (car LAST-SCENE).
                (while (< (point) x (point-max))
                  (fountain-forward-scene 1)
                  (when (fountain-match-scene-heading)
                    (setq last-scene current-scene
                          current-scene (or (fountain-scene-number-to-list (match-string 8))
                                            (list (1+ (car last-scene)))))
                    ;; However, this might make CURRENT-SCENE greater or
                    ;; equal to NEXT-SCENE (a problem), so if there is a
                    ;; NEXT-SCENE, and NEXT-SCENE is less or equal to
                    ;; CURRENT-SCENE:
                    ;;
                    ;; 1. pop (car LAST-SCENE), which should always be
                    ;;    less than NEXT-SCENE as N
                    ;; 2. set CURRENT-SCENE to (list TMP-SCENE (1+ N))
                    ;; 3. set TMP-SCENE to (list TMP-SCENE n)
                    ;;
                    ;; Loop through this so that the last (or only)
                    ;; element of CURRENT-SCENE is incremented by 1, and
                    ;; TMP-SCENE is appended with N or 1. e.g.
                    ;;
                    ;; CURRENT-SCENE (4 2) -> (4 3)
                    ;; TMP-SCENE (4 2) -> (4 2 1)
                    ;;
                    ;; Return CURRENT-SCENE.
                    (let (n tmp-scene)
                      (while (and next-scene (version-list-<= next-scene current-scene))
                        (setq n (pop last-scene)
                              current-scene (append tmp-scene (list (1+ (or n 0))))
                              tmp-scene (append tmp-scene (list (or n 1))))
                        (when (version-list-<= next-scene tmp-scene)
                          (user-error err-order
                                      (fountain-scene-number-to-string current-scene)))))))
                current-scene)))
          ;; Otherwise there were no scene numbers, so we can just count
          ;; the scenes.
          (goto-char (point-min))
          (unless (fountain-match-scene-heading)
            (fountain-forward-scene 1))
          (let ((current-scene 1))
            (while (< (point) x)
              (fountain-forward-scene 1)
              (when (fountain-match-scene-heading)
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
          (when (match-string 8)
            (delete-region (match-beginning 6) (match-end 9)))
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
          (unless (match-string 8)
            (end-of-line)
            (delete-horizontal-space t)
            (insert "\s#" (fountain-scene-number-to-string (fountain-get-scene-number)) "#"))
          (fountain-forward-scene 1)
          (progress-reporter-update job))
        (progress-reporter-done job)))))


;;; Font Lock

(defvar fountain-font-lock-keywords-plist
  `(;; Action
    ((lambda (limit)
       (fountain-match-element #'fountain-match-action limit))
     ((:level 1 :subexp 0 :face fountain-action)
      (:level 2 :subexp 1 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t))
     fountain-align-action)
    ;; Section Headings
    (,fountain-section-heading-regexp
     ((:level 2 :subexp 0 :face fountain-section-heading)
      (:level 2 :subexp 1 :face fountain-non-printing
              :override t))
     fountain-align-scene-heading)
    ;; Scene Headings
    ((lambda (limit)
       (fountain-match-element #'fountain-match-scene-heading limit))
     ((:level 2 :subexp 0 :face fountain-scene-heading)
      (:level 2 :subexp 1 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override prepend
              :laxmatch t)
      (:level 2 :subexp 7
              :laxmatch t)
      (:level 2 :subexp 8 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override prepend
              :laxmatch t)
      (:level 2 :subexp 9
              :override prepend
              :laxmatch t)
      (:level 2 :subexp 10 :face fountain-non-printing
              :invisible fountain-syntax-chars
              :override prepend
              :laxmatch t))
     fountain-align-scene-heading)
    ;; Character
    ((lambda (limit)
       (fountain-match-element #'fountain-match-character limit))
     ((:level 3 :subexp 0 :face fountain-character)
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
       (fountain-match-element #'fountain-match-paren limit))
     ((:level 3 :subexp 0 :face fountain-paren))
     fountain-align-paren)
    ;; Dialog
    ((lambda (limit)
       (fountain-match-element #'fountain-match-dialog limit))
     ((:level 3 :subexp 0 :face fountain-dialog))
     fountain-align-dialog)
    ;; Transition
    ((lambda (limit)
       (fountain-match-element #'fountain-match-trans limit))
     ((:level 3 :subexp 0 :face fountain-trans)
      (:level 2 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t))
     fountain-align-trans)
    ;; Center text
    (,fountain-center-regexp
     ((:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)
      (:level 3 :subexp 3)
      (:level 2 :subexp 4 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t))
     fountain-align-center)
    ;; Page-break
    (,fountain-page-break-regexp
     ((:level 2 :subexp 0 :face fountain-page-break)
      (:level 2 :subexp 2 :face fountain-page-number
              :override t
              :laxmatch t)))
    ;; Synopses
    (,fountain-synopsis-regexp
     ((:level 2 :subexp 0 :face fountain-synopsis)
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t))
     fountain-align-synopsis)
    ;; Notes
    (,fountain-note-regexp
     ((:level 2 :subexp 0 :face fountain-note)))
    ;; Templates
    (,fountain-template-regexp
     ((:level 2 :subexp 0 :face fountain-template)))
    ;; Metedata
    ((lambda (limit)
       (fountain-match-element #'fountain-match-metadata limit))
     ((:level 2 :subexp 0 :face fountain-metadata-key
              :laxmatch t)
      (:level 2 :subexp 3 :face fountain-metadata-value
              :override t
              :laxmatch t)))
    ;; Underline text
    (,fountain-underline-regexp
     ((:level 2 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face underline
              :override append)
      (:level 2 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Italic text
    (,fountain-italic-regexp
     ((:level 2 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face italic
              :override append)
      (:level 2 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Bold text
    (,fountain-bold-regexp
     ((:level 2 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face bold
              :override append)
      (:level 2 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Bold-Italic text
    (,fountain-bold-italic-regexp
     ((:level 2 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face bold-italic
              :override append)
      (:level 2 :subexp 4 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)))
    ;; Lyrics
    (,fountain-lyrics-regexp
     ((:level 1 :subexp 2 :face fountain-non-printing
              :invisible fountain-emphasis-delim
              :override append)
      (:level 1 :subexp 3 :face italic
              :override append))))
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
  (interactive
   (list (or current-prefix-arg
             (string-to-number (char-to-string
               (read-char-choice "Maximum decoration (1-3): "
                                 '(?1 ?2 ?3)))))))
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
        (font-lock-refresh-defaults))
    (user-error "Decoration must be an integer 1-3")))

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (let ((dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (var fountain-font-lock-keywords-plist keywords)
      (let ((matcher (car var))
            (plist-list (nth 1 var))
            (align (fountain-get-align (symbol-value (nth 2 var))))
            align-props facespec)
        (when (and align fountain-align-elements)
          (setq align-props
                `(line-prefix
                  (space :align-to ,align)
                  wrap-prefix
                  (space :align-to ,align))))
        (dolist (var plist-list)
          (let ((subexp (plist-get var :subexp))
                (face (when (<= (plist-get var :level) dec)
                        (plist-get var :face)))
                (invisible (plist-get var :invisible))
                invisible-props)
            (when invisible (setq invisible-props (list 'invisible invisible)))
            (setq facespec
                  (append facespec
                          (list `(,subexp '(face ,face
                                                 ,@align-props
                                                 ,@invisible-props)
                                          ,(plist-get var :override)
                                          ,(plist-get var :laxmatch)))))))
        (setq keywords
              (append keywords
                      (list (cons matcher facespec))))))))

;; FIXME: this onlys work for whole-line elements
(defun fountain-match-element (fun limit)
  "If FUN returns non-nil before LIMIT, return non-nil."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (when (funcall fun) (setq match t))
      (forward-line))
    match))

(defun fountain-redisplay-scene-numbers (start end)
  "Apply display text properties to scene numbers between START and END.

If `fountain-display-scene-numbers-in-margin' is non-nil and
scene heading has scene number, apply display text properties to
redisplay in margin. Otherwise, remove display text properties."
  ;; FIXME: Why use jit-lock rather than font-lock?
  (goto-char start)
  (while (< (point) (min end (point-max)))
    (when (fountain-match-scene-heading)
      (if (and fountain-display-scene-numbers-in-margin
               (match-string 9))
          (put-text-property (match-beginning 7) (match-end 10)
                             'display (list '(margin right-margin)
                                            (match-string-no-properties 9)))
        (remove-text-properties (match-beginning 0) (match-end 0)
                                '(display))))
    (forward-line)))


;;; Key Bindings

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing commands:
    (define-key map (kbd "TAB") #'fountain-dwim)
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
    (define-key map (kbd "C-c C-x RET") #'fountain-insert-page-break)
    (define-key map (kbd "M-TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-x a") #'fountain-completion-update)
    ;; Navigation commands:
    (define-key map [remap beginning-of-defun] #'fountain-beginning-of-scene)
    (define-key map [remap end-of-defun] #'fountain-end-of-scene)
    (define-key map (kbd "M-g s") #'fountain-goto-scene)
    (define-key map (kbd "M-g p") #'fountain-goto-page)
    (define-key map (kbd "M-n") #'fountain-forward-character)
    (define-key map (kbd "M-p") #'fountain-backward-character)
    ;; Block editing commands:
    (define-key map (kbd "<M-down>") #'fountain-shift-down)
    (define-key map (kbd "ESC <down>") #'fountain-shift-down)
    (define-key map (kbd "<M-up>") #'fountain-shift-up)
    (define-key map (kbd "ESC <up>") #'fountain-shift-up)
    ;; Outline commands:
    (define-key map [remap forward-list] #'fountain-outline-next)
    (define-key map [remap backward-list] #'fountain-outline-previous)
    (define-key map [remap forward-sexp] #'fountain-outline-forward)
    (define-key map [remap backward-sexp] #'fountain-outline-backward)
    (define-key map [remap backward-up-list] #'fountain-outline-up)
    (define-key map [remap mark-defun] #'fountain-outline-mark)
    (define-key map (kbd "C-c TAB") #'fountain-outline-cycle)
    (define-key map (kbd "<backtab>") #'fountain-outline-cycle-global)
    (define-key map (kbd "S-TAB") #'fountain-outline-cycle-global)
    (define-key map (kbd "C-c C-x b") #'fountain-outline-to-indirect-buffer)
    ;; Pages
    (define-key map (kbd "C-c C-x p") #'fountain-count-pages)
    ;; Exporting commands:
    (define-key map (kbd "C-c C-e e") #'fountain-export-buffer)
    (define-key map (kbd "C-c C-e C-e") #'fountain-export-default)
    (define-key map (kbd "C-c C-e h") #'fountain-export-buffer-to-html)
    (define-key map (kbd "C-c C-e l") #'fountain-export-buffer-to-latex)
    (define-key map (kbd "C-c C-e d") #'fountain-export-buffer-to-fdx)
    (define-key map (kbd "C-c C-e t") #'fountain-export-buffer-to-txt)
    (define-key map (kbd "C-c C-e f") #'fountain-export-buffer-to-fountain)
    (define-key map (kbd "C-c C-e s") #'fountain-export-shell-command)
    map)
  "Mode map for `fountain-mode'.")


;;; Menu

(require 'easymenu)

(easy-menu-define fountain-mode-menu fountain-mode-map
  "Menu for `fountain-mode'."
  '("Fountain"
    ("Navigate"
     ["Next Heading" fountain-outline-next]
     ["Previous Heading" fountain-outline-previous]
     ["Up Heading" fountain-outline-up]
     ["Forward Heading Same Level" fountain-outline-forward]
     ["Backward Heading Same Level" fountain-outline-backward]
     "---"
     ["Cycle Outline Visibility" fountain-outline-cycle]
     ["Cycle Global Outline Visibility" fountain-outline-cycle-global]
     ["Show All" fountain-outline-show-all]
     "---"
     ["Next Character" fountain-forward-character]
     ["Previous Character" fountain-backward-character]
     "---"
     ["Go to Scene Heading..." fountain-goto-scene]
     ["Go to Page..." fountain-goto-page])
    ("Edit Structure"
     ["Mark Subtree" fountain-outline-mark]
     ["Open Subtree in Indirect Buffer" fountain-outline-to-indirect-buffer]
     "---"
     ["Shift Element Up" fountain-shift-up]
     ["Shift Element Down" fountain-shift-down]
     "---"
     ["Shift All Elements" (customize-set-variable 'fountain-shift-all-elements
                                             (not fountain-shift-all-elements))
      :style toggle
      :selected fountain-shift-all-elements])
    ("Scene Numbers"
     ["Add Scene Numbers" fountain-add-scene-numbers]
     ["Remove Scene Numbers" fountain-remove-scene-numbers]
     "---"
     ["Display Scene Numbers in Margin"
      (customize-set-variable 'fountain-display-scene-numbers-in-margin
                              (not fountain-display-scene-numbers-in-margin))
      :style toggle
      :selected fountain-display-scene-numbers-in-margin])
    "---"
    ["Insert Metadata..." auto-insert]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    ["Count Pages" fountain-count-pages]
    ["Insert Page Break..." fountain-insert-page-break]
    ["Refresh Continued Dialog" fountain-continued-dialog-refresh]
    ["Update Auto-Completion" fountain-completion-update]
    "---"
    ("Export"
     ["Export buffer..." fountain-export-buffer]
     ["Default" fountain-export-default]
     "---"
     ["Buffer to plain text" fountain-export-buffer-to-txt]
     ["Buffer to LaTeX" fountain-export-buffer-to-latex]
     ["Buffer to HTML" fountain-export-buffer-to-html]
     ["Buffer to Final Draft" fountain-export-buffer-to-fdx]
     ["Buffer to Fountain" fountain-export-buffer-to-fountain]
     "---"
     ["Run Shell Command" fountain-export-shell-command]
     "---"
     ["US Letter Page Size" (customize-set-variable 'fountain-export-page-size
                                                    'letter)
      :style radio
      :selected (eq fountain-export-page-size 'letter)]
     ["A4 Page Size" (customize-set-variable 'fountain-export-page-size
                                             'a4)
      :style radio
      :selected (eq fountain-export-page-size 'a4)]
     "---"
     ["Include Title Page"
      (customize-set-variable 'fountain-export-include-title-page
                              (not fountain-export-include-title-page))
      :style toggle
      :selected fountain-export-include-title-page]
     ["Bold Scene Headings"
      (if (memq 'bold fountain-export-scene-heading-format)
          (customize-set-variable 'fountain-export-scene-heading-format
                             (remq 'bold fountain-export-scene-heading-format))
        (customize-set-variable 'fountain-export-scene-heading-format
                            (cons 'bold fountain-export-scene-heading-format)))
      :style toggle
      :selected (memq 'bold fountain-export-scene-heading-format)]
     ["Double-Space Scene Headings"
      (if (memq 'double-space fountain-export-scene-heading-format)
          (customize-set-variable 'fountain-export-scene-heading-format
                     (remq 'double-space fountain-export-scene-heading-format))
        (customize-set-variable 'fountain-export-scene-heading-format
                    (cons 'double-space fountain-export-scene-heading-format)))
      :style toggle
      :selected (memq 'double-space fountain-export-scene-heading-format)]
     ["Underline Scene Headings"
      (if (memq 'underline fountain-export-scene-heading-format)
          (customize-set-variable 'fountain-export-scene-heading-format
                        (remq 'underline fountain-export-scene-heading-format))
        (customize-set-variable 'fountain-export-scene-heading-format
                       (cons 'underline fountain-export-scene-heading-format)))
      :style toggle
      :selected (memq 'underline fountain-export-scene-heading-format)]
     "---"
     ["Customize Export"
      (customize-group 'fountain-export)])
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
      :selected (= (fountain-get-font-lock-decoration) 3)]
     "---"
     ["Hide Emphasis Delimiters"
      (customize-set-variable 'fountain-hide-emphasis-delim
                              (not fountain-hide-emphasis-delim))
      :style toggle
      :selected fountain-hide-emphasis-delim]
     ["Hide Syntax Characters"
      (customize-set-variable 'fountain-hide-syntax-chars
                              (not fountain-hide-syntax-chars))
      :style toggle
      :selected fountain-hide-syntax-chars])
    ["Display Elements Auto-Aligned"
     (customize-set-variable 'fountain-align-elements
                             (not fountain-align-elements))
     :style toggle
     :selected fountain-align-elements]
    ["Auto-Upcase Scene Headings"
     (customize-set-variable 'fountain-auto-upcase-scene-headings
                             (not fountain-auto-upcase-scene-headings))
     :style toggle
     :selected fountain-auto-upcase-scene-headings]
    ["Add Continued Dialog"
     (customize-set-variable 'fountain-add-continued-dialog
                             (not fountain-add-continued-dialog))
     :style toggle
     :selected fountain-add-continued-dialog]
    "---"
    ["Save Options" fountain-save-options]
    ["Customize Mode" (customize-group 'fountain)]
    ["Customize Faces" (customize-group 'fountain-faces)]))

(defun fountain-save-options ()
  "Save `fountain-mode' options with `customize'."
  (interactive)
  (let (unsaved)
    (dolist (option '(fountain-align-elements
                      fountain-auto-upcase-scene-headings
                      fountain-add-continued-dialog
                      fountain-display-scene-numbers-in-margin
                      fountain-hide-emphasis-delim
                      fountain-hide-syntax-chars
                      fountain-shift-all-elements
                      font-lock-maximum-decoration
                      fountain-export-page-size
                      fountain-export-include-title-page
                      fountain-export-scene-heading-format))
      (when (customize-mark-to-save option) (setq unsaved t)))
    (when unsaved (custom-save-all))))


;;; Mode Definition

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

;;;###autoload
(define-derived-mode fountain-mode text-mode "Fountain"
  "Major mode for screenwriting in Fountain markup."
  :group 'fountain
  (fountain-init-vars)
  (face-remap-add-relative 'default 'fountain)
  (add-hook 'post-self-insert-hook #'fountain-auto-upcase nil t)
  (when fountain-patch-emacs-bugs (fountain-patch-emacs-bugs))
  (jit-lock-register #'fountain-redisplay-scene-numbers))

(provide 'fountain-mode)

;; Local Variables:
;; coding: utf-8
;; fill-column: 72
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:

;;; fountain-mode.el ends here
