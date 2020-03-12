;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2019  Paul William Rankin
;; Copyright (c) 2019       Free Software Foundation, Inc.
;; Copyright (c) 2019-2020  Paul William Rankin

;; Author: William Rankin <code@william.bydasein.com>
;; Keywords: wp, text
;; Version: 3.1.0-beta
;; Package-Requires: ((emacs "24.5") (seq "2.20"))
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

;; # Fountain Mode #

;; Fountain Mode is a scriptwriting program for GNU Emacs using the
;; Fountain plain text markup format.

;; For more information on the Fountain format, visit <https://fountain.io>.

;; ## Features ##

;; - Support for Fountain 1.1 specification
;; - WYSIWYG auto-align elements (display only, does not modify file
;;   contents) specific to script format, e.g. screenplay, stageplay or
;;   user-defined format
;; - Traditional TAB auto-completion writing style
;; - Navigation by section, scene, character name, or page
;; - Integration with outline to fold/cycle visibility of sections and
;;   scenes
;; - Integration with imenu (sections, scene headings, notes)
;; - Intergration with auto-insert for title page metadata
;; - Automatically add/remove character (CONT'D)
;; - Toggle visibility of emphasis delimiters and syntax characters
;; - 3 levels of syntax highlighting
;; - Optionally display scene numbers in the right margin
;; - Intelligent insertion of a page breaks

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type C-h m.

;; ## Requirements ##

;; - Emacs 24.5

;; ## Exporting ##

;; Earlier versions of Fountain Mode had export functionality, but this was
;; never very good and there are several external tools available that better
;; handle exporting:

;; - [afterwriting](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md) (JavaScript)
;; - [Wrap](https://github.com/Wraparound/wrap) (Go)
;; - [screenplain](https://github.com/vilcans/screenplain) (Python 2)
;; - [Textplay](https://github.com/olivertaylor/Textplay) (Ruby, requires PrinceXML for PDF)

;; ## Installation ##

;; The latest stable release of Fountain Mode is available via
;; [MELPA-stable] and can be installed with:

;;     M-x package-install RET fountain-mode RET

;; Alternately, download the [latest release], move this file into your
;; load-path and add to your init.el file:

;;     (require 'fountain-mode)

;; If you prefer the latest but perhaps unstable version, install via
;; [MELPA], or clone the repository into your load-path and require as
;; above:

;;     git clone https://github.com/rnkn/fountain-mode.git

;; [melpa]: https://melpa.org/#/fountain-mode "MELPA"
;; [melpa-stable]: https://stable.melpa.org/#/fountain-mode "MELPA-stable"
;; [latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

;; ## History ##

;; See: <https://github.com/rnkn/fountain-mode/releases>

;; ## Bugs and Feature Requests ##

;; Report bugs and feature requests at: <https://github.com/rnkn/fountain-mode/issues>


;;; Code:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))
(require 'seq)

(eval-when-compile
  (require 'lisp-mnt)
  (defconst fountain-version
    (lm-version load-file-name)))

(defun fountain-version ()
  "Return `fountain-mode' version."
  (interactive)
  (message "Fountain Mode %s" fountain-version))


;;; Top-Level Options

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'text)

(defun fountain--set-and-refresh-font-lock (symbol value)
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
  :group 'fountain
  :type 'hook
  :options '(turn-on-visual-line-mode
             fountain-outline-hide-custom-level
             fountain-completion-update
             imenu-add-menubar-index
             turn-on-flyspell))

(define-obsolete-variable-alias 'fountain-script-format
  'fountain-default-script-format "3.0.0")
(defcustom fountain-default-script-format
  "screenplay"
  "Default script format.
Can be overridden in metadata with, e.g.

  format: teleplay"
  :group 'fountain
  :type 'string
  :safe 'string)

(defcustom fountain-add-continued-dialog
  t
  "\\<fountain-mode-map>If non-nil, \\[fountain-continued-dialog-refresh] will mark continued dialogue.

When calling `fountain-continued-dialog-refresh', append
`fountain-continued-dialog-string' to characters speaking in
succession, or if nil, remove this string."
  :group 'fountain
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-continued-dialog-string
  "(CONT'D)"
  "String to append to character name speaking in succession.
If `fountain-add-continued-dialog' is non-nil, append this string
to characters speaking in succession when calling
`fountain-continued-dialog-refresh'.

n.b. if you change this option then call
`fountain-continued-dialog-refresh', strings matching the
previous value will not be recognized. First remove all instances
in your script by setting `fountain-add-continued-dialog' to nil
and calling `fountain-continued-dialog-refresh', then customize
this option."
  :group 'fountain
  :type 'string
  :safe 'stringp)

(defcustom fountain-more-dialog-string
  "(MORE)"
  "String to append to dialog when breaking across pages."
  :type 'string
  :safe 'stringp)

(define-obsolete-variable-alias 'fountain-hide-emphasis-delim
  'fountain-hide-emphasis-markup "3.0.0")
(defcustom fountain-hide-emphasis-markup
  nil
  "If non-nil, make emphasis delimiters invisible."
  :group 'fountain
  :type 'boolean
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (if fountain-hide-emphasis-markup
                   (add-to-invisibility-spec 'fountain-emphasis-delim)
                 (remove-from-invisibility-spec 'fountain-emphasis-delim))
               (font-lock-refresh-defaults))))))

(define-obsolete-variable-alias 'fountain-hide-syntax-chars
  'fountain-hide-element-markup "3.0.0")
(defcustom fountain-hide-element-markup
  nil
  "If non-nil, make syntax characters invisible."
  :group 'fountain
  :type 'boolean
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (if fountain-hide-element-markup
                   (add-to-invisibility-spec 'fountain-syntax-chars)
                 (remove-from-invisibility-spec 'fountain-syntax-chars))
               (font-lock-refresh-defaults))))))

(defcustom fountain-note-template
  " %x - %n: "
  "\\<fountain-mode-map>Template for inserting notes with \\[fountain-insert-note].
Passed to `format-spec' with the following specification:

  %u  user-login-name
  %n  user-full-name
  %e  user-mail-address
  %x  date in locale's preferred format
  %F  date in ISO format

The default \" %x - %n:\" inserts something like:

  [[ 12/31/2017 - Alan Smithee: ]]"
  :group 'fountain
  :type 'string
  :safe 'stringp)


;;; Faces

(defgroup fountain-faces ()
  "\\<fountain-mode-map>Faces used in `fountain-mode'.
There are three levels of decoration, each with different
elements fontified:

  1. minimum:   comments
                emphasis

  2. default:   comments
                emphasis
                element characters
                section headings
                scene headings
                synopses
                notes
                metadata values

  3. maximum:   comments
                emphasis
                element characters
                section headings
                scene headings
                synopses
                notes
                metadata keys
                metadata values
                character names
                parentheticals
                dialog
                transitions
                center text

To switch between these levels, customize the value of
`font-lock-maximum-decoration'. This can be set with \\[fountain-set-font-lock-decoration]."
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
  "Default face for element and emphasis markup.")

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
  '((t (:inherit font-lock-string-face)))
  "Default face for parentheticals.")

(defface fountain-center
  '((t nil))
  "Default face for centered text.")

(defface fountain-note
  '((t (:inherit font-lock-comment-face)))
  "Default face for notes.")

(define-obsolete-face-alias 'fountain-section-heading
  'fountain-section-heading-1 "3.0.0")
(defface fountain-section-heading-1
  '((t (:inherit outline-1)))
  "Default face for section level 1 headings.")

(defface fountain-section-heading-2
  '((t (:inherit outline-2)))
  "Default face for section level 2 headings.")

(defface fountain-section-heading-3
  '((t (:inherit outline-3)))
  "Default face for section level 3 headings.")

(defface fountain-section-heading-4
  '((t (:inherit outline-4)))
  "Default face for section level 4 headings.")

(defface fountain-section-heading-5
  '((t (:inherit outline-5)))
  "Default face for section level 5 headings.")

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
  '((t nil))
  "Default face for transitions.")


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

Contructed with `fountain-init-scene-heading-regexp'. Requires
`fountain-match-scene-heading' for preceding blank line.")

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

(define-obsolete-variable-alias 'fountain-scene-heading-suffix-sep
  'fountain-scene-heading-suffix-separator)
(defcustom fountain-scene-heading-suffix-separator
  " --? "
  "Regular expression separating scene heading location from suffix.

n.b. If you change this any existing scene headings may not
be parsed correctly."
  :group 'fountain
  :type 'regexp
  :safe 'regexp
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-scene-heading-suffix-list
  '("DAY" "NIGHT" "CONTINUOUS" "LATER" "MOMENTS LATER")
  "List of scene heading suffixes (case insensitive).

These are only used for auto-completion. Any scene headings can
have whatever suffix you like.

Separated from scene heading locations with
`fountain-scene-heading-suffix-separator'."
  :group 'fountain
  :type '(repeat (string :tag "Suffix"))
  :set #'fountain--set-and-refresh-font-lock)

(defvar fountain-trans-regexp
  nil
  "Regular expression for matching transitions.

  Group 1: match forced transition mark
  Group 2: match transition

Constructed with `fountain-init-trans-regexp'. Requires
`fountain-match-trans' for preceding and succeeding blank
lines.")

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

(defcustom fountain-character-extension-list
  '("(V.O.)" "(O.S.)" "(O.C.)")
  "List of extensions after character names (case sensitive).

`fountain-continued-dialog-string' is automatically added to this
list.

These are only used for auto-completion. Any character can have
whatever extension you like."
  :group 'fountain
  :type '(repeat (string :tag "Extension")))

(defconst fountain-action-regexp
  "^\\(!\\)?\\(.*\\)[\s\t]*$"
  "Regular expression for forced action.

  Group 1: match forced action mark
  Group 2: match trimmed whitespace")

(defconst fountain-comment-regexp
  (concat "\\(?://[\s\t]*\\(?:.*\\)\\)"
          "\\|"
          "\\(?:\\(?:/\\*\\)[\s\t]*\\(?:\\(?:.\\|\n\\)*?\\)[\s\t]*\\*/\\)")
  "Regular expression for matching comments.")

(defconst fountain-metadata-regexp
  (concat "^\\([^:\s\t\n][^:\n]*\\):[\s\t]*\\(.+\\)?"
          "\\|"
          "^[\s\t]+\\(?2:.+\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-match-metadata' for `bobp'.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*"
          "\\(?:\\(?1:@\\)\\(?2:[^<>\n]+?\\)"
          "\\|"
          "\\(?2:[^<>\n[:lower:]]*?[[:upper:]]+[^<>\n[:lower:]]*?\\)"
          "\\)"
          "\\(?3:[\s\t]*\\(?4:(\\).*?)\\)*?"
          "\\(?5:[\s\t]*^\\)?"
          "[\s\t]*$")
  "Regular expression for matching character names.

  Group 1: match leading @ for forced character
  Group 2: match character name
  Group 3: match parenthetical extension
  Group 4: match opening parenthetical
  Group 5: match trailing ^ for dual dialog

Requires `fountain-match-character' for preceding blank line.")

(defconst fountain-dialog-regexp
  "^\\(\s\s\\)$\\|^[\s\t]*\\([^<>\n]+?\\)[\s\t]*$"
  "Regular expression for matching dialogue.

  Group 1: match trimmed whitespace

Requires `fountain-match-dialog' for preceding character,
parenthetical or dialogue.")

(defconst fountain-paren-regexp
  "^[\s\t]*([^)\n]*)[\s\t]*$"
  "Regular expression for matching parentheticals.

Requires `fountain-match-paren' for preceding character or
dialogue.")

(defconst fountain-page-break-regexp
  "^[\s\t]*\\(=\\{3,\\}\\)[\s\t]*\\([a-z0-9\\.-]+\\)?.*$"
  "Regular expression for matching page breaks.

  Group 1: leading ===
  Group 2: forced page number")

(defconst fountain-note-regexp
  "\\[\\[[\s\t]*\\(\\(?:.\\|\n\\)*?\\)[\s\t]*]]"
  "Regular expression for matching notes.

  Group 1: note text")

(defconst fountain-section-heading-regexp
  "^\\(?1:\\(?2:#\\{1,5\\}\\)[\s\t]*\\)\\(?3:[^#\n].*?\\)[\s\t]*$"
  "Regular expression for matching section headings.

  Group 1: match leading #'s and following whitespace
  Group 2: match leading #'s
  Group 3: match heading text")

(defconst fountain-synopsis-regexp
  "^\\(\\(=\\)[\s\t]*\\)\\([^=\n].*?\\)$"
  "Regular expression for matching synopses.

  Group 1: leading = and following whitespace
  Group 2: leading =
  Group 3: synopsis text")

(defconst fountain-center-regexp
  "^[\s\t]*\\(>\\)[\s\t]*\\(.+?\\)[\s\t]*\\(<\\)[\s\t]*$"
  "Regular expression for matching centered text.

  Group 1: match leading >
  Group 2: match center text
  Group 3: match trailing <")

(defconst fountain-underline-regexp
  "\\(?:^\\|[^\\]\\)\\(\\(_\\)\\([^\n\s\t_][^_\n]*?\\)\\(\\2\\)\\)"
  "Regular expression for matching underlined text.")

(defconst fountain-italic-regexp
  "\\(?:^\\|[^\\*\\]\\)\\(\\(\\*\\)\\([^\n\s\t\\*][^\\*\n]*?\\)\\(\\2\\)\\)"
  "Regular expression for matching italic text.")

(defconst fountain-bold-regexp
  "\\(?:^\\|[^\\]\\)\\(\\(\\*\\*\\)\\([^\n\s\t\\*][^\\*\n]*?\\)\\(\\2\\)\\)"
  "Regular expression for matching bold text.")

(defconst fountain-bold-italic-regexp
  "\\(?:^\\|[^\\]\\)\\(\\(\\*\\*\\*\\)\\([^\n\s\t\\*][^\\*\n]*?\\)\\(\\2\\)\\)"
  "Regular expression for matching bold-italic text.

Due to the nature of the syntax, bold-italic-underlined text must
be specified with the bold-italic delimiters together, e.g.

  This text is _***ridiculously important***_.

  This text is ***_stupendously significant_***.")

(defconst fountain-lyrics-regexp
  "^\\(~[\s\t]*\\)\\(.+\\)"
  "Regular expression for matching lyrics.")


;;; Aligning

(defgroup fountain-align ()
  "Options for element alignment.

Each Fountain element align option can be an integer representing
the align column for all formats, or a list where each element
takes the form:

  (FORMAT . INT)

Where FORMAT is a script format string and INT is the align
column for that format.

To disable element alignment, see `fountain-align-elements'."
  :prefix "fountain-align-"
  :group 'fountain)

(defcustom fountain-align-elements
  t
  "If non-nil, elements will be displayed auto-aligned.
This option does not affect file contents."
  :group 'fountain-align
  :type 'boolean
  :safe 'booleanp
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-section-heading
  '(("screenplay" . 0)
    ("stageplay" . 30))
  "Column integer to which section headings should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-scene-heading
  '(("screenplay" . 0)
    ("stageplay" . 30))
  "Column integer to which scene headings should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-synopsis
  '(("screenplay" . 0)
    ("stageplay" . 30))
  "Column integer to which synopses should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-action
  '(("screenplay" . 0)
    ("stageplay" . 20))
  "Column integer to which action should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-character
  '(("screenplay" . 20)
    ("stageplay" . 30))
  "Column integer to which characters names should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-dialog
  '(("screenplay" . 10)
    ("stageplay" . 0))
  "Column integer to which dialog should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-paren
  '(("screenplay" . 15)
    ("stageplay" . 20))
  "Column integer to which parentheticals should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-trans
  '(("screenplay" . 45)
    ("stageplay" . 30))
  "Column integer to which transitions should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-center
  '(("screenplay" . 20)
    ("stageplay" . 20))
  "Column integer to which centered text should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)


;;; Autoinsert

(require 'autoinsert)

(defvar fountain-metadata-skeleton
  '(nil
    "title: " (skeleton-read "Title: " (file-name-base (buffer-name))) | -7 "\n"
    "credit: " (skeleton-read "Credit: " "written by") | -9 "\n"
    "author: " (skeleton-read "Author: " user-full-name) | -9 "\n"
    "format: " (skeleton-read "Script format: " fountain-default-script-format) | -9 "\n"
    "source: " (skeleton-read "Source: ") | -9 "\n"
    "date: " (skeleton-read "Date: " (format-time-string "%x")) | -7 "\n"
    "contact:\n" ("Contact details, %s: " "    " str | -4 "\n") | -9))

(define-auto-insert '(fountain-mode . "Fountain metadata skeleton")
  fountain-metadata-skeleton)


;;; Initializing

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
         "\\(?:\\(?5:" fountain-scene-heading-suffix-separator "\\)"
         ;; Group 6: match suffix
         "\\(?6:.+?\\)?\\)?"
         "\\)\\|"
         ;; Group 2: match scene heading without scene number
         "^\\(?2:"
         ;; Group 3: match INT/EXT
         "\\(?3:" (regexp-opt fountain-scene-heading-prefix-list) "\\.?\s+\\)"
         ;; Group 4: match location
         "\\(?4:.+?\\)?"
         ;; Group 5: match suffix separator
         "\\(?:\\(?5:" fountain-scene-heading-suffix-separator "\\)"
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
         "\\(>\\)[\s\t]*"
         ;; Group 2: match forced transition
         "\\([^<>\n]*?\\)"
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

(require 'imenu)

(defcustom fountain-imenu-elements
  '(section-heading scene-heading synopsis note)
  "List of elements to include in `imenu'."
  :type '(set (const :tag "Section Headings" section-heading)
              (const :tag "Scene Headings" scene-heading)
              (const :tag "Synopses" synopsis)
              (const :tag "Notes" note))
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (fountain-init-imenu))))))

(defun fountain-init-imenu ()
  "Initialize `imenu-generic-expression'."
  (setq imenu-generic-expression nil)
  (if (memq 'section-heading fountain-imenu-elements)
      (push (list "Section Headings" fountain-section-heading-regexp 3)
            imenu-generic-expression))
  (if (memq 'scene-heading fountain-imenu-elements)
      (push (list "Scene Headings" fountain-scene-heading-regexp 2)
            imenu-generic-expression))
  (if (memq 'synopsis fountain-imenu-elements)
      (push (list "Synopses" fountain-synopsis-regexp 3)
            imenu-generic-expression))
  (if (memq 'note fountain-imenu-elements)
      (push (list "Notes" fountain-note-regexp 1)
            imenu-generic-expression))
  (when (featurep 'imenu) (imenu-update-menubar)))

(defun fountain-init-vars ()
  "Initialize important variables.
Needs to be called for every Fountain buffer because some
variatbles are required for functions to operate with temporary
buffers."
  (fountain-init-scene-heading-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-imenu)
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
  ;; This should be temporary. Feels better to ensure appropriate
  ;; case-fold within each function.
  (setq case-fold-search t)
  (setq imenu-case-fold-search nil)
  (setq font-lock-multiline 'undecided)
  (setq font-lock-defaults '(fountain-init-font-lock))
  (add-to-invisibility-spec (cons 'outline t))
  (when fountain-hide-emphasis-markup
    (add-to-invisibility-spec 'fountain-emphasis-delim))
  (when fountain-hide-element-markup
    (add-to-invisibility-spec 'fountain-syntax-chars)))


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

(defun fountain-maybe-in-dialog-p ()
  "Return non-nil if point may be in dialogue."
  (save-excursion
    (or (fountain-match-dialog)
        (fountain-match-paren)
        (and (save-restriction
               (widen)
               (forward-line -1)
               (or (fountain-match-character 'loose)
                   (fountain-match-dialog)))))))

(defun fountain-match-metadata ()
  "Match metadata if point is at metadata, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (looking-at fountain-metadata-regexp)
         (save-match-data
           (save-restriction
             (widen)
             (or (bobp)
                 (and (forward-line -1)
                      (fountain-match-metadata))))))))

(defun fountain-match-page-break ()
  "Match page break if point is at page break, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at fountain-page-break-regexp)))

(defun fountain-match-section-heading ()
  "Match section heading if point is at section heading, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at fountain-section-heading-regexp)))

(defun fountain-match-synopsis ()
  "Match synopsis if point is at synopsis, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at fountain-synopsis-regexp)))

(defun fountain-match-note ()
  "Match note if point is at note, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (or (looking-at fountain-note-regexp)
        (save-restriction
          (widen)
          (let ((x (point)))
            (and (re-search-backward "\\[\\[" nil t)
                 (looking-at fountain-note-regexp)
                 (< x (match-end 0))))))))

(defun fountain-match-scene-heading ()
  "Match scene heading if point is at a scene heading, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (looking-at fountain-scene-heading-regexp)
         (fountain-blank-before-p))))

(defun fountain-match-character (&optional loose)
  "Match character if point is at character, nil otherwise.
When LOOSE is non-nil, do not require non-blank line after."
  (unless (fountain-match-scene-heading)
    (save-excursion
      (beginning-of-line)
      (and (not (looking-at "!"))
           (let (case-fold-search)
             (looking-at fountain-character-regexp))
           (fountain-blank-before-p)
           (if loose t (not (fountain-blank-after-p)))))))

(defun fountain-match-dialog ()
  "Match dialog if point is at dialog, nil otherwise."
  (unless (or (and (bolp) (eolp))
              (fountain-match-paren)
              (fountain-match-note))
    (save-excursion
      (save-restriction
        (widen)
        (beginning-of-line)
        (and (looking-at fountain-dialog-regexp)
             (unless (bobp)
               (forward-line -1)
               (save-match-data
                 (or (fountain-match-character)
                     (fountain-match-paren)
                     (fountain-match-dialog)))))))))

(defun fountain-match-paren ()
  "Match parenthetical if point is at a paranthetical, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (looking-at fountain-paren-regexp)
         (save-match-data
           (save-restriction
             (widen)
             (unless (bobp)
               (forward-line -1)
               (or (fountain-match-character)
                   (fountain-match-dialog))))))))

(defun fountain-match-trans ()
  "Match transition if point is at a transition, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (let (case-fold-search)
           (looking-at fountain-trans-regexp))
         (fountain-blank-before-p)
         (fountain-blank-after-p))))

(defun fountain-match-center ()
  "Match centered text if point is at centered text, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at fountain-center-regexp)))

;; FIXME: too expensive
(defun fountain-match-action ()
  "Match action text if point is at action, nil otherwise.
Assumes that all other element matching has been done."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (and (looking-at fountain-action-regexp)
               (match-string-no-properties 1))
          (and (not (or (and (bolp) (eolp))
                        (fountain-match-section-heading)
                        (fountain-match-scene-heading)
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

;; FIXME: is it even worth bothering with dual-dialog?
(defun fountain-get-element ()
  "Return element at point as a symbol."
  (cond
   ((and (bolp) (eolp)) nil)
   ((fountain-match-metadata) 'metadata)
   ((fountain-match-section-heading) 'section-heading)
   ((fountain-match-scene-heading) 'scene-heading)
   ((and (fountain-match-character)
         (fountain-read-dual-dialog))
    'character-dd)
   ((fountain-match-character) 'character)
   ((and (fountain-match-dialog)
         (fountain-read-dual-dialog))
    'lines-dd)
   ((fountain-match-dialog) 'lines)
   ((and (fountain-match-paren)
         (fountain-read-dual-dialog))
    'paren-dd)
   ((fountain-match-paren) 'paren)
   ((fountain-match-trans) 'trans)
   ((fountain-match-center) 'center)
   ((fountain-match-synopsis) 'synopsis)
   ((fountain-match-page-break) 'page-break)
   ((fountain-match-note) 'note)
   (t 'action)))


;;; Auto-completion

(defvar-local fountain--completion-locations
  nil
  "List of scene locations in the current buffer.")

(defvar-local fountain--completion-characters
  nil
  "List of characters in the current buffer.
Each element is a cons (NAME . OCCUR) where NAME is a string, and
OCCUR is an integer representing the character's number of
occurrences.")

(defcustom fountain-completion-additional-characters
  nil
  "List of additional character strings to offer for completion.
Case sensitive. Prefix character names with `@' to use lowercase.

Characters speaking in the current script are already populated
with `fountain-completion-update', so this is usually better set
as a directory variable."
  :group 'fountain
  :type '(repeat (string :tag "Character"))
  :link '(info-link "(emacs) Directory Variables")
  :safe '(lambda (value)
           (and (listp value)
                (seq-every-p 'stringp value))))

(defcustom fountain-completion-additional-locations
  nil
  "List of additional location strings to offer for completion.
Case sensitive. Locations will be upcased if
`fountain-auto-upcase-scene-headings' is non-nil.

Locations occuring in the current script are already populated
with `fountain-completion-update', so this is usually better set
as a directory variable."
  :group 'fountain
  :type '(repeat (string :tag "Location"))
  :link '(info-link "(emacs) Directory Variables")
  :safe '(lambda (value)
           (and (listp value)
                (seq-every-p 'stringp value))))

(defun fountain-completion-get-characters ()
  "Return a list of characters for completion.

First, return second-last speaking character, then the last
speaking character, followed by each previously speaking
character within scene. After that, return characters from
`fountain-completion-additional-characters' then
`fountain--completion-characters'.

n.b. `fountain-completion-additional-characters' are offered as
candidates ahead of `fountain--completion-characters' because
these need to be manually set, and so are considered more
important."
  (let (scene-characters alt-character contd-character rest-characters)
    (save-excursion
      (save-restriction
        (widen)
        (fountain-forward-character 0 'scene)
        (while (not (or (bobp) (fountain-match-scene-heading)))
          (when (fountain-match-character)
            (cl-pushnew (match-string-no-properties 2) scene-characters))
          (fountain-forward-character -1 'scene))))
    (setq scene-characters (reverse scene-characters)
          alt-character (cadr scene-characters)
          contd-character (car scene-characters)
          rest-characters (cddr scene-characters)
          scene-characters nil)
    (when rest-characters (setq scene-characters rest-characters))
    (when contd-character (push contd-character scene-characters))
    (when alt-character (push alt-character scene-characters))
    (delete-dups
     (append scene-characters
             fountain-completion-additional-characters
             (mapcar 'car fountain--completion-characters)))))

(defun fountain-completion-at-point ()
  "\\<fountain-mode-map>Return completion table for entity at point.
Trigger completion with \\[fountain-dwim].

  1. If point is at a scene heading and matches
     `fountain-scene-heading-suffix-separator', offer completion
     candidates from `fountain-scene-heading-suffix-list'.
  2. If point is at a line matching `fountain-scene-heading-prefix-list',
     offer completion candidates from `fountain--completion-locations' plus
     `fountain-completion-additional-locations'.
  3. If point is at a possible character name with an opening parenthetical
     extension, offer completion candidates from
     `fountain-character-extension-list' plus `fountain-continued-dialog-string'.
  4. If point is at beginning of line with a preceding blank line, offer
     completion candidates from `fountain--completion-characters' plus
     `fountain-completion-additional-characters'. For more information of
     character completion sorting, see `fountain-completion-get-characters'.

Added to `completion-at-point-functions'."
  (cond ((and (fountain-match-scene-heading)
              (match-string-no-properties 5))
         ;; Return scene heading suffix completion
         (list (match-end 5)
               (point)
               (completion-table-case-fold
                fountain-scene-heading-suffix-list)))
        ((and (fountain-match-scene-heading)
              (match-string-no-properties 3))
         ;; Return scene location completion
         (list (match-end 3)
               (point)
               (completion-table-case-fold
                (append
                 fountain-completion-additional-locations
                 fountain--completion-locations))))
        ((and (fountain-match-scene-heading)
              (match-string-no-properties 1))
         ;; Return scene location completion (forced)
         (list (match-end 1)
               (point)
               (completion-table-case-fold
                (append
                 fountain-completion-additional-locations
                 fountain--completion-locations))))
        ;; Return character extension
        ((and (fountain-match-character 'loose)
              (match-string-no-properties 4))
         (list (match-beginning 4)
               (line-end-position)
               (completion-table-case-fold
                (append fountain-character-extension-list
                        (list fountain-continued-dialog-string)))))
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

While `fountain--completion-locations' are left unsorted for
`completion-at-point' to perform sorting,
`fountain--completion-characters' are sorted by number of lines.
For more information on character completion sorting, see
`fountain-completion-get-characters'.

Add to `fountain-mode-hook' to have completion upon load."
  (interactive)
  (setq fountain--completion-locations nil
        fountain--completion-characters nil)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (fountain-match-scene-heading)
          (cl-pushnew (match-string-no-properties 4)
                      fountain--completion-locations))
        (fountain-forward-scene 1))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (fountain-match-character)
          (let ((character (match-string-no-properties 2))
                candidate lines)
            (setq candidate (assoc-string character
                                          fountain--completion-characters)
                  lines (cdr candidate))
            (if (null lines)
                (push (cons character 1) fountain--completion-characters)
              (cl-incf (cdr candidate)))))
        (fountain-forward-character 1))
      (setq fountain--completion-characters
            (seq-sort (lambda (a b) (< (cdr b) (cdr a)))
                      fountain--completion-characters))))
  (message "Completion candidates updated"))


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

(define-obsolete-variable-alias 'fountain-fold-notes
  'fountain-outline-fold-notes "3.0.0")
(defcustom fountain-outline-fold-notes
  t
  "\\<fountain-mode-map>If non-nil, fold contents of notes when cycling outline visibility.

Notes visibility can be cycled with \\[fountain-dwim]."
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
                ((memq element '(character character-dd lines lines-dd paren paren-dd))
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

(defun fountain--insert-hanging-line-maybe ()
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

;; FIXME: this implementation will wipe out any overlays on the next block of
;; text that is deleted and reinserted. Must find a better way.
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
                  (when (setq hanging-line (fountain--insert-hanging-line-maybe))
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
                (when (setq hanging-line (fountain--insert-hanging-line-maybe))
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
            (setq hanging-line (fountain--insert-hanging-line-maybe))
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
      (cl-decf i))
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
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward fountain-note-regexp nil 'move)
             (outline-flag-region (match-beginning 1) (match-end 1)
                                  fountain-outline-fold-notes)))
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
     its children (\\[fountain-dwim]).
  2. If ARG is 4, cycle outline visibility of buffer (\\[universal-argument] \\[fountain-dwim],
     same as \\[fountain-outline-cycle-global]).
  3. If ARG is 16, show all (\\[universal-argument] \\[universal-argument] \\[fountain-dwim]).
  4. If ARG is 64, show outline visibility set in
     `fountain-outline-custom-level' (\\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-dwim])."
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
             level)))
        (fold-notes-fun
         (lambda (eohp eosp)
           (goto-char eohp)
           (while (re-search-forward fountain-note-regexp eosp 'move)
             (outline-flag-region (match-beginning 1) (match-end 1)
                                  fountain-outline-fold-notes)))))
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
          ((eq arg 64)
           (if custom-level
               (fountain-outline-hide-level custom-level)
             (outline-show-all)))
          (t
           (save-excursion
             (outline-back-to-heading)
             (let ((eohp
                    (save-excursion
                      (outline-end-of-heading)
                      (point)))
                   (eosp
                    (save-excursion
                      (outline-end-of-subtree)
                      (point)))
                   (eolp
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
                ((= eosp eohp)
                 (message "Empty heading")
                 (setq fountain--outline-cycle-subtree 0))
                ((and (<= eosp eolp)
                      children)
                 (outline-show-entry)
                 (outline-show-children)
                 (funcall fold-notes-fun eohp eosp)
                 (message "Showing headings")
                 (setq fountain--outline-cycle-subtree 2))
                ((or (<= eosp eolp)
                     (= fountain--outline-cycle-subtree 2))
                 (outline-show-subtree)
                 (goto-char eohp)
                 (funcall fold-notes-fun eohp eosp)
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
  (if (string-prefix-p "#" (match-string-no-properties 0))
      (string-width (match-string-no-properties 2))
    6))

(defun fountain-insert-section-heading ()
  "Insert an empty section heading at the current outline level."
  (interactive)
  (unless (and (bolp) (eolp))
    (if (bolp)
        (save-excursion (newline))
      (end-of-line) (newline)))
  (let (level)
    (save-excursion
      (save-restriction
        (widen)
        (ignore-errors
          (outline-back-to-heading t)
          (if (= (funcall outline-level) 6)
              (outline-up-heading 1)))
        (setq level
              (if (outline-on-heading-p)
                  (funcall outline-level)
                1))))
    (insert (make-string level ?#) " ")))

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
          (setq heading-name (string-trim (match-string-no-properties 0)
                                          "[#\s]+")
                target-buffer (concat base-buffer "-" heading-name))
          (outline-end-of-subtree)
          (setq end (point)))))
    (if (and (get-buffer target-buffer)
             (with-current-buffer target-buffer
               (goto-char (point-min))
               (skip-chars-forward "\n\s\t")
               (= (point) beg)))
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
                   (car (fountain-scene-number-to-list
                         (match-string-no-properties 8)))
                 0)))
    (while (and (< scene n)
                (< (point) (point-max)))
      (fountain-forward-scene 1)
      (when (fountain-match-scene-heading)
        (setq scene (or (car (fountain-scene-number-to-list
                              (match-string-no-properties 8)))
                        (1+ scene)))))))

(defun fountain-goto-page (n)
  "Move point to Nth appropropriate page in current buffer."
  (interactive "NGo to page: ")
  (widen)
  (push-mark)
  (goto-char (point-min))
  (let ((i n))
    (while (fountain-match-metadata) (forward-line))
    (if (looking-at "[\n\s\t]*\n") (goto-char (match-end 0)))
    (while (< 1 i)
      (if (and (fountain-match-page-break) (match-string-no-properties 2))
          (setq i (- n (string-to-number (match-string-no-properties 2)))))
      (fountain-forward-page)
      (cl-decf i))))

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
        (match-string-no-properties 2)))))

(defun fountain-read-metadata ()
  "Read metadata of current buffer and return as a property list.

Key string is slugified and interned. Value string remains a
string. e.g.

  Draft date: 2015-12-25 -> (draft-date \"2015-12-25\")"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (list)
        (while (and (bolp) (fountain-match-metadata))
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (forward-line)
            (while (and (fountain-match-metadata)
                        (null (match-string-no-properties 1)))
              (setq value
                    (concat value (when value "\n")
                            (match-string-no-properties 2)))
              (forward-line))
            (push (cons (intern (string-join (split-string (downcase
                (replace-regexp-in-string "[^\n\s\t[:alnum:]]" "" key))
                    "[^[:alnum:]]+" t) "-")) value)
                  list)))
        list))))

(defun fountain-read-dual-dialog (&optional pos)
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
                           (stringp (match-string-no-properties 5))))
               'right)
              ((progn (fountain-forward-character 1 'dialog)
                      (and (fountain-match-character)
                           (stringp (match-string-no-properties 5))))
               'left))))))


;;; Editing

(defcustom fountain-auto-upcase-scene-headings
  t
  "If non-nil, automatically upcase lines matching `fountain-scene-heading-regexp'."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain)

(defcustom fountain-dwim-insert-next-character
  nil
  "When non-nil `fountain-dwim' inserts next character after dialogue."
  :type 'boolean
  :safe 'booleanp
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

  1. If prefixed with ARG, call `fountain-outline-cycle' and pass ARG.
  2. If point is inside an empty parenthetical, delete it.
  3. If point is inside a non-empty parenthetical, move to a newline.
  4. If point is at a blank line within dialogue, insert a parenthetical.
  5. If point is at a note, cycle visibility of that note.
  6. If point is at the end of line, call `completion-at-point'.
  7. If point is a scene heading or section heading, cycle visibility of that
     heading."
  (interactive "p")
  (cond ((and arg (< 1 arg))
         (fountain-outline-cycle arg))
        ((save-excursion
           (beginning-of-line)
           (looking-at "()"))
         (delete-region (match-beginning 0) (match-end 0)))
        ((and (fountain-match-paren)
              (fountain-blank-after-p))
         (end-of-line)
         (newline))
        ((fountain-match-paren)
         (forward-line))
        ((and (bolp) (eolp)
              (fountain-maybe-in-dialog-p))
         (insert-parentheses))
        ((and fountain-dwim-insert-next-character
              (eolp)
              (fountain-maybe-in-dialog-p))
         (newline 2)
         (completion-at-point))
        ((fountain-match-note)
         (outline-flag-region (match-beginning 1) (match-end 1)
            (not (get-char-property (match-beginning 1) 'invisible))))
        ((or (eolp) (looking-at ")$"))
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
          (unless (eobp) (forward-char 1)))))
    (set-marker end nil)))

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
      (insert (format-spec fountain-note-template
        (format-spec-make ?u user-login-name
                          ?n user-full-name
                          ?e user-mail-address
                          ?F (format-time-string "%F")
                          ?x (format-time-string "%x")))))))

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
               (unless (fountain-match-character)
                 (fountain-forward-character))
               (while (< (point) (point-max))
                 (if (re-search-forward
                      (concat "[\s\t]*" string) (line-end-position) t)
                     (delete-region (match-beginning 0) (match-end 0)))
                 (fountain-forward-character)
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

(defgroup fountain-scene-numbers ()
  "Options for scene numbers."
  :prefix "fountain-scene-numbers-"
  :group 'fountain)

(define-obsolete-variable-alias 'fountain-display-scene-numbers-in-margin
  'fountain-scene-numbers-display-in-margin "3.0.0")
(defcustom fountain-scene-numbers-display-in-margin
  nil
  "If non-nil, display scene numbers in the right margin.

If nil, do not change scene number display.

This option does affect file contents."
  :group 'fountain-scene-numbers
  :type 'boolean
  :safe 'booleanp
  :set #'fountain--set-and-refresh-font-lock)

(define-obsolete-variable-alias 'fountain-prefix-revised-scene-numbers
  'fountain-scene-numbers-prefix-revised "3.0.0")
(defcustom fountain-scene-numbers-prefix-revised
  nil
  "If non-nil, revised scene numbers are prefixed.

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

n.b. Using conflicting revised scene number format in the same
script may result in errors in output."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-scene-numbers)

(define-obsolete-variable-alias 'fountain-scene-number-first-revision
  'fountain-scene-numbers-first-revision-char "3.0.0")
(defcustom fountain-scene-numbers-first-revision-char
  ?A
  "Character to start revised scene numbers."
  :type 'character
  :safe 'characterp
  :group 'fountain-scene-numbers)

(define-obsolete-variable-alias 'fountain-scene-number-separator
  'fountain-scene-numbers-separator "3.0.0")
(defcustom fountain-scene-numbers-separator
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

If `fountain-scene-numbers-prefix-revised' is non-nil:

  \"10\" -> (10)
  \"AA10\" -> (9 1 1)

Or if nil:

  \"10\" -> (10)
  \"10AA\" -> (10 1 1)"
  ;; FIXME: does not account for user option
  ;; `fountain-scene-numbers-separator' or
  ;; `fountain-scene-numbers-first-revision-char'.
  (let (number revision)
    (when (stringp string)
      (if fountain-scene-numbers-prefix-revised
          (when (string-match "\\([a-z]*\\)[\\.-]*\\([0-9]+\\)[\\.-]*" string)
            (setq number (string-to-number (match-string-no-properties 2 string))
                  revision (match-string-no-properties 1 string))
            (unless (string-empty-p revision) (setq number (1- number))))
        (when (string-match "\\([0-9]+\\)[\\.-]*\\([a-z]*\\)[\\.-]*" string)
          (setq number (string-to-number (match-string-no-properties 1 string))
                revision (match-string-no-properties 2 string))))
      (setq revision (mapcar (lambda (n) (- (upcase n) 64)) revision))
      (cons number revision))))

(defun fountain-scene-number-to-string (scene-num-list)
  "Read scene number SCENE-NUM-LIST and return a string.

If `fountain-scene-numbers-prefix-revised' is non-nil:

    (10) -> \"10\"
    (9 1 2) -> \"AB10\"

Or, if nil:

    (10) -> \"10\"
    (9 1 2) -> \"9AB\""
  (let ((number (car scene-num-list))
        separator revision)
    (when (< 1 (length scene-num-list))
      (setq separator
            (if fountain-scene-numbers-separator
                (char-to-string fountain-scene-numbers-separator)
              "")
            revision
            (mapconcat (lambda (char)
                         (char-to-string
                          (+ (1- char) fountain-scene-numbers-first-revision-char)))
                       (cdr scene-num-list) separator)))
    (if fountain-scene-numbers-prefix-revised
        (progn
          (unless (string-empty-p revision) (cl-incf number))
          (concat revision separator (number-to-string number)))
      (concat (number-to-string number) separator revision))))

(defun fountain-get-scene-number (&optional n)
  "Return the scene number of the Nth next scene as a list.
Return Nth previous if N is negative.

Scene numbers will not be accurate if buffer contains directives
to include external files."
  (unless n (setq n 0))
  ;; FIXME: the whole scene number (and page number) logic could be
  ;; improved by first generating a list of existing numbers,
  ;; e.g. '((4) (5) (5 1) (6))
  ;; then only calculating revised scene when current = next.
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
            ;; FIXME: scenes should never be treated as out of order.
            (err-order "Scene %S seems to be out of order")
            found)
        ;; First, check if there are any scene numbers already. If not
        ;; we can save a lot of work.
        ;; FIXME: this is just extra work since we're doing for each
        ;; scene heading
        (save-match-data
          (goto-char (point-min))
          (while (not (or found (eobp)))
            (when (and (re-search-forward fountain-scene-heading-regexp nil 'move)
                       (match-string-no-properties 9))
              (setq found t))))
        (if found
            ;; There are scene numbers, so this scene number needs to be
            ;; calculated relative to those.
            (let ((current-scene
                   (fountain-scene-number-to-list (match-string-no-properties 9)))
                  last-scene next-scene)
              ;; Check if scene heading is already numbered and if there
              ;; is a NEXT-SCENE. No previousscene number can be greater
              ;; or equal to this.
              (goto-char x)
              (while (not (or next-scene (eobp)))
                (fountain-forward-scene 1)
                (when (fountain-match-scene-heading)
                  (setq next-scene (fountain-scene-number-to-list
                                    (match-string-no-properties 9)))))
              (cond
               ;; If there's both a NEXT-SCENE and CURRENT-SCENE, but
               ;; NEXT-SCENE is less or equal to CURRENT-SCENE, scene
               ;; numbers are out of order.
               ((and current-scene next-scene
                     (version-list-<= next-scene current-scene))
                (user-error
                 err-order (fountain-scene-number-to-string current-scene)))
               ;; Otherwise, if there is a CURRENT-SCENE and either no
               ;; NEXT-SCENE or there is and it's greater then
               ;; CURRENT-SCENE, just return CURRENT-SCENE.
               (current-scene)
               (t
        ;; There is no CURRENT-SCENE yet, so go to the first scene heading and
        ;; if it's already numberd set it to that, or just (list 1).
        (goto-char (point-min))
        (unless (fountain-match-scene-heading)
          (fountain-forward-scene 1))
        (when (<= (point) x)
          (setq current-scene
                (or (fountain-scene-number-to-list
                     (match-string-no-properties 9))
                    (list 1))))
        ;; While before point X, go forward through each scene heading, setting
        ;; LAST-SCENE to CURRENT-SCENE and CURRENT-SCENE to an incement of (car
        ;; LAST-SCENE).
        (while (< (point) x (point-max))
          (fountain-forward-scene 1)
          (when (fountain-match-scene-heading)
            (setq last-scene current-scene
                  current-scene (or (fountain-scene-number-to-list
                                     (match-string-no-properties 9))
                                    (list (1+ (car last-scene)))))
            ;; However, this might make CURRENT-SCENE greater or equal to
            ;; NEXT-SCENE (a problem), so if there is a NEXT-SCENE, and
            ;; NEXT-SCENE is less or equal to CURRENT-SCENE:
            ;;
            ;; 1. pop (car LAST-SCENE), which should always be less than
            ;;    NEXT-SCENE as N
            ;; 2. set CURRENT-SCENE to (list TMP-SCENE (1+ N))
            ;; 3. set TMP-SCENE to (list TMP-SCENE n)
            ;;
            ;; Loop through this so that the last (or only) element of
            ;; CURRENT-SCENE is incremented by 1, and TMP-SCENE is appended with
            ;; N or 1. e.g.
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
                  (user-error
                   err-order (fountain-scene-number-to-string current-scene)))))))
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
                (cl-incf current-scene)))
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
          (when (match-string-no-properties 9)
            (delete-region (match-beginning 7) (match-end 10)))
          (fountain-forward-scene 1))))))

(defun fountain-add-scene-numbers ()
  "Add scene numbers to scene headings in current buffer.

Adding scene numbers to scene headings after numbering existing
scene headings will use a prefix or suffix letter, depending on
the value of `fountain-scene-numbers-prefix-revised':

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
          (unless (match-string-no-properties 9)
            (end-of-line)
            (delete-horizontal-space t)
            (insert "\s#" (fountain-scene-number-to-string
                           (fountain-get-scene-number)) "#"))
          (fountain-forward-scene 1)
          (progress-reporter-update job))
        (progress-reporter-done job)))))


;;; Pages

(defgroup fountain-pages ()
  "Options for calculating page length."
  :group 'fountain
  :prefix "fountain-page-")

(define-obsolete-variable-alias 'fountain-export-page-size
  'fountain-page-size "3.0.0")
(defcustom fountain-page-size
  'letter
  "Paper size to use on export."
  :group 'fountain-pages
  :type '(radio (const :tag "US Letter" letter)
                (const :tag "A4" a4)))

(define-obsolete-variable-alias 'fountain-pages-max-lines
  'fountain-page-max-lines "3.0.0")
(defcustom fountain-page-max-lines
  '((letter . 55) (a4 . 60))
  "Integer representing maximum number of lines on a page.

n.b. If you change this option after locking pages in a script,
you may get incorrect output."
  :group 'fountain-pages
  :type '(choice integer
                 (list (cons (const :tag "US Letter" letter) integer)
                       (cons (const :tag "A4" a4) integer))))

(define-obsolete-variable-alias 'fountain-pages-ignore-narrowing
  'fountain-page-ignore-restriction "3.0.0")
(defcustom fountain-page-ignore-restriction
  nil
  "Non-nil if counting pages should ignore buffer narrowing."
  :group 'fountain-pages
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-page-size
  'letter
  "Paper size to use on export."
  :group 'fountain-pages
  :type '(radio (const :tag "US Letter" letter)
                (const :tag "A4" a4)))

(defvar fountain-printed-elements
  '(scene-heading action character dialog paren trans center
character-dd dialog-dd paren-dd lines lines-dd)
  "List of elements considered as printed.
i.e. Only these elements count towards page length.")

(defun fountain-goto-page-break-point ()
  "Move point to appropriate place to break a page.
This is usually before point, but may be after if only skipping
over whitespace.

Comments are assumed to be deleted."
  (when (looking-at fountain-more-dialog-string) (forward-line))
  (when (looking-at "[\n\s\t]*\n") (goto-char (match-end 0)))
  (let ((element (fountain-get-element)))
    (cond
     ;; If element is not included in export, we can safely break
     ;; before.
     ((not (memq element fountain-printed-elements))
      (beginning-of-line))
     ;; We cannot break page in dual dialogue. If we're at right dual
     ;; dialogue, skip back to previous character.
     ((and (memq element '(character-dd lines-dd paren-dd))
           (eq (fountain-read-dual-dialog) 'right))
      (fountain-forward-character 0)
      (fountain-forward-character -1))
     ;; If we're at left dual dialogue, break at character.
     ((memq element '(character-dd lines-dd paren-dd))
      (fountain-forward-character 0))
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
     ;; If we're at dialogue, skip over spaces then go to the beginning
     ;; of the current sentence. If previous line is a character or
     ;; parenthetical, call recursively on that element. Otherwise,
     ;; break page here.
     ((eq element 'lines)
      (skip-chars-forward "\s\t")
      (unless (or (bolp)
                  (looking-back (sentence-end) nil))
        (forward-sentence -1))
      (let ((x (point)))
        (backward-char)
        (if (or (fountain-match-character)
                (fountain-match-paren))
            (progn
              (beginning-of-line)
              (fountain-goto-page-break-point))
          (goto-char x))))
     ;; If we're at a transition or center text, skip backwards to
     ;; previous element and call recursively on that element.
     ((memq element '(trans center))
      (skip-chars-backward "\n\s\t")
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
        (skip-chars-backward "\n\s\t")
        (beginning-of-line)
        (if (fountain-match-scene-heading)
            (fountain-goto-page-break-point)
          (goto-char x)))))))

(defun fountain-move-to-fill-width (element)
  "Move point to column of ELEMENT fill limit suitable for breaking line.
Skip over comments."
  (let ((fill-width
         (intern-soft (concat "fountain-fill-" (symbol-name element)))))
    (let ((i 0))
      (while (and (< i fill-width) (not (eolp)))
        (cond ((= (syntax-class (syntax-after (point))) 0)
               (forward-char 1) (cl-incf i))
              ((forward-comment 1))
              (t
               (forward-char 1) (cl-incf i)))))
    (skip-chars-forward "\s\t")
    (when (eolp) (forward-line))
    (unless (bolp) (fill-move-to-break-point (line-beginning-position)))))

(defun fountain-forward-page ()
  "Move point forward by an approximately page.

Moves forward from point, which is unlikely to correspond to
final exported pages and so should not be used interactively."
  (let ((skip-whitespace-fun
         (lambda ()
           (when (looking-at "[\n\s\t]*\n")
             (goto-char (match-end 0))))))
    (while (fountain-match-metadata)
      (forward-line 1))
    ;; Pages don't begin with blank space, so skip over any at point.
    (funcall skip-whitespace-fun)
    ;; If we're at a page break, move to its end and skip over
    ;; whitespace.
    (when (fountain-match-page-break)
      (end-of-line)
      (funcall skip-whitespace-fun))
    ;; Start counting lines.
    (let ((page-lines
           (cdr (assq fountain-page-size fountain-page-max-lines)))
          (line-count 0)
          (line-count-left 0)
          (line-count-right 0))
      ;; Begin the main loop, which only halts if we reach the end
      ;; of buffer, a forced page break, or after the maximum lines
      ;; in a page.
      (while (and (< line-count page-lines)
                  (not (eobp))
                  (not (fountain-match-page-break)))
        (cond
         ;; If we're at the end of a line (but not also the
         ;; beginning, i.e. not a blank line) then move forward a
         ;; line and increment line-count.
         ((and (eolp) (not (bolp)))
          (forward-line)
          (cl-incf line-count))
         ;; If we're looking at newline, skip over it and any
         ;; whitespace and increment line-count.
         ((funcall skip-whitespace-fun)
          (cl-incf line-count))
         ;; We are at an element. Find what kind of element. If it is
         ;; not included in export, skip over without incrementing
         ;; line-count. Otherwise move to fill-width and increment
         ;; appropriate line-count: for dual-dialogue, increment either
         ;; LINE-COUNT-LEFT/RIGHT, otherwise increment LINE-COUNT. Once
         ;; we're at a blank line, add the greater of the former two to
         ;; the latter.
         ;; FIXME: using block-bounds here could benefit.
         (t
        (let ((element (fountain-get-element))
              (dd (fountain-read-dual-dialog)))
          (if (memq element fountain-printed-elements)
              (progn
                (fountain-move-to-fill-width element)
                (cond ((eq dd 'left)
                       (cl-incf line-count-left))
                      ((eq dd 'right)
                       (cl-incf line-count-right))
                      (t
                       (cl-incf line-count)))
                (when (and (eolp) (bolp)
                           (< 0 line-count-left) (< 0 line-count-right))
                  (setq line-count
                        (+ line-count (max line-count-left line-count-right)))))
            ;; Element is not exported, so skip it without
            ;; incrementing line-count.
            (end-of-line)
            (funcall skip-whitespace-fun)))))))
    ;; We are not at the furthest point in a page. Skip over any
    ;; remaining whitespace, then go back to page-break point.
    (fountain-goto-page-break-point)))

(defun fountain-insert-page-break (&optional ask page-num)
  "Insert a page break at appropriate place preceding point.
When optional argument ASK is non-nil (if prefixed with
\\[universal-argument] when called interactively), prompt for PAGE-NUM
as a string to force the page number."
  (interactive "P")
  (when ask
    (setq page-num (read-string "Page number (RET for none): ")))
  ;; Save a marker where we are.
  (let ((x (point-marker))
        (page-break
         (concat "===" (when (and (stringp page-num)
                                  (< 0 (string-width page-num)))
                         (concat "\s" page-num "\s==="))))
        element)
    ;; Move point to appropriate place to break page.
    (fountain-goto-page-break-point)
    (setq element (fountain-get-element))
    ;; At this point, element can only be: section-heading,
    ;; scene-heading, character, action, paren or lines. Only paren and
    ;; lines require special treatment.
    (if (memq element '(lines paren))
        (let ((name (fountain-get-character -1)))
          (delete-horizontal-space)
          (unless (bolp) (insert-before-markers "\n"))
          (insert-before-markers
           (concat fountain-more-dialog-string "\n\n"
                   page-break "\n\n"
                   name "\s" fountain-continued-dialog-string "\n")))
      ;; Otherwise, insert the page break where we are. If the preceding
      ;; element is a page break, only replace the page number,
      ;; otherwise, insert the page break.
      (if (save-excursion
            (save-restriction
              (widen)
              (skip-chars-backward "\n\s\t")
              (fountain-match-page-break)))
          (replace-match page-break t t)
        (delete-horizontal-space)
        (unless (bolp) (insert-before-markers "\n"))
        (unless (fountain-blank-before-p) (insert-before-markers "\n"))
        (insert-before-markers page-break "\n\n")))
    ;; Return to where we were.
    (goto-char x)
    (set-marker x nil)))

(defun fountain-get-page-count ()
  "Return a cons of the current page number and the total pages."
  (let ((x (point))
        (total 0)
        (current 0)
        found)
    (save-excursion
      (save-restriction
        (when fountain-page-ignore-restriction (widen))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (fountain-forward-page)
          (cl-incf total)
          (when (and (not found) (< x (point)))
            (setq current total found t)))
        (cons current total)))))

(defun fountain-count-pages ()
  "Message the current page of total pages in current buffer.
n.b. This is an approximate calculation."
  (interactive)
  (let ((pages (fountain-get-page-count)))
    (message "Page %d of %d" (car pages) (cdr pages))))

(defun fountain-paginate-buffer ()
  "Add forced page breaks to buffer.

Move through buffer with `fountain-forward-page' and call
`fountain-insert-page-break'."
  (interactive)
  (let ((job (make-progress-reporter "Paginating...")))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((page 1))
          (fountain-forward-page)
          (while (< (point) (point-max))
            (cl-incf page)
            (fountain-insert-page-break nil (number-to-string page))
            (fountain-forward-page)
            (progress-reporter-update job))
          (progress-reporter-done job))))))


;;; Filling

(defgroup fountain-fill ()
  "Options for filling elements.

Filling elements is used in exporting to plaintext and
PostScript, and in calculating page length for page locking."
  :prefix "fountain-fill-"
  :group 'fountain)

(defcustom fountain-fill-section-heading
  '(0 . 61)
  "Cons cell of integers for indenting and filling section headings.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-scene-heading
  '(0 . 61)
  "Cons cell of integers for indenting and filling scene headings.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-action
  '(0 . 61)
  "Cons cell of integers for indenting and filling action.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-character
  '(20 . 38)
  "Cons cell of integers for indenting and filling character.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-dual-character
  '(10 . 28)
  "Cons cell of integers for indenting and filling dual-dialogue character.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-paren
  '(15 . 26)
  "Cons cell of integers for indenting and filling parenthetical.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-dual-paren
  '(5 . 16)
  "Cons cell of integers for indenting and filling dual-dialogue parenthetical.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-dialog
  '(10 . 35)
  "Cons cell of integers for indenting and filling dialogue.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-dual-dialog
  '(2 . 28)
  "Cons cell of integers for indenting and filling dual-dialogue.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-trans
  '(42 . 16)
  "Cons cell of integers for indenting and filling transition.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-synopsis
  '(0 . 61)
  "Cons cell of integers for indenting and filling synopses.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))

(defcustom fountain-fill-note
  '(0 . 61)
  "Cons cell of integers for indenting and filling notes.
The car sets `left-margin' and cdr `fill-column'."
  :group 'fountain-fill
  :type '(cons (integer :tag "Indent")
               (integer :tag "Width")))


;;; Exporting

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

(defcustom fountain-export-command-profiles
  '(("afterwriting-usletterpdf-doublespace" . "afterwriting \
--source %b --pdf %B.pdf --overwrite \
--setting double_space_between_scenes=true \
--setting print_profile=usletter")
    ("afterwriting-a4pdf-doublespace" . "afterwriting \
--source %b --pdf %B.pdf --overwrite \
--setting double_space_between_scenes=true \
--setting print_profile=a4")
    ("wrap-usletterpdf-cprime" . "wrap \
pdf %b --use-courier-prime --out %B.pdf")
    ("textplay-fdx" . "textplay --fdx < %b > %B.fdx"))
  "Shell command profiles for exporting Fountain files.

Each profile is a cons-cell of PROFILE-NAME string and the
COMMAND string.

COMMAND is passed to `format-spec' and allows for interpolation
of the following values:

  %b is the `buffer-file-name'
  %B is the `buffer-file-name' sans extension
  %n is the `user-full-name'
  %t is the title (from script metadata)
  %a is the author (from script metadata)
  %F is the current date in ISO format
  %x is the current date in your locale's \"preferred\" format

If a command does not include %b, `fountain-export' will pass the
buffer or active region to the command via stdin.

If a command outputs to stdout, this will be redirected to
`fountain-export-output-buffer'.

The first profile is considered default."
  :type '(repeat (cons (string :tag "Name")
                       (string :tag "Shell command")))
  :group 'fountain-export)

(defcustom fountain-export-output-buffer
  "*Fountain Export*"
  "Buffer name for `fountain-export' output."
  :type 'string
  :safe 'string
  :group 'fountain-export)

(require 'format-spec)

(defun fountain-export-command (profile-name &optional edit-command)
  "Call export shell command for PROFILE-NAME.

If EDIT-COMMAND is non-nil (when prefixed with \\[universal-argument]) allow
interactive editing of the command before running it.

Export command profiles are defined in
`fountain-export-command-profiles'."
  (interactive
   (list (let ((default (caar fountain-export-command-profiles)))
           (completing-read
            (format "Export profile [default %s]: " default)
            (mapcar #'car fountain-export-command-profiles)
            nil t nil nil default))
         current-prefix-arg))
  (unless profile-name
    (user-error "No `fountain-export-command-profiles' found"))
  (if (buffer-live-p (get-buffer fountain-export-output-buffer))
      (kill-buffer fountain-export-output-buffer))
  (let ((command
         (cdr (assoc-string profile-name fountain-export-command-profiles)))
        (infile
         (if buffer-file-name (file-name-nondirectory (buffer-file-name))))
        (infile-base
         (if buffer-file-name (file-name-base (buffer-file-name))))
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end   (if (use-region-p) (region-end) (point-max)))
        (metadata (fountain-read-metadata))
        title author use-stdin)
    (setq title  (cdr (assq 'title metadata))
          author (cdr (assq 'author metadata)))
    (unless (let (case-fold-search) (string-match "%b" command))
      (setq use-stdin t))
    (setq command (format-spec command
        (format-spec-make
         ?b (shell-quote-argument (or infile ""))
         ?B (shell-quote-argument (or infile-base ""))
         ?t (shell-quote-argument (or title ""))
         ?a (shell-quote-argument (or author ""))
         ?F (shell-quote-argument (format-time-string "%F"))
         ?x (shell-quote-argument (format-time-string "%x")))))
    (when edit-command
      (setq command (read-shell-command "Shell command: " command)))
    (unwind-protect
        (if use-stdin
            (shell-command-on-region start end command
                                     fountain-export-output-buffer)
          (shell-command command fountain-export-output-buffer))
      (with-current-buffer fountain-export-output-buffer
        (if (< 0 (string-width (buffer-string)))
            (set-auto-mode t)
          (kill-buffer))))))

(require 'dired-x)

(defun fountain-export-view ()
  "Attempt to open the last exported output file.

This works by finding the most recently modified file in the
current directory matching the current file base-name (excluding
the current file).

The file is then passed to `dired-guess-default'."
  (interactive)
  (let ((file-list
         (directory-files-and-attributes
          default-directory nil (file-name-base (buffer-file-name)) t))
        file)
    (setq file-list
          (seq-remove
           (lambda (f)
             (string-match (file-name-nondirectory (buffer-file-name))
                           (car f)))
           file-list))
    (unless file-list
      (user-error "Could not find export file for %S"
                  (file-name-nondirectory (buffer-file-name))))
    (setq file-list
          (seq-sort (lambda (a b) (time-less-p (nth 5 b) (nth 5 a)))
                    file-list))
    (setq file (caar file-list))
    (unless (file-exists-p file)
      (user-error "File %S does not exist" file))
    (call-process (dired-guess-default (list file))
                  nil nil nil file)))


;;; Font Lock

(defun fountain--get-font-lock-decoration ()
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
    (read-char-choice "Maximum decoration (1-3): " '(?1 ?2 ?3)))))))
  (when (and (integerp n)
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
      (font-lock-refresh-defaults))))

(defun fountain--get-section-heading-face ()
  (save-excursion
    (beginning-of-line)
    (looking-at outline-regexp)
    (intern-soft (format "fountain-section-heading-%s"
                         (funcall outline-level)))))

(defmacro define-fountain-font-lock-matcher (fn)
  (let ((fn-name (intern (format "%s-font-lock" fn)))
        (docstring (format "\
Call `%s' on each line before LIMIT.
Return non-nil if match occurs." fn)))
    `(defun ,fn-name (limit)
       ,docstring
       (let (match)
         (while (and (null match)
                     (< (point) limit))
           (when (,fn) (setq match t))
           (forward-line))
         match))))

(defun fountain-toggle-hide-emphasis-markup ()
  (interactive)
  (customize-set-variable 'fountain-hide-emphasis-markup
                          (not fountain-hide-emphasis-markup))
  (message "Emphasis markup is now %s"
           (if fountain-hide-emphasis-markup "invisible" "visible")))

(defun fountain-toggle-hide-element-markup ()
  (interactive)
  (customize-set-variable 'fountain-hide-element-markup
                          (not fountain-hide-element-markup))
  (message "Element markup is now %s"
           (if fountain-hide-element-markup "invisible" "visible")))

(defvar fountain--font-lock-keywords
  '((section-heading
     (quote eval)
     (2 list fountain-section-heading-regexp
        0 '(fountain--get-section-heading-face)))

    (section-heading
     fountain-section-heading-regexp
     (2 1 nil nil nil fountain-syntax-chars)
     (2 2 fountain-non-printing prepend))

    (scene-heading
     (define-fountain-font-lock-matcher fountain-match-scene-heading)
     (2 0 fountain-scene-heading)
     (2 8 fountain-non-printing prepend t fountain-syntax-chars)
     (2 10 fountain-non-printing prepend t fountain-syntax-chars)
     (3 1 fountain-non-printing prepend t fountain-syntax-chars))

    (action
     (define-fountain-font-lock-matcher fountain-match-action)
     (1 0 fountain-action)
     (3 1 fountain-non-printing t t fountain-syntax-chars))

    (character
     (define-fountain-font-lock-matcher fountain-match-character)
     (3 0 fountain-character)
     (3 1 fountain-non-printing t t fountain-syntax-chars)
     (3 5 highlight prepend t))

    (dialog
     (define-fountain-font-lock-matcher fountain-match-dialog)
     (3 0 fountain-dialog))

    (paren
     (define-fountain-font-lock-matcher fountain-match-paren)
     (3 0 fountain-paren))

    (trans
     (define-fountain-font-lock-matcher fountain-match-trans)
     (3 0 fountain-trans)
     (2 1 fountain-non-printing t t fountain-syntax-chars))

    (synopsis
     (define-fountain-font-lock-matcher fountain-match-synopsis)
     (2 0 fountain-synopsis)
     (2 1 nil nil nil fountain-syntax-chars)
     (2 2 fountain-non-printing prepend))

    (note
     (define-fountain-font-lock-matcher fountain-match-note)
     (2 0 fountain-note))

    (metadata
     (define-fountain-font-lock-matcher fountain-match-metadata)
     (3 0 fountain-metadata-key nil t)
     (2 2 fountain-metadata-value t t))

    (center
     fountain-center-regexp
     (2 1 fountain-non-printing t nil fountain-syntax-chars)
     (2 3 fountain-non-printing t nil fountain-syntax-chars))

    (page-break
     fountain-page-break-regexp
     (2 0 fountain-page-break)
     (2 2 fountain-page-number t t))

    (underline
     fountain-underline-regexp
     (3 2 fountain-non-printing prepend nil fountain-emphasis-delim)
     (1 1 underline prepend)
     (3 4 fountain-non-printing prepend nil fountain-emphasis-delim))

    (italic
     fountain-italic-regexp
     (3 2 fountain-non-printing prepend nil fountain-emphasis-delim)
     (1 1 italic prepend)
     (3 4 fountain-non-printing prepend nil fountain-emphasis-delim))

    (bold
     fountain-bold-regexp
     (3 2 fountain-non-printing prepend nil fountain-emphasis-delim)
     (1 1 bold prepend)
     (3 4 fountain-non-printing prepend nil fountain-emphasis-delim))

    (bold-italic
     fountain-bold-italic-regexp
     (3 2 fountain-non-printing prepend nil fountain-emphasis-delim)
     (1 1 bold-italic prepend)
     (3 4 fountain-non-printing prepend nil fountain-emphasis-delim))

    (lyrics
     fountain-lyrics-regexp
     (3 1 fountain-non-printing prepend nil fountain-emphasis-delim)
     (2 2 italic prepend)))
  "Association list of properties for generating `font-lock-keywords'.")

(defun fountain-init-font-lock ()
  "Return a new list of `font-lock-keywords' for elements."
  (let ((dec (fountain--get-font-lock-decoration))
        keywords)

    (dolist (element fountain--font-lock-keywords)
      (let ((matcher (eval (cadr element)))
            (subexp-hl (cddr element))
            highlight align-col use-form)
        (when (eq matcher 'eval) (setq use-form t))
        (setq align-col
              (eval (intern-soft (format "fountain-align-%s" (car element)))))
        (when (and align-col fountain-align-elements)
          (unless (integerp align-col)
            (setq align-col
        (cdr (or (assoc-string
                  (or (cdr (assq 'format (fountain-read-metadata)))
                      fountain-default-script-format)
                  align-col)
                 (car align-col))))))

        (dolist (match-hl subexp-hl)
          (if (and use-form (<= (car match-hl) dec))
              (setq highlight (cdr match-hl))
            (let ((subexp (nth 1 match-hl))
                  (face
                   (when (<= (nth 0 match-hl) dec)
                     (nth 2 match-hl)))
                  (align
                   (when (integerp align-col)
                     `(line-prefix (space :align-to ,align-col)
                       wrap-prefix (space :align-to ,align-col))))
                  (invisible
                   (when (nth 5 match-hl)
                     `(invisible ,(nth 5 match-hl))))
                  (override (nth 3 match-hl))
                  (laxmatch (nth 4 match-hl)))
              (push (list subexp `(quote (face ,face ,@align ,@invisible))
                          override laxmatch)
                    highlight))))

        (push (cons matcher (if use-form highlight (reverse highlight)))
              keywords)))
    (reverse keywords)))

;; FIXME: make scene numbers display in both margins, like a real script.
(defun fountain-redisplay-scene-numbers (start end)
  "Apply display text properties to scene numbers between START and END.

If `fountain-scene-numbers-display-in-margin' is non-nil and
scene heading has scene number, apply display text properties to
redisplay in margin. Otherwise, remove display text properties."
  ;; FIXME: Why use jit-lock rather than font-lock?
  (goto-char start)
  (while (< (point) (min end (point-max)))
    (when (fountain-match-scene-heading)
      (if (and fountain-scene-numbers-display-in-margin
               (match-string-no-properties 9))
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
    (define-key map (kbd "C-c C-x *") #'fountain-toggle-hide-emphasis-markup)
    (define-key map (kbd "C-c C-x !") #'fountain-toggle-hide-element-markup)
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
    (define-key map (kbd "M-RET") #'fountain-insert-section-heading)
    (define-key map (kbd "C-c C-x b") #'fountain-outline-to-indirect-buffer)
    ;; Pages
    (define-key map (kbd "C-c C-x p") #'fountain-count-pages)
    ;; Exporting commands:
    (define-key map (kbd "C-c C-e") #'fountain-export-command)
    (define-key map (kbd "C-c C-v") #'fountain-export-view)
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
     ["Next Character" fountain-forward-character]
     ["Previous Character" fountain-backward-character]
     "---"
     ["Go to Scene Heading..." fountain-goto-scene]
     ["Go to Page..." fountain-goto-page]
     "---"
     ["Cycle Outline Visibility" fountain-outline-cycle]
     ["Cycle Global Outline Visibility" fountain-outline-cycle-global]
     ["Show All" fountain-outline-show-all]
     "---"
     ["Fold Notes When Cycling"
      (customize-set-variable 'fountain-outline-fold-notes
                              (not fountain-outline-fold-notes))
      :style toggle
      :selected fountain-outline-fold-notes])
    ("Edit Structure"
     ["Insert Section Heading" fountain-insert-section-heading]
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
      (customize-set-variable 'fountain-scene-numbers-display-in-margin
                              (not fountain-scene-numbers-display-in-margin))
      :style toggle
      :selected fountain-scene-numbers-display-in-margin])
    "---"
    ["Do What I Mean" fountain-dwim]
    ["Upcase Line" fountain-upcase-line]
    ["Upcase Line and Newline" fountain-upcase-line-and-newline]
    "---"
    ["Insert Metadata..." auto-insert]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    ["Count Pages" fountain-count-pages]
    ["Insert Page Break..." fountain-insert-page-break]
    ["Refresh Continued Dialog" fountain-continued-dialog-refresh]
    ["Update Auto-Completion" fountain-completion-update]
    "---"
    ("Syntax Highlighting"
     ["Minimum"
      (fountain-set-font-lock-decoration 1)
      :style radio
      :selected (= (fountain--get-font-lock-decoration) 1)]
     ["Default"
      (fountain-set-font-lock-decoration 2)
      :style radio
      :selected (= (fountain--get-font-lock-decoration) 2)]
     ["Maximum"
      (fountain-set-font-lock-decoration 3)
      :style radio
      :selected (= (fountain--get-font-lock-decoration) 3)]
     "---"
     ["Hide Emphasis Markup" fountain-toggle-hide-emphasis-markup
      :style toggle
      :selected fountain-hide-emphasis-markup]
     ["Hide Element Markup" fountain-toggle-hide-element-markup
      :style toggle
      :selected fountain-hide-element-markup])
    "---"
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
    ["Run Export Command..." fountain-export-command]
    ["View Last Exported File" fountain-export-view]
    "---"
    ["Save Options" fountain-save-options]
    ["Customize Mode" (customize-group 'fountain)]
    ["Customize Faces" (customize-group 'fountain-faces)]))

(defun fountain-save-options ()
  "Save `fountain-mode' menu options with `customize'."
  (interactive)
  (let (unsaved)
    (dolist (option '(fountain-align-elements
                      fountain-auto-upcase-scene-headings
                      fountain-add-continued-dialog
                      fountain-scene-numbers-display-in-margin
                      fountain-hide-emphasis-markup
                      fountain-hide-element-markup
                      fountain-shift-all-elements
                      fountain-outline-fold-notes
                      font-lock-maximum-decoration
                      fountain-page-size))
      (when (customize-mark-to-save option) (setq unsaved t)))
    (when unsaved (custom-save-all))))


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
  (declare-function fountain-outline-invisible-p "fountain-mode")
  (unless (or (advice-member-p 'fountain-outline-invisible-p 'outline-invisible-p)
              (<= 26 emacs-major-version))

    (defun fountain-outline-invisible-p (&optional pos)
      "Non-nil if the character after POS has outline invisible property.
If POS is nil, use `point' instead."
      (eq (get-char-property (or pos (point)) 'invisible) 'outline))

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
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:

;;; fountain-mode.el ends here
