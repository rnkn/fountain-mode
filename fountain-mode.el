;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 1.3.3
;; Package-Requires: ((s "1.9.0"))
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

;; Fountain Mode aims to be a full-featured screenwriting environment for
;; GNU Emacs using the Fountain markup format. For more information on the
;; Fountain markup format, visit <http://fountain.io>.

;; Features
;; --------

;; - support for the Fountain 1.1 specification (except scene numbers and
;;   dual dialog)
;; - export to HTML and PDF (requires [Prince][])
;; - include or omit a title page
;; - multiple levels of syntax highlighting for all elements (see below)
;; - auto-indentation for a kind of WYSIWYG (display only, does not modify
;;   file contents)
;; - add/remove automatic (CONT'D) to successively speaking characters
;; - automatic (MORE) and (CONT'D) when breaking dialog across pages in PDF
;;   output
;; - `occur` navigator for section headings, synopses, notes and scene
;;   headings
;; - templates for inserting synopses, notes and metadata
;; - navigate by scene heading
;; - support for emphasis (bold, italic, underlined text)
;; - toggle visibility of emphasis delimiters and syntax characters
;; - support for both official and legacy commenting (boneyard) syntax
;; - everything is customizable, of course

;; The following features are not *yet* supported:

;; - scene numbers
;; - dual dialog

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type `C-h m`. Bugs and feature requests are
;; encouraged on the [Issues][] page, or you can email me directly (email
;; in the source code header). The overlap of Emacs users and screenwriters
;; is rather small, so any feature request or bug fix will usually be
;; implemented quickly.

;; See the [Wiki][] for ways to extend Fountain Mode.

;; [prince]: http://www.princexml.com "Prince"
;; [issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"
;; [wiki]: https://github.com/rnkn/fountain-mode/wiki "Fountain Mode wiki"

;; Requirements
;; ------------

;; - Emacs 24.1 (not tested on earlier versions, only tested on Mac OS X
;;   and Linux, not tested on Windows).
;; - [s.el][], the long lost Emacs string manipulation library.
;; - Exporting to PDF requires [Prince][], which is free for personal use.
;;   Prince adds a removable PDF annotation on the first page; if you don't
;;   like it, delete the annotation in a PDF application that supports
;;   editing annotations, or open the PDF and print to PDF, which will
;;   remove all annotations.
;; - To insert UUIDs (useful for using notes as linked bookmarks) you'll
;;   need either `uuidgen` CLT (usually preinstalled on OS X and Linux) or
;;   [uuid.el][] Emacs package.

;; [s.el]: https://github.com/magnars/s.el "s.el"
;; [uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

;; Installation
;; ------------

;; *For users on OS X with no experience with Emacs, see the
;; [Absolute Beginner's Guide (OS X)][].*

;; Fountain Mode is available through [MELPA][] and [MELPA-stable][]. I
;; encourage installing the stable version.

;; Alternately, download the [latest release][], move the files into your
;; `load-path` and add the following line to your `.emacs` or `init.el`
;; file:

;;     (require 'fountain-mode)

;; To load Fountain Mode whenever you open a `.fountain` file, also add the
;; following:

;;     (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

;; [beginners guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
;; [melpa]: http://melpa.milkbox.net "MELPA"
;; [melpa-stable]: http://melpa-stable.milkbox.net "MELPA"
;; [latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

;; Syntax Highlighting
;; -------------------

;; To change the level of syntax highlighting, customize the value of
;; `font-lock-maximum-decoration`. This can be set indirectly with the
;; menu, or with `M-x fountain-set-font-lock-decoration` and saved with
;; `M-x fountain-save-font-lock-decoration`.

;; History
;; -------

;; See [Releases][].

;; [releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"

;;; Code:

(defconst fountain-version
  "1.3.3")

;;; Required ===================================================================

(require 's)
(require 'thingatpt)
(require 'easymenu)

;; Groups ======================================================================

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "https://github.com/rnkn/fountain-mode"))

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

(defgroup fountain-faces ()
  "Faces used in `fountain-mode'.
There are three levels of Font Lock decoration:

    1. minimum: only highlights comments and syntax characters

    2. default: highlights comments, metadata, scene headings,
       sections, synopses, notes and syntax characters

    3. maximum: highlights comments, metadata keys, metadata
       values, scene headings, sections, synopses, notes,
       character names, parentheticals, dialog, transitions,
       center text and syntax characters

To switch between these levels of Font Lock decoration, customize
the value of `font-lock-maximum-decoration'. This can be set
indirectly with \\[fountain-set-font-lock-decoration] and saved
with \\[fountain-save-font-lock-decoration]."
  :prefix "fountain-"
  :link '(info-link "(emacs)Font Lock")
  :group 'fountain)

;;; Obsolete Aliases ===========================================================

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

(define-obsolete-variable-alias 'fountain-export-title-page-template
  'fountain-export-title-page-title-template "1.1.0")

(define-obsolete-variable-alias 'fountain-hide-escapes
  'fountain-hide-syntax-chars "1.3.0")

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

;;; Customization ==============================================================

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

    INT./EXT. HOUSE - DAY

Call `fountain-mode' again for changes to take effect."
  :type '(repeat (string :tag "Prefix"))
  :group 'fountain)

(defcustom fountain-trans-list
  '("TO:" "WITH:" "FADE OUT" "TO BLACK")
  "List of transition endings (case insensitive).
This list is used to match the endings of transitions,
e.g. \"TO:\" will match both the following:

    CUT TO:

    DISSOLVE TO:

Call `fountain-mode' again for changes to take effect."
  :type '(repeat (string :tag "Transition"))
  :group 'fountain)

(defcustom fountain-add-continued-dialog
  t
  "\\<fountain-mode-map>If non-nil, add continued dialog appropriately with \\[fountain-continued-dialog-refresh].
When same character speaks in succession, append
`fountain-continued-dialog-string'."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-continued-dialog-string
  "CONT'D"
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

(defcustom fountain-align-character
  20
  "Column integer to which characters names should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-dialog
  10
  "Column integer to which dialog should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-paren
  15
  "Column integer to which parentheticals should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-trans
  45
  "Column integer to which transitions should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-center
  10
  "Column integer to which centered text should be aligned.
This option does not affect file contents."
  :type 'integer
  :group 'fountain)

(defcustom fountain-align-elements t
  "If non-nil, elements will be displayed auto-aligned.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain)

(defcustom fountain-switch-comment-syntax
  nil
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

;;; Export Customization =======================================================

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-pdf-via-html
  "\\<fountain-mode-map>Default function to call with \\[fountain-export-default]."
  :type '(radio (function-item fountain-export-buffer-to-pdf-via-html)
                (function-item fountain-export-buffer-to-html))
  :group 'fountain-export)

(defcustom fountain-export-include-title-page
  t
  "Generate a title page on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-inline-style
  t
  "If non-nil, use inline stylesheet.
Otherwise, use an external stylesheet file."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-page-size
  "us-letter"
  "Paper size to use on export."
  :type '(radio (const :tag "US Letter" "us-letter")
                (const :tag "A4" "a4"))
  :group 'fountain-export)

(defcustom fountain-export-font
  '("Courier" "Courier New" "monospace")
  "List of font names to use when exporting, by priority."
  :type '(repeat (string :tag "Font"))
  :group 'fountain-export)

(defcustom fountain-export-bold-scene-headings
  nil
  "If non-nil, bold scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-underline-scene-headings
  nil
  "If non-nil, underline scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-bold-title
  nil
  "If non-nil, bold title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-underline-title
  t
  "If non-nil, underline title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-upcase-title
  t
  "If non-nil, underline title on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-double-space-scene-headings
  nil
  "If non-nil, double space before scene headings on export."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-action-orphans
  2
  "Number of allowable action orphan lines.
When breaking action across pages, this integer is the minimum
number of lines on the previous page."
  :type 'integer
  :group 'fountain-export)

(defcustom fountain-export-action-widows
  2
  "Number of allowable action widow lines.
When breaking action across pages, this integer is the minimum
number of lines on the following page."
  :type 'integer
  :group 'fountain-export)

(defcustom fountain-export-dialog-orphans
  2
  "Number of allowable dialog orphan lines.
When breaking dialog across pages, this integer is the minimum
number of lines on the previous page."
  :type 'integer
  :group 'fountain-export)

(defcustom fountain-export-dialog-widows
  2
  "Number of allowable dialog widow lines.
When breaking dialog across pages, this integer is the minimum
number of lines on the following page."
  :type 'integer
  :group 'fountain-export)

(defcustom fountain-export-more-dialog-string
  "(MORE)"
  "String to append to dialog when breaking across pages.
Parentheses are not automatically added."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-preserve-line-breaks
  t
  "If non-nil, convert all newlines into line breaks.
Otherwise, only break paragraphs at explicit line breaks (one or
more blank lines)."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-convert-quotes
  nil
  "If non-nil, replace TeX-style quotes with \"smart-quotes\".

    \`\`foobar\'\'

will be exported as

    &ldquo;foobar&rdquo;"
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-pdf-via-html-command
  "prince %s --verbose"
  "Shell command string to convert HTML file to PDF."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-title-template
  "${title}
${credit}
${author}"
  "Template for creating title page title block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-left-template
  "${draft}
${date}
${notes}"
  "Template for creating title page left block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-page-right-template
  "${contact}"
  "Template for creating title page right block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-style-template
"@page {
    size: ${page-size};
    margin-top: 1in;
    margin-right: 1in;
    margin-bottom: 0.5in;
    margin-left: 1.5in;
}

#title-page {
    page: title;
    margin: 0 auto;
    width: 6in;
    clear: both;
    page-break-after: always;
}

#screenplay {
    margin: 0 auto;
    width: 6in;
    clear: both;
    counter-reset: page 1;
    page: screenplay;
}

@media print {
    #title {
        margin-top: 3.5in;
        margin-bottom: 4in;
    }
}

#title-page #title {
    text-align: center;
}

#title-page #title img {
    width: 100%;
}

#title-page #left {
    width: 50%;
    float: left;
}

#title-page #right {
    width: 50%;
    float: right;
    text-align: right;
}

#title h1 {
    text-decoration: ${title-underline};
    text-transform: ${title-upcase};
    font-weight: ${title-bold};
}

@page screenplay {
    @top-right-corner {
        font-family: ${font};
        font-size: 12pt;
        content: counter(page)\".\";
        vertical-align: bottom;
        padding-bottom: 1em;
    }
    @bottom-left {
        font-family: ${font};
        font-size: 12pt;
        content: string(dialog-more, last);
        margin-left: 2in;
        vertical-align: top;
    }
}

@page screenplay:first {
    @top-right-corner {
        content: normal;
    }
}

h1,h2,h3,h4,h5,h6 {
    font-weight: normal;
    font-size: 12pt;
}

body {
    font-family: ${font};
    font-size: 12pt;
    line-height: 1;
}

hr {
    visibility: hidden;
    page-break-after: always;
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

span.strikethrough {
    text-line-through-style: solid;
}

mark {
    background-color: inherit;
}

mark:before {
    content: '*';
    width: 0.5in;
    position: absolute;
    right: 0in;
}

del:before {
    content: '*';
    width: 0.5in;
    position: absolute;
    right: 0in;
}

p {
    margin-top: 1em;
    margin-bottom: 1em;
    margin-left: 0in;
    width: auto;
}

h2.scene-heading {
    font-weight: ${scene-bold};
    text-decoration: ${scene-underline};
    margin-top: ${scene-spacing};
    page-break-after: avoid;
}

p.action {
    white-space: pre-wrap;
    orphans: ${action-orphans};
    widows: ${action-widows};
}    

p.center {
    text-align: center;
    margin-left: 0;
    width: 100%;
    white-space: pre-wrap;
}

p.trans {
    margin-left: 4in;
    width: 2in;
    page-break-before: avoid;
}

p.note {
    display: none;
}

p.section {
    display: none;
}

p.synopsis {
    display: none;
}

p.page-break {
    visibility: hidden;
    page-break-after: always;
}

table.dialog {
    margin-top: 1em;
    margin-bottom: 1em;
    margin-left: 1in;
    border-spacing: 0px;
    width: 4in;
    string-set: character attr(character) dialog-more \"${dialog-more}\";
}

table.dialog:after {
    display: table-row;
    content: \"\";
    string-set: dialog-more \"\";
}

tr.character {
    page-break-after: avoid;
}

tr.dialog {
    orphans: ${dialog-orphans};
    widows: ${dialog-widows};
}

tr.paren {
    orphans: ${dialog-orphans};
    widows: ${dialog-widows};
    page-break-inside: avoid;
    page-break-after: avoid;
}

td {
    display: block;
}

td.character {
    margin-left: 1in;
}

td.dialog {
    width: 3.5in;
}

td.paren {
    margin-left: 0.6in;
    text-indent: -0.6em;
    width: 2in;
}

table.dialog caption.character {
    margin-left: 1in;
    text-align: left;
    caption-side: top;
    caption-page: following;
    content: string(character, last)\" ${dialog-contd}\";
}"
"Style template for exporting to HTML, and PDF via HTML.
Only customize this if you really know what you're doing."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-html-head-template
  "<head>
<meta charset=\"${charset}\">
<meta name=\"author\" content=\"${author}\" />
<meta name=\"generator\" content=\"Emacs ${emacs-version} running Fountain Mode ${fountain-version}\" />
<title>${title}</title>
${insert-style}
</head>"
  "HTML head template inserted into export buffer.
Only customize this if you really know what you're dong.
Currently, ${charset} will default to UTF-8."
  :type 'string
  :group 'fountain-export)

;;; Variables ==================================================================

(defvar fountain-metadata
  nil
  "Metadata alist in the form of (KEY . VALUE).
This buffer-local variable is set with `fountain-read-metadata'
upon calling `fountain-mode' or saving a file.")
(make-variable-buffer-local 'fountain-metadata)

(defvar fountain-block-limit
  10000
  "Integer to limit fontification block in characters.
Used by `fountain-get-block-bounds'.")

;;; Element Regular Expressions ================================================

(defvar fountain-scene-heading-regexp
  nil
  "Regular expression for matching scene headings.
Set with `fountain-initialize-regexp'. Requires
`fountain-scene-heading-p' for preceding blank line.")

(defvar fountain-trans-regexp
  nil
  "Regular expression for matching transitions.
Set with `fountain-initialize-regexp'. Requires
`fountain-trans-p' for preceding and succeeding blank lines.")

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
  (concat "^\\<\\([^:\n]+\\):\s*\\(.+\\)?\\|"
          "^\s+\\(?2:.+\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-metadata-p' for bobp.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*"
          "\\(@[^<>()\n]*?\\|[^!<>()[:lower:]\s\t\n][^<>[:lower:]\n]*?\\)"
          "[\s\t]*"
          "\\((.*\\)?$")
  "Regular expression for matching character names.
Requires `fountain-character-p' for preceding invisible and
succeeding non-invisibles.")

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

(defconst fountain-center-regexp
  "\\(^[\s\t]*>[\s\t]*\\)\\(.*?\\)\\([\s\t]*<[\s\t]*$\\)"
  "Regular expression for matching centered text.")

;;; Emphasis Regular Expressions ===============================================

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

;;; Faces ======================================================================

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
  '((t (:inherit font-lock-comment-face)))
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

;;; Thing Definitions ==========================================================

(put 'scene 'forward-op 'fountain-forward-scene)

;;; Internal Functions =========================================================

(defun fountain-initialize-regexp ()    ; WTF break these into separate funs
  "Set variable regular expression values.
Sets `fountain-trans-regexp' and
`fountain-scene-heading-prefix-list'."
  (setq fountain-trans-regexp
        (concat "^\\([\s\t]*>\s*\\)\\([^<>\n]*\\)$\\|"
                "^[\s\t]*\\(?2:[[:upper:]\s]*"
                (regexp-opt fountain-trans-list)
                "\\)$")
        fountain-scene-heading-regexp
        (concat "^\\(\\.\\)\\(\\<.*\\)\\|"
                "^\\(?2:"
                (regexp-opt fountain-scene-heading-prefix-list)
                "[.\s\t]+.*\\)")))

(defun fountain-get-block-bounds ()
  "Return the beginning and end points of block at point."
  (let ((block-beginning
         (save-excursion
           (re-search-backward fountain-blank-regexp
                               (- (point) fountain-block-limit) t)))
        (block-end
         (save-excursion
           (re-search-forward fountain-blank-regexp
                              (+ (point) fountain-block-limit) t))))
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

(defun fountain-tachyon-p ()
  "Return non-nil if point is at a non-interfering element.
These include blank lines, section headings, synopses, notes, and
comments."
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
             (fountain-tachyon-p))))))

(defun fountain-character-p ()
  "Match character if point is at character, nil otherwise."
  (unless (fountain-scene-heading-p)
    (save-excursion
      (forward-line 0)
      (and (let ((case-fold-search nil))
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
                   (fountain-tachyon-p))))
           (save-match-data
             (save-excursion
               (forward-line 1)
               (or (eobp)
                   (fountain-tachyon-p))))))))

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

Optionally, use \"$@\" to set the `mark' and \"$?\" to set the
`point', but only use one of each."
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
      (when (search-forward "$@" end t)
        (replace-match "")
        (push-mark (point) t t))
      (goto-char start)
      (if (search-forward "$?" end t)
          (replace-match "")
        (goto-char end)))))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID by calling `fountain-uuid-func'."
  (let ((s (downcase (funcall fountain-uuid-func))))
    (car (split-string s "-"))))

(defun fountain-get-character (&optional n)
  "Return character at point or Nth previous character.
If N is non-nil, return Nth following (or Nth previous if N is
negative) character within scene, otherwise return nil. If N is
nil or 0, return character at point, otherwise return nil."
  (let* ((i (or n 0))
         (p (if (<= i 0) -1 1)))
    (save-excursion
      (save-restriction
        (widen)
        (while (/= i 0)
          (unless (fountain-scene-heading-p)
            (forward-line p))
          (while (null (or (fountain-character-p)
                           (fountain-scene-heading-p)
                           (bobp)))
            (forward-line p))
          (setq i (- i p)))
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
    (if (null (and start end))
        (error "Region bounds overflow")
      (goto-char font-lock-beg)
      (unless (or (bobp)
                  (eq start font-lock-beg))
        (setq font-lock-beg start changed t))
      (goto-char font-lock-end)
      (unless (or (eobp)
                  (eq end font-lock-end))
        (setq font-lock-end end changed t))
      changed)))

;;; Export Internal Functions ==================================================

(defun fountain-export-fontify-buffer ()
  "If `font-lock-mode' is enables, fontify entire buffer."
  (if font-lock-mode
      (let ((job (make-progress-reporter "Fontifying..." 0 100))
            (chunk (/ (buffer-size) 100))
            (n 0))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((limit (+ (point) chunk)))
              (jit-lock-fontify-now (point) limit)
              (goto-char limit)
              (progress-reporter-update job n)
              (setq n (+ n 1))))
          (progress-reporter-done job)))
    (error "Font Lock is not active")))

(defun fountain-export-strip-comments ()
  "Strips buffer of all comments and metadata.
Matches and deletes any text with `fountain-comment',
`fountain-metadata-key' or `fountain-metadata-value' face."
  (save-excursion
    (goto-char (point-min))
    (while (null (eobp))
      (if (memq (face-at-point) '(fountain-comment
                                  fountain-metadata-key
                                  fountain-metadata-value))
          (let ((m (point)))
            (goto-char (next-single-property-change
                        (point) 'face nil (point-max)))
            (delete-region m (point)))
        (goto-char (next-single-property-change
                    (point) 'face nil (point-max)))))))

(defun fountain-export-get-name (ext)
  "If BUFFER is visiting a file, concat file name base and EXT.
Otherwise return `fountain-export-buffer'"
  (if (buffer-file-name)
      (concat (file-name-base (buffer-file-name)) "." ext)
    (format "*Fountain %s Export*" ext)))

(defun fountain-export-get-metadata-value (key) ;FIXME combine with other
  "Like `fountain-get-metadata-value' but filters for HTML."
  (let* ((value (cdr (assoc key fountain-metadata)))
         (s (if (listp value)
                (s-join "\n" value)
              value))
         (s (fountain-export-filter s)))
    s))

(defun fountain-export-create-title-page-element (key)
  "Gets metadata value associated with KEY and creates HTML element."
  (let* ((s (fountain-export-get-metadata-value key))
         (content (fountain-export-line-breaks s)))
    (if (string= key "title")
        (format "<h1>%s</h1>" content)
      (format "<p>%s</p>" content))))

(defun fountain-export-create-html-title-page ()
  "Create title page based on `fountain-export-title-page-template'."
  (if (cdr (assoc "title" fountain-metadata))
      (concat
       "<div id=\"title\">\n"
       (s-format fountain-export-title-page-title-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n<div id=\"left\">\n"
       (s-format fountain-export-title-page-left-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n<div id=\"right\">\n"
       (s-format fountain-export-title-page-right-template
                 'fountain-export-create-title-page-element)
       "\n</div>\n")))

(defun fountain-export-create-style ()
  "Create stylesheet using `fountain-export-style-template'."
  (let* ((page-size fountain-export-page-size)
         (font
          (mapconcat
           (lambda (font) (concat "'" font "'"))
           fountain-export-font ","))
         (scene-bold
          (if fountain-export-bold-scene-headings
              "bold" "normal"))
         (scene-underline
          (if fountain-export-underline-scene-headings
              "underline" "none"))
         (scene-spacing
          (if fountain-export-double-space-scene-headings
              "2em" "1em"))
         (title-bold
          (if fountain-export-bold-title
              "bold" "normal"))
         (title-underline
          (if fountain-export-underline-title
              "underline" "none"))
         (title-upcase
          (if fountain-export-upcase-title
              "uppercase" "none"))
         (dialog-contd (concat "(" fountain-continued-dialog-string ")"))
         (dialog-more fountain-export-more-dialog-string)
         (action-orphans (int-to-string fountain-export-action-orphans))
         (action-widows (int-to-string fountain-export-action-widows))
         (dialog-orphans (int-to-string fountain-export-dialog-orphans))
         (dialog-widows (int-to-string fountain-export-dialog-widows))
         (style-rules (s-format fountain-export-style-template
                          '(lambda (var)
                             (symbol-value (intern var))))))
    (if fountain-export-inline-style
        (concat "<style type=\"text/css\">\n"
                style-rules
                "\n</style>")
      (let ((cssfile (get-buffer-create (fountain-export-get-name "css")))
            (outputdir (expand-file-name
                        (file-name-directory (buffer-file-name)))))
        (with-current-buffer cssfile
          (erase-buffer)
          (insert
           (format
            "/* Created with Emacs %s running Fountain Mode %s */\n"
            emacs-version fountain-version)
           style-rules)
          (write-file outputdir))
        (concat "<link rel=\"stylesheet\" href=\""
                (buffer-name cssfile)
                "\">")))))

(defun fountain-export-create-html-head ()
  "Create HTML head using `fountain-export-html-head-template'."
  (let ((insert-style (fountain-export-create-style))
        (charset "utf-8")
        (title (or (fountain-export-get-metadata-value "title")
                   (file-name-base (buffer-name))))
        (author (or (fountain-export-get-metadata-value "author")
                    user-full-name)))
    (s-format fountain-export-html-head-template
              '(lambda (var)
                 (symbol-value (intern var))))))

(defun fountain-export-underline (s)
  "Replace underlined text in S with HTML underline span tags."
  (replace-regexp-in-string "_\\(.+?\\)_"
                            "<span class=\"underline\">\\1</span>"
                            s t))

(defun fountain-export-bold (s)
  "Replace bold text in S with HTML strong tags."
  (replace-regexp-in-string "\\*\\*\\(.+?\\)\\*\\*"
                            "<strong>\\1</strong>"
                            s t))

(defun fountain-export-italic (s)
  "Replace italic text in S with HTML emphasis tags."
  (replace-regexp-in-string "\\*\\(.+?\\)\\*"
                            "<em>\\1</em>"
                            s t))

(defun fountain-export-lyrics (s)
  "Replace lyrics in S with HTML italic tags."
  (replace-regexp-in-string "^~\s*\\(.+\\)"
                            "<i>\\1</i>"
                            s t))

(defun fountain-export-line-breaks (s)
  "Replace newlines in S with HTML line breaks."
  (replace-regexp-in-string "\n"
                            "<br>\n"
                            s))

(defun fountain-export-tex-quotes (s)
  "Replace TeX-style quotes in S with \"smart\" quotes."
  (s-replace-all '(("\\`" . "&#96;")
                   ("\\'" . "&apos;")
                   ("``" . "&ldquo;")
                   ("''" . "&rdquo;")
                   ("`" . "&lsquo;")
                   ("'" . "&rsquo;")) s))

(defun fountain-export-sanitize (s)
  "Escape HTML characters in S."
  (s-replace-all '(("&" . "&amp;")
                   ("<" . "&lt;")
                   (">" . "&gt;")) s))

(defun fountain-export-filter (sub-s)   ;FIXME update doc
  (let* ((s (substring-no-properties sub-s))
         (s (fountain-export-sanitize s))
         (s (s-replace-all '(("\\\s" . "&nbsp;")
                             ("^\\\\$" . "<br>\n")
                             ("\\_" . "&#95;")
                             ("\\*" . "&#42;")) s))
         ;; (s (if fountain-export-preserve-line-breaks
         ;;        (fountain-export-line-breaks s)
         ;;      s))
         (s (if fountain-export-convert-quotes
                (fountain-export-tex-quotes s)
              s))
         (s (fountain-export-underline s))
         (s (fountain-export-bold s))
         (s (fountain-export-italic s))
         (s (fountain-export-lyrics s)))
    s))

(defun fountain-export-create-html-dialog-table (content limit)
  ""
  (let* ((dialog-contd (concat "(" fountain-continued-dialog-string ")"))
         (character (fountain-export-filter
                     (s-trim (car (s-slice-at dialog-contd content)))))
         (table-start
          (format (concat "<table class=\"dialog\" character=\"%s\">\n"
                          "<caption class=\"character\">\n"
                          "<tr class=\"character\"><td class=\"character\">%s</td></tr>\n")
                  character content))
         (table-body "")
         (table-end "</table>\n"))
    (goto-char (next-single-property-change
                (point) 'fountain-element nil limit))
    (while (< (point) limit)
      (skip-chars-forward "\n")
      (setq table-body
            (concat table-body
                    (fountain-export-create-html-element limit))))
    (concat table-start table-body table-end)))

(defun fountain-export-create-html-element (limit)
  ""
  (let* ((index (point))
         (class (or (get-text-property index 'fountain-element)
                    "action"))
         (change (next-single-property-change
                  index 'fountain-element nil limit)))
    (when change
      (let* ((sub-s (buffer-substring index change))
             (content (fountain-export-filter sub-s))
             (element
              (cond ((string= class "character")
                     (fountain-export-create-html-dialog-table content limit))
                    ((member class '("dialog" "paren"))
                     (format "<tr class=\"%s\"><td class=\"%s\">%s</td></tr>\n"
                             class class content))
                    ((string= class "scene-heading")
                     (format "<h2 class=\"%s\">%s</h2>\n"
                             class content))
                    ((format "<p class=\"%s\">%s</p>\n"
                             class content)))))
        (if (string= class "character")
            (goto-char limit)
          (goto-char change))
        element))))

(defun fountain-export-parse-buffer (destbuf)
  "Find and insert elements into DESTBUF.
First, skip forward to next available text, mark point as index,
then find the next \"fountain-element\" text property change from
index, then pass substring from index to change to
`fountain-export-create-html-element', then insert the newly
created HTML element to DESTBUF."
  (let ((job (make-progress-reporter "Parsing..." 0 100)))
    (goto-char (point-min))
    (while (null (eobp))
      (while (looking-at "\n*\s*\n")
        (goto-char (match-end 0)))
      (let* ((limit (save-excursion
                     (re-search-forward "\n\s*\n\\|\\'" nil t)
                     (match-beginning 0)))
             (element (fountain-export-create-html-element limit)))
        (when element
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert element)))))
      (progress-reporter-update
       job (truncate (* (/ (float (point)) (buffer-size)) 100))))))

(defun fountain-export--html ()
  ;; internal function, don't call externally
  ;; use `fountain-export-buffer-to-html' instead
  ;; first read the metadata
  (fountain-read-metadata)
  (let* ((sourcebuf (current-buffer))
         (destbuf (get-buffer-create
                   (fountain-export-get-name "html")))
         (head (fountain-export-create-html-head))
         (title-page (fountain-export-create-html-title-page))
         complete)
    (unwind-protect
        (progn
          ;; fontify the accessible buffer
          (fountain-export-fontify-buffer)
          ;; create a temp buffer with source
          (with-temp-buffer
            (insert-buffer-substring sourcebuf)
            ;; strip comments
            (fountain-export-strip-comments)
            ;; insert HTML head
            (with-current-buffer destbuf
              (with-silent-modifications
                (erase-buffer)
                (insert "<!DOCTYPE html>\n<html>\n"
                        head "\n")
                ;; close head and open body
                (insert "<body>\n")
                ;; add the title page maybe
                (if (and title-page
                         fountain-export-include-title-page)
                    (insert "<section id=\"title-page\">\n"
                            title-page
                            "</section>\n"))
                (insert "<section id=\"screenplay\">\n")))
            ;; parse the temp buffer
            (fountain-export-parse-buffer destbuf))
          ;; close HTML tags
          (with-current-buffer destbuf
            (with-silent-modifications
              (insert "</section>\n</body>\n</html>")))
          ;; signal completion and return DESTBUF
          (setq complete t)
          destbuf)
      ;; if error occurs, kill the unsaved buffer
      (unless complete
        (kill-buffer destbuf)))))

;;; Interactive Functions  =====================================================

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
                                  (fountain-get-character -1)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" s)))
            (forward-line 1)
            (progress-reporter-update job)))
        (progress-reporter-done job)))))

(defun fountain-export-default ()
  "Call function defined in `fountain-export-default-command'"
  (interactive)
  (funcall fountain-export-default-command))

(defun fountain-export-buffer-to-html (&optional buffer)
  "Export BUFFER to HTML file, then switch to HTML buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (let ((destbuf (fountain-export--html))
              (outputdir (if (buffer-file-name buffer)
                             (expand-file-name (file-name-directory
                                                (buffer-file-name buffer))))))
          (with-current-buffer destbuf
            (if outputdir
                (write-file outputdir)))
          (if (called-interactively-p 'interactive)
              (switch-to-buffer-other-window destbuf))
          destbuf)))))

(defun fountain-export-buffer-to-pdf-via-html (&optional buffer)
  "Export BUFFER to HTML file, then convert HTML to PDF."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (shell-quote-argument (buffer-file-name
                                      (fountain-export-buffer-to-html
                                       buffer))))
         (command (format fountain-export-pdf-via-html-command file)))
    (async-shell-command command "*Fountain PDF Process*")))

;;; Menu Functions =============================================================

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

(defun fountain-toggle-hide-element (element s)
  "Toggle visibility of fountain-ELEMENT, using S for feedback.
Toggles the value of fountain-hide-ELEMENT, then, if
fountain-hide-ELEMENT is non-nil, adds fountain-ELEMENT to
`buffer-invisibility-spec', otherwise removes it. Returns a
message of \"S are now invisible/visible\"."
  (let* ((option (intern (concat "fountain-hide-" element)))
         (symbol (intern (concat "fountain-" element))))
    (set option
         (null (symbol-value option)))
    (if (symbol-value option)
        (add-to-invisibility-spec symbol)
      (remove-from-invisibility-spec symbol))
    (jit-lock-refontify)
    (message "%s are now %s"
             s (if (symbol-value option)
                   "invisible" "visible"))))

(defun fountain-toggle-hide-emphasis-delim ()
  "Toggle `fountain-hide-emphasis-delim'."
  (interactive)
  (fountain-toggle-hide-element "emphasis-delim" "Emphasis delimiters"))

(defun fountain-toggle-hide-syntax-chars ()
  "Toggle `fountain-hide-syntax-chars'."
  (interactive)
  (fountain-toggle-hide-element "syntax-chars" "Syntax characters"))

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
  (customize-save-variable 'fountain-hide-syntax-chars
                           fountain-hide-syntax-chars))

(defun fountain-get-font-lock-decoration ()
  "Return the value of `font-lock-maximum-decoration'."
  (cond ((null font-lock-maximum-decoration) 2)
        ((eq font-lock-maximum-decoration t) 3)
        ((integerp font-lock-maximum-decoration)
         font-lock-maximum-decoration)
        ((cdr (assoc 'fountain-mode font-lock-maximum-decoration)))
        ((cdr (assoc 't font-lock-maximum-decoration)) 3)))

(defun fountain-toggle-export-bold-scene-headings ()
  "Toggle `fountain-export-bold-scene-headings'"
  (interactive)
  (setq fountain-export-bold-scene-headings
        (null fountain-export-bold-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-bold-scene-headings
               "bold" "normal")))

(defun fountain-toggle-export-underline-scene-headings ()
  "Toggle `fountain-export-underline-scene-headings'"
  (interactive)
  (setq fountain-export-underline-scene-headings
        (null fountain-export-underline-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-underline-scene-headings
               "underlined" "normal")))

(defun fountain-toggle-export-double-space-scene-headings ()
  "Toggle `fountain-export-double-space-scene-headings'"
  (interactive)
  (setq fountain-export-double-space-scene-headings
        (null fountain-export-double-space-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-double-space-scene-headings
               "double-spaced" "single-spaced")))

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords-plist
  `(("note" ,fountain-note-regexp
     ((:level 2 :subexp 0 :invisible t)))
    ("scene-heading"
     (lambda (limit)
       (fountain-match-element 'fountain-scene-heading-p limit))
     ((:level 2 :subexp 0 :override keep)
      (:level 1 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("character"
     (lambda (limit)
       (fountain-match-element 'fountain-character-p limit))
     ((:level 3 :subexp 0 :override keep)))
    ("dialog"
     (lambda (limit)
       (fountain-match-element 'fountain-dialog-p limit))
     ((:level 3 :subexp 0 :override keep)))
    ("paren"
     (lambda (limit)
       (fountain-match-element 'fountain-paren-p limit))
     ((:level 3 :subexp 0 :override keep)))
    ("trans"
     (lambda (limit)
       (fountain-match-element 'fountain-trans-p limit))
     ((:level 3 :subexp 0 :override keep)
      (:level 1 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("forced-action-mark" ,fountain-forced-action-mark-regexp
     ((:level 1 :subexp 0 :face fountain-comment
              :invisible fountain-syntax-chars)))
    ("center" ,fountain-center-regexp
     ((:level 1 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars)
      (:level 3 :subexp 2)
      (:level 1 :subexp 3 :face fountain-comment
              :invisible fountain-syntax-chars)))
    ("section" ,fountain-section-regexp
     ((:level 2 :subexp 0 :invisible t)
      (:level 1 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("synopsis" ,fountain-synopsis-regexp
     ((:level 2 :subexp 0 :invisible t)
      (:level 1 :subexp 1 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("page-break" ,fountain-page-break-regexp
     ((:level 2 :subexp 0 :face fountain-page-break)))
    ("metadata"
     (lambda (limit)
       (fountain-match-element 'fountain-metadata-p limit))
     ((:level 3 :subexp 1 :face fountain-metadata-key
              :invisible t
              :laxmatch t)
      (:level 3 :subexp 2 :face fountain-metadata-value
              :invisible t
              :laxmatch t)
      (:level 1 :subexp 0 :face fountain-comment
              :invisible t
              :override keep)))
    (nil ,fountain-nbsp-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-syntax-chars)))
    (nil ,fountain-underline-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 2 :subexp 3 :face underline)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 2 :subexp 3 :face italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-bold-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 2 :subexp 3 :face bold)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-bold-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 2 :subexp 3 :face bold-italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-lyrics-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 2 :subexp 3 :face italic))))
  "List of face properties to create element Font Lock keywords.
Has the format:

    (ELEMENT MATCHER PLIST-LIST)

The first element, ELEMENT, is a string naming the element; if
nil, this face is not considered an element. MATCHER is a regular
expression or search function. PLIST-LIST is a list of plists,
assigning the following keywords:

    :level - integer representing level of `font-lock-maximum-decoration'
        at which face is applied
    :subexp - subexpression to match
    :face - face name to apply
    :invisible - if t, adds :face property to invisible text property
    :override - as per `font-lock-keywords'
    :laxmatch - as per `font-lock-keywords'")

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (let ((dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (var fountain-font-lock-keywords-plist keywords)
      (let* ((element (car var))
             (matcher (nth 1 var))
             (plist-list (nth 2 var))
             (align (intern (concat "fountain-align-" element)))
             ;; if we're using auto-align and the align var is bound,
             ;; set the align properties
             (align-props (if (and fountain-align-elements
                                   (boundp align))
                              `(line-prefix
                                (space :align-to ,align)
                                wrap-prefix
                                (space :align-to ,align))))
             facespec)
        (dolist (plist plist-list)
          (let* ((subexp (plist-get plist :subexp))
                 ;; if LEVEL is less or equal to DEC, use either face
                 ;; supplied in PLIST or intern fountain-ELEMENT,
                 ;; otherwise use nil
                 (face (if (<= (plist-get plist :level) dec)
                           (or (plist-get plist :face)
                               (intern (concat "fountain-" element)))))
                 ;; if INVISIBLE is non-nil, add to INVISIBLE-PROPS
                 (invisible (plist-get plist :invisible))
                 (invisible-props
                  (cond ((eq invisible t)
                         `(invisible ,(intern (concat "fountain-" element))))
                        (invisible
                         `(invisible ,invisible)))))
            (setq facespec
                  (append facespec
                          (if element
                              (list `(,subexp '(face ,face
                                                     ,@align-props
                                                     ,@invisible-props
                                                     fountain-element ,element)
                                              ,(plist-get plist :override)
                                              ,(plist-get plist :laxmatch)))
                            (list `(,subexp '(face ,face
                                                   ,@invisible-props)
                                            append)))))))
        (setq keywords
              (append keywords
                      (list (cons matcher facespec))))))))

(defun fountain-match-element (fun limit)
  "If FUNC returns non-nil before LIMIT, return match data."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (if (funcall fun)
          (setq match t))
      (forward-line 1))
    match))

;;; Mode Map ===================================================================

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
    (define-key map (kbd "C-c C-x !") 'fountain-toggle-hide-syntax-chars)
    (define-key map (kbd "C-c C-x *") 'fountain-toggle-hide-emphasis-delim)
    (define-key map (kbd "M-s 1") 'fountain-occur-sections)
    (define-key map (kbd "M-s 2") 'fountain-occur-synopses)
    (define-key map (kbd "M-s 3") 'fountain-occur-notes)
    (define-key map (kbd "M-s 4") 'fountain-occur-scene-headings)
    map)
  "Mode map for `fountain-mode'.")

;;; Menu =======================================================================

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
     ["Hide Emphasis Delimiters" fountain-toggle-hide-emphasis-delim
      :style toggle
      :selected fountain-hide-emphasis-delim]
     ["Hide Syntax Characters" fountain-toggle-hide-syntax-chars
      :style toggle
      :selected fountain-hide-syntax-chars]
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
  "Major mode for screenwriting in Fountain markup."
  :group 'fountain
  (set (make-local-variable 'comment-start)
       (if fountain-switch-comment-syntax "//" "/*"))
  (set (make-local-variable 'comment-end)
       (if fountain-switch-comment-syntax "" "*/"))
  (set (make-local-variable 'font-lock-comment-face)
       'fountain-comment)
  (setq font-lock-defaults
        '(fountain-create-font-lock-keywords nil t))
  (setq font-lock-extra-managed-props
        '(line-prefix wrap-prefix invisible fountain-element))
  (if fountain-hide-emphasis-delim
      (add-to-invisibility-spec 'fountain-emphasis-delim))
  (if fountain-hide-syntax-chars
      (add-to-invisibility-spec 'fountain-syntax))
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec nil))
  (add-hook 'font-lock-extend-region-functions
            'fountain-font-lock-extend-region t t)
  (add-hook 'after-save-hook
            'fountain-read-metadata)
  (fountain-initialize-regexp)
  (fountain-read-metadata))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
