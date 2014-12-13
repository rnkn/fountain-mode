;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: wp
;; Version: 1.4.3
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

;; - Support for most of the Fountain 1.1 specification (scene numbers and
;;   dual dialog are forthcoming)
;; - Auto-align elements for a kind of WYSIWYG (display only, does not
;;   modify file contents)
;; - Integration with `outline` to toggle visibility of sections and
;;   scenes
;; - Export to HTML and PDF (PDF export requires [Prince][])
;; - Include or omit a title page
;; - Navigate by scene heading
;; - Emphasis (bold, italic, underlined text)
;; - Toggle visibility of emphasis delimiters and syntax characters
;; - Multiple levels of syntax highlighting for all elements
;; - Add/remove automatic "(CONT'D)" to successively speaking characters
;; - Automatic "(MORE)" and "(CONT'D)" when breaking dialog across pages in
;;   PDF output
;; - Templates for inserting synopses, notes and metadata
;; - Support for both official and legacy commenting (boneyard) syntax
;; - Navigator (using `occur`) for section headings, synopses, notes and
;;   scene headings
;; - everything is customizable, of course

;; The following features are not *yet* supported:

;; - scene numbers
;; - dual dialog

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type `C-h m`.

;; See the [Wiki][] on GitHub for ways to extend Fountain Mode.

;; [prince]: http://www.princexml.com "Prince"
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
;; - If you want to use UUIDs (useful for using notes as linked bookmarks) you'll
;;   need either `uuidgen` CLT (usually preinstalled on OS X and Linux) or
;;   [uuid.el][] Emacs package.

;; [s.el]: https://github.com/magnars/s.el "s.el"
;; [uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

;; Installation
;; ------------

;; *For users on OS X with no experience with Emacs, see the
;; [Absolute Beginner's Guide (OS X)][beginners guide].*

;; Fountain Mode is available through [MELPA][] and [MELPA-stable][]. I
;; encourage installing the stable version.

;; Alternately, download the [latest release][], move the files into your
;; `load-path` and add the following line to your `.emacs` or `init.el`
;; file:

;;     (require 'fountain-mode)

;; If you want to use the `develop` branch (not recommended) to stay on
;; the bleeding-edge, clone the repository in your `load-path` and
;; require as above:

;;     git clone https://github.com/rnkn/fountain-mode.git

;; To load Fountain Mode whenever you open a `.fountain` file, also add the
;; following:

;;     (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

;; [beginners guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
;; [melpa]: http://melpa.org "MELPA"
;; [melpa-stable]: http://stable.melpa.org "MELPA-stable"
;; [latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

;; Outlining
;; ---------

;; There are six possible levels of outline subtrees. Section headings
;; count as the first five levels and scene headings count as the sixth
;; level, e.g.:

;;     # section level 1
;;     ## section level 2
;;     ### section level 3
;;     #### section level 4
;;     ##### section level 5
;;     ###### invalid section level
;;     INT. LEVEL 6 - DAY
;;
;;     An obese man (40s) with a large mustard stain on his shirt exits the
;;     elevator. He holds a hotdog.

;; Cycle subtree visibility with `TAB`. Cycle global outline visibility
;; with `<backtab>` (shift-TAB) or `C-u TAB`. More navigation and structure
;; editing commands are:

;; - `C-c C-f fountain-outline-forward`
;; - `C-c C-b fountain-outline-backward`
;; - `C-c C-n fountain-outline-next`
;; - `C-c C-p fountain-outline-previous`
;; - `C-c C-u fountain-outline-up`
;; - `C-c C-v fountain-outline-shift-down`
;; - `C-c C-^ fountain-outline-shift-up`
;; - `C-c C-SPC fountain-outline-mark`

;; Bugs and Feature Requests
;; -------------------------

;; Raise an issue on the [Issues][] page on GitHub, or simply send an email
;; to the mailing list: <emacs.fountain@librelist.com>.

;; [issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"

;; History
;; -------

;; See [Releases][] on GitHub.

;; [releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"

;;; Code:

(defconst fountain-version
  "1.4.3")

;;; Required ===================================================================

(require 's)
(require 'thingatpt)
(require 'easymenu)
(require 'outline)

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
  '("INT" "EXT" "I/E" "EST")
  "List of scene heading prefixes (case insensitive).
Any scene heading prefix can be followed by a dot and/or a space,
so the following are equivalent:

    INT HOUSE - DAY

    INT. HOUSE - DAY

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

;; (defcustom fountain-align-scene-num
;;   65
;;   "Column integer to which scene numbers should be aligned.
;; This option affects file contents"
;;   :type 'integer
;;   :group 'fountain)

(defcustom fountain-align-elements
  t
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

(defcustom fountain-outline-startup-level
  0
  "Outline level to show when visiting a file."
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

(defcustom fountain-uuid-func
  '(lambda () (shell-command-to-string "uuidgen"))
  "Function for generating a UUID.
The default function requires the command line tool \"uuidgen\"."
  :tag "Fountain UUID Function"
  :type 'function
  :group 'fountain)

;;; Export Customization =======================================================

;; (defcustom fountain-export-element-set
;;   '("scene-heading" "action" "character" "paren" "dialog" "trans")
;;   "List of elements to include when exporting.
;; You would usually keep this at its default, but changing becomes
;; useful if, for example, you want to include your script notes, or
;; only want to export your synopses.

;; This set does not apply to metadata."
;;   :type '(set
;;           (const :tag "Scene Headings" "scene-heading")
;;           (const :tag "Action" "action")
;;           (const :tag "Character Names" "character")
;;           (const :tag "Parentheticals" "paren")
;;           (const :tag "Dialog" "dialog")
;;           (const :tag "Transitions" "trans")
;;           (const :tag "Section Headings" "section")
;;           (const :tag "Synopses" "synopsis")
;;           (const :tag "Notes" "note")
;;           (const :tag "Comments" "comment"))
;;   :group 'fountain-export)

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
    margin-bottom: 1em;
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
    padding: 0;
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

(defvar-local fountain-metadata
  nil
  "Metadata alist in the form of (KEY . VALUE).
This buffer-local variable is set with `fountain-read-metadata'
upon calling `fountain-mode' or saving a file.")

(defvar-local fountain-outline-cycle
  0
  "Integer representing global outline cycling status.
Used by `fountain-outline-cycle'.")

(defvar-local fountain-outline-cycle-subtree
  0
  "Integer representing subtree outline cycling status.
Used by `fountain-outline-cycle'.")

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

;; (defconst fountain-scene-num-regexp
;;   "\\(.*?\\)\\([\s\t]*\\)\\(#[0-9]+[a-z]?\\)[\s\t]*$"
;;   "Regular expression for matching scene numbers.
;; Assumes line matches `fountain-scene-heading-p'.

;;     Group 1: scene heading
;;     Group 2: whitespace
;;     Group 3: scene number (including leading #)")

(defconst fountain-blank-regexp
  "^\s?$"
  "Regular expression for matching an empty line.")

(defconst fountain-forced-action-mark-regexp
  "^!"
  "Regular expression for forced action mark.")

(defconst fountain-nbsp-regexp
  "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:\\\\\\)\s\\)"
  "Regular expression for non-breaking space.")

(defconst fountain-comment-regexp
  (concat "\\(//[\s\t]*\\(.*\\)\\)"
          "\\|"
          "\\(?1:\\(?2:/\\*\\)[\s\t]*\\(?3:\\(.\\|\n\\)*?\\)[\s\t]*\\*/\\)")
  "Regular expression for matching comments.")

(defconst fountain-metadata-regexp
  (concat "^\\(?1:\\(?2:\\<[^:\n]+\\):\s*\\(?3:.+\\)?\\)"
          "\\|"
          "^\s+\\(?1:\\(?3:.+\\)\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-metadata-p' for bobp.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*\\(?1:\\(?:"
          "\\(?2:@\\)\\(?3:\\(?4:[^<>\n]+?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\|"
          "\\(?3:\\(?4:[A-Z][^<>a-z\n]*?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\)[\s\t]*\\(?5:\\^\\)?\\)[\s\t]*$")
  "Regular expression for matching character names.

    Group 1: trim whitespace
    Group 2: trim @ and ^ (for export)
    Group 3: character name only
    Group 4: trailing ^ for dual dialog

Requires `fountain-character-p'.")

(defconst fountain-paren-regexp
  "^[\s\t]*\\(\\(([^)\n]*)\\)\\)[\s\t]*$"
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or dialog.")

(defconst fountain-page-break-regexp
  "^[\s\t]*=\\{3,\\}.*"
  "Regular expression for matching page breaks.")

(defconst fountain-note-regexp
  "\\(\\[\\[\\(\\(?:.\n?\\)*?\\)]]\\)"
  "Regular expression for matching notes.")

(defconst fountain-section-regexp
  "^\\(?1:\\(?2:\\(?4:#\\{1,5\\}\\)[\s\t]*\\)\\(?3:[^#\n].*\\)\\)[\s\t]*$"
  "Regular expression for matching section headings.")

(defconst fountain-synopsis-regexp
  "^\\(\\(?3:=[\s\t]*\\)\\(?2:[^=\n].*\\)\\)[\s\t]*"
  "Regular expression for matching synopses.")

(defconst fountain-center-regexp
  "^[\s\t]*\\(?1:\\(?2:>[\s\t]*\\)\\(?3:.*?\\)\\(?4:[\s\t]*<\\)\\)[\s\t]*$"
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

(defun fountain-init-scene-heading-regexp ()
  "Initializes `fountain-scene-heading-regexp'."
  (setq fountain-scene-heading-regexp
        (concat "^\\(?1:\\(?2:\\(?4:\\.\\)\\)\\(?3:\\<.*?\\)\\)[\s\t]*$"
                "\\|"
                "^\\(?1:\\(?3:\\(?4:"
                (regexp-opt fountain-scene-heading-prefix-list)
                "\\)[.\s\t]+.*?\\)\\)[\s\t]*$")))

(defun fountain-init-trans-regexp ()
  "Initializes `fountain-trans-regexp'."
  (setq fountain-trans-regexp
        (concat "^[\s\t]*\\(?1:\\(?2:>[\s\t]*\\)\\(?3:[^<>\n]*?\\)\\)[\s\t]*$"
                "\\|"
                "^[\s\t]*\\(?1:\\(?3:[[:upper:]\s\t]*"
                (regexp-opt fountain-trans-list)
                "\\)\\)[\s\t]*$")))

(defun fountain-init-outline-regexp ()
  "Initializes `outline-regexp'."
  (setq-local outline-regexp
              (concat fountain-section-regexp
                      "\\|"
                      fountain-scene-heading-regexp)))

(defun fountain-init-regexp ()
  "Set variable regular expression values."
  (fountain-init-scene-heading-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-trans-regexp))

(defun fountain-init-comment-syntax ()
  "Set comment syntax according to `fountain-switch-comment-syntax'."
  (setq-local comment-start
              (if fountain-switch-comment-syntax "//" "/*"))
  (setq-local comment-end
              (if fountain-switch-comment-syntax "" "*/")))

(defun fountain-get-block-bounds ()
  "Return the beginning and end points of block at point."
  (let* ((r (concat fountain-blank-regexp "\\|\\`\\|\\'"))
         (beg (save-excursion
                (re-search-backward r
                                    (- (point) fountain-block-limit) t)))
         (end (save-excursion
                (re-search-forward r
                                   (+ (point) fountain-block-limit) t))))
    (cons beg end)))

;; currently unused
;; (defun fountain-strip-comments (start end)
;;   "Strip comments between START and END and return string."
;;   (let ((start
;;          (save-excursion
;;            (goto-char start)
;;            ;; using thing-at-point-looking-at is very slow, better to
;;            ;; use a simpler function.
;;            ;;
;;            ;; (if (thing-at-point-looking-at fountain-comment-regexp)
;;            ;;     (match-beginning 0)
;;            ;;   start)))
;;            (if (and (search-forward "*/" end t)
;;                     (null (search-backward "/*" start t))
;;                     (search-backward "/*" nil t)
;;                     (comment-only-p (point) start))
;;                (point)
;;              start)))
;;         (end
;;          (save-excursion
;;            (goto-char end)
;;            ;; (if (thing-at-point-looking-at fountain-comment-regexp)
;;            ;;     (match-end 0)
;;            ;;   end))))
;;            (if (and (search-backward "/*" start t)
;;                     (null (search-forward "*/" end t))
;;                     (search-forward "*/" nil t)
;;                     (comment-only-p (point) end))
;;                (point)
;;              end))))
;;     (replace-regexp-in-string
;;      fountain-comment-regexp ""
;;      (buffer-substring-no-properties start end))))

(defun fountain-blank-p ()
  "Return non-nil if point is at a blank line or single space."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      ;; don't modify match-data
      (looking-at-p fountain-blank-regexp))))

(defun fountain-metadata-p ()           ; OPTIMIZE
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
  (save-excursion
    (save-restriction
      (widen)
      (if (eq (char-after) ?\*) (forward-char -1))
      (or (forward-comment 1)
          (let ((x (point)))
            (search-backward "/*" nil t)
            (and (forward-comment 1)
                 (<= x (point))))))))

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
             (or (bobp)
                 (fountain-tachyon-p)))))))

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
        (let ((key (downcase (match-string-no-properties 2)))
              (value                    ; FIXME make a list
               (progn
                 (forward-line 1)       ; FIXME on line ahead
                 (or (match-string-no-properties 3)
                     (let (s)
                       (while (and (fountain-metadata-p)
                                   (null (match-string 2)))
                         (setq s
                               (append s
                                       (list (match-string-no-properties 3))))
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
        (goto-char end))
      (set-marker end nil))))

(defun fountain-uuid ()
  "Return a lowercase 8-digit UUID by calling `fountain-uuid-func'."
  (let ((s (downcase (funcall fountain-uuid-func))))
    (car (split-string s "-"))))

(defun fountain-forward-character (&optional n limit)
  "Goto Nth next character (or Nth previous is N is negative).
If LIMIT is 'scene, halt at next scene heading. If LIMIT is
'dialog, halt at next non-dialog element."
  (interactive "^p")
  (let* ((i (or n 1))
         (p (if (< i 1) -1 1)))
    (while (/= i 0)
      (if (fountain-character-p)
          (forward-line p))
      (while (cond ((eq limit 'scene)
                    (not (or (fountain-character-p)
                             (fountain-scene-heading-p)
                             (eq (point) (buffer-end p)))))
                   ((eq limit 'dialog)
                    (or (fountain-dialog-p)
                        (fountain-paren-p)
                        (fountain-tachyon-p)))
                   ((not (or (fountain-character-p)
                             (eq (point) (buffer-end p))))))
        (forward-line p))
      (setq i (- i p)))))

(defun fountain-backward-character (&optional n)
  "Move backward N character (foward if N is negative)."
  (interactive "^p")
  (let ((i (or n 1)))
    (fountain-forward-character (- i))))

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
        (if (fountain-character-p)
            (match-string-no-properties 4))))))

;; (defun fountain-get-scene-num ()
;;   "Return the scene number of current scene."
;;   (if (looking-at fountain-scene-num-regexp)
;;       (s-chop-prefix "#" (match-string-no-properties 3))))

;; (defun fountain-add-scene-num (n)
;;   "Add scene number N to current scene heading.
;; Assumes line matched `fountain-scene-heading-p'."
;;   (end-of-line)
;;   (insert "#" n)
;;   (beginning-of-line)
;;   (fountain-align-scene-num))

;; (defun fountain-add-scene-nums (&optional arg)
;;   "Add scene numbers to all scene headings lacking.
;; If prefaced with ARG, overwrite existing scene numbers."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (unless (fountain-scene-heading-p)
;;       (fountain-forward-scene 1))
;;     (let ((prev-scene-num "0"))
;;       (while (not (eobp))
;;         (let ((current-scene-num (fountain-get-scene-num)))
;;           (if current-scene-num
;;               ;; (fountain-align-scene-num)
;;               (setq prev-scene-num current-scene-num)
;;             (let* ((prev-scene-int (string-to-number prev-scene-num))
;;                    (prev-scene-alpha
;;                     (if (string-match "[a-z]+" prev-scene-num)
;;                         (match-string 0 prev-scene-num)))
;;                    (next-scene-num
;;                     (save-excursion
;;                       (while (not (or (eobp)
;;                                       (fountain-get-scene-num)))
;;                         (fountain-forward-scene 1))
;;                       (fountain-get-scene-num)))
;;                    (next-scene-int (if next-scene-num
;;                                        (string-to-number next-scene-num)))
;;                    (current-scene-num
;;                     (if (or (not next-scene-int)
;;                             (< (1+ prev-scene-int) next-scene-int))
;;                         (int-to-string (1+ prev-scene-int))
;;                       (concat (int-to-string prev-scene-int)
;;                               (if prev-scene-alpha
;;                                   (string (1+ (string-to-char prev-scene-alpha)))
;;                                 "A")))))
;;               (fountain-add-scene-num current-scene-num)
;;               (setq prev-scene-num current-scene-num))))
;;         (fountain-forward-scene 1)))))

;; (defun fountain-align-scene-num ()
;;   "Align scene number to `fountain-align-scene-num'."
;;   (if (fountain-scene-heading-p)
;;       (let ((pos (point)))
;;         (forward-line 0)
;;         (if (and (looking-at fountain-scene-num-regexp)
;;                  (< pos (match-beginning 3)))
;;             (let* ((scene-heading-length
;;                     (string-width (match-string 1)))
;;                    (num-length
;;                     (string-width (match-string 3)))
;;                    (space-length
;;                     (- fountain-align-scene-num
;;                        scene-heading-length
;;                        num-length)))
;;               (if (< 1 space-length)
;;                   (replace-match (make-string space-length ?\s)
;;                                  nil nil nil 2)
;;                 (replace-match " " nil nil nil 2))))
;;         (goto-char pos))))

(defun fountain-font-lock-extend-region ()
  "Extend region for fontification to text block."
  (defvar font-lock-beg nil)
  (defvar font-lock-end nil)
  (let ((beg
         (save-excursion
           (goto-char font-lock-beg)
           (re-search-backward
            "^[\s\t]*$"
            (- (point) fountain-block-limit) 1)
           (point)))
        (end
         (save-excursion
           (goto-char font-lock-end)
           (re-search-forward
            "^[\s\t]*$"
            (+ (point) fountain-block-limit) 1)
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

;;; Outline ====================================================================

(defalias 'fountain-outline-next 'outline-next-visible-heading)
(defalias 'fountain-outline-previous 'outline-previous-visible-heading)
(defalias 'fountain-outline-forward 'outline-forward-same-level)
(defalias 'fountain-outline-backward 'outline-backward-same-level)
(defalias 'fountain-outline-up 'outline-up-heading)
(defalias 'fountain-outline-mark 'outline-mark-subtree)

(defun fountain-outline-shift-down (&optional n)
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
            ;; newline if none at eof
            (if (and (eobp)
                     (/= (char-before) ?\n))
                (insert-char ?\n))
            ;; temp newline if only 1 at eof
            (when (and (eobp)
                       (save-excursion
                         (forward-line -1)
                         (not (fountain-blank-p))))
              (insert-char ?\n)
              (setq hanging-line t))
            ;; avoid eobp signal
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
    (move-marker insert-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char insert-point)
    (if folded
        (hide-subtree))
    ;; remove temp newline
    (if hanging-line
        (save-excursion
          (goto-char (point-max))
          (delete-char -1)))
    (set-marker insert-point nil)))

(defun fountain-outline-shift-up (&optional n)
  (interactive "p")
  (fountain-outline-shift-down (- n)))

(defun fountain-outline-hide-level (n)
  (cond ((= n 0)
         (show-all)
         (message "Showing all"))
        ((= n 6)
         (hide-sublevels n)
         (message "Showing scene headings"))
        (t
         (hide-sublevels n)
         (message "Showing level %s headings" n)))
  (setq fountain-outline-cycle n))

(defun fountain-outline-cycle (&optional arg)
  "\\<fountain-mode-map>Cycle outline visibility of buffer or current subtree.

\t\\[fountain-outline-cycle]\t\t\t\t\tCycle outline visibility of current subtree and its children
\t\\[universal-argument] \\[fountain-outline-cycle]\t\t\t\tCycle outline visibility of buffer
\t\\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]\t\t\tShow all
\t\\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]\t\tShow outline visibility set in `fountain-outline-custom-level'"
  (interactive "p")
  (let* ((custom-level
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
            ((and custom-level
                  (= fountain-outline-cycle 1))
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
           (show-all)
           (message "Showing all")
           (setq fountain-outline-cycle 0))
          ((and custom-level
                (eq arg 64))
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
                 (show-entry)
                 (show-children)
                 (message "Showing headings")
                 (setq fountain-outline-cycle-subtree 2))
                ((or (<= eos eol)
                     (= fountain-outline-cycle-subtree 2))
                 (show-subtree)
                 (message "Showing contents")
                 (setq fountain-outline-cycle-subtree 3))
                (t
                 (hide-subtree)
                 (message "Hiding contents")
                 (setq fountain-outline-cycle-subtree 1)))))))))

(defun fountain-outline-cycle-global ()
  "Globally cycle outline visibility.

Calls `fountain-outline-cycle' with argument 4 wot cycle buffer
outline visibility through the following states:

\t1. top-level section headins
\t2. custom set with `fountain-outline-custom-level'
\t3. all sections and scene headings
\t4. everything"
  (interactive)
  (fountain-outline-cycle 4))

(defun fountain-outline-level ()
  "Return the depth to which a heading is nested in outline."
  (if (string-prefix-p "#" (match-string 0))
      (string-width (match-string 4)) ; FIXME use group 2 after export rewrite
    6))                              ; FIXME bobp? text outside outline?

;;; Export Internal Functions ==================================================

;; (defun fountain-export-parse-buffer ()
;;   ;; is the element fontified?
;;   (if (get-text-property (point) 'fontified)
;;       (let ((element (get-text-property (point) 'fountain-element)))
;;         (if element
;;             ;; is the element what font lock says it is?
;;             (unless (funcall (intern (concat "fountain-" element "-p")))
;;               (font-lock-fontify-region index limit))

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
              (setq n (1+ n))))
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

(defun fountain-export-get-metadata-value (key) ; FIXME combine with other
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

(defun fountain-export-filter (sub-s)   ; FIXME update doc
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

;;; Commands ===================================================================

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
         (p (if (<= i 0) -1 1))
         (move-fun
          (lambda ()
            (while (not (or (eq (point) (buffer-end p))
                            (fountain-scene-heading-p)))
              (forward-line p)))))
    (if (/= i 0)
        (while (/= i 0)
          (if (fountain-scene-heading-p)
              (forward-line p))
          (funcall move-fun)
          (setq i (- i p)))
      (forward-line 0)
      (funcall move-fun))))

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
  (when (outline-back-to-heading)
    (forward-line 1)
    (unless (bolp) (insert-char ?\n))
    (unless (and (fountain-blank-p)
                 (save-excursion
                   (forward-line 1)
                   (fountain-blank-p)))
      (save-excursion
        (insert-char ?\n)))
    (insert "= ")
    (if (outline-invisible-p) (fountain-outline-cycle))))

(defun fountain-insert-note (&optional arg)
  "Insert a note based on `fountain-note-template' underneath current element.
If prefixed with \\[universal-argument], only insert note delimiters (\"[[\" \"]]\")."
  (interactive "P")
  (let ((comment-start "[[")
        (comment-end "]]"))
    (if arg
        (comment-dwim nil)
      (unless (fountain-blank-p)
        (re-search-forward "^[\s\t]*$" nil 1))
      (unless (save-excursion
                (forward-line 1)
                (fountain-blank-p))
        (save-excursion
          (insert-char ?\n)))
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
            (s (concat "(" fountain-continued-dialog-string ")")) ; FIXME do not add "(" ")"
            ;; create progress report
            (job (make-progress-reporter "Refreshing continued dialog...")))
        ;; set START and END markers since buffer contents will change
        (set-marker start
                    (cond (arg (point-min))
                          ((use-region-p)
                           (region-beginning))
                          (t
                           (fountain-beginning-of-scene)
                           (point))))
        (set-marker end
                    (cond (arg (point-max))
                          ((use-region-p)
                           (region-end))
                          (t
                           (fountain-end-of-scene)
                           (point))))
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
                                  (fountain-get-character -1 'scene)))
              (re-search-forward "\s*$" (line-end-position) t)
              (replace-match (concat "\s" s)))
            (forward-line 1)
            (progress-reporter-update job)))
        (set-marker start nil)
        (set-marker end nil)
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
  (customize-set-variable 'fountain-switch-comment-syntax
                          (not fountain-switch-comment-syntax))
  (fountain-init-comment-syntax)
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
    (customize-set-variable option
                            (not (symbol-value option)))
    (if (symbol-value option)
        (add-to-invisibility-spec symbol)
      (remove-from-invisibility-spec symbol))
    (jit-lock-refontify)
    ;; (font-lock-fontify-block)
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
  (customize-set-variable 'fountain-align-elements
                          (not fountain-align-elements))
  (font-lock-refresh-defaults)
  (message "Elements are now displayed %s"
           (if fountain-align-elements
               "aligned" "non-aligned")))

(defun fountain-toggle-add-continued-dialog ()
  "Toggle `fountain-add-continued-dialog'"
  (interactive)
  (customize-set-variable 'fountain-add-continued-dialog
                          (not fountain-add-continued-dialog))
  (message "Continued dialog is now %s"
           (if fountain-add-continued-dialog
               "added" "removed")))

(defun fountain-toggle-export-include-title-page ()
  "Toggle `fountain-export-include-title-page'."
  (interactive)
  (customize-set-variable 'fountain-export-include-title-page
                          (not fountain-export-include-title-page))
  (message "Title page is now %s on export"
           (if fountain-export-include-title-page
               "included" "omitted")))

(defun fountain-set-font-lock-decoration (n)
  "Set `font-lock-maximum-decoration' for `fountain-mode' to N."
  (interactive "NMaximum decoration (1-3): ")
  (if (and (integerp n)
           (<= 1 n 3))
      (let ((level (cond ((= n 1) 1)
                         ((= n 2) nil)
                         ((= n 3) t)))
            (dec font-lock-maximum-decoration))
        (cond ((listp dec)
               (setq dec (assq-delete-all 'fountain-mode dec))
               (customize-set-variable 'font-lock-maximum-decoration
                                       (cons (cons 'fountain-mode level)
                                             dec)))
              ((or (booleanp dec)
                   (integerp dec))
               (customize-set-variable 'font-lock-maximum-decoration
                                       (list (cons 'fountain-mode level)
                                             (cons 't dec))))
              ((error "Malformed variable `font-lock-maximum-decoration'")))
        (message "Syntax highlighting is now set at %s"
                 (cond ((= n 1) "minimum")
                       ((= n 2) "default")
                       ((= n 3) "maximum")))
        (font-lock-refresh-defaults))
    (user-error "Decoration must be an integer 1-3")))

(defun fountain-get-font-lock-decoration ()
  "Return the value of `font-lock-maximum-decoration'."
  (let* ((dec font-lock-maximum-decoration)
         (n (if (listp dec)
                (if (assoc 'fountain-mode dec)
                    (cdr (assoc 'fountain-mode dec))
                  (cdr (assoc 't dec)))
              dec)))
    (cond ((null n) 2)
          ((eq n t) 3)
          ((integerp n) n)
          (t 2))))

(defun fountain-toggle-export-bold-scene-headings ()
  "Toggle `fountain-export-bold-scene-headings'"
  (interactive)
  (customize-set-variable 'fountain-export-bold-scene-headings
                          (not fountain-export-bold-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-bold-scene-headings
               "bold" "normal")))

(defun fountain-toggle-export-underline-scene-headings ()
  "Toggle `fountain-export-underline-scene-headings'"
  (interactive)
  (customize-set-variable 'fountain-export-underline-scene-headings
                          (not fountain-export-underline-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-underline-scene-headings
               "underlined" "normal")))

(defun fountain-toggle-export-double-space-scene-headings ()
  "Toggle `fountain-export-double-space-scene-headings'"
  (interactive)
  (customize-set-variable fountain-export-double-space-scene-headings
                          (not fountain-export-double-space-scene-headings))
  (message "Scene headings will now export %s"
           (if fountain-export-double-space-scene-headings
               "double-spaced" "single-spaced")))

(defun fountain-save-options ()
  (interactive)
  (let (unsaved)
    (dolist (opt '(fountain-switch-comment-syntax
                   fountain-hide-emphasis-delim
                   fountain-hide-syntax-chars
                   fountain-align-elements
                   fountain-add-continued-dialog
                   fountain-export-include-title-page
                   fountain-export-bold-scene-headings
                   fountain-export-underline-scene-headings
                   fountain-export-double-space-scene-headings
                   font-lock-maximum-decoration))
      (if (customize-mark-to-save opt)
          (setq unsaved t)))
    (if unsaved (custom-save-all))))

;;; Font Lock ==================================================================

(defvar fountain-font-lock-keywords-plist
  `(("note" ,fountain-note-regexp
     ((:level 2 :subexp 0 :invisible t)))
    ("scene-heading"
     (lambda (limit)
       (fountain-match-element 'fountain-scene-heading-p limit))
     ((:level 2 :subexp 0
              :override keep)
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("character"
     (lambda (limit)
       (fountain-match-element 'fountain-character-p limit))
     ((:level 3 :subexp 0)
      (:level 2 :subexp 2 :face fountain-comment ; FIXME maybe
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("dialog"
     (lambda (limit)
       (fountain-match-element 'fountain-dialog-p limit))
     ((:level 3 :subexp 0)))
    ("paren"
     (lambda (limit)
       (fountain-match-element 'fountain-paren-p limit))
     ((:level 3 :subexp 0)))
    ("trans"
     (lambda (limit)
       (fountain-match-element 'fountain-trans-p limit))
     ((:level 3 :subexp 0)
      (:level 1 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("forced-action-mark" ,fountain-forced-action-mark-regexp
     ((:level 1 :subexp 0 :face fountain-comment
              :invisible fountain-syntax-chars)))
    ("center" ,fountain-center-regexp
     ((:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)
      (:level 3 :subexp 3)
      (:level 2 :subexp 4 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("section" ,fountain-section-regexp
     ((:level 2 :subexp 3)
      (:level 2 :subexp 2 :face fountain-comment)))
    ("synopsis" ,fountain-synopsis-regexp
     ((:level 2 :subexp 0 :invisible t)
      (:level 2 :subexp 3 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("page-break" ,fountain-page-break-regexp
     ((:level 2 :subexp 0 :face fountain-page-break)))
    ("metadata"
     (lambda (limit)
       (fountain-match-element 'fountain-metadata-p limit))
     ((:level 2 :subexp 2 :face fountain-metadata-key
              :invisible t
              :laxmatch t)
      (:level 2 :subexp 3 :face fountain-metadata-value
              :invisible t
              :laxmatch t)
      (:level 2 :subexp 0 :face fountain-comment
              :invisible t
              :override keep)))
    (nil ,fountain-nbsp-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-syntax-chars)))
    (nil ,fountain-underline-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face underline)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-bold-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face bold)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-bold-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face bold-italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil ,fountain-lyrics-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face italic))))
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
    :laxmatch - as per `font-lock-keywords'

Regular expression should take the form:

    Group 1: whole string with trimmed whitespace
    Group 2: string intended for export
    Group 3: special
    Group 9: invisible characters")

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

(defun fountain-match-element (func limit)
  "If FUNC returns non-nil before LIMIT, return match data."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (if (funcall func)
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
    ;; (define-key map (kbd "C-c C-x #") 'fountain-add-scene-nums)
    (define-key map (kbd "C-c C-x f") 'fountain-set-font-lock-decoration)
    ;; navigation commands
    (define-key map (kbd "C-M-n") 'fountain-forward-scene)
    (define-key map (kbd "C-M-p") 'fountain-backward-scene)
    (define-key map (kbd "C-M-a") 'fountain-beginning-of-scene)
    (define-key map (kbd "C-M-e") 'fountain-end-of-scene)
    (define-key map (kbd "C-M-h") 'fountain-mark-scene)
    (define-key map (kbd "M-n") 'fountain-forward-character)
    (define-key map (kbd "M-p") 'fountain-backward-character)
    ;; outline commands
    (define-key map (kbd "C-c C-n") 'fountain-outline-next)
    (define-key map (kbd "C-c C-p") 'fountain-outline-previous)
    (define-key map (kbd "C-c C-f") 'fountain-outline-forward)
    (define-key map (kbd "C-c C-b") 'fountain-outline-backward)
    (define-key map (kbd "C-c C-u") 'fountain-outline-up)
    (define-key map (kbd "C-c C-^") 'fountain-outline-shift-up)
    (define-key map (kbd "C-c C-v") 'fountain-outline-shift-down)
    (define-key map (kbd "C-c C-SPC") 'fountain-outline-mark)
    (define-key map (kbd "TAB") 'fountain-outline-cycle)
    (define-key map (kbd "<backtab>") 'fountain-outline-cycle-global)
    (define-key map (kbd "S-TAB") 'fountain-outline-cycle-global)
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
    ("Navigate"
     ["Next Scene Heading" fountain-forward-scene]
     ["Previous Scene Heading" fountain-backward-scene]
     "---"
     ["Next Character" fountain-forward-character]
     ["Previous Character" fountain-backward-character])
    "---"
    ("Outline"
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
    "---"
    ["Insert Metadata" fountain-insert-metadata]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    ["Add/Remove Continued Dialog" fountain-continued-dialog-refresh]
    ;; ["Add Scene Numbers" fountain-add-scene-nums]
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
      :selected (= (fountain-get-font-lock-decoration) 3)])
    ("Show/Hide"
     ["Hide Emphasis Delimiters" fountain-toggle-hide-emphasis-delim
      :style toggle
      :selected fountain-hide-emphasis-delim]
     ["Hide Syntax Characters" fountain-toggle-hide-syntax-chars
      :style toggle
      :selected fountain-hide-syntax-chars])
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
     ["Customize Export" (customize-group 'fountain-export)])
    "---"
    ["Save Options" fountain-save-options]
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
  (fountain-init-regexp)
  (fountain-init-comment-syntax)
  (setq comment-use-syntax t)
  (setq font-lock-defaults
        '(fountain-create-font-lock-keywords nil t))
  (add-hook 'font-lock-extend-region-functions
            'fountain-font-lock-extend-region t t)
  (add-to-invisibility-spec '(outline . t))
  (if fountain-hide-emphasis-delim
      (add-to-invisibility-spec 'fountain-emphasis-delim))
  (if fountain-hide-syntax-chars
      (add-to-invisibility-spec 'fountain-syntax))
  (fountain-read-metadata)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local font-lock-comment-face 'fountain-comment)
  (setq-local outline-level 'fountain-outline-level)
  (setq-local fountain-outline-startup-level
              (let ((n (fountain-get-metadata-value "startup-level")))
                (if (s-present? n)
                    (string-to-number n)
                  fountain-outline-startup-level)))
  (setq-local font-lock-extra-managed-props
              '(line-prefix wrap-prefix invisible fountain-element))
  ;; (add-hook 'post-self-insert-hook
  ;;           'fountain-align-scene-num t t)
  ;; (add-hook 'after-save-hook
  ;;           'fountain-read-metadata)
  (fountain-outline-hide-level fountain-outline-startup-level))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
