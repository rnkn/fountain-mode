;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4.0") (s "1.9.0"))
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
;; - Integration with `imenu` (Sections, Scene Headings, Notes)
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

;; - Emacs 24.4
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

;; Roadmap
;; -------

;; See [Milestones][] on GitHub.

;; [milestones]: https://github.com/rnkn/fountain-mode/milestones "Fountain Mode milestones"

;; History
;; -------

;; See [Releases][] on GitHub.

;; [releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"

;;; Code:

(defconst fountain-version
  "2.0.0")

;;; Requirements ===============================================================

(require 's)
(require 'easymenu)
(require 'outline)

;;; Group Definitions ==========================================================

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :group 'wp
  :link '(url-link "https://github.com/rnkn/fountain-mode"))

(defgroup fountain-faces ()
  "Faces used in `fountain-mode'.
There are three levels of Font Lock decoration:

    1: minimum: highlights comments and syntax characters

    2: default: highlights comments, metadata, scene headings,
       sections, synopses, notes and syntax characters

    3: maximum: highlights comments, metadata keys, metadata
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

(defgroup fountain-export ()
  "Options for exporting Fountain files."
  :prefix "fountain-export-"
  :group 'fountain)

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

;;; Obsolete Warnings ==========================================================

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

(define-obsolete-face-alias 'fountain-section
  'fountain-section-heading "1.4.1")

(make-obsolete 'fountain-export-title-page-left-template
               'fountain-export-contact-template "2.0.0")

(make-obsolete 'fountain-export-title-page-right-template
               'fountain-export-contact-template "2.0.0")

(make-obsolete 'fountain-export-buffer-to-pdf-via-html
               'fountain-export-to-latex "2.0.0")

(make-obsolete 'fountain-export-pdf-via-html-command
               'fountain-export-shell-command "2.0.0")

;;; Customization ==============================================================

;;;; General Customization =====================================================

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

;;;; Align Group Customization =================================================

(defcustom fountain-align-elements
  t
  "If non-nil, elements will be displayed auto-aligned.
This option does not affect file contents."
  :type 'boolean
  :group 'fountain-align)

(defcustom fountain-align-section-heading
  '(("screenplay" 0) ("stageplay" 30))
  "Column integer to which section headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-scene-heading
  '(("screenplay" 0) ("stageplay" 30))
  "Column integer to which scene headings should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-synopsis
  '(("screenplay" 0) ("stageplay" 30))
  "Column integer to which synopses should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-action
  '(("screenplay" 0) ("stageplay" 20))
  "Column integer to which action should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-character
  '(("screenplay" 20) ("stageplay" 30))
  "Column integer to which characters names should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-dialog
  '(("screenplay" 10) ("stageplay" 0))
  "Column integer to which dialog should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-paren
  '(("screenplay" 15) ("stageplay" 20))
  "Column integer to which parentheticals should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-trans
  '(("screenplay" 45) ("stageplay" 30))
  "Column integer to which transitions should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-center
  '(("screenplay" 20) ("stageplay" 20))
  "Column integer to which centered text should be aligned.

This option does not affect file contents."
  :type '(choice integer
                 (repeat (group (string :tag "Format") integer)))
  :group 'fountain-align)

(defcustom fountain-align-scene-number
  8
  "Column integer from right margin to which scene numbers should be aligned.

If nil, do not align scene numbers.

This option does affect file contents."
  :type '(choice (const :tag "Do not align scene numbers" nil)
                 (integer 8))
  :group 'fountain-align)

;;;; Export Group Customization ================================================

(defcustom fountain-export-include-elements-alist
  '(("screenplay" scene-heading action character paren lines trans center)
    ("stageplay" section-heading scene-heading action character paren lines trans center))
  "Association list of elements to include when exporting.
Note that comments (boneyard) are never included."
  :type '(alist :key-type (string :tag "Format")
                :value-type (set :tag "Elements"
                                 (const :tag "Section Headings" section-heading)
                                 (const :tag "Scene Headings" scene-heading)
                                 (const :tag "Action" action)
                                 (const :tag "Character Names" character)
                                 (const :tag "Parentheticals" paren)
                                 (const :tag "Dialog" lines)
                                 (const :tag "Transitions" trans)
                                 (const :tag "Center Text" center)
                                 (const :tag "Synopses" synopsis)
                                 (const :tag "Notes" note)))
  :group 'fountain-export)

(defcustom fountain-export-standalone
  t
  "If non-nil, export a standalone document.
Otherwise export a snippet."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-buffer-name
  "*Fountain %s Export*"
  "Name of export buffer when source is not visiting a file.
Passed to `format' with export format as single variable."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-default-command
  'fountain-export-buffer-to-html
  "\\<fountain-mode-map>Default function to call with `fountain-export-default' \(\\[fountain-export-default]\)."
  :type '(radio (function-item fountain-export-buffer-to-html)
                (function-item fountain-export-shell-command))
  :group 'fountain-export)

;; (defcustom fountain-export-include-title-page
;;   t
;;   "Generate a title page on export."
;;   :type 'boolean
;;   :group 'fountain-export)

(defcustom fountain-export-html-inline-style
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

;; (defcustom fountain-export-action-orphans
;;   2
;;   "Number of allowable action orphan lines.
;; When breaking action across pages, this integer is the minimum
;; number of lines on the previous page."
;;   :type 'integer
;;   :group 'fountain-export)

;; (defcustom fountain-export-action-widows
;;   2
;;   "Number of allowable action widow lines.
;; When breaking action across pages, this integer is the minimum
;; number of lines on the following page."
;;   :type 'integer
;;   :group 'fountain-export)

;; (defcustom fountain-export-dialog-orphans
;;   2
;;   "Number of allowable dialog orphan lines.
;; When breaking dialog across pages, this integer is the minimum
;; number of lines on the previous page."
;;   :type 'integer
;;   :group 'fountain-export)

;; (defcustom fountain-export-dialog-widows
;;   2
;;   "Number of allowable dialog widow lines.
;; When breaking dialog across pages, this integer is the minimum
;; number of lines on the following page."
;;   :type 'integer
;;   :group 'fountain-export)

(defcustom fountain-export-more-dialog-string
  "(MORE)"
  "String to append to dialog when breaking across pages.
Parentheses are not automatically added."
  :type 'string
  :group 'fountain-export)

;; (defcustom fountain-export-preserve-line-breaks
;;   t
;;   "If non-nil, convert all newlines into line breaks.
;; Otherwise, only break paragraphs at explicit line breaks (one or
;; more blank lines)."
;;   :type 'boolean
;;   :group 'fountain-export)

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
\"%s\" will be substituted with `buffer-file-name'"
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-title-template
  "\
${title}
written by
${author}"
  "Template for creating title page title block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-contact-template
  "\
${author}"
  "Template for creating title page left block."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-use-title-as-filename
  nil
  "If non-nil, use title metadata as export filename."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-html-use-inline-style
  t
  "If non-nil, use inline stylesheet.
Otherwise, use an external stylesheet file."
  :type 'boolean
  :group 'fountain-export)

(defcustom fountain-export-html-style-template
  "\
@page screenplay, screenplay-title {
  size: ${html-page-size};
  margin-top: 1in;
  margin-right: 1in;
  margin-bottom: 0.75in;
  margin-left: 1.5in;
}
@page screenplay {
  @top-right-corner {
    font-family: ${html-font};
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
  font-family: \"Courier Prime\", \"Courier\", \"Courier Final Draft\", \"Courier New\";
  font-size: 12pt;
  line-height: 1;
  width: 6in;
  margin: 1em auto;
  -webkit-text-size-adjust: none;
}
.screenplay .title-page {
  page: screenplay-title;
  page-break-after: always;
  width: 6in;
  margin-top: 0in;
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
  text-transform: uppercase;
  text-decoration: underline;
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
.screenplay .scene {
  margin-top: ${html-scene-spacing};
  width: auto;
}
.screenplay .scene-heading {
  font-weight: ${html-scene-bold};
  text-decoration: ${html-scene-underline};
  margin-bottom: 0em;
  page-break-after: avoid;
}
.screenplay .action {
  margin: 1em 0in;
  white-space: pre-wrap;
  orphans: 2;
  widows: 2;
}
.screenplay .dialog {
  display: table;
  width: 4in;
  margin-top: 1em;
  margin-bottom: 1em;
  margin-left: 1.5in;
}
.screenplay .dialog .character {
  margin-top: 0em;
  margin-bottom: 0em;
  margin-left: 1in;
}
.screenplay .dialog .lines {
  width: 3.5in;
  margin-top: 0em;
  margin-bottom: 0em;
  orphans: 2;
  widows: 2;
}
.screenplay .dialog .paren {
  width: 2in;
  margin-top: 0em;
  margin-bottom: 0em;
  margin-left: 0.6in;
  text-indent: -0.6em;
  page-break-inside: avoid;
  page-break-after: avoid;
}
.screenplay .dialog.dual {
  width: 3in;
}
.screenplay .dialog.dual .lines {
  width: 2.9in;
}
.screenplay .dialog.dual.left {
  margin-top: 0em;
  margin-left: 0in;
  float: left;
}
.screenplay .dialog.dual.right {
  clear: none;
}
.screenplay .trans {
  margin-left: 4in;
  width: 2in;
  page-break-before: avoid;
}
.screenplay .note {
  display: block;
  font-size: 11pt;
  font-family: \"Comic Sans MS\";
  line-height: 1.5;
  background-color: lightgoldenrodyellow;
  padding: 1em;
}
.screenplay .synopsis {
  display: block;
  margin-top: 0em;
  color: grey;
  font-style: italic;
}
.screenplay .center {
  text-align: center;
  margin-left: 0in;
  width: 100%;
  white-space: pre-wrap;
}
.screenplay .underline {
  text-decoration: underline;
}
.screenplay .section-heading {
  display: none;
}
"
"Style template for HTML export."
  :type 'string
  :group 'fountain-export)

(defcustom fountain-export-format-replace-alist
  '((html
     ("&" "&amp;")
     ("<" "&lt;")
     (">" "&gt;")
     ("\\\\\s" "&nbsp;")
     ("^\\\\$" "<br>")
     ("\\\\_" "&#95;")
     ("\\\\\\*" "&#42;")
     ("\\\\`" "&#96;")
     ("\\\\'" "&apos;")
     ("``" "&ldquo;")
     ("''" "&rdquo;")
     ("`" "&lsquo;")
     ("'" "&rsquo;")
     ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "<strong><em>\\1</em></strong>")
     ("\\*\\*\\(.+?\\)\\*\\*" "<strong>\\1</strong>")
     ("\\*\\(.+?\\)\\*" "<em>\\1</em>")
     ("^~\s*\\(.+?\\)$\\*\\*" "<i>\\1</i>")
     ("_\\(.+?\\)_" "<span class=\"underline\">\\1</span>")
     ("\n" "<br>"))
    (tex
     ("%" "\\\\%")
     ("\\$" "\\\\$")
     ("\\*\\*\\*\\(.+?\\)\\*\\*\\*" "\\\\textbf{\\\\emph{\\1}}")
     ("\\*\\*\\(.+?\\)\\*\\*" "\\\\textbf{\\1}")
     ("\\*\\(.+?\\)\\*" "\\\\emph{\\1}")
     ("^~\s*\\(.+?\\)$\\*\\*" "\\\\textit{\\1}")
     ("_\\(.+?\\)_" "\\\\uline{\\1}")
     ("\n" "\n\n")))
  "Association list of regular expression export replacements.
Replacements are made in sequential order. The sequence is
important: first, characters that are special in the export
format are sanitized, then escaped characters are converted to
character codes, then format replacement is made."
  :type '(alist :key-type (symbol :tag "Format")
                :value-type (repeat (group regexp (string :tag "Replacement")))))

(defcustom fountain-export-template-replace-functions
  '(("html-dialog-class"
     fountain-export-html-dialog-class)
    ("fountain-scene-heading-forced"
     fountain-export-fountain-forced-scene-heading)
    ("fountain-action-forced"
     fountain-export-fountain-forced-action)
    ("fountain-character-forced"
     fountain-export-fountain-forced-character)
    ("fountain-trans-forced"
     fountain-export-fountain-forced-trans))
  "Association list of replacement functions for formatting templates.
FUNCTION is called with one argument, the property list of the
format element (see `fountain-parse-element'), and must return a
string.

When exporting, this list take precedence over
`fountain-generic-template-replace-functions'"
  :type '(repeat (group string function))
  :group 'fountain-export)

(defcustom fountain-generic-template-replace-functions
  '(("emacs-version"
     (lambda () emacs-version))
    ("fountain-version"
     (lambda () fountain-version))
    ("title"
     (lambda ()
       (or (plist-get (fountain-read-metadata)
                      'title)
           (file-base-name (buffer-name)))))
    ("author"
     (lambda ()
       (plist-get (fountain-read-metadata)
                  'author)))
    ("email"
     (lambda () user-mail-address))
    ("date"
     (lambda ()
       (format-time-string "%F")))
    ("html-page-size"
     (lambda () fountain-export-page-size))
    ("html-font"
     (lambda ()
       (mapconcat
        (lambda (font) (concat "'" font "'"))
        fountain-export-font ", ")))
    ("html-scene-bold"
     (lambda ()
       (if (memq 'bold
                 fountain-export-scene-heading-format)
           "bold" "normal")))
    ("html-scene-spacing"
     (lambda ()
       (if (memq 'double-space
                 fountain-export-scene-heading-format)
           "2em" "1em")))
    ("html-scene-underline"
     (lambda ()
       (if (memq 'underline
                 fountain-export-scene-heading-format)
           "underline" "none")))
    ("html-style" fountain-export-html-create-style))
  "Association list of replacement funcations for formatting templates.
FUNCTION is called no arguments and must return a string.

This list is used for making template replacements in-buffer and
when exporting, but `fountain-export-template-replace-functions'
takes precedence when exporting."
  :type '(repeat (group string function))
  :group 'fountain)

(defcustom fountain-export-templates
  '((html
     (document "\
<head>
<meta charset=\"utf-8\">
<meta name=\"author\" content=\"${author}\" />
<meta name=\"generator\" content=\"Emacs ${emacs-version} running Fountain Mode ${fountain-version}\" />
<title>${title}</title>
${html-style}
</head>
<body>
<section class=\"screenplay\">
${content}\
</section>
</body>
")
     (section "<section class=\"section\">\n${content}</section>\n")
     (section-heading "<h1 class=\"section-heading\">${content}</h1>\n")
     (scene "<section class=\"scene\">\n${content}</section>\n")
     (scene-heading "<h2 class=\"scene-heading\" id=\"${scene-number}\">${content}</h2>\n")
     (dialog "<div class=\"${html-dialog-class}\">\n${content}</div>\n")
     (character "<p class=\"character\">${content}</p>\n")
     (paren "<p class=\"paren\">${content}</p>\n")
     (lines "<p class=\"lines\">${content}</p>\n")
     (trans "<p class=\"trans\">${content}</p>\n")
     (action "<p class=\"action\">${content}</p>\n")
     (synopsis "<p class=\"synopsis\">${content}</p>\n")
     (note "<p class=\"note\">${content}</p>\n")
     (center "<p class=\"center\">${content}</p>\n"))
    (tex
     (document "\
\\documentclass[12pt,letterpaper]{article}
\\usepackage[left=1.25in,top=1in,bottom=0.75in,text=6in]{geometry}
\\usepackage{fontspec}
\\setmonofont{Courier Prime}
\\renewcommand{\\familydefault}{\\ttdefault}
\\usepackage{fancyhdr}
\\usepackage{atbegshi}
\\usepackage[normalem]{ulem}
\\renewcommand{\\ULthickness}{1pt}

\\pagestyle{fancy}
\\fancyhf{}
\\fancyhead[R]{\\thepage.}
\\renewcommand{\\headrulewidth}{0pt}

\\setlength{\\parskip}{12pt plus 0pt minus 0pt}
\\linespread{0.82}
\\hyphenpenalty=10000
\\widowpenalty=10000
\\clubpenalty=10000
\\frenchspacing
\\raggedright
\\hfuzz=1in

\\usepackage[style=english]{csquotes}
\\MakeOuterQuote{\"}

\\newif\\ifVSsceneheading
\\VSsceneheadingtrue
\\newif\\ifULsceneheading
\\ULsceneheadingfalse
\\newif\\ifBFsceneheading
\\BFsceneheadingfalse

\\newcommand*{\\sceneheading}[1]{%
\\def\\thesceneheading{#1}
\\ifVSsceneheading
\\vspace{\\parskip}
\\fi
\\ifBFsceneheading
\\let\\BFtmp\\thesceneheading
\\renewcommand{\\thesceneheading}{\\textbf{\\BFtmp}}
\\fi
\\ifULsceneheading
\\let\\ULtmp\\thesceneheading
\\renewcommand{\\thesceneheading}{\\uline{\\ULtmp}}
\\fi
\\thesceneheading\\nopagebreak[4]%
}

\\newcommand{\\contd}{(CONT'D)}
\\newcommand{\\more}{(MORE)}

\\newcommand*{\\character}[1]{%
\\hspace*{1.2in}\\parbox[t]{4in}{#1}\\parskip=0pt%
}

\\newenvironment*{dialog}[1]{%
\\leftskip=1.5in%
\\character{#1}\\mark{#1}\\par\\nopagebreak[4]%
\\rightskip=2in
}{%
\\par\\mark{\\empty}%
}

\\newcommand*{\\paren}[1]{%
\\parskip=0pt\\par%
\\hspace*{0.5in}\\parbox[t]{2in}{%
\\hangindent=0.1in\\hangafter=1#1}\\par\\nopagebreak[4]
\\vspace{2pt}%
}

\\newcommand*{\\trans}[1]{%
\\nopagebreak[4]\\hspace*{4in}\\parbox[t]{2in}{#1}%
}

\\AtBeginShipout{%
\\if\\botmark\\empty
\\else
\\character{\\botmark{}~\\contd}%
\\fi
}

\\title{${title}}
\\author{${author}}
\\date{${date}}

\\begin{document}
\\thispagestyle{empty}
${content}\
\\end{document}
")
     (section nil)
     (section-heading nil)
     (scene nil)
     (scene-heading "\\sceneheading{${content}}\n\n")
     (dialog "\\begin{dialog}${content}\n\\end{dialog}\n\n")
     (character "{${content}}\n")
     (paren "\\paren{${content}}\n")
     (lines "${content}\n")
     (trans "\\trans{${content}}\n\n")
     (action "${content}\n\n")
     (synopsis "")
     (note "")
     (center "\\center{${content}}\n\n"))
    (fdx
     (document "\
<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<FinalDraft DocumentType=\"Script\">
<Content>
${content}\
</Content>
</FinalDraft>
")
     (section nil)
     (section-heading nil)
     (scene nil)
     (scene-heading "<Paragraph Number=\"${scene-number}\" Type=\"Scene Heading\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (dialog nil)
     (character "<Paragraph Type=\"Character\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (paren "<Paragraph Type=\"Parenthetical\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (lines "<Paragraph Type=\"Dialogue\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (trans "<Paragraph Type=\"Transition\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (action "<Paragraph Type=\"Action\">\n<Text>${content}</Text>\n</Paragraph>\n")
     (synopsis "")
     (note "")
     (center "<Paragraph Alignment=\"Center\" Type=\"Action\">\n<Text>${content}</Text>\n</Paragraph>\n"))
    (fountain
     (document "\
title: ${title}
credit: ${credit}
author: ${author}
date: ${date}

${content}\
")
     (section "${content}")
     (section-heading "${content}\n\n")
     (scene "${content}")
     (scene-heading "${fountain-scene-heading-forced}${content}\n\n")
     (dialog "${content}")
     (character "${fountain-character-forced}${content}\n")
     (paren "${content}\n")
     (lines "${content}\n\n")
     (trans "${fountain-trans-forced} ${content}\n\n")
     (action "${fountain-action-forced}${content}\n\n")
     (synopsis "= ${content}\n\n")
     (note "[[ ${content} ]]\n\n")
     (center "> ${content} <")))
  "Association list of templates for each Fountain element.
Takes the form:

    (FORMAT (TYPE TEMPLATE) ...)

FORMAT is the export format, a symbol. TYPE is the Fountain
element, a symbol (see below). TEMPLATE is the template with
which to format the format string. If TEMPLATE is nil, the format
string is passed without formatting, whereas an empty string
discards the format string and passes the empty string.

Fountain element TYPES:

    document            wrapper template for all content, see
                        `fountain-export-standalone'
    section             string of section, including child elements
    section-heading     string of section heading, excluding syntax chars
    scene               string of scene, including child elements
    scene-heading       string of scene heading, excluing  syntax chars
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

If a TYPE is not included, its TEMPLATE is treated as nil.

The format of TEMPLATE can include replacement keys in the form
\"${key}\". Each TEMPLATE should include the \"${content}\" key,
which will be replaced with the format string. Replacements are
calculated in the following order:

    1. ${content} is replaced with the format string.
    2. If KEY corresponds to a property in the element's property list, and that
       property is a string, ${KEY} is replaced with that string. See
       `fountain-parse-element' for details on Fountain element property lists.
    3. If KEY corresponds to a function in `fountain-export-template-replace-functions'
       then ${KEY} is replaced with the result of that function.
    4. If KEY corresponds to a function in `fountain-generic-template-replace-functions'
       then ${KEY} is replace with the result of that function.
    5. If none of the above, ${KEY} is replaced with an empty string."
  :type '(alist :key-type (choice :tag "Format"
                                  (const :tag "HTML" html)
                                  (const :tag "LaTeX" tex)
                                  (const :tag "Final Draft" fdx)
                                  (const :tag "Fountain" fountain)
                                  (symbol :tag "Custom"))
                :value-type (group
                             (group (const :tag "Document" document) (choice string (const nil)))
                             (group (const :tag "Section" section) (choice string (const nil)))
                             (group (const :tag "Section Heading" section-heading) (choice string (const nil)))
                             (group (const :tag "Scene" scene) (choice string (const nil)))
                             (group (const :tag "Scene Heading" scene-heading) (choice string (const nil)))
                             (group (const :tag "Dialogue" dialog) (choice string (const nil)))
                             (group (const :tag "Character" character) (choice string (const nil)))
                             (group (const :tag "Parenthetical" paren) (choice string (const nil)))
                             (group (const :tag "Dialogue Lines" lines) (choice string (const nil)))
                             (group (const :tag "Transition" trans) (choice string (const nil)))
                             (group (const :tag "Action" action) (choice string (const nil)))
                             (group (const :tag "Synopsis" synopsis) (choice string (const nil)))
                             (group (const :tag "Note" note) (choice string (const nil)))
                             (group (const :tag "Center Text" center) (choice string (const nil)))))
  :group 'fountain-export)

;;; Variables ==================================================================
;;;; Buffer Local Variables ====================================================
(defvar-local fountain-metadata
  nil
  "Metadata alist in the form of (KEY . VALUE).
Set with `fountain-read-metadata' upon calling `fountain-mode'.")

(defvar-local fountain-outline-cycle
  0
  "Integer representing global outline cycling status.

    0:  Show all
    1:  Show level 1 section headings
    2:  Show level 2 section headings
    3:  Show level 3 section headings
    4:  Show level 4 section headings
    5:  Show level 5 section headings
    6:  Show scene headings

Used by `fountain-outline-cycle'.")

(defvar-local fountain-outline-cycle-subtree
  0
  "Integer representing subtree outline cycling status.
Used by `fountain-outline-cycle'.")

;;;; Regular Expression Variables ==============================================

(defvar fountain-scene-heading-regexp
  nil
  "Regular expression for matching scene headings.
Set with `fountain-init-scene-heading-regexp'.

    Group 1:    match trimmed whitespace
    Group 2:    match leading . (for forced element)
    Group 3:    match scene heading without scene number (for export)
    Group 4:    match space between scene heading and scene number
    Group 5:    match scene number with # prefix
    Group 6:    match scene number

Requires `fountain-scene-heading-p' for preceding blank line.")

(defconst fountain-forced-scene-heading-regexp
  "\\(?2:\\.\\)\\(?3:\\<.*?\\)"
  "Regular expression for matching forced scene headings.
Requires `fountain-scene-heading-p' for preceding blank line.")

(defconst fountain-scene-number-regexp
  "\\(?:\\(?4:[\s\t]+\\)\\(?:#\\(?5:[a-z]*[0-9]+[a-z]*\\)\\)\\)?"
  "Regular expression for matching scene numbers.
Assumes line matches `fountain-scene-heading-p'.")

(defvar fountain-trans-regexp
  nil
  "Regular expression for matching transitions.
Set with `fountain-init-trans-regexp'. Requires
`fountain-trans-p' for preceding and succeeding blank lines.")

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
  (concat "^\\(?1:\\(?2:\\<[^;:'\",?()\\\n]+\\):[\s\t]*\\(?3:.+\\)?\\)"
          "\\|"
          "^\s+\\(?1:\\(?3:.+\\)\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-metadata-p' for `bobp'.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*\\(?1:\\(?:"
          "\\(?2:@\\)\\(?3:\\(?4:[^<>\n]+?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\|"
          "\\(?3:\\(?4:[A-Z][^<>a-z\n]*?\\)\\(?:[\s\t]*(.*?)\\)*?\\)"
          "\\)[\s\t]*\\(?5:\\^\\)?\\)[\s\t]*$")
  "Regular expression for matching character names.

    Group 1:    match trimmed whitespace
    Group 2:    match leading @ (for forced element)
    Group 3:    match character name and parenthetical (export group)
    Group 4:    match character name only
    Group 5:    match trailing ^ (for dual dialog)

Requires `fountain-character-p'.")

(defconst fountain-paren-regexp
  "^[\s\t]*\\(?3:([^)\n]*)\\)[\s\t]*$"
  "Regular expression for matching parentheticals.
Requires `fountain-paren-p' for preceding character or dialog.")

(defconst fountain-action-regexp
  (concat "\\(.\\|\n\\)+?\n" fountain-blank-regexp)
  "Regular expression for matching action.")

(defconst fountain-page-break-regexp
  "^[\s\t]*=\\{3,\\}.*"
  "Regular expression for matching page breaks.")

(defconst fountain-note-regexp
  "\\(\\[\\[[\s\t]*\\(?3:\\(?:.\n?\\)*?\\)[\s\t]*]]\\)"
  "Regular expression for matching notes.")

(defconst fountain-section-heading-regexp
  "^\\(?1:\\(?2:#\\{1,5\\}\\)[\s\t]*\\(?3:[^#\n].*?\\)\\)[\s\t]*$"
  "Regular expression for matching section headings.")

(defconst fountain-synopsis-regexp
  "^\\(\\(?2:=[\s\t]*\\)\\(?3:[^=\n].*?\\)\\)[\s\t]*$"
  "Regular expression for matching synopses.")

(defconst fountain-center-regexp
  "^[\s\t]*\\(?1:\\(?2:>[\s\t]*\\)\\(?3:.*?\\)\\(?4:[\s\t]*<\\)\\)[\s\t]*$"
  "Regular expression for matching centered text.")

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
  "\\${\\([^\s\t\n()\"'`;]*?\\)}"
  "Regular expression key for making template replacements.")

;;; Faces ======================================================================

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
  '((t (:inherit font-lock-type-face)))
  "Default face for metadata keys."
  :group 'fountain-faces)

(defface fountain-metadata-value
  '((t (:inherit font-lock-constant-face)))
  "Default face for metadata values."
  :group 'fountain-faces)

(defface fountain-page-break
  '((t (:inherit fountain-comment)))
  "Default face for page breaks."
  :group 'fountain-faces)

(defface fountain-scene-heading
  '((t (:inherit font-lock-keyword-face)))
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
  '((t (:inherit font-lock-function-name-face)))
  "Default face for section headings."
  :group 'fountain-faces)

(defface fountain-synopsis
  '((t (:inherit font-lock-preprocessor-face)))
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

;;; Functions ==================================================================
;;;; Initializing Functions ====================================================

(defun fountain-init-scene-heading-regexp ()
  "Initializes `fountain-scene-heading-regexp'."
  (setq fountain-scene-heading-regexp
        (concat "^\\(?1:"
                fountain-forced-scene-heading-regexp
                fountain-scene-number-regexp
                "\\)[\s\t]*$"
                "\\|"
                "^\\(?1:\\(?3:"
                (regexp-opt fountain-scene-heading-prefix-list)
                "[.\s\t].*?\\)"
                fountain-scene-number-regexp
                "\\)[\s\t]*$")))

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
              (concat fountain-section-heading-regexp
                      "\\|"
                      fountain-scene-heading-regexp)))

(defun fountain-init-imenu-generic-expression ()
  "Initializes `imenu-generic-expression'."
  (setq imenu-generic-expression
        (list
         (list "Notes" fountain-note-regexp 3)
         (list "Scene Headings" fountain-scene-heading-regexp 1)
         (list "Sections" fountain-section-heading-regexp 1))))

(defun fountain-init-regexp ()
  "Set variable regular expression values."
  (fountain-init-scene-heading-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-imenu-generic-expression))

(defun fountain-init-comment-syntax ()
  "Set comment syntax according to `fountain-switch-comment-syntax'."
  (setq-local comment-start
              (if fountain-switch-comment-syntax "//" "/*"))
  (setq-local comment-end
              (if fountain-switch-comment-syntax "" "*/")))

(defun fountain-init-vars ()
  "Initializes important variables."
  (fountain-init-scene-heading-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-comment-syntax)
  (setq-local comment-use-syntax t)
  (setq-local outline-level 'fountain-outline-level)
  (setq-local require-final-newline mode-require-final-newline))

;;;; Element Functions =========================================================

(defun fountain-blank-p ()
  "Return non-nil if point is at a blank line."
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

(defun fountain-section-heading-p ()
  "Match section heading if point is at section heading, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-section-heading-regexp))))

(defun fountain-synopsis-p ()
  "Match synopsis if point is at synopsis, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (looking-at fountain-synopsis-regexp))))

(defun fountain-note-p ()
  "Match note if point is at a note, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (or (looking-at fountain-note-regexp)
          (let ((pos (point)))
            (if (re-search-backward "^[\s\t]*\n" nil 'move)
                (goto-char (match-end 0)))
            (and (looking-at fountain-note-regexp)
                 (< pos (match-end 0))))))))

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
      (fountain-comment-p)
      (fountain-section-heading-p) ; FIXME: what about stageplays?
      (fountain-synopsis-p)
      (fountain-note-p)))

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
        (and (looking-at "[\s\t]*\\(?1:\\(?3:[^<>\n]+?\\)\\)[\s\t]*$")
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

(defun fountain-action-p ()
  "Match action text if point is at action, nil otherwise."
  (unless (or (fountain-tachyon-p)
              (fountain-metadata-p)
              (fountain-section-heading-p)
              (fountain-scene-heading-p)
              (fountain-character-p)
              (fountain-dialog-p)
              (fountain-paren-p)
              (fountain-trans-p)
              (fountain-center-p)
              (fountain-synopsis-p)
              (fountain-note-p))
    (save-excursion
      (save-restriction
        (widen)
        (forward-line 0)
        (looking-at ".*")))))

;;;; Reading Functions =========================================================

(defun fountain-read-metadata ()
  "Read metadata of current buffer and return as a property list.

Key string is converted to lowercase, spaces are converted to
dashes, and then interned.

    \"Draft date: 2015-12-25\" -> (draft-date \"2015-12-25\")

Value string remains a string."
  (let (list)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (fountain-metadata-p)
          (let ((key (intern (replace-regexp-in-string
                              "\s" "-"
                              (downcase (match-string 2)))))
                (value (match-string-no-properties 3)))
            (forward-line 1)
            (while (and (fountain-metadata-p)
                        (null (match-string 2)))
              (setq value (concat value (if value "\n")
                                  (match-string-no-properties 3)))
              (forward-line 1))
            (setq list (append list (list key value)))))
        (skip-chars-forward "\n\s\t")
        (setq list (append list (list 'content-start (point))))))
    list))

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

Optionally, use \"$@\" to set the `mark' and \"$?\" to set the
`point', but only use one of each."
  (let ((start (point)))
    (insert (s-format template 'aget
                      `(("title" . ,(file-name-base (buffer-name)))
                        ("longtime" . ,(format-time-string fountain-long-time-format))
                        ("time" . ,(format-time-string fountain-short-time-format))
                        ("fullname" . ,user-full-name)
                        ("nick" . ,(capitalize user-login-name))
                        ("email" . ,user-mail-address))))
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

(defun fountain-get-scene-number ()
  "Return the scene number of current scene."
  (if (fountain-scene-heading-p)
      (match-string-no-properties 5)))

;; (defun fountain-add-scene-number (n)
;;   "Add scene number N to current scene heading."
;;   (when (fountain-scene-heading-p)
;;     (end-of-line)
;;     (unless (eq (char-before) ?\s) (insert ?\s))
;;     (insert "#" n)))

;; (defun fountain-add-scene-numbers (&optional arg)
;;   "Add scene numbers to all scene headings.
;; If prefaced with ARG, overwrite existing scene numbers."
;;   (interactive)
;;   (let ((job (make-progress-reporter "Adding scene numbers...")))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (unless (fountain-scene-heading-p)
;;         (fountain-forward-scene 1))
;;       (let ((prev-scene-num "0"))
;;         (while (not (eobp))
;;           (let ((current-scene-num (fountain-get-scene-number)))
;;             (if current-scene-num
;;                 (setq prev-scene-num current-scene-num)
;;               (let* ((prev-scene-int (string-to-number prev-scene-num))
;;                      (prev-scene-alpha
;;                       (if (string-match "[a-z]+" prev-scene-num)
;;                           (match-string 0 prev-scene-num)))
;;                      (next-scene-num
;;                       (save-excursion
;;                         (while (not (or (eobp)
;;                                         (fountain-get-scene-number)))
;;                           (fountain-forward-scene 1))
;;                         (fountain-get-scene-number)))
;;                      (next-scene-int (if next-scene-num
;;                                          (string-to-number next-scene-num)))
;;                      (current-scene-num
;;                       (if (or (not next-scene-int)
;;                               (< (1+ prev-scene-int) next-scene-int))
;;                           (int-to-string (1+ prev-scene-int))
;;                         (concat (int-to-string prev-scene-int)
;;                                 (if prev-scene-alpha
;;                                     (string (1+ (string-to-char prev-scene-alpha)))
;;                                   "A")))))
;;                 (fountain-add-scene-number current-scene-num)
;;                 (setq prev-scene-num current-scene-num))))
;;           (fountain-forward-scene 1)
;;           (progress-reporter-update job))))
;;     (progress-reporter-done job)))

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

;;;; Text Functions ============================================================

(defun fountain-delete-comments-in-region (beg end)
  (let ((beg
         (save-excursion
           (goto-char beg)
           (if (and (search-forward "*/" end t)
                    (not (search-backward "/*" beg t))
                    (search-backward "/*" nil t))
               (match-beginning 0)
             beg)))
        (end
         (save-excursion
           (goto-char end)
           (if (and (search-backward "/*" beg t)
                    (not (search-forward "*/" end t))
                    (search-forward "*/" nil t))
               (match-end 0)
             end))))
  (goto-char beg)
  (while (re-search-forward fountain-comment-regexp end t)
    (delete-region (match-beginning 0) (match-end 0)))))

;;;; Outline Functions =========================================================

(defalias 'fountain-outline-next 'outline-next-visible-heading)
(defalias 'fountain-outline-previous 'outline-previous-visible-heading)
(defalias 'fountain-outline-forward 'outline-forward-same-level)
(defalias 'fountain-outline-backward 'outline-backward-same-level)
(defalias 'fountain-outline-up 'outline-up-heading)
(defalias 'fountain-outline-mark 'outline-mark-subtree)

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
  "Move the current subtree up past N headings of same level."
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

    \\[fountain-outline-cycle]				Cycle outline visibility of current subtree and its children
    \\[universal-argument] \\[fountain-outline-cycle]			Cycle outline visibility of buffer
    \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]		Show all
    \\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-outline-cycle]	Show outline visibility set in `fountain-outline-startup-level'"
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

Calls `fountain-outline-cycle' with argument 4 to cycle buffer
outline visibility through the following states:

    1:  Top-level section headins
    2:  Value of `fountain-outline-custom-level'
    3:  All section headings and scene headings
    4:  Everything"
  (interactive)
  (fountain-outline-cycle 4))

(defun fountain-outline-level ()
  "Return the heading's nesting level in the outline.
Assumes that point is at the beginning of a heading and match
data reflects `outline-regexp'."
  (if (string-prefix-p "#" (match-string 0))
      (string-width (match-string 2))
    6))

;;;; Parsing Functions =========================================================

(defun fountain-parse-metadata ()
  (let ((beg (match-beginning 0))
        (end (match-end 0))
        (key (downcase (match-string-no-properties 2)))
        (value (match-string-no-properties 3)))
    (save-excursion
      (forward-line 1)
      (while (and (fountain-metadata-p)
                  (null (match-string 2)))
        (setq value (concat value (if value "\n")
                            (match-string-no-properties 3))
              end (match-end 0))
        (forward-line 1))
      (list 'metadata
            (list 'begin beg
                  'end end
                  'key key)
            value))))

(defun fountain-parse-section-heading ()
  (list 'section-heading
        (list 'beg (match-beginning 0)
              'end (match-end 0)
              'level (funcall outline-level))
        (match-string-no-properties 3)))

(defun fountain-parse-section ()
  (let ((heading (fountain-parse-section-heading))
        (level (funcall outline-level))
        (beg (point))
        (end (save-excursion
               (outline-end-of-subtree)
               (unless (eobp)
                 (forward-char 1))
               (point))))
    (save-excursion
      (goto-char (plist-get (nth 1 heading) 'end))
      (let ((contents (fountain-parse-region (point) end t)))
        (list 'section
              (list 'begin beg
                    'end end
                    'level level)
              (cons heading contents))))))

(defun fountain-parse-scene-heading ()
  (list 'scene-heading
        (list 'beg (match-beginning 0)
              'end (match-end 0)
              'forced (stringp (match-string 2))
              'scene-number (match-string-no-properties 5))
        (match-string-no-properties 3)))

(defun fountain-parse-scene ()
  (let ((heading (fountain-parse-scene-heading))
        (num (match-string-no-properties 5))
        (omit (string-prefix-p "OMIT" (match-string 0)))
        (beg (point))
        (end (save-excursion
               (outline-end-of-subtree)
               (unless (eobp)
                 (forward-char 1))
               (point))))
    (save-excursion
      (goto-char (plist-get (nth 1 heading) 'end))
      (let ((contents (fountain-parse-region (point) end t)))
        (list 'scene
              (list 'begin beg
                    'end end
                    'scene-number num
                    'omit omit)
              (cons heading contents))))))

(defun fountain-parse-dialog ()
  (let ((heading (list 'character
                       (list 'beg (match-beginning 0)
                             'end (match-end 0)
                             'forced (stringp (match-string 2)))
                       (match-string-no-properties 3)))
        (name (match-string-no-properties 4))
        (forced (stringp (match-string 2)))
        (dual (cond ((stringp (match-string 5))
                     'right)
                    ((save-excursion
                       (fountain-forward-character 1 'dialog)
                       (and (fountain-character-p)
                            (stringp (match-string 5))))
                     'left)))
        (beg (point))
        (end (save-excursion
               (if (re-search-forward "^[\s\t]*$" nil 'move)
                   (match-beginning 0)
                 (point)))))
    (save-excursion
      (goto-char (plist-get (nth 1 heading) 'end))
      (let ((contents (fountain-parse-region (point) end t)))
        (list 'dialog
              (list 'begin beg
                    'end end
                    'character name
                    'dual dual)
              (cons heading contents))))))

(defun fountain-parse-lines ()
  (list 'lines
        (list 'begin (match-beginning 0)
              'end (match-end 0))
        (match-string-no-properties 3)))

(defun fountain-parse-paren ()
  (list 'paren
        (list 'begin (match-beginning 0)
              'end (match-end 0))
        (match-string-no-properties 3)))

(defun fountain-parse-trans ()
  (list 'trans
        (list 'begin (match-beginning 0)
              'end (match-end 0)
              'forced (stringp (match-string 2)))
        (match-string-no-properties 3)))

(defun fountain-parse-center ()
  (list 'center
        (list 'begin (match-beginning 0)
              'end (match-end 0))
        (match-string-no-properties 3)))

(defun fountain-parse-synopsis ()
  (list 'synopsis
        (list 'begin (match-beginning 0)
              'end (match-end 0))
        (match-string-no-properties 3)))

(defun fountain-parse-note ()
  (list 'note
        (list 'begin (match-beginning 0)
              'end (match-end 0))
        (match-string-no-properties 3)))

(defun fountain-parse-action ()
  (let ((beg (point))
        (end (save-excursion
               (while (not (fountain-tachyon-p))
                 (forward-char 1))
               (1- (point)))))
    (list 'action
          (list 'begin beg
                'end end)
          (s-trim-right (buffer-substring-no-properties beg end)))))

(defun fountain-parse-element ()
  (cond
   ((fountain-section-heading-p)
    (fountain-parse-section))
   ((fountain-scene-heading-p)
    (fountain-parse-scene))
   ((fountain-character-p)
    (fountain-parse-dialog))
   ((fountain-dialog-p)
    (fountain-parse-lines))
   ((fountain-paren-p)
    (fountain-parse-paren))
   ((fountain-trans-p)
    (fountain-parse-trans))
   ((fountain-center-p)
    (fountain-parse-center))
   ((fountain-synopsis-p)
    (fountain-parse-synopsis))
   ((fountain-note-p)
    (fountain-parse-note))
   (t
    (fountain-parse-action))))

(defun fountain-parse-region (beg end &optional recursive)
  (let ((metadata (fountain-read-metadata))
        list)
    (goto-char (max beg (plist-get metadata 'content-start)))
    (while (< (point) end)
      (while (looking-at "\n*\s?\n")
        (goto-char (match-end 0)))
      (if (< (point) end)
          (let ((element (fountain-parse-element)))
            (setq list (cons element list))
            (goto-char (plist-get (nth 1 element) 'end)))))
    (if recursive (reverse list)
      (list 'document
            (append (list 'begin beg
                          'end end)
                    metadata)
            (reverse list)))))

;;;; Export Functions ==========================================================

(defun fountain-export-get-filename (format)
  "If BUFFER is visiting a file, concat file name base and FORMAT.
Otherwise return `fountain-export-buffer'"
  (cond (fountain-export-use-title-as-filename
         (concat (plist-get (fountain-read-metadata) 'title) "." format))
        ((buffer-file-name)
         (concat (file-name-base (buffer-file-name)) "." format))
        (t
         (format fountain-export-buffer-name format))))

;; (defun fountain-export-create-title-page-element (key)
;;   "Gets metadata value associated with KEY and creates HTML element."
;;   (let* ((s (fountain-export-get-metadata-value key))
;;          (content (fountain-export-line-breaks s)))
;;     (if (string= key "title")
;;         (format "<h1>%s</h1>" content)
;;       (format "<p>%s</p>" content))))

;; (defun fountain-export-create-html-title-page ()
;;   "Create title page based on `fountain-export-title-page-template'."
;;   (if (cdr (assoc "title" fountain-metadata))
;;       (concat
;;        "<div id=\"title\">\n"
;;        (s-format fountain-export-title-page-title-template
;;                  'fountain-export-create-title-page-element)
;;        "\n</div>\n<div id=\"left\">\n"
;;        (s-format fountain-export-title-page-left-template
;;                  'fountain-export-create-title-page-element)
;;        "\n</div>\n<div id=\"right\">\n"
;;        (s-format fountain-export-title-page-right-template
;;                  'fountain-export-create-title-page-element)
;;        "\n</div>\n")))

(defun fountain-export-html-create-style ()
  "Create stylesheet using `fountain-export-html-style-template'."
  (let ((style (replace-regexp-in-string
                fountain-template-key-regexp
                (lambda (match)
                  (let ((replacer
                         (cadr (assoc (match-string 1 match)
                                      fountain-generic-template-replace-functions))))
                    (if replacer (funcall replacer) "")))
                fountain-export-html-style-template t t)))
    (if fountain-export-html-use-inline-style
        (concat "<style type=\"text/css\">\n"
                style
                "\n</style>")
      (let ((cssfile (get-buffer-create (fountain-export-get-filename "css")))
            (dir (expand-file-name
                  (file-name-directory (buffer-file-name)))))
        (with-current-buffer cssfile
          (erase-buffer)
          (insert (format "/* Created with Emacs %s running Fountain Mode %s */\n"
                          emacs-version fountain-version)
                  style)
          (write-file dir))
        (concat "<link rel=\"stylesheet\" href=\"" (buffer-name cssfile) "\">")))))

(defun fountain-export-html-dialog-class (plist)
  (let ((side (plist-get plist 'dual)))
    (cond ((eq side 'left)
           "dialog dual left")
          ((eq side 'right)
           "dialog dual right")
          (t "dialog"))))

(defun fountain-export-fountain-forced-scene-heading (plist)
  (if (plist-get plist 'forced)
      "." ""))

(defun fountain-export-fountain-forced-character (plist)
  (if (plist-get plist 'forced)
      "@" ""))

(defun fountain-export-fountain-forced-trans (plist)
  (if (plist-get plist 'forced)
      ">" ""))

(defun fountain-export-fountain-forced-action (plist)
  (if (plist-get plist 'forced)
      "!" ""))

(defun fountain-export-format-replace (format str)
  (dolist (var
           (cdr (assoc format fountain-export-format-replace-alist))
           str)
    (setq str
          (replace-regexp-in-string
           (car var) (cadr var) str t))))

(defun fountain-export-template-replace (template str plist)
  (replace-regexp-in-string
   fountain-template-key-regexp
   (lambda (match)
     (let ((key (match-string 1 match)))
       (if (string= key "content") str
         (let ((value (plist-get plist (intern key))))
           (if (stringp value) value
             (let ((replacer-plist
                    (cadr (assoc key fountain-export-template-replace-functions)))
                   (replacer-generic
                    (cadr (assoc key fountain-generic-template-replace-functions))))
               (cond (replacer-plist (funcall replacer-plist plist))
                     (replacer-generic (funcall replacer-generic))
                     (""))))))))
   template t t))

(defun fountain-export-format-element (element format includes)
  (let ((type (car element))
        (plist (nth 1 element))
        (content (nth 2 element))
        template)
    (setq template
          (if (or (and (eq type 'document)
                       fountain-export-standalone)
                  (not (eq type 'document)))
              (cadr (assoc type (assoc format fountain-export-templates)))))
    (if (listp content)
        (let (str)
          (dolist (element content
                           (if template
                               (fountain-export-template-replace
                                template str plist)
                             str))
            (setq str
                  (concat str
                          (fountain-export-format-element
                           element format includes)))))
      (let ((str (fountain-export-format-replace
                  format content)))
        (if (memq type includes)
            (if template
                (fountain-export-template-replace
                 template str plist)
              str))))))

(defun fountain-export-region (beg end format)
  (let ((content (fountain-parse-region beg end)))
    (fountain-export-format-element
     content format
     (cdr (or (assoc (or (plist-get (nth 1 content)
                                    'format)
                         "screenplay")
                     fountain-export-include-elements-alist)
              (car fountain-export-include-elements-alist))))))

(defun fountain-export-buffer (format &optional snippet buffer)
  (interactive
   (list (intern (completing-read "Format: "
                                  (mapcar 'car fountain-export-templates)
                                  nil t))
         (car current-prefix-arg)))
  (let ((sourcebuf (or buffer (current-buffer)))
        (bufname (generate-new-buffer-name
                  (fountain-export-get-filename (symbol-name format))))
        destbuf complete)
    (setq destbuf (get-buffer-create bufname))
    (unwind-protect
        (with-current-buffer sourcebuf
          (let ((str (fountain-export-region (point-min) (point-max)
                                             format)))
            (with-current-buffer destbuf
              (with-silent-modifications
                (erase-buffer))
              (insert str)))
          (setq complete t)
          (switch-to-buffer destbuf))
      (unless complete
        (kill-buffer destbuf)))))

(defun fountain-export-buffer-to-html ()
  (interactive)
  (fountain-export-buffer 'html))

(defun fountain-export-buffer-to-latex ()
  (interactive)
  (fountain-export-buffer 'tex))

(defun fountain-export-buffer-to-fdx ()
  (interactive)
  (fountain-export-buffer 'fdx))

(defun fountain-export-buffer-to-fountain ()
  (interactive)
  (fountain-export-buffer 'fountain))

;;;; Commands ==================================================================

(defun fountain-version ()
  "Return `fountain-mode' version."
  (interactive)
  (message "Fountain Mode %s" fountain-version))

;; not in use, delete?
(defun fountain-upcase-line (&optional arg)
  "Upcase the line.
If prefixed with ARG, insert \".\" at beginning of line to force
a scene heading."
  (interactive "P")
  (if arg
      (save-excursion
        (forward-line 0)
        (insert-char ?.)))
  (upcase-region (line-beginning-position) (line-end-position)))

(defun fountain-upcase-line-and-newline (&optional arg)
  "Upcase the line and insert a newline.
If prefixed with ARG, insert \".\" at beginning of line to force
a scene heading."
  (interactive "P")
  (if arg
      (save-excursion
        (forward-line 0)
        (insert-char ?.)))
  (upcase-region (line-beginning-position) (point))
  (insert-char ?\n))

(defun fountain-insert-alternate-character ()
  "Insert the alternate character and newline.
The alternate character is the second-last character within the
scene."
  (interactive)
  (if (and (fountain-blank-p)
           (save-excursion
             (forward-line -1)
             (fountain-blank-p)))
      (let ((character (fountain-get-character -2 'scene)))
        (if character
            (insert character ?\n)
          (message "No alternate character within scene")
          (insert-char ?\n)))
    (insert-char ?\n)))

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
  (if (null (or (fountain-section-heading-p)
                (fountain-scene-heading-p)))
      (progn
        (goto-char (mark))
        (user-error "Before first scene heading"))
    (push-mark)
    (fountain-outline-next 1)
    (exchange-point-and-mark)))

(defun fountain-goto-scene (n)
  "Move point to Nth scene."
  (interactive "NGoto scene: ")
  (goto-char (point-min))
  (let ((scene (if (fountain-scene-heading-p)
                   (or (string-to-number (match-string 5))
                       1)
                 0)))
    (while (and (< scene n)
                (not (eobp)))
      (fountain-forward-scene 1)
      (setq scene (if (match-string 5)
                      (string-to-number (match-string 5))
                    (1+ scene))))))

(defun fountain-forward-character (&optional n limit)
  "Goto Nth next character (or Nth previous is N is negative).
If LIMIT is 'scene, halt at end of scene. If LIMIT is 'dialog,
halt at end of dialog."
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
                    (and (not (eq (point) (buffer-end p)))
                         (or (fountain-dialog-p)
                             (fountain-paren-p)
                             (fountain-tachyon-p))))
                   ((not (or (fountain-character-p)
                             (eq (point) (buffer-end p))))))
        (forward-line p))
      (setq i (- i p)))))

(defun fountain-backward-character (&optional n)
  "Move backward N character (foward if N is negative)."
  (interactive "^p")
  (let ((i (or n 1)))
    (fountain-forward-character (- i))))

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
        (re-search-forward "^[\s\t]*$" nil 'move))
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
                                             (cons 't dec)))))
        (message "Syntax highlighting is now set at %s"
                 (cond ((= n 1) "minimum")
                       ((= n 2) "default")
                       ((= n 3) "maximum")))
        (font-lock-refresh-defaults))
    (user-error "Decoration must be an integer 1-3")))

(defun fountain-export-default ()
  "Call function defined in `fountain-export-default-command'."
  (interactive)
  (funcall fountain-export-default-command))

(defun fountain-export-shell-command (&optional buffer)
  "Call shell script defined in `fountain-export-shell-command'."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (if file
        (async-shell-command
         (format fountain-export-shell-command (shell-quote-argument file))
         "*Fountain Export Process*")
      (user-error "Buffer `%s' is not visiting a file" buffer))))

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
  `(("note"
     ,fountain-note-regexp
     ((:level 2 :subexp 0
              :invisible t)))
    ("scene-heading"
     (lambda (limit)
       (fountain-match-element 'fountain-scene-heading-p limit))
     ((:level 2 :subexp 0)
      (:level 2 :subexp 2 :face fountain-comment
              ;; :invisible fountain-syntax-chars
              :override t
              :laxmatch t)
      (:level 1 :subexp 4
              :display (- right-margin fountain-align-scene-number)
              :laxmatch t)))
    ("character"
     (lambda (limit)
       (fountain-match-element 'fountain-character-p limit))
     ((:level 3 :subexp 0)
      (:level 3 :subexp 2
              :invisible fountain-syntax-chars
              :override append
              :laxmatch t)
      (:level 3 :subexp 5 :face highlight
              :override append
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
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t
              :laxmatch t)))
    ("forced-action-mark"
     ,fountain-forced-action-mark-regexp
     ((:level 1 :subexp 0 :face fountain-comment
              :invisible fountain-syntax-chars)))
    ("center"
     ,fountain-center-regexp
     ((:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)
      (:level 3 :subexp 3)
      (:level 2 :subexp 4 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("section-heading"
     ,fountain-section-heading-regexp
     ((:level 2 :subexp 3)
      (:level 2 :subexp 2 :face fountain-comment)))
    ("synopsis"
     ,fountain-synopsis-regexp
     ((:level 2 :subexp 0 :invisible t)
      (:level 2 :subexp 2 :face fountain-comment
              :invisible fountain-syntax-chars
              :override t)))
    ("page-break"
     ,fountain-page-break-regexp
     ((:level 2 :subexp 0 :face fountain-page-break)))
    ("metadata"
     (lambda (limit)
       (fountain-match-element 'fountain-metadata-p limit))
     ((:level 2 :subexp 0 :face fountain-metadata-key
              :invisible t
              :laxmatch t)
      (:level 2 :subexp 3 :face fountain-metadata-value
              :invisible t
              :override t
              :laxmatch t)))
    ("action"
     (lambda (limit)
       (fountain-match-element 'fountain-action-p limit))
     ((:level 1 :subexp 0)))
    (nil
     ,fountain-nbsp-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-syntax-chars)))
    (nil
     ,fountain-underline-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face underline)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil
     ,fountain-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil
     ,fountain-bold-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face bold)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil
     ,fountain-bold-italic-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face bold-italic)
          (:level 1 :subexp 4 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)))
    (nil
     ,fountain-lyrics-regexp
         ((:level 1 :subexp 2 :face fountain-non-printing
                  :invisible fountain-emphasis-delim)
          (:level 1 :subexp 3 :face italic))))
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
    :laxmatch   as per `font-lock-keywords'

Regular expression should take the form:

    Group 1:    match whole string with trimmed whitespace
    Group 2:    syntax characters
    Group 3:    export group
    Group 4+:  syntax characters")


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

(defun fountain-get-align (element)
  "Return ELEMENT align integer based on buffer format"
  (if (integerp element) element
    (let ((format (or (plist-get (fountain-read-metadata)
                                 'format)
                      "screenplay")))
      (cadr (or (assoc format element)
                (car element))))))

(defun fountain-create-font-lock-keywords ()
  "Return a new list of `font-lock-mode' keywords.
Uses `fountain-font-lock-keywords-plist' to create a list of
keywords suitable for Font Lock."
  (fountain-init-vars)
  (let ((dec (fountain-get-font-lock-decoration))
        keywords)
    (dolist (var fountain-font-lock-keywords-plist keywords)
      (let* ((element (car var))
             (matcher (nth 1 var))
             (sub-plist (nth 2 var))
             (align (let ((element (intern (concat "fountain-align-" element))))
                      (if (boundp element)
                          (fountain-get-align (symbol-value element)))))
             ;; if we're using auto-align and the align var is bound,
             ;; set the align properties
             (align-props (if (and align fountain-align-elements)
                              `(line-prefix
                                (space :align-to ,align)
                                wrap-prefix
                                (space :align-to ,align))))
             facespec)
        (dolist (plist sub-plist)
          (let* ((subexp (plist-get plist :subexp))
                 ;; if LEVEL is less or equal to DEC, use either face
                 ;; supplied in PLIST or intern fountain-ELEMENT,
                 ;; otherwise use nil
                 (face (if (<= (plist-get plist :level) dec)
                           (or (plist-get plist :face)
                               (intern (concat "fountain-" element)))))
                 ;; if DISPLAY is non-nil, add to DISPLAY-PROPS
                 ;; FIXME: kinda hackish
                 (display (plist-get plist :display))
                 (display-props
                  (if display
                      (list 'display `(space :align-to ,display))))
                 ;; if INVISIBLE is non-nil, add to INVISIBLE-PROPS
                 (invisible (plist-get plist :invisible))
                 (invisible-props
                  (cond ((eq invisible t)
                         (list 'invisible (intern (concat "fountain-" element))))
                        (invisible
                         (list 'invisible invisible)))))
            (setq facespec
                  (append facespec
                          (if element
                              ;; (list (list subexp (list 'face face
                              ;;                        align-props
                              ;;                        invisible-props
                              ;;                        'fountain-element element)
                              ;;                 (plist-get plist :override)
                              ;;                 (plist-get plist :laxmatch)))
                              (list `(,subexp '(face ,face
                                                     ,@align-props
                                                     ,@display-props
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
  "If FUN returns non-nil before LIMIT, return match data."
  (let (match)
    (while (and (null match)
                (< (point) limit))
      (if (funcall fun)
          (setq match t))
      (forward-line 1))
    match))

;;; Keys =======================================================================

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; editing commands
    (define-key map (kbd "C-c C-m") 'fountain-upcase-line-and-newline)
    (define-key map (kbd "C-S-m") 'fountain-upcase-line-and-newline)
    ;; (define-key map (kbd "C-M-m") 'fountain-insert-alternate-character)
    (define-key map (kbd "C-c C-c") 'fountain-upcase-line)
    (define-key map (kbd "C-c C-d") 'fountain-continued-dialog-refresh)
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
    (define-key map (kbd "M-g s") 'fountain-goto-scene)
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
    (define-key map (kbd "C-i") 'fountain-outline-cycle)
    (define-key map (kbd "<backtab>") 'fountain-outline-cycle-global)
    (define-key map (kbd "C-S-i") 'fountain-outline-cycle-global)
    ;; exporting commands
    (define-key map (kbd "C-c C-e C-e") 'fountain-export-default)
    (define-key map (kbd "C-c C-e h") 'fountain-export-buffer-to-html)
    (define-key map (kbd "C-c C-e l") 'fountain-export-buffer-to-latex)
    (define-key map (kbd "C-c C-e d") 'fountain-export-buffer-to-fdx)
    (define-key map (kbd "C-c C-e f") 'fountain-export-buffer-to-fountain)
    (define-key map (kbd "C-c C-e s") 'fountain-export-shell-command)
    ;; view commands
    (define-key map (kbd "C-c C-x !") 'fountain-toggle-hide-syntax-chars) ; FIXME ??
    (define-key map (kbd "C-c C-x *") 'fountain-toggle-hide-emphasis-delim) ; FIXME ??
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
    ("Export"
     ["Default" fountain-export-default]
     "---"
     ["Buffer to HTML" fountain-export-buffer-to-html]
     ["Buffer to LaTeX" fountain-export-buffer-to-latex]
     ["Buffer to FDX" fountain-export-buffer-to-fdx]
     ["Buffer to Fountain" fountain-export-buffer-to-fountain]
     "---"
     ["Run shell command" fountain-export-shell-command]
     ;; ["Include Title Page"
     ;;  fountain-toggle-export-include-title-page
     ;;  :style toggle
     ;;  :selected fountain-export-include-title-page]
     ;; "---"
     ;; ["Bold Scene Headings"
     ;;  fountain-toggle-export-bold-scene-headings
     ;;  :style toggle
     ;;  :selected fountain-export-bold-scene-headings]
     ;; ["Underline Scene Headings"
     ;;  fountain-toggle-export-underline-scene-headings
     ;;  :style toggle
     ;;  :selected fountain-export-underline-scene-headings]
     ;; ["Double-Space Scene Headings"
     ;;  fountain-toggle-export-double-space-scene-headings
     ;;  :style toggle
     ;;  :selected fountain-export-double-space-scene-headings]
     "---"
     ["Customize Export" (customize-group 'fountain-export)])
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
  (fountain-init-vars)
  (fountain-init-imenu-generic-expression)
  (setq font-lock-defaults
        '(fountain-create-font-lock-keywords nil t))
  (add-to-invisibility-spec (cons 'outline t))
  (if fountain-hide-emphasis-delim
      (add-to-invisibility-spec 'fountain-emphasis-delim))
  (if fountain-hide-syntax-chars
      (add-to-invisibility-spec 'fountain-syntax))
  (setq-local font-lock-comment-face 'fountain-comment)
  (setq-local outline-level 'fountain-outline-level)
  (setq-local font-lock-extra-managed-props
              '(display line-prefix wrap-prefix invisible))
  (let ((n (plist-get (fountain-read-metadata) 'startup-level)))
    (if (stringp n)
        (setq-local fountain-outline-startup-level
                    (min (string-to-number n) 6))))
  (add-hook 'font-lock-extend-region-functions
            'fountain-font-lock-extend-region t t)
  (add-hook 'after-save-hook
            'font-lock-refresh-defaults)
  (fountain-outline-hide-level fountain-outline-startup-level))

(provide 'fountain-mode)
;;; fountain-mode.el ends here
