;;; fountain-mode.el --- Major mode for screenwriting in Fountain markup -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2024  Paul W. Rankin

;; Author: Paul W. Rankin <rnkn@rnkn.xyz>
;; Keywords: wp, text
;; Version: 3.8.0
;; Package-Requires: ((emacs "24.4") (seq "2.20"))
;; URL: https://www.fountain-mode.org
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

;; Fountain Mode is a screenwriting program for GNU Emacs using the
;; Fountain plain text markup format.

;; For more information about the Fountain format, visit <https://fountain.io>

;; Features
;; --------

;;  - Support for the Fountain 1.1 specification
;;  - Export to PostScript or PDF using troff
;;  - WYSIWYG auto-align elements (display only, does not modify file
;;    contents) specific to script format, e.g. screenplay, stageplay or
;;    user-defined formats
;;  - Traditional TAB auto-completion writing style
;;  - Highly accurate pagination calculation
;;  - Navigation by section, scene, character name, specified character
;;    name, or page
;;  - Integration with outline to fold/cycle visibility of sections,
;;    scenes and notes
;;  - Integration with electric-pair-mode to insert emphasis delimiters
;;    with single key (i.e. * or _)
;;  - Integration with imenu (sections, scene headings, notes)
;;  - Integration with auto-insert for title page metadata
;;  - Integration with which-function-mode to display live-update of current
;;    page and page count in mode-line
;;  - Automatically add/remove character (CONT'D)
;;  - Toggle syntax highlighting of each element
;;  - Toggle visibility of emphasis and syntax markup
;;  - Optionally display scene numbers in margins
;;  - Intelligent insertion of page breaks

;; Most common features are accessible from the menu. For a full list of
;; functions and key-bindings, type C-h m.


;; Requirements
;; ------------

;;  - Emacs 24.4
;;  - seq 2.20 (part of Emacs 25 and later)


;; Exporting
;; ---------

;; Fountain Mode can export to PostScript or PDF using the troff format as
;; an intermediary. This requires only a troff program on your system,
;; such as [GNU roff](https://www.gnu.org/software/groff/).

;; Alternatively you can export using an external command-line program,
;; such as:

;;  - [afterwriting](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md) (JavaScript)
;;  - [Wrap](https://github.com/Wraparound/wrap) (Go)
;;  - [screenplain](https://github.com/vilcans/screenplain) (Python 3, requires ReportLab for PDF)
;;  - [Textplay](https://github.com/olivertaylor/Textplay) (Ruby, requires PrinceXML for PDF)

;; The option fountain-export-command-profiles provides some shell
;; commands to interface with these tools, but you are encouraged to edit
;; or completely replace these to suit your own needs. The format is simple
;; while still allowing for flexibility.


;; Installation
;; ------------

;; The latest stable release of Fountain Mode is available via
;; [MELPA-stable][1]. First, add MELPA-stable to your package archives:

;;     M-x customize-option RET package-archives RET

;; Insert an entry named melpa-stable with URL:
;; https://stable.melpa.org/packages/

;; You can then find the latest stable version of fountain-mode in the
;; list returned by:

;;     M-x list-packages RET

;; If you prefer the latest but perhaps unstable version, do the above
;; using [MELPA][2].


;; Advanced Installation
;; ---------------------

;; Download the latest tagged release, move this file into your load-path
;; and add to your init.el file:

;;     (require 'fountain-mode)

;; If you wish to contribute to or alter Fountain Mode's code, clone the
;; repository into your load-path and require as above:

;;     git clone https://github.com/rnkn/fountain-mode.git


;; Bugs and Feature Requests
;; -------------------------

;; Use GitHub issues or send me an email (address in the package header).
;; For bugs, please ensure you can reproduce with:

;;     $ emacs -Q -l fountain-mode.el


;; [1]: https://stable.melpa.org/#/fountain-mode
;; [2]: https://melpa.org/#/fountain-mode


;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(require 'seq)

;;; Top-Level Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain ()
  "Major mode for screenwriting in Fountain markup."
  :prefix "fountain-"
  :link '(info-link "(fountain-mode) Fountain Mode")
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
  '(visual-line-mode)
  "Mode hook for `fountain-mode', run after the mode is turned on."
  :group 'fountain
  :type 'hook
  :options '(visual-line-mode
             electric-pair-local-mode
             imenu-add-menubar-index
             which-function-mode
             fountain-completion-update
             fountain-pagination-update
             fountain-completion-auto-update-mode
             flyspell-mode))

(defcustom fountain-insert-synopsis-hook
  nil
  "Hook run after command `fountain-insert-synopsis'."
  :group 'fountain
  :type 'hook)

(defcustom fountain-insert-note-hook
  nil
  "Hook run after command `fountain-insert-note'."
  :group 'fountain
  :type 'hook)

(defcustom fountain-insert-page-break-hook
  nil
  "Hook run after command `fountain-insert-page-break'."
  :group 'fountain
  :type 'hook)

(define-obsolete-variable-alias 'fountain-default-script-format
  'fountain-script-format "`fountain-mode' 3.8")
(defcustom fountain-script-format
  "screenplay"
  "Default script format.
Can be overridden in metadata with, e.g.

  format: teleplay"
  :group 'fountain
  :type 'string
  :safe 'string)

(make-obsolete-variable 'fountain-add-continued-dialog
                        "use command `fountain-add-continued-dialog' instead."
                        "`fountain-mode' 3.5")

(defcustom fountain-continued-dialog-string
  "(CONT'D)"
  "\\<fountain-mode-map>String to append to character name speaking in succession.
Append this string to characters speaking in succession when
calling `fountain-add-continued-dialog' (\\[fountain-add-continued-dialog]).

n.b. Before changing this option, first call
`fountain-remove-continued-dialog' (\\[fountain-remove-continued-dialog])
to remove any previous continued dialogue."
  :group 'fountain
  :type 'string
  :safe 'stringp)

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
                   (add-to-invisibility-spec 'fountain-emphasis-markup)
                 (remove-from-invisibility-spec 'fountain-emphasis-markup))
               (font-lock-refresh-defaults))))))

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
                   (add-to-invisibility-spec 'fountain-element-markup)
                 (remove-from-invisibility-spec 'fountain-element-markup))
               (font-lock-refresh-defaults))))))

(defcustom fountain-auto-upcase-scene-headings
  t
  "If non-nil, automatically upcase scene headings as you type.
Upcases lines matching `fountain-scene-heading-regexp'."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain)

(defcustom fountain-dwim-insert-next-character
  nil
  "When non-nil `fountain-dwim' inserts next character after dialogue."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain)

(make-obsolete-variable 'fountain-note-template
                        "note templates are no longer supported."
                        "`fountain-mode' 3.8")

(defcustom fountain-highlight-elements
  '(section-heading scene-heading character dialog synopsis note
                    metadata page-break)
  "List of elements highlighted with `font-lock-mode'."
  :type '(choice (const :tag "No Highlighting" nil)
                 (set (const :tag "Section Headings" section-heading)
                      (const :tag "Scene Headings" scene-heading)
                      (const :tag "Action" action)
                      (const :tag "Character Names" character)
                      (const :tag "Dialogue" dialog)
                      (const :tag "Parentheticals" paren)
                      (const :tag "Transitions" trans)
                      (const :tag "Synopses" synopsis)
                      (const :tag "Notes" note)
                      (const :tag "Metadata" metadata)
                      (const :tag "Page Breaks" page-break)))
  :group 'fountain
  :set #'fountain--set-and-refresh-font-lock)

(defvar fountain-highlight-elements-always
  '(underline italic bold bold-italic lyrics)
  "List of elements always highlighted with `font-lock-mode'.")

(defcustom fountain-double-space-scene-headings
  nil
  "When non-nil, display an additional newline before scene headings.
This option does not affect file contents.

n.b. This option does not affect calculation of pagination, see
instead `fountain-export-scene-heading-format'."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain
  :set #'fountain--set-and-refresh-font-lock)

(define-obsolete-variable-alias 'fountain-shift-all-elements
  'fountain-transpose-all-elements "`fountain-mode' 3.2")
(defcustom fountain-transpose-all-elements
  t
  "\\<fountain-mode-map>When non-nil, \\[fountain-forward-paragraph-or-transpose] and \\[fountain-backward-paragraph-or-transpose] should transpose all elements.
Otherwise, only transpose outline elements."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain)


;;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fountain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map (kbd "TAB") #'fountain-dwim)
    (define-key map (kbd "C-c RET") #'fountain-upcase-line-and-newline)
    (define-key map (kbd "<S-return>") #'fountain-upcase-line-and-newline)
    (define-key map (kbd "C-c C-c") #'fountain-upcase-line)
    (define-key map (kbd "C-c C-d") #'fountain-add-continued-dialog)
    (define-key map (kbd "C-c C-z") #'fountain-insert-note)
    (define-key map (kbd "C-c C-a") #'fountain-insert-synopsis)
    (define-key map (kbd "C-c C-x i") #'auto-insert)
    (define-key map (kbd "C-c C-x #") #'fountain-add-scene-numbers)
    (define-key map (kbd "C-c C-x RET") #'fountain-insert-page-break)
    (define-key map (kbd "C-c C-x a") #'fountain-completion-update)
    (define-key map (kbd "C-c C-x *") #'fountain-toggle-hide-emphasis-markup)
    (define-key map (kbd "C-c C-x !") #'fountain-toggle-hide-element-markup)

    ;; Navigation commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map [remap beginning-of-defun] #'fountain-outline-beginning)
    (define-key map (kbd "M-g s") #'fountain-goto-scene)
    (define-key map (kbd "M-g p") #'fountain-goto-page)
    (define-key map (kbd "M-n") #'fountain-forward-character)
    (define-key map (kbd "M-p") #'fountain-backward-character)
    (define-key map (kbd "C-c M-n") #'fountain-forward-this-character)
    (define-key map (kbd "C-c M-p") #'fountain-backward-this-character)

    ;; Block editing commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map (kbd "<M-down>") #'fountain-forward-paragraph-or-transpose)
    (define-key map (kbd "ESC <down>") #'fountain-forward-paragraph-or-transpose)
    (define-key map (kbd "<M-up>") #'fountain-backward-paragraph-or-transpose)
    (define-key map (kbd "ESC <up>") #'fountain-backward-paragraph-or-transpose)

    ;; Outline commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map [remap forward-list] #'fountain-outline-next)
    (define-key map [remap backward-list] #'fountain-outline-previous)
    (define-key map [remap forward-sexp] #'fountain-outline-forward)
    (define-key map [remap backward-sexp] #'fountain-outline-backward)
    (define-key map [remap backward-up-list] #'fountain-outline-up)
    (define-key map [remap mark-defun] #'fountain-outline-mark)
    (define-key map (kbd "C-c TAB") #'fountain-outline-cycle)
    (define-key map (kbd "<backtab>") #'fountain-outline-cycle-buffer)
    (define-key map (kbd "S-TAB") #'fountain-outline-cycle-buffer)
    (define-key map (kbd "C-M-i") #'fountain-outline-cycle-buffer)
    (define-key map (kbd "M-RET") #'fountain-insert-section-heading)
    (define-key map (kbd "C-c C-x b") #'fountain-outline-to-indirect-buffer)
    (define-key map (kbd "C-c C-q") #'fountain-outline-hide-sublevels)

    ;; Pagination commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map [remap forward-page] #'fountain-forward-page)
    (define-key map [remap backward-page] #'fountain-backward-page)
    (define-key map (kbd "C-c C-p") #'fountain-count-pages)
    (define-key map (kbd "C-c C-x p") #'fountain-pagination-update)

    ;; Exporting commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-key map (kbd "C-c C-e t") #'fountain-export-troff)
    (define-key map (kbd "C-c C-e e") #'fountain-export-command)
    (define-key map (kbd "C-c C-v") #'fountain-export-view)
    map)
  "Mode map for `fountain-mode'.")


;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain-faces ()
  "Faces used in `fountain-mode'.
You can specify which elements are highlighted with the option
`fountain-highlight-elements'."
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
  '((t (:inherit font-lock-keyword-face)))
  "Default face for metadata keys.")

(defface fountain-metadata-value
  '((t (:inherit font-lock-string-face)))
  "Default face for metadata values.")

(defface fountain-page-break
  '((t (:inherit font-lock-constant-face)))
  "Default face for page breaks.")

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
  '((t (:extend t :inherit font-lock-comment-face)))
  "Default face for notes.")

(define-obsolete-face-alias 'fountain-section-heading
  'fountain-section-heading-1 "`fountain-mode' 3.0")

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
  "Default face for dialogue.")

(defface fountain-trans
  '((t nil))
  "Default face for transitions.")


;;; Export Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain-export ()
  "Options for exporting files in `fountain-mode'."
  :prefix "fountain-export-"
  :link '(info-link "(fountain-mode) Exporting")
  :group 'fountain)

(defcustom fountain-export-format
  'pdf
  "Target format for export. Valid options are `ps' or `pdf'."
  :type '(choice (const :tag "PostScript" ps)
                 (const :tag "PDF" pdf))
  :group 'fountain-export)

(defcustom fountain-export-scene-heading-format
  '(double-space)
  "List of format options applied when exporting scene headings.
Options are: bold, double-space, underline."
  :type '(set (const :tag "Bold" bold)
              (const :tag "Double-spaced" double-space)
              (const :tag "Underlined" underline))
  :group 'fountain-export)

(defcustom fountain-export-title-page
  t
  "When non-nil, include a title page in export."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-export)

(defcustom fountain-export-number-first-page
  nil
  "When non-nil, first page is numbered in export."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-export)

(defcustom fountain-export-scene-numbers
  nil
  "When non-nil, include scene numbers in export."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-export)

(defcustom fountain-export-troff-command
  "groff"
  "Name of troff command."
  :type 'string
  :safe 'stringp
  :group 'fountain-export)

(defcustom fountain-export-troff-extra-options
  '("-Kutf8")
  "Option flags passed to `fountain-export-troff-command'.

n.b. the `-Tdev' option is calculated automatically from
`fountain-export-format' and the page size from
`fountain-page-size'."
  :type '(repeat (string :tag "Option"))
  :group 'fountain-export)

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
    ("make-pdf" . "make %B.pdf")
    ("screenplain-html" . "screenplain \
--format html > %B.html")
    ("textplay-fdx" . "textplay --fdx < %b > %B.fdx"))
  "Shell command profiles for exporting Fountain files.

n.b. The default command profiles are only intended as examples.
You are encouraged to edit/replace these to suit your own needs.

Each profile is a cons-cell of PROFILE-NAME string and the
COMMAND string.

COMMAND is passed to `format-spec' and allows for interpolation
of the following values:

  %b is the variable `buffer-file-name'
  %B is the variable `buffer-file-name' sans extension
  %n is the variable `user-full-name'
  %t is the title (from script metadata)
  %a is the author (from script metadata)
  %F is the current date in ISO format
  %x is the current date in your locale's \"preferred\" format

If a command does not include %b, `fountain-export-command' will pass the
buffer or active region to the command via stdin.

If a command outputs to stdout, this will be redirected to
`fountain-export-output-buffer'.

The first profile is considered default.

COMMAND may be edited interactively when calling
`fountain-export-command' prefixed with \\[universal-argument]."
  :type '(repeat (cons (string :tag "Name")
                       (string :tag "Shell command")))
  :group 'fountain-export)

(defcustom fountain-export-output-buffer
  "*Fountain Export*"
  "Buffer name for `fountain-export' output."
  :type 'string
  :safe 'stringp
  :group 'fountain-export)

(defcustom fountain-export-switch-to-output-buffer
  t
  "When non-nil, switch to `fountain-export-output-buffer' on export."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-export)

(defcustom fountain-export-kill-output-buffer
  nil
  "When non-nil, kill `fountain-export-output-buffer' on export."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-export)

(defcustom fountain-export-title-page-title-keys
  '("title" "credit" "author" "authors" "source")
  "Elements to include in the title page title section."
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value) (seq-every-p 'stringp value)))
  :group 'fountain-export)

(defcustom fountain-export-title-page-contact-keys
  '("draft" "date" "draft date" "contact")
  "Elements to include in the title page contact section."
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value) (seq-every-p 'stringp value)))
  :group 'fountain-export)


;;; Regular Expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

Constructed with `fountain-init-scene-heading-regexp'. Requires
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
         (when (featurep 'fountain-mode)
           (fountain-init-scene-heading-regexp)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'fountain-mode)
                 (fountain-init-outline-regexp)
                 (font-lock-refresh-defaults)))))))

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

(defconst fountain-forced-action-regexp
  "^\\(!\\)\\(.*\\)[\s\t]*$"
  "Regular expression for forced action.")

(defconst fountain-metadata-regexp
  (concat "^\\([^:\s\t\n][^:[\n]*\\):[\s\t]*\\(.+\\)?"
          "\\|"
          "^[\s\t]+\\(?2:.+\\)")
  "Regular expression for matching multi-line metadata values.
Requires `fountain-match-metadata' for `bobp'.")

(defconst fountain-character-regexp
  (concat "^[\s\t]*"
          "\\(?:\\(?1:@\\)\\(?2:.+?\\)"
          "\\|"
          "\\(?2:[^<>\n[:lower:]]*?[[:upper:]]+[^<>\n[:lower:]]*?\\)"
          "\\)"
          "\\(?3:[\s\t]*\\(?4:\(\\)[^\)\n]*\)?\\)*?"
          "[\s\t]*\\(?5:[\s\t]*^\\)?"
          "[\s\t]*$")
  "Regular expression for matching character names.

  Group 1: match leading @ for forced character
  Group 2: match character name
  Group 3: match parenthetical extension
  Group 4: match opening parenthetical
  Group 5: match trailing ^ for dual dialogue

Requires `fountain-match-character' for preceding blank line.")

(defconst fountain-dialog-regexp
  "^\\(\s\s\\)$\\|^[\s\t]*\\(?2:[^<>\n]+?\\)[\s\t]*$"
  "Regular expression for matching dialogue.

  Group 1: omitted
  Group 2: match dialogue

Requires `fountain-match-dialog' for preceding character,
parenthetical or dialogue.")

(defconst fountain-paren-regexp
  "^[\s\t]*\\(?2:([^)\n]*)\\)[\s\t]*$"
  "Regular expression for matching parentheticals.

  Group 1: omitted
  Group 2: match parenthetical

Requires `fountain-match-paren' for preceding character or
dialogue.")

(defconst fountain-page-break-regexp
  "^[\s\t]*\\(=\\{3,\\}\\)[\s\t]*\\([a-z0-9\\.-]+\\)?.*$"
  "Regular expression for matching page breaks.

  Group 1: leading ===
  Group 2: forced page number")

(defconst fountain-note-regexp
  "\\[\\[[\s\t]*\\(\\(?:\s\s\n\\|.\n?\\)*?\\)[\s\t]*]]"
  "Regular expression for matching notes.

  Group 1: note text")

(defconst fountain-section-heading-regexp
  "^\\(?1:\\(?2:#\\{1,5\\}\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching section headings.

  Group 1: match leading #s and following whitespace
  Group 2: match leading #s
  Group 3: match heading text")

(defconst fountain-section-heading-1-regexp
  "^\\(?1:\\(?2:#\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching level 1 section headings.")

(defconst fountain-section-heading-2-regexp
  "^\\(?1:\\(?2:##\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching level 2 section headings.")

(defconst fountain-section-heading-3-regexp
  "^\\(?1:\\(?2:###\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching level 3 section headings.")

(defconst fountain-section-heading-4-regexp
  "^\\(?1:\\(?2:####\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching level 4 section headings.")

(defconst fountain-section-heading-5-regexp
  "^\\(?1:\\(?2:#####\\)[\s\t]*\\)\\(?3:.*\\)[\s\t]*$"
  "Regular expression for matching level 5 section headings.")

(defconst fountain-synopsis-regexp
  "^\\(\\(=\\)[\s\t]*\\)\\([^=\n].*\\)"
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
  "^\\(?1:\\(~[\s\t]*\\)\\(?3:.+\\)\\)"
  "Regular expression for matching lyrics.")


;;; Aligning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain-align ()
  "Options for visual alignment of `fountain-mode' elements.
Each element align option can be an integer representing the
align column for all formats, or a list where each element takes
the form:

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
  "Column integer to which dialogue should be aligned.

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
  '(("screenplay" . 50)
    ("stageplay" . 30))
  "Column integer to which transitions should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-align-center
  20
  "Column integer to which centered text should be aligned.

This option does not affect file contents."
  :group 'fountain-align
  :type '(choice (integer :tag "Column")
                 (repeat (cons (string :tag "Format")
                               (integer :tag "Column"))))
  :set #'fountain--set-and-refresh-font-lock)


;;; Autoinsert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autoinsert)

(defvar fountain-metadata-skeleton
  '(nil
    "title: " (skeleton-read "Title: " (file-name-base (buffer-name))) | -7 "\n"
    "credit: " (skeleton-read "Credit: " "written by") | -9 "\n"
    "author: " (skeleton-read "Author: " user-full-name) | -9 "\n"
    "format: " (skeleton-read "Script format: " fountain-script-format) | -9 "\n"
    "source: " (skeleton-read "Source: ") | -9 "\n"
    "date: " (skeleton-read "Date: " (format-time-string "%x")) | -7 "\n"
    "contact:\n" ("Contact details, %s: " "    " str | -4 "\n") | -9))

(define-auto-insert '(fountain-mode . "Fountain metadata skeleton")
  fountain-metadata-skeleton)


;;; Element Matching ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fountain-max-metadata-length
  1000
  "Maximum length in characters to test for metadata.")

(defsubst fountain-comment-p ()
  "Return non-nil if point is at comment (boneyard)."
  (let ((faceprop (get-char-property (point) 'face)))
    (if (listp faceprop)
        (memq 'fountain-comment faceprop)
      (eq 'fountain-comment faceprop))))

(defun fountain-blank-before-p ()
  "Return non-nil if preceding line is blank or a comment."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (bobp)
          (progn (forward-line -1)
                 (or (and (bolp) (eolp))
                     (fountain-comment-p)))))))

(defun fountain-blank-after-p ()
  "Return non-nil if following line is blank or a comment."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line)
      (or (eobp)
          (and (bolp) (eolp))
          (fountain-comment-p)))))

(defun fountain-in-dialog-maybe ()
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
  (unless (fountain-match-trans)
    (save-excursion
      (beginning-of-line)
      (when (< (or (save-excursion (re-search-forward "\n[\s\t]*\n" nil t))
                   (point-max))
               fountain-max-metadata-length)
        (and (looking-at fountain-metadata-regexp)
             (save-match-data
               (save-restriction
                 (widen)
                 (or (bobp)
                     (and (forward-line -1)
                          (fountain-match-metadata))))))))))

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
            (progn (re-search-backward "\\`\\|^$" nil 'move)
                   (and (re-search-forward fountain-note-regexp nil t)
                        (<= (match-beginning 0) x (match-end 0)))))))))

(defun fountain-match-scene-heading ()
  "Match scene heading if point is at a scene heading, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (looking-at fountain-scene-heading-regexp)
         (fountain-blank-before-p))))

(defun fountain-match-scene-heading-blank ()
  "Match blank line directly before scene heading, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (and (looking-at fountain-scene-heading-regexp)
         (fountain-blank-before-p)
         (looking-back "\n" (line-beginning-position 0)))))

(defun fountain-match-character (&optional loose)
  "Match character if point is at character, nil otherwise.
When LOOSE is non-nil, do not require non-blank line after."
  (unless (or (fountain-match-scene-heading)
              (fountain-comment-p))
    (save-excursion
      (beginning-of-line)
      (and (not (looking-at fountain-forced-action-regexp))
           (let ((case-fold-search nil))
             (looking-at fountain-character-regexp))
           (fountain-blank-before-p)
           (if loose t (not (fountain-blank-after-p)))))))

(defun fountain-match-dialog ()
  "Match dialogue if point is at dialogue, nil otherwise."
  (unless (or (and (bolp) (eolp))
              (fountain-match-paren))
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
    (and (let ((case-fold-search nil))
           (looking-at fountain-trans-regexp))
         (fountain-blank-before-p)
         (fountain-blank-after-p))))

(defun fountain-match-center ()
  "Match centered text if point is at centered text, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at fountain-center-regexp)))

(defun fountain-get-element ()
  "Return element at point as a symbol."
  (cond
   ((and (bolp) (eolp))                 nil)
   ((fountain-comment-p)                nil)
   ((fountain-match-section-heading)    'section-heading)
   ((fountain-match-scene-heading)      'scene-heading)
   ((fountain-match-character)
    (cl-case (fountain-get-dual-dialog)
      (left                             'dual-character-left)
      (right                            'dual-character-right)
      (t                                'character)))
   ((fountain-match-dialog)
    (cl-case (fountain-get-dual-dialog)
      (left                             'dual-dialog-left)
      (right                            'dual-dialog-right)
      (t                                'dialog)))
   ((fountain-match-paren)
    (cl-case (fountain-get-dual-dialog)
      (left                             'dual-paren-left)
      (right                            'dual-paren-right)
      (t                                'paren)))
   ((fountain-match-trans)              'trans)
   ((fountain-match-center)             'center)
   ((fountain-match-synopsis)           'synopsis)
   ((fountain-match-page-break)         'page-break)
   ((fountain-match-metadata)           'metadata)
   ((fountain-match-note)               'note)
   ((looking-at "\\(?2:.+\\)")          'action)))

(defun fountain-match-action ()
  "Match action text if point is at action, nil otherwise.
Assumes that all other element matching has been done."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (or (looking-at fountain-forced-action-regexp)
          (eq (fountain-get-element) 'action)))))


;;; Auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local fountain--completion-locations
  nil
  "List of scene locations in the current buffer.")

(defvar-local fountain--completion-characters
  nil
  "List of characters in the current buffer.
Each element is a cons (NAME . OCCUR) where NAME is a string, and
OCCUR is an integer representing the character's number of
occurrences.")

(defvar fountain--completion-auto-update-timer
  nil
  "Timer to run `fountain--completion-auto-update'.
See `fountain-completion-auto-update-mode'.")

(defcustom fountain-completion-auto-update-delay
  5.0
  "Idle delay in seconds before updating completion candidates.
See `fountain-completion-auto-update-mode'."
  :group 'fountain
  :type 'number
  :safe 'numberp)

(defcustom fountain-completion-auto-update-lighter
  nil
  "Lighter for `fountain-completion-auto-update-mode'."
  :group 'fountain
  :type '(choice (const :tag "No lighter" nil) string)
  :safe 'string-or-null-p)

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
  :safe (lambda (value)
          (and (listp value) (seq-every-p 'stringp value))))

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
  :safe (lambda (value)
          (and (listp value) (seq-every-p 'stringp value))))

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
        (when (fountain-match-character)
          (fountain-forward-character -1 'scene))
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

If point is at a scene heading and matches
`fountain-scene-heading-suffix-separator', offer completion
candidates from `fountain-scene-heading-suffix-list'.

If point is at a line matching
`fountain-scene-heading-prefix-list', offer location completion
candidates plus `fountain-completion-additional-locations'.

If point is at a possible character name with an opening
parenthetical extension, offer character completion candidates
from `fountain-character-extension-list' plus
`fountain-continued-dialog-string'.

If point is at beginning of line with a preceding blank line,
offer character completion candidates plus
`fountain-completion-additional-characters'. For more information
of character completion sorting, see
`fountain-completion-get-characters'.

Added to `completion-at-point-functions'."
  (cond ((and (fountain-match-scene-heading)
              (match-string-no-properties 5))
         ;; Return scene heading suffix completion
         (list (match-end 5)
               (point)
               fountain-scene-heading-suffix-list))
        ((and (fountain-match-scene-heading)
              (match-string-no-properties 3))
         ;; Return scene location completion
         (list (match-end 3)
               (point)
               (append fountain-completion-additional-locations
                       fountain--completion-locations)))
        ((and (fountain-match-scene-heading)
              (match-string-no-properties 1))
         ;; Return scene location completion (forced)
         (list (match-end 1)
               (point)
               (append fountain-completion-additional-locations
                       fountain--completion-locations)))
        ;; Return character extension
        ((and (fountain-match-character 'loose)
              (match-string-no-properties 4))
         (list (match-beginning 4)
               (line-end-position)
               (append fountain-character-extension-list
                       (list fountain-continued-dialog-string))))
        ;; Return character completion
        ((and (eolp)
              (fountain-blank-before-p))
         (list (line-beginning-position)
               (point)
               (lambda (string pred action)
                 (if (eq action 'metadata)
                     (list 'metadata
                           (cons 'display-sort-function 'identity)
                           (cons 'cycle-sort-function 'identity))
                   (complete-with-action
                    action (fountain-completion-get-characters)
                    string pred)))))))

(defun fountain-completion-update ()
  "Update completion candidates for current buffer.
Enable `fountain-completion-auto-update-mode' to automatically
update completion candidates when idle.

While `fountain--completion-locations' are left unsorted for
`completion-at-point' to perform sorting,
`fountain--completion-characters' are sorted by number of lines.
For more information on character completion sorting, see
`fountain-completion-get-characters'.

Add this function to option `fountain-mode-hook' to have
completion upon load."
  (interactive)
  (setq fountain--completion-locations nil
        fountain--completion-characters nil)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (and (fountain-match-scene-heading)
                   (match-string 4))
          (cl-pushnew (match-string-no-properties 4)
                      fountain--completion-locations))
        (fountain-move-forward-scene 1))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (and (fountain-match-character)
                   (match-string 2))
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

(defun fountain--completion-auto-update ()
  "Convenience function for `fountain-completion-auto-update-mode'."
  (when (eq major-mode 'fountain-mode)
    (fountain-completion-update)))

(defvar fountain--completion-idle-timer nil
  "Idle timer used for `fountain-completion-auto-update-mode'.")

;;;###autoload
(define-minor-mode fountain-completion-auto-update-mode
  "Updates `fountain-mode' completion candidates when idle.
Calls `fountain-completion-update' in `fountain-mode' buffers
after `fountain-completion-auto-update-delay'."
  :init-value nil
  :global t
  :lighter fountain-completion-auto-update-lighter
  (if fountain-completion-auto-update-mode
      (setq fountain--completion-idle-timer
            (run-with-idle-timer fountain-completion-auto-update-delay t
                                 #'fountain--completion-auto-update))
    (when (timerp fountain--completion-idle-timer)
      (cancel-timer fountain--completion-idle-timer)
      (setq fountain--completion-idle-timer nil))))


;;; Outlining ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'outline)

(defvar-local fountain--outline-buffer-state
  nil
  "Internal local representation of buffer outline cycle state.
Used by `fountain-outline-cycle'.")

(defgroup fountain-outline ()
  "Options for outlining in `fountain-mode'."
  :prefix "fountain-outline-"
  :group 'fountain)

(define-obsolete-variable-alias 'fountain-outline-custom-level
  'fountain-outline-show-all-section-headings "`fountain-mode' 3.4")
(defcustom fountain-outline-show-all-section-headings
  t
  "If non-nil, show all level section headings when cycling outline.
Otherwise, cycle from showing top-level section headings to all
section and scene headings."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-outline)

(define-obsolete-variable-alias 'fountain-outline-fold-notes
  'fountain-outline-hide-notes "`fountain-mode' 3.4")
(defcustom fountain-outline-hide-notes
  t
  "\\<fountain-mode-map>If non-nil, fold contents of notes when cycling outline visibility.

Notes visibility can be cycled with \\[fountain-dwim]."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-outline)

(defcustom fountain-outline-show-synopses
  nil
  "If non-nil, show synopses following headings when cycling outline visibility."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-outline
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'fountain-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'fountain-mode)
                 (fountain-init-outline-regexp)))))))

(defalias 'fountain-outline-next 'outline-next-visible-heading)
(defalias 'fountain-outline-previous 'outline-previous-visible-heading)
(defalias 'fountain-outline-forward 'outline-forward-same-level)
(defalias 'fountain-outline-backward 'outline-backward-same-level)
(defalias 'fountain-outline-up 'outline-up-heading)
(defalias 'fountain-outline-mark 'outline-mark-subtree)

(declare-function fountain-outline-show-all "fountain-mode")
(declare-function fountain-outline-show-entry "fountain-mode")
(declare-function fountain-outline-show-children "fountain-mode")
(declare-function fountain-outline-hide-subtree "fountain-mode")
(declare-function fountain-outline-hide-sublevels "fountain-mode")

(if (<= 25 emacs-major-version)
    (progn
      (defalias 'fountain-outline-show-all 'outline-show-all)
      (defalias 'fountain-outline-show-entry 'outline-show-entry)
      (defalias 'fountain-outline-show-children 'outline-show-children)
      (defalias 'fountain-outline-hide-subtree 'outline-hide-subtree)
      (defalias 'fountain-outline-hide-sublevels 'outline-hide-sublevels))
  (defalias 'fountain-outline-show-all 'show-all)
  (defalias 'fountain-outline-show-entry 'show-entry)
  (defalias 'fountain-outline-show-children 'show-children)
  (defalias 'fountain-outline-hide-subtree 'hide-subtree)
  (defalias 'fountain-outline-hide-sublevels 'hide-sublevels))

(defun fountain-outline-beginning ()
  "Move to the beginning of the current subtree."
  (interactive)
  (fountain-outline-forward 0))

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

(define-obsolete-function-alias 'fountain-outline-shift-down
  'fountain-outline-move-subtree-down "`fountain-mode' 3.2")

(defun fountain-outline-move-subtree-down (&optional n)
  "Move the current subtree down past N headings of same level."
  (interactive "*p")
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
    (when folded (fountain-outline-hide-subtree))
    (when hanging-line
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))
    (set-marker insert-point nil)))

(define-obsolete-function-alias 'fountain-outline-shift-up
  'fountain-outline-move-subtree-up "`fountain-mode' 3.2")

(defun fountain-outline-move-subtree-up (&optional n)
  "Move the current subtree up past N headings of same level."
  (interactive "*p")
  (fountain-outline-move-subtree-down (- n)))

(defun fountain-outline-flag-notes (start end)
  "Collapse notes between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward fountain-note-regexp end 'move)
      (outline-flag-region (match-beginning 1) (match-end 1)
                           fountain-outline-hide-notes))))

(defun fountain-outline-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree nil)
  (save-excursion
    (while (re-search-forward fountain-note-regexp nil 'move)
           (outline-flag-region (match-beginning 1) (match-end 1)
                                fountain-outline-hide-notes))))

(defun fountain-outline-set-buffer-state (state &optional silent)
  "Set buffer outline visibilty to outline level for STATE.
Valid values for STATE are:

  top-level         (show level 1 section headings)
  section-headings  (show all section headings)
  scene-headings    (show all section and scene headings)
  nil               (show all)

Display a message unless SILENT."
  (cl-case state
    (top-level
     (fountain-outline-hide-sublevels 1)
     (unless silent (message "Showing top-level section headings")))
    (section-headings
     (fountain-outline-hide-sublevels 5)
     (unless silent (message "Showing all section headings")))
    (scene-headings
     (fountain-outline-hide-sublevels 6)
     (unless silent (message "Showing scene headings")))
    (t
     (fountain-outline-show-all)
     (fountain-outline-flag-notes (point-min) (point-max))
     (unless silent (message "Showing all"))))
  (setq fountain--outline-buffer-state state))

(make-obsolete 'fountain-outline-hide-custom-level
  'fountain-outline-set-buffer-state "`fountain-mode' 3.4")

(defun fountain-outline-cycle ()
  "Cycle outline visibility of heading at point.

Visibility cycles between showing just the heading, showing
subheadings, and showing all.

See also `fountain-outline-show-synopses'."
  (interactive)
  (let (heading-start heading-end subtree-end overlay-list has-subheadings)
    (save-excursion
      (outline-back-to-heading t)
      (setq heading-start (point))
      (outline-end-of-heading)
      (setq heading-end (point))
      (outline-end-of-subtree)
      (setq subtree-end (point)))
    (save-excursion
      (outline-back-to-heading t)
      (setq has-subheadings
            (< (save-excursion (outline-next-heading) (point))
               (save-excursion (outline-end-of-subtree) (point)))))
    (setq overlay-list
          (seq-filter
           (lambda (overlay)
             (and (eq (overlay-get overlay 'invisible) 'outline)
                  (save-excursion
                    (goto-char (overlay-start overlay))
                    (or (outline-on-heading-p t)
                        (fountain-match-synopsis)))))
           (overlays-in heading-start subtree-end)))
    (cond ((= heading-end subtree-end)
           (message "Empty heading"))
          ((null overlay-list)
           (fountain-outline-hide-subtree)
           (message "Hiding all"))
          ((and has-subheadings
                (or (= subtree-end (point-max)
                       (1+ (overlay-end (car overlay-list))))
                    (= (overlay-end (car overlay-list)) subtree-end))
                (= (overlay-start (car overlay-list)) heading-end))
           (fountain-outline-show-children)
           (message "Showing headings"))
          (t
           (fountain-outline-show-subtree)
           (message "Showing all")))))

(define-obsolete-function-alias 'fountain-outline-cycle-global
  'fountain-outline-cycle-buffer "`fountain-mode' 3.4")

(defun fountain-outline-cycle-buffer (&optional arg)
  "\\<fountain-mode-map>Cycle outline visibility of the buffer.

Visibility cycles between showing top-level headings, showing the
value of `fountain-outline-custom-level', showing all headings,
and showing all.

When prefixed with ARG:

  1. If ARG is 4, cycle outline visibility of buffer (\\[universal-argument] \\[fountain-dwim],
     same as \\[fountain-outline-cycle-buffer]).
  2. If ARG is 16, show all (\\[universal-argument] \\[universal-argument] \\[fountain-dwim]).
  3. If ARG is 64, show top-level outline (\\[universal-argument] \\[universal-argument] \\[universal-argument] \\[fountain-dwim]).

See also `fountain-outline-show-synopses'."
  (interactive "P")
  (unless arg (setq arg 4))
  (let (has-top-level has-secondary-level has-scenes)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (outline-on-heading-p t)
          (when (= (funcall outline-level) 1)
            (setq has-top-level t))
          (when (< 1 (funcall outline-level) 6)
            (setq has-secondary-level t))
          (when (= (funcall outline-level) 6)
            (setq has-scenes t)))
        (outline-next-heading)))
    (cl-case arg
      ;; Show top-level headings.
      (64 (fountain-outline-set-buffer-state 'top-level))
      ;; Show all.
      (16 (fountain-outline-set-buffer-state 'all))
      ;; Cycle whole buffer headings.
      (4  (cond
           ((and (null fountain--outline-buffer-state)
                 has-top-level)
            (fountain-outline-set-buffer-state 'top-level))
           ((and (null fountain--outline-buffer-state)
                 fountain-outline-show-all-section-headings
                 has-secondary-level)
            (fountain-outline-set-buffer-state 'section-headings))
           ((and (null fountain--outline-buffer-state)
                 has-scenes)
            (fountain-outline-set-buffer-state 'scene-headings))
           ((and (eq fountain--outline-buffer-state 'top-level)
                 fountain-outline-show-all-section-headings
                 has-secondary-level)
            (fountain-outline-set-buffer-state 'section-headings))
           ((and (eq fountain--outline-buffer-state 'top-level)
                 has-scenes)
            (fountain-outline-set-buffer-state 'scene-headings))
           ((and (eq fountain--outline-buffer-state 'section-headings)
                 has-scenes)
            (fountain-outline-set-buffer-state 'scene-headings))
           (t (fountain-outline-set-buffer-state nil)))))))

(defun fountain-outline-level ()
  "Return the heading's nesting level in the outline.
Assumes that point is at the beginning of a heading and match
data reflects `outline-regexp'."
  (if (string-prefix-p "#" (match-string-no-properties 0))
      (string-width (match-string-no-properties 2))
    6))

(defun fountain-transpose (arg)
  "Transpose the current element forward past ARG elements.
If ARG is negative, transpose element backward instead."
  (interactive "*p")
  (let ((x (point))
        (offset 0))
    (unless (and (bolp) (eolp))
      (save-excursion
        (forward-paragraph 1)
        (setq offset (- x (point)))))
    (transpose-subr 'forward-paragraph arg)
    (goto-char (+ (point) offset))))

(defun fountain-forward-paragraph-or-transpose (arg)
  "Move forward to end of paragraph or transpose element forward.
Which depends on option `fountain-transpose-all-elements'.
With argument ARG, do it ARG times."
  (interactive "p")
  (if (outline-on-heading-p)
      (fountain-outline-move-subtree-down arg)
    (if fountain-transpose-all-elements
        (fountain-transpose arg)
      (forward-paragraph arg))))

(defun fountain-backward-paragraph-or-transpose (arg)
  "Move backward to start of paragraph or transpose element backward.
Which depends on option `fountain-transpose-all-elements'.
With argument ARG, do it ARG times."
  (interactive "p")
  (fountain-forward-paragraph-or-transpose (- arg)))

(defun fountain-insert-section-heading ()
  "Insert an empty section heading at the current outline level."
  (interactive "*")
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
              (outline-up-heading 1 t)))
        (setq level
              (if (outline-on-heading-p t)
                  (funcall outline-level)
                1))))
    (insert (make-string level ?#) " ")))

(define-obsolete-variable-alias 'fountain-pop-up-indirect-windows
  'fountain-outline-pop-up-indirect-windows "`fountain-mode' 3.5")
(defcustom fountain-outline-pop-up-indirect-windows
  nil
  "Non-nil if opening indirect buffers should make a new window."
  :type 'boolean
  :group 'fountain-outline)

(defun fountain-outline-to-indirect-buffer ()
  "Clone section/scene at point to indirect buffer.

Set `fountain-pop-up-indirect-windows' to control how indirect
buffer windows are opened."
  (interactive)
  (let ((pop-up-windows fountain-pop-up-indirect-windows)
        (base-buffer (buffer-name (buffer-base-buffer)))
        beg end target-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (outline-back-to-heading t)
        (setq beg (point))
        (when (or (fountain-match-section-heading)
                  (fountain-match-scene-heading))
          (let ((heading-name (match-string-no-properties 0)))
            (setq target-buffer
                  (concat base-buffer "-"
                          (if (string-match "^[#\s]+" heading-name)
                              (substring heading-name (match-end 0))
                            heading-name))))
          (outline-end-of-subtree)
          (setq end (point)))))
    (if (and (get-buffer target-buffer)
             (with-current-buffer target-buffer
               (goto-char (point-min))
               (skip-chars-forward "\n\s\t")
               (= (point) beg)))
        (pop-to-buffer target-buffer)
      (clone-indirect-buffer target-buffer t)
      (fountain-outline-show-all))
    (narrow-to-region beg end)))


;;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fountain-move-forward-scene (n)
  "Move forward N scene headings (backward if N is negative).
If N is 0, move to beginning of scene."
  (let ((p (if (<= n 0) -1 1))
        (move-fun
         (lambda (p)
           (while (not (or (= (point) (buffer-end p))
                           (fountain-match-scene-heading)))
             (forward-line p)))))
    (if (/= n 0)
        (while (/= n 0)
          (when (fountain-match-scene-heading) (forward-line p))
          (funcall move-fun p)
          (setq n (- n p)))
      (beginning-of-line)
      (funcall move-fun p))))

(defun fountain-goto-scene (scene)
  "Move point to SCENE in current buffer.
SCENE can be a string or number."
  (interactive "sGo to scene: ")
  (push-mark)
  (goto-char (point-min))
  (unless (fountain-match-scene-heading)
    (fountain-move-forward-scene 1))
  ;; First search for a basic string match for SCENE.
  (let (found)
    (while (and (not found)
                (< (point) (point-max))
                (fountain-match-scene-heading))
      (or (and (match-string 9)
               (prog1 (setq found
                            (let ((case-fold-search t))
                              (search-forward scene (line-end-position) t)))
                 (beginning-of-line)))
          (fountain-move-forward-scene 1)))
    ;; SCENE is not found but is a number.
    (when (and (not found)
               (< 0 (setq scene (string-to-number scene))))
      (goto-char (point-min))
      (unless (fountain-match-scene-heading)
        (fountain-move-forward-scene 1))
      (let ((scene-abs-fun
             (lambda (n)
               (setq n (car (fountain-revision-string-to-list n t)))
               (cond ((and n fountain-prefix-revised-scene-numbers) n)
                     (n (1- n)))))
            scene-count)
        (when (fountain-match-scene-heading)
          (setq scene-count (or (funcall scene-abs-fun (match-string 9))
                                1)))
        (while (and (< scene-count scene)
                    (< (point) (point-max)))
          (fountain-move-forward-scene 1)
          (when (fountain-match-scene-heading)
            (setq scene-count
                  (or (funcall scene-abs-fun (match-string 9))
                      (1+ scene-count)))))))))

(defun fountain-forward-character (&optional n limit character)
  "Goto Nth next character (or Nth previous if N is negative).

If LIMIT is `dialog', halt at end of dialogue. If LIMIT is `scene',
halt at end of scene.

If CHARACTER is non-nil, goto Nth next CHARACTER."
  (interactive "^p")
  (unless n (setq n 1))
  (let ((p (if (<= n 0) -1 1))
        (move-fun
         (lambda (p)
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
          (funcall move-fun p)
          (if (and character (fountain-match-character))
              (when (string= (upcase character)
                             (upcase (match-string-no-properties 2)))
                (setq n (- n p)))
            (setq n (- n p))))
      (beginning-of-line)
      (funcall move-fun p))))

(defun fountain-backward-character (&optional n)
  "Goto Nth previous character (or Nth next if N is negative)."
  (interactive "^p")
  (unless n (setq n 1))
  (fountain-forward-character (- n)))

(defun fountain-forward-this-character (n character)
  "\\<fountain-mode-map>Goto Nth next matching CHARACTER.

Successive calls to \\[fountain-forward-character] or \\[fountain-backward-character] will continue to navigate
by CHARACTER."
  (interactive
   (list current-prefix-arg
         (if (fountain-match-character)
              (match-string-no-properties 2)
           (completing-read "Forward by character: "
                            fountain--completion-characters
                            nil t (fountain-get-character 0)))))
  (unless n (setq n 1))
  (let ((nn (abs n))
        (forward-key  (where-is-internal 'fountain-forward-character
                                         fountain-mode-map t))
        (backward-key (where-is-internal 'fountain-backward-character
                                         fountain-mode-map t)))
    (when forward-key
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map forward-key
                     (lambda () (interactive)
                       (fountain-forward-character nn nil character)))
         (define-key map backward-key
                     (lambda () (interactive)
                       (fountain-forward-character (- nn) nil character)))
         map)
       t))
    (message "%s by character %s (`%s' or `%s' to repeat)"
             (if (<= n 0) "Backward" "Forward")
             character
             (key-description forward-key)
             (key-description backward-key)))
  (fountain-forward-character n nil character))

(defun fountain-backward-this-character (n character)
  "\\<fountain-mode-map>Goto Nth previous matching CHARACTER.

Successive calls to \\[fountain-forward-character] or \\[fountain-backward-character] will continue to navigate
by CHARACTER."
  (interactive
   (list current-prefix-arg
         (if (fountain-match-character)
              (match-string-no-properties 2)
           (completing-read "Backward by character: "
                            fountain--completion-characters
                            nil t (fountain-get-character 0)))))
  (unless n (setq n 1))
  (fountain-forward-this-character (- n) character))


;;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fountain-get-character (&optional n limit)
  "Return Nth next character (or Nth previous if N is negative).

If N is non-nil, return Nth next character or Nth previous
character if N is negative, otherwise return nil. If N is nil or
0, return character at point, otherwise return nil.

If LIMIT is `scene', halt at next scene heading. If LIMIT is
`dialog', halt at next non-dialogue element."
  (unless n (setq n 0))
  (save-excursion
    (save-restriction
      (widen)
      (fountain-forward-character n limit)
      (when (fountain-match-character)
        (match-string-no-properties 2)))))

(defun fountain-get-metadata ()
  "Return metadata of current buffer as an association list.

Key string is slugified and interned. Value string remains a
string. e.g.

  Draft date: 2015-12-25 -> (draft-date \"2015-12-25\")"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (metadata)
        (while (and (fountain-match-metadata)
                    (< (point) (point-max)))
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (when (stringp key)
              (setq key (intern (downcase (replace-regexp-in-string
                                           "[^[:alnum:]]+" "-" key)))))
            (forward-line 1)
            (if value
                (push (cons key value) metadata)
              (while (and (fountain-match-metadata)
                          (null (match-string 1))
                          (< (point) (point-max)))
                (push (match-string-no-properties 2) value)
                (forward-line 1))
              (push (cons key (string-join (reverse value) "\n")) metadata))))
        metadata))))

(defun fountain-get-dual-dialog (&optional pos)
  "Non-nil if point or POS is within dual dialogue.
Returns `'right' if within right-side dual dialogue, `left' if
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


;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fountain--auto-upcase-maybe ()
  "Upcase all or part of the current line contextually.

If `fountain-auto-upcase-scene-headings' is non-nil and point is
at a scene heading, activate auto upcasing for beginning of line
to scene number or point."
  (when (and fountain-auto-upcase-scene-headings
             (fountain-match-scene-heading))
    (upcase-region (line-beginning-position) (or (match-end 2) (point)))))

(defun fountain-indent ()
  "Appropriately indent a metadata value.
When point is at metadata value on its own line, indent to
`tab-width'."
  (when (save-excursion (forward-line -1) (fountain-match-metadata))
    (if (= tab-width (current-column))
        (delete-horizontal-space)
      (delete-horizontal-space)
      (indent-to tab-width))))

(defun fountain-dwim (&optional arg)
  "Call a command based on context (Do What I Mean).

  1. If ARG is non-nil (when prefixed with \\[universal-argument]), call
    `fountain-outline-cycle-buffer' and pass ARG.
  2. If point is at metadata, call `indent-for-tab-command'.
  3. If point is inside an empty parenthetical, delete it.
  4. If point is inside a non-empty parenthetical, move to a newline.
  5. If point is at a blank line within dialogue, insert a parenthetical.
  6. If point is at a note, cycle visibility of that note.
  7. If point is at the end of line, call `fountain-completion-at-point'.
  8. If point is an outline heading, call `fountain-outline-cycle'."
  (interactive "p")
  (cond ((and arg (<= 4 arg))
         (fountain-outline-cycle-buffer arg))
        ((and (eq (char-before) ?\()
              (eq (char-after)  ?\)))
         (delete-region (1- (point)) (1+ (point))))
        ((and (fountain-match-paren)
              (fountain-blank-after-p))
         (end-of-line)
         (newline))
        ((fountain-match-paren)
         (forward-line))
        ((and (bolp) (eolp)
              (fountain-in-dialog-maybe))
         (insert-parentheses))
        ((and fountain-dwim-insert-next-character
              (eolp)
              (fountain-in-dialog-maybe))
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
         (fountain-outline-cycle))
        ((indent-for-tab-command))))

(defun fountain-upcase-line (&optional arg)
  "Upcase the line.
If ARG is non-nil (when prefixed with \\[universal-argument]), insert `.' at
beginning of line (if not already present) to force a scene heading."
  (interactive "*P")
  (when (and arg (not (fountain-match-scene-heading)))
    (save-excursion (beginning-of-line) (insert ".")))
  (upcase-region (line-beginning-position) (point)))

(defun fountain-upcase-line-and-newline (&optional arg)
  "Upcase the line and insert a newline.
If ARG is non-nil (when prefixed with \\[universal-argument]), insert `.' at
beginning of line (if not already present) to force a scene heading."
  (interactive "*P")
  (fountain-upcase-line arg)
  (newline))

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
  (interactive "*")
  (widen)
  (when (outline-back-to-heading)
    (forward-line)
    (or (bolp) (newline))
    (unless (and (bolp) (eolp)
                 (fountain-blank-after-p))
      (save-excursion
        (newline)))
    (insert "= ")
    (when (outline-invisible-p) (fountain-outline-cycle))
    (run-hooks 'fountain-insert-synopsis-hook)))

(defun fountain-insert-note ()
  "Call `comment-dwim' with note delimiters (`[[ ]]')."
  (interactive)
  (let ((comment-start "[[")
        (comment-end "]]")
        (comment-continue ""))
    (comment-dwim nil)
    (run-hooks 'fountain-insert-note-hook)))

(define-obsolete-function-alias 'fountain-refresh-continued-dialog
  'fountain-add-continued-dialog "`fountain-mode' 3.5")

(defun fountain-add-continued-dialog (&optional delete)
  "Add or update continued dialogue in buffer.
If DELETE is non-nil (when prefixed with \\[universal-argument]), remove continued
dialogue.

Add `fountain-continued-dialog-string' to characters speaking in
succession, or remove where appropriate, in accessible portion of
the buffer."
  (interactive "*P")
  (save-excursion
    (goto-char (point-min))
    (unless (fountain-match-character) (fountain-forward-character))
    (let ((case-fold-search nil))
      (while (< (point) (point-max))
        (when (fountain-match-character)
          (let ((contd (string= (fountain-get-character 0)
                                (fountain-get-character -1 'scene)))
                (with-string (looking-at-p
                              (concat ".*" fountain-continued-dialog-string "$"))))
            (cond ((or delete (and (not contd) with-string))
                   (when (re-search-forward
                          (concat "[\s\t]*" fountain-continued-dialog-string "[\s\t]*")
                          (line-end-position) t)
                     (delete-region (match-beginning 0) (match-end 0))))
                  ((and contd (not with-string))
                   (when (re-search-forward "\s*$" (line-end-position) t)
                     (replace-match (concat "\s" fountain-continued-dialog-string))))))
          (fountain-forward-character))))))

(defun fountain-remove-continued-dialog ()
  "Remove continued dialogue in buffer.
Remove `fountain-continued-dialog-string' on all characters in
accessible portion of the buffer."
  (interactive "*")
  (fountain-add-continued-dialog t))


;;; Scene Numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain-scene-numbers ()
  "Options for scene numbers in `fountain-mode'."
  :prefix "fountain-scene-numbers-"
  :group 'fountain)

(defcustom fountain-display-scene-numbers-in-margin
  nil
  "If non-nil, display scene numbers in the right margin.
This option does not affect file contents."
  :group 'fountain-scene-numbers
  :type 'boolean
  :safe 'booleanp
  :set #'fountain--set-and-refresh-font-lock)

(defcustom fountain-prefix-revised-scene-numbers
  nil
  "If non-nil, revised scene numbers are prefixed.

If non-nil, when inserting new scene headings after numbering
existing scene headings, revised scene number format works as
follows:

  10
  A11 <- new scene
  B11 <- new scene
  11

If further scene headings are inserted:

  10
  AA11 <- new scene
  BA11 <- new scene
  A11
  AB11 <- new scene
  BB11 <- new scene
  B11
  11

Otherwise, when nil:

  10
  10A <- new scene
  10B <- new scene
  11

And if further scene headings are inserted:

  10
  10AA <- new scene
  10AB <- new scene
  10A
  10BA <- new scene
  10BB <- new scene
  10B
  11

  10
  10A <- new scene
  11

n.b. Using conflicting revised scene number format in the same
script may result in errors in output."
  :type 'boolean
  :safe 'booleanp
  :group 'fountain-scene-numbers)

(make-obsolete-variable 'fountain-scene-numbers-first-revision-char
                        "use \"A\" and \"B\" system." "`fountain-mode' 3.8")

(defcustom fountain-scene-numbers-separator
  nil
  "Character to separate scene numbers."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Dash" ?-)
                 (const :tag "Period" ?.))
  :safe (lambda (value)
          (or (null value) (characterp value)))
  :group 'fountain-scene-numbers)

(defun fountain-revision-string-to-list (string &optional scene)
  "Convert revision number STRING to a list.

If SCENE and `fountain-prefix-revised-scene-numbers' are non-nil:

  \"A10\" -> (10 -26)
  \"BA10\" -> (10 -25 -26)
  \"B10\" -> (10 -25)
  \"10\" -> (10)

Otherwise:

  \"10\" -> (10)
  \"10A\" -> (11 -26)
  \"10BA\" -> (11 -25 -26)
  \"10B\" -> (11 -25)"
  (let (number revision)
    (when (stringp string)
      (if (and scene fountain-prefix-revised-scene-numbers)
          (when (string-match "\\([a-z]*\\)[\\.-]*\\([0-9]+\\)[\\.-]*" string)
            (setq number (string-to-number (match-string-no-properties 2 string)))
            (setq revision (match-string-no-properties 1 string)))
        (when (string-match "\\([0-9]+\\)[\\.-]*\\([a-z]*\\)[\\.-]*" string)
          (setq number (string-to-number (match-string-no-properties 1 string)))
          (unless (string-empty-p
                   (setq revision (match-string-no-properties 2 string)))
            (setq number (1+ number)))))
      (setq revision
            (mapcar (lambda (n)
                      (- (upcase n) 91)) ; hardcoded to A
                    revision))
      (cons number revision))))

(defun fountain-revision-list-to-string (list &optional scene)
  "Convert revision number LIST to a string.

If SCENE and `fountain-prefix-revised-scene-numbers' are non-nil:

    (10 -26) -> \"A10\"
    (10 -25 -26) -> \"BA10\"
    (10 -25) -> \"B10\"
    (10) -> \"10\"

Otherwise:

    (10) -> \"10\"
    (11 -26) -> \"10A\"
    (11 -25 -26) -> \"10BA\"
    (11 -25) -> \"10B\""
  (let ((number (car list))
        separator revision)
    (when (cdr list)
      (setq separator (if fountain-scene-numbers-separator
                         (char-to-string fountain-scene-numbers-separator)
                       ""))
      (setq revision
            (mapconcat (lambda (char)
                         (char-to-string
                          (+ char 91))) ; hardcoded to A
                       (cdr list) separator)))
    (if (and scene fountain-prefix-revised-scene-numbers)
        (concat revision separator (number-to-string number))
      (concat (number-to-string (if (cdr list) (1- number) number))
              separator revision))))

(defun fountain-calc-revision-number (low high)
  "Find the next significant revision number between LOW and HIGH."
  ;; If we have HIGH and LOW is greater than HIGH, signal an error.
  (if (and low high (version-list-< high low))
      (user-error "Can't find revision between %s and %s"
                  (fountain-revision-list-to-string low)
                  (fountain-revision-list-to-string high))
    (let ((current (or low (list 0))) previous)
      ;; First the easiest case, HIGH is null so we either chop off all
      ;; revisions, or increment.
      (or (and (null high)
               (or (and (cdr low) (list (car low)))
                   (list (1+ (car current)))))
          ;; CURRENT is LOW. First chop off revisions to get to higher number.
          ;; Set PREVIOUS so we remember the last number that was okay. Once we
          ;; shoot over HIGH, move to next.
          (while (and current (version-list-< current high))
            (setq previous current
                  current (butlast current)))
          ;; When < LOW PREVIOUS HIGH, we have our number.
          (when (and (version-list-< low previous)
                     (version-list-< previous high))
            previous)
          ;; Set CURRENT to PREVIOUS. While CURRENT = LOW, increment the least
          ;; significant number. We need to return nil to move to next.
          (progn (setq current previous)
                 (when (version-list-= current low)
                   (let ((rev (reverse current)))
                     (setq current (reverse (cons (1+ (car rev)) (cdr rev)))))
                   nil))
          ;; When < LOW PREVIOUS HIGH, we have our number.
          (when (and (version-list-< low current)
                     (version-list-< current high))
            current)
          ;; HIGH should now be <= to CURRENT. Append a revision and return.
          (while (and current (version-list-<= high current))
            (let ((rev (reverse current)))
              (setq current (reverse (cons -26 rev)))))
          current))))

(defun fountain-get-forced-scene-numbers ()
  "Return an association list of forced page number positions in buffer.
List is comprised of cons cells of postion and page number list."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (list)
        (while (< (point) (point-max))
          (fountain-move-forward-scene 1)
          (when (and (fountain-match-scene-heading)
                     (match-string 9))
            (push (cons (point)
                        (fountain-revision-string-to-list (match-string 9) t))
                  list)))
        (reverse list)))))

(defun fountain-get-scene-number ()
  (save-excursion
    (save-restriction
      (widen)
      ;; Make sure we're at a scene heading.
      (fountain-move-forward-scene 0)
      ;; Unless we're at a scene heading now, raise a user error.
      (unless (fountain-match-scene-heading)
        (user-error "Before first scene heading"))
      ;; If scene heading is numbered, no further action is required.
      (if (and (fountain-match-scene-heading)
               (match-string 9))
          (fountain-revision-string-to-list (match-string 9) t)
        ;; Otherwise set position as X and get all forced scene numbers.
        (let ((x (point))
              (forced (fountain-get-forced-scene-numbers))
              low high y)
          ;; While X is less than the top of SCENE-NUMBERS, pop and set to
          ;; PREVIOUS.
          (while (and forced (< (caar forced) x))
            (let ((previous (pop forced)))
              (setq y (car previous))
              (setq low (cdr previous))))
          ;; If we still have SCENE-NUMBERS the top is our HIGH scene number.
          (when forced (setq high (cdr (car forced))))
          (goto-char (or y (point-min)))
          (while (< (point) x)
            (fountain-move-forward-scene 1)
            (setq low (fountain-calc-revision-number low high)))
          low)))))

(defun fountain-add-scene-numbers (&optional arg)
  "Add scene numbers to scene headings in current buffer.
If ARG is non-nil (when prefixed with \\[universal-argument]), remove
scene numbers.

Adding scene numbers to scene headings after numbering existing
scene headings will use a prefix or suffix letter, depending on
the value of `fountain-prefix-revised-scene-numbers', which see."
  (interactive "*P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((job (make-progress-reporter
                  (format "%s scene numbers..." (if arg "Removing" "Adding"))))
            (n (list 0))
            buffer-invisibility-spec)
        (goto-char (point-min))
        (unless (fountain-match-scene-heading)
          (fountain-move-forward-scene 1))
        (while (and (fountain-match-scene-heading)
                    (< (point) (point-max)))
          (if arg
              (when (match-string 9)
                (delete-region (match-beginning 7) (match-end 10)))
            (unless (match-string 9)
              (end-of-line)
              (delete-horizontal-space t)
              (insert "\s#"
                      (fountain-revision-list-to-string
                       (fountain-get-scene-number) t)
                      "#")))
          (fountain-move-forward-scene 1)
          (progress-reporter-update job))
        (progress-reporter-done job)))))

(defun fountain-remove-scene-numbers ()
  "Remove scene numbers from scene headings in current buffer."
  (interactive "*")
  (fountain-add-scene-numbers t))


;;; Pagination ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fountain-pagination ()
  "Options for calculating page length in `fountain-mode'."
  :group 'fountain
  :prefix "fountain-page-"
  :prefix "fountain-pagination-")

(defcustom fountain-page-size
  'letter
  "Paper size to use on export."
  :group 'fountain-pagination
  :type '(radio (const :tag "US Letter" letter)
                (const :tag "A4" a4))
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (fountain-pagination-update))))))

(defcustom fountain-page-max-lines
  '((letter . 55) (a4 . 60))
  "Integer representing maximum number of lines on a page.

n.b. If you change this option after locking pages in a script,
you may get incorrect output."
  :group 'fountain-pagination
  :type '(choice integer
                 (list (cons (const :tag "US Letter" letter) integer)
                       (cons (const :tag "A4" a4) integer))))

(defcustom fountain-pagination-ignore-restriction
  nil
  "When non-nil, counting pages should ignore buffer narrowing."
  :group 'fountain-pagination
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-pagination-update-invalid
  nil
  "\\<fountain-mode-map>When non-nil, update pagination properties when using paging commands.
Otherwise, only update pagination with \\[fountain-pagination-update]."
  :group 'fountain-pagination
  :type 'boolean
  :safe 'booleanp)

(defcustom fountain-pagination-max-change
  150
  "Maximum change in page characters before invalidating pagination."
  :group 'fountain-pagination
  :type 'integer
  :safe 'integerp)

(defcustom fountain-pagination-break-sentences
  nil
  "When non-nil, pagination disregards sentences.
That is, page breaks may occur mid-sentence."
  :group 'fountain-pagination
  :type 'boolean
  :safe 'booleanp)

(define-obsolete-variable-alias 'fountain-more-dialog-string
  'fountain-pagination-more-dialog-string "`fountain-mode' 3.5")
(defcustom fountain-pagination-more-dialog-string
  "(MORE)"
  "String to append to dialogue when breaking across pages."
  :group 'fountain-pagination
  :type 'string
  :safe 'stringp)

(defconst fountain-dual-dialog-left-elements
  '(dual-character-left dual-dialog-left dual-paren-left)
  "List of elements constituent of left-side dual dialogue.")

(defconst fountain-dual-dialog-right-elements
  '(dual-character-right dual-dialog-right dual-paren-right)
  "List of elements constituent of right-side dual dialogue.")

(defvar fountain-fill-elements
  '((section-heading      . 63)
    (scene-heading        . 59)
    (action               . 63)
    (character            . 38)
    (paren                . 26)
    (dialog               . 35)
    (dual-character-left  . 28)
    (dual-character-right . 28)
    (dual-paren-left      . 16)
    (dual-paren-right     . 16)
    (dual-dialog-left     . 28)
    (dual-dialog-right    . 28)
    (trans                . 16)
    (center               . 63)
    (synopsis             . 63)
    (note                 . 63)
    (page-break           . 63))
  "Association list of elements and their widths in columns.")

(defvar fountain-printed-elements
  (append '(scene-heading action character dialog paren trans center)
          fountain-dual-dialog-left-elements
          fountain-dual-dialog-right-elements)
  "List of elements considered printed on a page.
i.e. Only these elements count towards page length.")

(defun fountain-goto-page-break-point ()
  "Move point to appropriate place to break a page.
This is usually before point, but may be after if only skipping
over whitespace.

Comments are assumed to be deleted."
  (when (looking-at fountain-pagination-more-dialog-string) (forward-line))
  (when (looking-at "[\n\s\t]*\n") (goto-char (match-end 0)))
  (let ((element (fountain-get-element)))
    (cond
     ;; End of buffer
     ((eobp) nil)
     ;; If element is not included in export, we can safely break
     ;; before.
     ((not (memq element fountain-printed-elements))
      (beginning-of-line))
     ;; We cannot break page in dual dialogue. If we're at right dual
     ;; dialogue, skip back to previous character.
     ((memq element fountain-dual-dialog-right-elements)
      (fountain-forward-character 0)
      (fountain-forward-character -1))
     ;; If we're at left dual dialogue, break at character.
     ((memq element fountain-dual-dialog-left-elements)
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
     ((eq element 'dialog)
      (skip-chars-forward "\s\t")
      (unless (or (bolp)
                  fountain-pagination-break-sentences
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
                  fountain-pagination-break-sentences
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

(defun fountain-move-to-fill-width (element &optional troff)
  "Move point to column of ELEMENT fill limit suitable for breaking line.
If TROFF is non-nil, consider troff escapes. Skip over comments."
  (let ((fill-width
         (cdr (assq element fountain-fill-elements))))
    (let ((i 0))
      (while (and (< i fill-width) (not (eolp)))
        (cond ((and troff (looking-at "\\\\\\(z.\\|f\\[.*?\\]\\)"))
               (goto-char (match-end 0)))
              ((= (syntax-class (syntax-after (point))) 0)
               (forward-char 1) (cl-incf i))
              ((forward-comment 1))
              (t
               (forward-char 1) (cl-incf i)))))
    (skip-chars-forward "\s\t")
    (when (eolp) (forward-line))
    (unless (or (bolp) (eobp))
      (fill-move-to-break-point (line-beginning-position)))))

(defun fountain-move-forward-page ()
  "Move forward from point by an approximate page."
  (let ((skip-whitespace-fun
         (lambda ()
           (when (looking-at "[\n\s\t]*\n")
             (goto-char (match-end 0))))))
    ;; Consider the title page as page 0.
    (if (fountain-match-metadata)
        (progn
          (while (and (fountain-match-metadata)
                      (< (point) (point-max)))
            (forward-line))
          (funcall skip-whitespace-fun))
    ;; Pages don't begin with blank space, so skip over any at point.
    (funcall skip-whitespace-fun)
    ;; If we're at a page break, move to its end and skip over whitespace.
    (when (fountain-match-page-break)
      (end-of-line)
      (funcall skip-whitespace-fun))
    ;; Start counting lines.
    (let ((page-lines
           (cdr (assq fountain-page-size fountain-page-max-lines)))
          (line-count 0)
          (line-count-left 0)
          (line-count-right 0)
          (first-child t))
      ;; Right away, if we're at dialogue it means it was broken from the
      ;; previous page, so account for the additional line for character name.
      (when (fountain-match-dialog) (cl-incf line-count))
      ;; Begin the main loop, which only halts if we reach the end of buffer, a
      ;; forced page break, or after the maximum lines in a page.
      (while (and (< line-count page-lines)
                  (< (point) (point-max))
                  (not (fountain-match-page-break)))
        (cond
         ;; If we're at the end of a line (but not also the beginning, i.e. not
         ;; a blank line) then move forward a line and increment line-count.
         ((and (eolp) (not (bolp)))
          (forward-line)
          (cl-incf line-count))
         ;; If we're looking at newline, skip over it and any whitespace and
         ;; increment line-count.
         ((funcall skip-whitespace-fun)
          (cl-incf line-count))
         (t
    ;; We are at an element. Find what kind of element. If it is not included in
    ;; export, skip over without incrementing line-count. Otherwise move to
    ;; fill-width and increment appropriate line-count: for dual-dialogue,
    ;; increment either line-count-left/right, otherwise increment line-count.
    ;; Once we're at a blank line, add the greater of the former two to the
    ;; latter.
    (let ((element (fountain-get-element)))
      (if (not (memq element fountain-printed-elements))
          (progn (end-of-line) (funcall skip-whitespace-fun))
        (fountain-move-to-fill-width element)
        (cond
         ;; Account for dual dialogue elements as distinct columns.
         ((memq element fountain-dual-dialog-left-elements)
          (cl-incf line-count-left))
         ((memq element fountain-dual-dialog-right-elements)
          (cl-incf line-count-right))
         ;; All other elements.
         (t (cl-incf line-count)))
        (when (and (eolp) (bolp)
                   (or (< 0 line-count-left)
                       (< 0 line-count-right)))
          (setq line-count
                (+ line-count (max line-count-left line-count-right)))))
      ;; Scene headings might count as two lines.
      (when (and (eq element 'scene-heading) (not first-child)
                 (memq 'double-space fountain-export-scene-heading-format))
        (cl-incf line-count))
      (setq first-child nil))))))
    ;; We are not at the furthest point in a page. Skip over any
    ;; remaining whitespace, then go back to page-break point.
    (fountain-goto-page-break-point))))

;; FIXME: This could be more efficient by only updating pagination props from
;; the point where they're invalid.
(defun fountain-pagination-update ()
  "Update pagination properties in current buffer.

Gives buffer content a `fountain-pagination' text property
of (PAGE . LENGTH) where PAGE is a linear page count using
`fountain-move-forward-page' from `point-min' and LENGTH is the
number of characters of each such page.

If a page character length changes by more than
`fountain-pagination-max-change' then the pagination properties
are considered invalid (see `fountain-pagination-validate')."
  (interactive)
  (let ((job (make-progress-reporter "Updating pagination...")))
    (save-excursion
      (save-restriction
        (when fountain-pagination-ignore-restriction (widen))
        (goto-char (point-min))
        (with-silent-modifications
          (let ((page-num (if (fountain-match-metadata) 0 1))
                (previous-page (point))
                )
            (while (< (point) (point-max))
              (fountain-move-forward-page)
              (put-text-property previous-page (point) 'fountain-pagination
                                 (cons page-num (- (point) previous-page)))
              (cl-incf page-num)
              (setq previous-page (point))
              (progress-reporter-update job))))))
    (progress-reporter-done job)))

(defun fountain-pagination-validate ()
  "Validate pagination properties in current buffer.

Returns non-nil if the car of `fountain-pagination' text property
equals the car of previous page + 1 (i.e. that pages are in
order) and the cdr is within the length of the current page +/-
`fountain-pagination-max-change' (i.e. that page length hasn't
changed too much), otherwise pagination is considered invalid and
returns nil."
  (save-excursion
    (save-restriction
      (when fountain-pagination-ignore-restriction (widen))
      (goto-char (point-min))
      (let ((page-num (if (fountain-match-metadata) 0 1))
            (page-order t)
            (change 0))
        (while (and (<= change fountain-pagination-max-change)
                    page-order
                    (< (point) (point-max)))
          (let ((page-props (get-text-property (point) 'fountain-pagination))
                (page-start (point)))
            (unless (eobp) (setq page-order (equal (car-safe page-props) page-num)))
            (goto-char (or (next-single-property-change (point) 'fountain-pagination)
                           (point-max)))
            (setq change (max change (abs (- (point)
                                             page-start
                                             (or (cdr-safe page-props) 0)))))
            (cl-incf page-num)))
        (and page-order (<= change fountain-pagination-max-change))))))

(defun fountain-forward-page (n)
  "Move to Nth next page (or Nth previous if N is negative).

First check if pagination properties are valid and call
`fountain-pagination-update' if not."
  (interactive "^p")
  (when fountain-pagination-update-invalid
    (unless (fountain-pagination-validate) (fountain-pagination-update)))
  (let ((p (if (<= n 0) -1 1))
        (move-fun
         (lambda (p)
           (funcall (if (< p 0) #'re-search-backward #'re-search-forward)
                    fountain-page-break-regexp
                    (funcall (if (< p 0)
                                 #'previous-single-property-change
                               #'next-single-property-change)
                             (point) 'fountain-pagination)
                    'move))))
    (if (/= n 0)
        (while (/= n 0)
          (when (fountain-match-page-break) (forward-line p))
          (funcall move-fun p)
          (setq n (- n p)))
      (beginning-of-line)
      (unless (fountain-match-page-break) (funcall move-fun p)))))

(defun fountain-backward-page (n)
  "Move to Nth previous page (or Nth next if N is negative).

This command simply calls `fountain-forward-page' with 1 - N."
  (interactive "^p")
  (fountain-forward-page (- n)))

(defun fountain-goto-page (n)
  "Move point to Nth page in current buffer.

This is an approximate calculation. Different export tools will
paginate in slightly different ways. Customize options
`fountain-page-max-lines' and `fountain-pagination-break-sentences'
to suit your preferred tool's pagination method."
  (interactive "NGo to page: ")
  (when fountain-pagination-update-invalid
    (unless (fountain-pagination-validate) (fountain-pagination-update)))
  (save-restriction
    (when fountain-pagination-ignore-restriction (widen))
    (push-mark)
    (goto-char (point-min))
    (let ((p (car (get-text-property (point) 'fountain-pagination))))
      (unless (<= n p)
        (fountain-forward-page (- n p))))))

(defun fountain-insert-page-break ()
  "Insert a page break at appropriate place preceding point."
  (interactive "*")
  ;; Save a marker where we are.
  (let ((x (point-marker))
        (page-break "===")
        element)
    ;; Move point to appropriate place to break page.
    (fountain-goto-page-break-point)
    (setq element (fountain-get-element))
    (unless (eq element 'page-break)
      ;; At this point, element can only be: section-heading,
      ;; scene-heading, character, action, paren or dialog. Only paren and
      ;; dialog require special treatment.
      (if (memq element '(dialog paren))
          (let ((name (fountain-get-character -1)))
            (delete-horizontal-space)
            (unless (bolp) (insert-before-markers "\n"))
            (insert-before-markers
             (concat fountain-pagination-more-dialog-string "\n\n"
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
          ;; Here we could be mid-line, but still with leading whitespace.
          (skip-chars-backward "\s\t")
          (unless (bolp)
            (delete-horizontal-space)
            (insert-before-markers "\n"))
          (unless (fountain-blank-before-p) (insert-before-markers "\n"))
          (insert-before-markers page-break "\n\n")))
      ;; Return to where we were.
      (goto-char x)
      (set-marker x nil)))
  (run-hooks 'fountain-insert-page-break-hook))

(defun fountain-get-page-count ()
  "Return a cons of the current page number and the total pages."
  (save-restriction
    (when fountain-pagination-ignore-restriction (widen))
    (when fountain-pagination-update-invalid
      (unless (fountain-pagination-validate) (fountain-pagination-update)))
    (cons (car (get-text-property (max (if (eobp) (1- (point)) (point)) 1)
                                  'fountain-pagination))
          (car (get-text-property (max (1- (point-max)) 1)
                                  'fountain-pagination)))))

(defun fountain-count-pages (&optional interactive)
  "Return the current page of total page count of current buffer.
When called interactively or with optional argument INTERACTIVE,
return with `message'.

This is an approximate calculation. Different export tools will
paginate in slightly different ways. Customize options
`fountain-page-max-lines', `fountain-pagination-break-sentences'
and `fountain-export-scene-heading-format' to suit
your preferred tool's pagination method."
  (interactive "p")
  (let ((page-count (fountain-get-page-count))
        string)
    (setq string (format "Page %s of %s" (car page-count) (cdr page-count)))
    (if interactive (message string) string)))


;;; Revisions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fountain-compare-buffers (old new)
  "Compare buffers OLD and NEW and apply text properties to new text.

Applies the property `fountain-revision' with value `addition' to
new text.

OLD and NEW must be buffers visiting files."
  ;; (interactive "bOld file buffer: \nbNew file buffer: ")
  (let ((rev-buf (get-buffer-create "*Fountain Revision*"))
        (old-file (or (buffer-file-name (get-buffer old))
                      (user-error "%s must be visiting a file" old)))
        (new-file (or (buffer-file-name (get-buffer new))
                      (user-error "%s must be visiting a file" new)))
        old-line-length new-line-length)
    (with-current-buffer old
      (setq old-line-length (line-number-at-pos (point-max) t)))
    (with-current-buffer new
      (setq new-line-length (line-number-at-pos (point-max) t)))
    (with-current-buffer rev-buf
      (erase-buffer)
      (call-process diff-command nil (list t nil) nil
                    (format "-BbTU%d" (max old-line-length new-line-length))
                    old-file new-file)
      ;; Delete diff metadata output
      (goto-char (point-min))
      (when (re-search-forward "^@@.*@@\n" nil t)
        (delete-region (point-min) (match-end 0)))
      ;; Delete stuff we don't want - deleted lines and prefixes of unchanged
      ;; lines
      (goto-char (point-min))
      (while (re-search-forward "^\\(-\t.*\n\\|\s\t\\)" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward "^\\(+\t\\)\\(.*?\\)$" nil t)
        (put-text-property (match-beginning 2) (match-end 2)
                           'fountain-revision 'addition)
        (delete-region (match-beginning 1) (match-end 1))))
    (switch-to-buffer rev-buf)
    (goto-char (point-min))
    (fountain-mode)))


;;; Exporting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fountain-export-troff-requests
  '(scene-heading
    action
    character
    dialog
    paren
    dual-character-left
    dual-character-right
    dual-dialog-left
    dual-dialog-right
    dual-paren-left
    dual-paren-right
    trans
    center
    page-break)
  "List of elements and associated roff requests.")

(defvar fountain-export-troff-buffer
  "*Fountain troff Output*"
  "Buffer name for `fountain-export-troff' output.")

(defvar fountain-export-troff-macro
  "\
.\\\" -*- mode: nroff -*-
.do xflag 3
.if !rPS .nr PS 12
.ps \\n[PS]
.if !rVS .nr VS 12.5
.vs \\n[VS]
.if !rPW .nr PW 6
.ll \\n[PW]i
.pl 999
.
.if !rscenespace .nr scenespace 2
.if !rscenebold .nr scenebold 0
.if !rsceneunderline .nr sceneunderline 0
.if !rnumberfirst .nr numberfirst 0
.if !rscenenumbers .nr scenenumbers 0
.nr pagetop 1
.po 1.25i
.nf
.nh
.
.\\\" Format reset
.de reset
.ft C
'ce 0
'in 0
..
.\\\" Blank line
.de blank
.sp |\\\\n[.h]u
.ie \\\\n[pagetop] .sp |1i
.el .sp 1
.nr pagetop 0
..
.\\\" Normal page header
.de header
.sp 0.5i
.tl '''%.'
.sp |1i
..
.\\\" Blank page header
.de headerblank
.sp 0.5i
.tl ''''
.sp |1i
.wh 0 header
..
.\\\" Action
.de action
.reset
.blank
..
.\\\" Scene heading
.de scene-heading
.reset
.sp |\\\\n[.h]u
.ie \\\\n[pagetop] .sp |1i
.el .sp \\n[scenespace]
.nr pagetop 0
.if \\\\n[scenebold] .ft 8
.if \\\\n[scenenumbers] \\{\\
\\h[|-0.75i] \\\\$1 \\h[|6i] \\\\$1
.sp -1 \\}
..
.\\\" Character name
.de character
.reset
.blank
.in 2.5i
..
.\\\" Parenthetical
.de paren
.reset
.in 2i
.ti -8p
..
.\\\" Dialogue
.de dialog
.reset
.in 1.5i
..
.\\\" Transition
.de trans
.reset
.blank
.in 4i
..
.\\\" Dual-dialogue character name (left)
.de dual-character-left
.reset
.blank
.mk dualtop
.in 1i
..
.\\\" Dual-dialogue parenthetical (left)
.de dual-paren-left
.reset
.in 0.5i
.ti -8p
..
.\\\" Dual dialogue (left)
.de dual-dialog-left
.reset
..
.\\\" Dual-dialogue character name (right)
.de dual-character-right
.reset
.blank
.sp |\\\\n[dualtop]u
.in 4i
..
.\\\" Dual-dialogue parenthetical (right)
.de dual-paren-right
.reset
.in 3.3i
..
.\\\" Dual dialogue (right)
.de dual-dialog-right
.reset
.in 3.1i
..
.\\\" Center text
.de center
.reset
.blank
.ce 99
..
.\\\" Page break
.de page-break
.bp \\\\$1
.nr pagetop 1
..
.\\\" Title page title section line
.de titleline
.reset
.sp
.ce 99
..
.\\\" Title page contact/note section line
.de titlenote
.reset
.sp
..
.reset"
  "Troff macro header for exporting to PostScript/PDF.

n.b. This is not intended to be used independently of buffers
prepared with `fountain-export-troff'.")

(defvar fountain-export-troff-macro-page-1
  "\
.\\\" Page 1.
.nr % 0
.ie \\n[numberfirst] .wh 0 header
.el .wh 0 headerblank
.nr nl -1"
  "Troff macro for page 1 when exporting to PostScript/PDF.")

(defun fountain-export-troff-string (string element)
  "Convert STRING of ELEMENT to troff markup."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    ;; Leading dot
    (if (looking-at "\\.") (replace-match "\\&." nil t))
    ;; Curly quotes
    (goto-char (point-min))
    (while (re-search-forward "\\(\\b\\|\\[\\.!?]\\)\\(\"\\)" nil t)
      (replace-match "\\[rq]" t t nil 2))
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (replace-match "\\[lq]" t t))
    ;; Bold-italic
    (goto-char (point-min))
    (while (re-search-forward fountain-bold-italic-regexp nil t)
      (replace-match "\\\\f[9]\\3\\\\f[]" t nil nil 1))
    ;; Bold
    (goto-char (point-min))
    (while (re-search-forward fountain-bold-regexp nil t)
      (replace-match "\\\\f[8]\\3\\\\f[]" t nil nil 1))
    ;; Italics
    (goto-char (point-min))
    (while (re-search-forward (concat "\\(?:" fountain-italic-regexp
                                      "\\|"
                                      fountain-lyrics-regexp
                                      "\\)")
                              nil t)
      (replace-match "\\\\f[7]\\3\\\\f[]" t nil nil 1))
    ;; Underline
    (goto-char (point-min))
    (if (and (eq element 'scene-heading)
             (memq 'underline fountain-export-scene-heading-format))
        (while (re-search-forward "." nil t)
          (forward-char -1)
          (if (looking-at "\\\\f?\\[.*?\\]")
              (goto-char (match-end 0))
            (replace-match "\\\\z_\\&" t)))
      (while (re-search-forward fountain-underline-regexp nil t)
        (let ((beg (match-beginning 2))
              (end (progn
                     (goto-char (match-end 0))
                     (point-marker))))
          (goto-char beg)
          (while (re-search-forward "." end t)
            (forward-char -1)
            (cond ((= (char-after) ?_)
                   (delete-char 1))
                  ((looking-at "\\\\f?\\[.*?\\]")
                   (goto-char (match-end 0)))
                  (t
                   (replace-match "\\\\z_\\&" t)))))))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (fountain-move-to-fill-width element t)
      (when (looking-back "\\\\z_" (- (point) 3))
        (delete-region (match-beginning 0) (match-end 0)))
      (unless (or (bolp) (eolp))
        (delete-horizontal-space)
        (insert "\n")))
    (buffer-string)))

(defun fountain-export-region-to-troff (start end &optional
                                              metadata output-buffer)
  "Convert from START to END to troff.
If passed METADATA, convert that info to title page. Send to
buffer OUTPUT-BUFFER.

If OUTPUT in nil, `fountain-export-output-buffer' is used."
  (let ((job (make-progress-reporter "Converting to troff...")))
    (unless output-buffer (setq output-buffer fountain-export-troff-buffer))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (insert fountain-export-troff-macro)
      (unless (bolp) (insert "\n"))
      (when metadata
        (insert ".\\\" Title page\n"
                ".sp |4i\n")
        (let (string)
          (dolist (key fountain-export-title-page-title-keys)
            (when (setq string (cdr (assq
                                     (intern (downcase (replace-regexp-in-string
                                                        "[^[:alnum:]]+" "-" key)))
                                     metadata)))
              (insert
               (format ".titleline\n%s\n" (fountain-export-troff-string string 'action)))))
          (insert ".sp |8i\n")
          (dolist (key fountain-export-title-page-contact-keys)
            (when (setq string (cdr (assq
                                     (intern (downcase (replace-regexp-in-string
                                                        "[^[:alnum:]]+" "-" key)))
                                     metadata)))
              (insert
               (format ".titlenote\n%s\n" (fountain-export-troff-string string 'action)))))
          (insert ".page-break\n")))
      (insert fountain-export-troff-macro-page-1)
      (unless (bolp) (insert "\n")))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let (element request)
          (skip-chars-forward "\n\s\t")
          (beginning-of-line)
          (when (and (setq element (fountain-get-element))
                     (setq request (car (memq element fountain-export-troff-requests))))
            (let ((param
                   (cond ((eq element 'scene-heading)
                          (match-string-no-properties 9))
                         ((eq element 'page-break)
                          (match-string-no-properties 2))))
                  (content
                   (cond ((eq element 'action)
                          (let (action)
                            (while (and (fountain-match-action)
                                        (< (point) (point-max)))
                              (push (match-string-no-properties 2) action)
                              (forward-line 1))
                            (string-join (reverse action) "\n")))
                         ((eq element 'center)
                          (let (center)
                            (while (and (fountain-match-center)
                                        (< (point) (point-max)))
                              (push (match-string-no-properties 2) center)
                              (forward-line 1))
                            (string-join (reverse center) "\n")))
                         ((eq element 'character)
                                  (concat (match-string-no-properties 2)
                                          (match-string-no-properties 3)))
                         (t
                          (or (match-string-no-properties 2) "")))))
              (setq content (fountain-export-troff-string content element))
              ;; Finally insert the request
              (with-current-buffer output-buffer
                (insert (format ".%s%s%s%s%s" request
                                (if param " " "") (or param "")
                                (if (eq element 'page-break) "" "\n")
                                content))
                (delete-horizontal-space)
                (insert "\n")))))
        (forward-line 1)
        (progress-reporter-update job)))
    (progress-reporter-done job)))

(defalias 'fountain-export-troff 'fountain-export)

(defun fountain-export ()
  "Export region to `fountain-export-format' via troff.

If region is not active, default to whole buffer.

Requires a `troff' program."
  (interactive)
  (let ((source-buffer (current-buffer))
        (start
         (if (use-region-p) (region-beginning) (point-min)))
        (end
         (if (use-region-p) (region-end) (point-max)))
        (troff-buffer
         (get-buffer-create fountain-export-troff-buffer))
        (output-buffer
         (get-buffer-create fountain-export-output-buffer))
        (command
         (string-join
          (append
           (list fountain-export-troff-command
                 (format "-T%s" fountain-export-format)
                 (format "-rscenespace=%d"
                         (if (memq 'double-space
                                   fountain-export-scene-heading-format)
                             2 1))
                 (format "-rscenebold=%d"
                         (if (memq 'bold
                                   fountain-export-scene-heading-format)
                             1 0))
                 (format "-rsceneunderline=%d"
                         (if (memq 'underline
                                   fountain-export-scene-heading-format)
                             1 0))
                 (format "-rnumberfirst=%d"
                         (if fountain-export-number-first-page 1 0))
                 (format "-rscenenumbers=%d"
                         (if fountain-export-scene-numbers 1 0))
                 (format "-P -p%s" fountain-page-size))
           fountain-export-troff-extra-options)
                      " "))
        (metadata
         (when fountain-export-title-page (fountain-get-metadata)))
        job)
    ;; Prepare script
    (with-temp-buffer
      (fountain-init-comments)
      (insert-buffer-substring source-buffer start end)
      (fountain-delete-comments-in-region (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward fountain-note-regexp nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (and (fountain-match-metadata)
                  (< (point) (point-max)))
        (forward-line))
      (skip-chars-forward "\n\s\t")
      (fountain-pagination-update)
      (setq job (make-progress-reporter "Preparing..."))
      (while (< (point) (point-max))
        (fountain-move-forward-page)
        (unless (eobp)
          (fountain-insert-page-break))
        (progress-reporter-update job))
      (progress-reporter-done job)
      (fountain-export-region-to-troff (point-min) (point-max)
                                       metadata troff-buffer))
    ;; Export to troff
    (with-current-buffer troff-buffer
      (call-process-region nil nil shell-file-name nil
                           (list output-buffer nil)
                           nil shell-command-switch command))
    ;; Write PDF
    (with-current-buffer output-buffer
      (when (buffer-file-name source-buffer)
        (write-file
         (format "%s.%s" (file-name-base (buffer-file-name source-buffer))
                 fountain-export-format)
         t)))
    (when fountain-export-switch-to-output-buffer
      (switch-to-buffer output-buffer))
    (when fountain-export-kill-output-buffer
      (kill-buffer output-buffer))))

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
        (metadata (fountain-get-metadata))
        use-stdin)
    (unless (let ((case-fold-search nil)) (string-match "%b" command))
      (setq use-stdin t))
    (setq command (format-spec command
        (format-spec-make
         ?b (shell-quote-argument (or infile ""))
         ?B (shell-quote-argument (or infile-base ""))
         ?n (shell-quote-argument (or user-full-name ""))
         ?t (shell-quote-argument (or (cdr (assq 'title metadata)) ""))
         ?a (shell-quote-argument (or (cdr (assq 'author metadata)) ""))
         ?F (shell-quote-argument (format-time-string "%F"))
         ?x (shell-quote-argument (format-time-string "%x")))))
    (when edit-command
      (setq command (read-shell-command "Shell command: " command)))
    (unwind-protect
        (if use-stdin
            (shell-command-on-region start end command
                                     fountain-export-output-buffer)
          (when (and buffer-file-name
                     (buffer-modified-p)
                     (y-or-n-p (format "Save file %s? "
                                       (buffer-file-name))))
            (save-buffer))
          (shell-command command fountain-export-output-buffer))
      (with-current-buffer fountain-export-output-buffer
        (if (< 0 (string-width (buffer-string)))
            (set-auto-mode t)
          (kill-buffer))))))

(if (<= emacs-major-version 28)
    (require 'dired-x)
  (declare-function dired-guess-default "dired-aux"))

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
        file command-list command)
    (setq file-list
          (seq-remove
           (lambda (f)
             (string-match (file-name-nondirectory (buffer-file-name))
                           (car f)))
           file-list))
    (unless file-list
      (user-error "Could not find export file for %S"
                  (file-name-nondirectory (buffer-file-name))))
    (setq file (caar (seq-sort (lambda (a b)
                                 (time-less-p (nth 6 b) (nth 6 a)))
                               file-list)))
    (unless (file-exists-p file)
      (user-error "File %S does not exist" file))
    (setq command-list (dired-guess-default (list file)))
    (unless (listp command-list) (setq command-list (list command-list)))
    (setq command
          (seq-find (lambda (str)
                      (locate-file str exec-path nil 'file-executable-p))
                    command-list))
    (unless (stringp command)
      (user-error "%S not configured correctly" 'dired-guess-shell-alist-user))
    (call-process command nil 0 nil file)))


;;; Font Lock ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-fountain-font-lock-matcher (matcher)
  "Define a `font-lock-mode' matcher for MATCHER.
MATCHER must be a Lisp form."
  `(lambda (limit)
     (let (match)
       (while (and (null match)
                   (< (point) limit))
         (when (and (not (fountain-comment-p))
                    ,matcher)
           (setq match t))
         (forward-line))
       match)))

(defun fountain-toggle-highlight-element (element)
  "Toggle the inclusion of ELEMENT in `fountain-highlight-elements'."
  (interactive
   (list (intern
          (completing-read "Element: "
                           '(section-heading scene-heading action character
                                             dialog paren trans synopsis note
                                             metadata center page-break)
                           nil t))))
  (if (memq element fountain-highlight-elements)
      (customize-set-variable 'fountain-highlight-elements
                              (delq element fountain-highlight-elements))
    (customize-set-variable 'fountain-highlight-elements
                            (cons element fountain-highlight-elements))))

(defun fountain-toggle-hide-emphasis-markup ()
  "Toggle value of `fountain-hide-emphasis-markup'."
  (interactive)
  (customize-set-variable 'fountain-hide-emphasis-markup
                          (not fountain-hide-emphasis-markup))
  (message "Emphasis markup is now %s"
           (if fountain-hide-emphasis-markup "invisible" "visible")))

(defun fountain-toggle-hide-element-markup ()
  "Toggle value of `fountain-hide-element-markup'."
  (interactive)
  (customize-set-variable 'fountain-hide-element-markup
                          (not fountain-hide-element-markup))
  (message "Element markup is now %s"
           (if fountain-hide-element-markup "invisible" "visible")))

(defun fountain--get-section-heading-face ()
  "Return appropriate face for current heading."
  (save-excursion
    (beginning-of-line)
    (looking-at outline-regexp)
    (intern-soft (format "fountain-section-heading-%s"
                         (funcall outline-level)))))

(defun fountain--normalize-align-facespec (value)
  "Return appropriate face property for VALUE.
VALUE is from options group `fountain-align' and return value
takes the form:

    (space :width N)"
  (list 'space :width
        (if (and value fountain-align-elements)
            (if (integerp value)
                value
              (cdr (or (assoc-string
                        (or (let ((metadata (fountain-get-metadata)))
                              (cdr (or (assq 'format metadata)
                                       (assq 'x-format metadata))))
                            fountain-script-format)
                        value)
                       (car value))))
          0)))

(defun fountain--get-scene-number-facespec (subexp)
  "Return `font-lock-mode' display faceprop for scene heading SUBEXP."
  (if (and (stringp (match-string-no-properties subexp))
           fountain-display-scene-numbers-in-margin)
      (let ((scene-num (match-string-no-properties 9))
            (both (<= 28 emacs-major-version)))
        (cond ((and (= subexp 7) both)
               `(face nil display ((margin left-margin)
                 (space :width (- left-margin ,(+ (string-width scene-num) 3))))))
              ((and (= subexp 8) both)
               `(face nil display ((margin left-margin) ,scene-num)))
              ((= subexp 9)
               `(face nil display ((margin right-margin) ,scene-num)))
              ((or (<= 7 subexp 10))
               '(face nil invisible t))))
    (if (or (= subexp 8) (= subexp 10))
        '(face fountain-non-printing
               display nil invisible fountain-element-markup)
      '(face nil display nil invisible nil))))

(defun fountain--font-lock-extend-region (beg end _old-len)
  "Appropriately extend region between BEG and END to be fontified."
  (save-match-data
    (when (fountain-in-dialog-maybe)
      (save-excursion
        (fountain-backward-character)
        (cons (min beg (point)) end)))))

(defun fountain-init-font-lock ()
  "Return a new list of `font-lock-keywords'."
  (let ((highlight-elements
         (append fountain-highlight-elements
                 fountain-highlight-elements-always))
        (align-section-headings
         (fountain--normalize-align-facespec fountain-align-section-heading)))

    (delq nil
     (list
      ;; Section Headings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'section-heading highlight-elements)
                    'fountain-section-heading-5)))
        (cons fountain-section-heading-5-regexp
              `((0 '(face ,face
                          line-prefix ,align-section-headings
                          wrap-prefix ,align-section-headings))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      (let ((face (when (memq 'section-heading highlight-elements)
                    'fountain-section-heading-4)))
        (cons fountain-section-heading-4-regexp
              `((0 '(face ,face
                          line-prefix ,align-section-headings
                          wrap-prefix ,align-section-headings))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      (let ((face (when (memq 'section-heading highlight-elements)
                    'fountain-section-heading-3)))
        (cons fountain-section-heading-3-regexp
              `((0 '(face ,face
                          line-prefix ,align-section-headings
                          wrap-prefix ,align-section-headings))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      (let ((face (when (memq 'section-heading highlight-elements)
                    'fountain-section-heading-2)))
        (cons fountain-section-heading-2-regexp
              `((0 '(face ,face
                          line-prefix ,align-section-headings
                          wrap-prefix ,align-section-headings))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      (let ((face (when (memq 'section-heading highlight-elements)
                    'fountain-section-heading-1)))
        (cons fountain-section-heading-1-regexp
              `((0 '(face ,face
                          line-prefix ,align-section-headings
                          wrap-prefix ,align-section-headings))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      ;; Scene Headings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face
             (when (memq 'scene-heading highlight-elements) 'fountain-scene-heading))
            (align
             (fountain--normalize-align-facespec fountain-align-scene-heading)))
        (cons 'eval
              `(cons
                (define-fountain-font-lock-matcher (fountain-match-scene-heading))
                '((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                  (1 '(face fountain-non-printing invisible fountain-element-markup)
                     prepend t)
                  (7 (fountain--get-scene-number-facespec 7)  t t)
                  (8 (fountain--get-scene-number-facespec 8)  prepend t)
                  (9 (fountain--get-scene-number-facespec 9)  prepend t)
                 (10 (fountain--get-scene-number-facespec 10) prepend t)))))

      (when fountain-double-space-scene-headings
        (cons
         (define-fountain-font-lock-matcher (fountain-match-scene-heading-blank))
         '(0 '(face nil display "\n\n"))))

      ;; Action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'action highlight-elements) 'fountain-action))
            (align (fountain--normalize-align-facespec fountain-align-action)))
        (if (or face (< 0 (nth 2 align)))
            (cons (define-fountain-font-lock-matcher (fountain-match-action))
                  `((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                    (1 '(face fountain-non-printing invisible fountain-element-markup)
                       prepend t)))
          (cons fountain-forced-action-regexp
                '((1 '(face fountain-non-printing invisible fountain-element-markup)
                     prepend t)))))

      ;; Characters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'character highlight-elements)
                    'fountain-character))
            (align (fountain--normalize-align-facespec fountain-align-character)))
        (cons (define-fountain-font-lock-matcher (fountain-match-character))
              `((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                (1 '(face fountain-non-printing invisible fountain-element-markup)
                   prepend t)
                (5 '(face highlight) prepend t))))

      ;; Dialogue ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'dialog highlight-elements)
                    'fountain-dialog))
            (align (fountain--normalize-align-facespec fountain-align-dialog)))
        (when (or face (< 0 (nth 2 align)))
          (cons (define-fountain-font-lock-matcher (fountain-match-dialog))
                `(0 '(face ,face line-prefix ,align wrap-prefix ,align)))))

      ;; Parentheticals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'paren highlight-elements)
                    'fountain-paren))
            (align (fountain--normalize-align-facespec fountain-align-paren)))
        (when (or face (< 0 (nth 2 align)))
          (cons (define-fountain-font-lock-matcher (fountain-match-paren))
                `(0 '(face ,face line-prefix ,align wrap-prefix ,align)))))

      ;; Transitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'trans highlight-elements) 'fountain-trans))
            (align (fountain--normalize-align-facespec fountain-align-trans)))
        (when (or face (< 0 (nth 2 align)))
          (cons (define-fountain-font-lock-matcher (fountain-match-trans))
                `((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                  (1 '(face fountain-non-printing invisible fountain-element-markup)
                     prepend t)))))

      ;; Synopses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'synopsis highlight-elements) 'fountain-synopsis))
            (align (fountain--normalize-align-facespec fountain-align-synopsis)))
        (cons fountain-synopsis-regexp
              `((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                (1 '(face nil invisible fountain-element-markup))
                (2 '(face fountain-non-printing) prepend))))

      ;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (memq 'note highlight-elements)
        (cons (define-fountain-font-lock-matcher (fountain-match-note))
              '(0 '(face 'fountain-note) t)))

      ;; Center ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((face (when (memq 'action highlight-elements) 'fountain-action))
            (align (fountain--normalize-align-facespec fountain-align-center)))
        (cons fountain-center-regexp
              `((0 '(face ,face line-prefix ,align wrap-prefix ,align))
                (1 '(face fountain-non-printing invisible fountain-element-markup)
                   prepend)
                (3 '(face fountain-non-printing invisible fountain-element-markup)
                   prepend))))

      ;; Metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (memq 'metadata highlight-elements)
        (cons (define-fountain-font-lock-matcher (fountain-match-metadata))
              '((0 '(face fountain-metadata-key))
                (2 '(face fountain-metadata-value) t t))))

      ;; Page-Break ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (memq 'page-break highlight-elements)
        (cons fountain-page-break-regexp
              '((0 '(face fountain-page-break)))))

      ;; Lyrics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cons fountain-lyrics-regexp
            '((2 '(face fountain-non-printing invisible fountain-element-markup)
                 prepend)
              (1 '(face italic) prepend)))

      ;; Underline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cons fountain-underline-regexp
            '((2 '(face nil invisible fountain-emphasis-markup) prepend)
              (1 '(face underline) prepend)
              (4 '(face nil invisible fountain-emphasis-markup) prepend)))

      ;; Italic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cons fountain-italic-regexp
            '((2 '(face nil invisible fountain-emphasis-markup) prepend)
              (1 '(face italic) prepend)
              (4 '(face nil invisible fountain-emphasis-markup) prepend)))

      ;; Bold ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cons fountain-bold-regexp
            '((2 '(face nil invisible fountain-emphasis-markup) prepend)
              (1 '(face bold) prepend)
              (4 '(face nil invisible fountain-emphasis-markup) prepend)))

      ;; Bold-Italic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cons fountain-bold-italic-regexp
            '((2 '(face nil invisible fountain-emphasis-markup) prepend)
              (1 '(face bold-italic) prepend)
              (4 '(face nil invisible fountain-emphasis-markup) prepend)))))))


;;; Menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)

(easy-menu-define fountain-mode-menu fountain-mode-map
  "Menu for `fountain-mode'."
  `("Fountain"
    ("Navigate"
     ["Next Heading" fountain-outline-next]
     ["Previous Heading" fountain-outline-previous]
     ["Up Heading" fountain-outline-up]
     ["Forward Heading Same Level" fountain-outline-forward]
     ["Backward Heading Same Level" fountain-outline-backward]
     "---"
     ["Forward Character" fountain-forward-character]
     ["Backward Character" fountain-backward-character]
     ["Forward This Character" fountain-forward-this-character]
     ["Backward This Character" fountain-backward-this-character]
     "---"
     ["Go to Scene Heading..." fountain-goto-scene]
     "---"
     ["Cycle Outline Visibility" fountain-outline-cycle]
     ["Cycle Buffer Outline Visibility" fountain-outline-cycle-buffer]
     ["Show All" fountain-outline-show-all
      :keys ,(key-description
              (vconcat [?\^u] [?\^u]
                       (where-is-internal 'fountain-dwim fountain-mode-map t)))]
     "---"
     ["Hide Notes When Cycling Outline"
      (customize-set-variable 'fountain-outline-hide-notes
                              (not fountain-outline-hide-notes))
      :style toggle
      :selected fountain-outline-hide-notes]
     ["Show Synopses When Cycling Outline"
      (customize-set-variable 'fountain-outline-show-synopses
                              (not fountain-outline-show-synopses))
      :style toggle
      :selected fountain-outline-show-synopses])
    ("Edit Structure"
     ["Insert Section Heading" fountain-insert-section-heading]
     ["Mark Subtree" fountain-outline-mark]
     ["Open Subtree in Indirect Buffer" fountain-outline-to-indirect-buffer]
     "---"
     ["Transpose Element Backward" fountain-backward-paragraph-or-transpose]
     ["Transpose Element Forward" fountain-forward-paragraph-or-transpose]
     "---"
     ["Transpose All Elements"
      (customize-set-variable 'fountain-transpose-all-elements
                              (not fountain-transpose-all-elements))
      :style toggle
      :selected fountain-transpose-all-elements])
    ("Dialogue"
     ["Add Continued Dialogue" fountain-add-continued-dialog]
     ["Remove Continued Dialogue" fountain-remove-continued-dialog
      :keys ,(key-description
              (vconcat [?\^u]
                       (where-is-internal 'fountain-add-continued-dialog fountain-mode-map t)))])
    ("Pagination"
     ["Forward Page" fountain-forward-page]
     ["Backward Page" fountain-backward-page]
     "---"
     ["Count Pages" fountain-count-pages]
     ["Go to Page..." fountain-goto-page]
     "---"
     ["Insert Page Break" fountain-insert-page-break]
     ["Update Pagination" fountain-pagination-update]
     "---"
     ["US Letter" (customize-set-variable 'fountain-page-size 'letter)
      :style radio
      :selected (eq fountain-page-size 'letter)]
     ["A4" (customize-set-variable 'fountain-page-size 'a4)
      :style radio
      :selected (eq fountain-page-size 'a4)]
     "---"
     ["Display Page Count in Mode Line" which-function-mode
      :style toggle
      :selected which-function-mode]
     ["Keep Pagination Updated"
      (customize-set-variable 'fountain-pagination-update-invalid
                              (not fountain-pagination-update-invalid))
      :style toggle
      :selected fountain-pagination-update-invalid]
     ["Page Count Ignores Restriction"
      (customize-set-variable 'fountain-pagination-ignore-restriction
                              (not fountain-pagination-ignore-restriction))
      :style toggle
      :selected fountain-pagination-ignore-restriction])
    ("Scene Numbers"
     ["Add Scene Numbers" fountain-add-scene-numbers]
     ["Remove Scene Numbers" fountain-remove-scene-numbers
      :keys ,(key-description
              (vconcat [?\^u]
                       (where-is-internal 'fountain-add-scene-numbers fountain-mode-map t)))]
     "---"
     ["Display Scene Numbers in Margin"
      (customize-set-variable 'fountain-display-scene-numbers-in-margin
                              (not fountain-display-scene-numbers-in-margin))
      :style toggle
      :selected fountain-display-scene-numbers-in-margin])
    "---"
    ["Do What I Mean" fountain-dwim]
    ["Upcase Line" fountain-upcase-line]
    ["Upcase Line and Newline" fountain-upcase-line-and-newline]
    "---"
    ["Insert Metadata..." auto-insert]
    ["Insert Synopsis" fountain-insert-synopsis]
    ["Insert Note" fountain-insert-note]
    "---"
    ("Auto-Completion"
     ["Update Auto-Completion" fountain-completion-update]
     "---"
     ["Update Auto-Completion When Idle" fountain-completion-auto-update-mode
      :style toggle
      :selected fountain-completion-auto-update-mode])
    ("Syntax Highlighting"
     ["Section Headings"
      (fountain-toggle-highlight-element 'section-heading)
      :style toggle
      :selected (memq 'section-heading fountain-highlight-elements)]
     ["Scene Headings"
      (fountain-toggle-highlight-element 'scene-heading)
      :style toggle
      :selected (memq 'scene-heading fountain-highlight-elements)]
     ["Action"
      (fountain-toggle-highlight-element 'action)
      :style toggle
      :selected (memq 'action fountain-highlight-elements)]
     ["Character Names"
      (fountain-toggle-highlight-element 'character)
      :style toggle
      :selected (memq 'character fountain-highlight-elements)]
     ["Dialogue"
      (fountain-toggle-highlight-element 'dialog)
      :style toggle
      :selected (memq 'dialog fountain-highlight-elements)]
     ["Parentheticals"
      (fountain-toggle-highlight-element 'paren)
      :style toggle
      :selected (memq 'paren fountain-highlight-elements)]
     ["Transitions"
      (fountain-toggle-highlight-element 'trans)
      :style toggle
      :selected (memq 'trans fountain-highlight-elements)]
     ["Synopses"
      (fountain-toggle-highlight-element 'synopsis)
      :style toggle
      :selected (memq 'synopsis fountain-highlight-elements)]
     ["Notes"
      (fountain-toggle-highlight-element 'note)
      :style toggle
      :selected (memq 'note fountain-highlight-elements)]
     ["Metadata"
      (fountain-toggle-highlight-element 'metadata)
      :style toggle
      :selected (memq 'metadata fountain-highlight-elements)]
     ["Page Breaks"
      (fountain-toggle-highlight-element 'page-break)
      :style toggle
      :selected (memq 'page-break fountain-highlight-elements)]
     "---"
     ["No Highlighting"
      (customize-set-variable 'fountain-highlight-elements nil)
      :style toggle
      :selected (null fountain-highlight-elements)]
     "---"
     ["Hide Emphasis Markup" fountain-toggle-hide-emphasis-markup
      :style toggle
      :selected fountain-hide-emphasis-markup]
     ["Hide Element Markup" fountain-toggle-hide-element-markup
      :style toggle
      :selected fountain-hide-element-markup]
     "---"
     ["Customize Faces" (customize-group 'fountain-faces)])
    "---"
    ["Display Elements Auto-Aligned"
     (customize-set-variable 'fountain-align-elements
                             (not fountain-align-elements))
     :style toggle
     :selected fountain-align-elements]
    ["Display Scene Headings Double-Spaced"
     (customize-set-variable 'fountain-double-space-scene-headings
                             (not fountain-double-space-scene-headings))
     :style toggle
     :selected fountain-double-space-scene-headings]
    ["Auto-Upcase Scene Headings"
     (customize-set-variable 'fountain-auto-upcase-scene-headings
                             (not fountain-auto-upcase-scene-headings))
     :style toggle
     :selected fountain-auto-upcase-scene-headings]
    "---"
    ["Export" fountain-export-troff
     :label (format "Export to %s"
             (if (eq fountain-export-format 'ps) "PostScript" "PDF"))]
    ["Run Export Command..." fountain-export-command]
    ["View Last Exported File" fountain-export-view]
    "---"
    ["Save Options" fountain-save-options]
    ["Customize Mode" (customize-group 'fountain)]))

(defun fountain-save-options ()
  "Save `fountain-mode' menu options with `customize'."
  (interactive)
  (let (unsaved)
    (dolist (option
             '(fountain-highlight-elements
               fountain-align-elements
               fountain-double-space-scene-headings
               fountain-auto-upcase-scene-headings
               fountain-outline-hide-notes
               fountain-outline-show-synopses
               fountain-transpose-all-elements
               fountain-page-size
               fountain-pagination-ignore-restriction
               fountain-display-scene-numbers-in-margin
               fountain-highlight-elements
               fountain-hide-element-markup
               fountain-hide-emphasis-markup
               fountain-completion-auto-update-mode
               which-function-mode))
      (when (customize-mark-to-save option) (setq unsaved t)))
    (when unsaved (custom-save-all))))


;;; Emacs Bugs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (< emacs-major-version 26)

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
after POS or point has invisible text property eq to `outline'.
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
        (mapc (lambda (fun)
                (let ((source (find-function-noselect fun)))
                  (with-current-buffer (car source)
                    (goto-char (cdr source))
                    (eval (read (current-buffer)) lexical-binding))))
              '(outline-back-to-heading
                outline-on-heading-p
                outline-next-visible-heading))
        (message "fountain-mode: Function `outline-invisible-p' has been patched")))))


;;; Initializing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fountain-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry (string-to-char "/") ". 14" table)
    (modify-syntax-entry (string-to-char "*") "$*23" table)
    (modify-syntax-entry (string-to-char "_") "$_"   table)
    (modify-syntax-entry (string-to-char "\\") "\\"  table)
    table)
  "Syntax table for `fountain-mode'.")

(defun fountain-init-comments ()
  "Initialize comment variables.
This is important for calling `fountain-delete-comments-in-region'
during `fountain-export'."
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-use-syntax t))

(defun fountain-init-scene-heading-regexp ()
  "Initialize scene heading regular expression.
Uses `fountain-scene-heading-prefix-list' to create non-forced
scene heading regular expression."
  (setq fountain-scene-heading-regexp
        (concat
         "^\\(?:"
         ;; Group 1: match leading . (for forced scene heading)
         "\\(?1:[\s\t]*\\.\\)"
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
         "^[\s\t]*\\(?:"
         ;; Group 1: match forced transition mark
         "\\(>[\s\t]*\\)"
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
                      fountain-scene-heading-regexp))
  (setq-local outline-heading-end-regexp
              (if fountain-outline-show-synopses
                  (concat "\n\\(" fountain-synopsis-regexp "\n\\)?")
                "\n")))

(require 'imenu)

(defcustom fountain-imenu-elements
  '(section-heading scene-heading synopsis note)
  "List of elements to include in `imenu'."
  :type '(set (const :tag "Section Headings" section-heading)
              (const :tag "Scene Headings" scene-heading)
              (const :tag "Synopses" synopsis)
              (const :tag "Notes" note))
  :group 'fountain
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'fountain-mode)
               (fountain-init-imenu))))))

(defun fountain-init-imenu ()
  "Initialize `imenu-generic-expression'."
  (setq imenu-generic-expression nil)
  (when (memq 'section-heading fountain-imenu-elements)
    (push (list "Section Headings" fountain-section-heading-regexp 3)
          imenu-generic-expression))
  (when (memq 'scene-heading fountain-imenu-elements)
    (push (list "Scene Headings" fountain-scene-heading-regexp 2)
          imenu-generic-expression))
  (when (memq 'synopsis fountain-imenu-elements)
    (push (list "Synopses" fountain-synopsis-regexp 3)
          imenu-generic-expression))
  (when (memq 'note fountain-imenu-elements)
    (push (list "Notes" fountain-note-regexp 1)
          imenu-generic-expression))
  (when (featurep 'imenu) (imenu-update-menubar)))

(require 'elec-pair)

;; FIXME: improve this!
(defun fountain-electric-pair-skip-self (char)
  "Return non-nil if syntax before that of CHAR is word syntax."
  (and electric-pair-preserve-balance
       (save-excursion
         (skip-syntax-backward (char-to-string (char-syntax char))
                               (line-beginning-position))
         (unless (bobp)
           (= (char-syntax (char-before)) ?w)))))


;;; Mode Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

;;;###autoload
(define-derived-mode fountain-mode text-mode "Fountain"
  "Major mode for screenwriting in Fountain markup."
  :group 'fountain
  (fountain-init-scene-heading-regexp)
  (fountain-init-trans-regexp)
  (fountain-init-outline-regexp)
  (fountain-init-imenu)
  (fountain-init-comments)
  (setq-local electric-pair-skip-self #'fountain-electric-pair-skip-self)
  (setq-local font-lock-comment-face 'fountain-comment)
  (setq-local page-delimiter fountain-page-break-regexp)
  (setq-local outline-level #'fountain-outline-level)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local completion-ignore-case t)
  (setq-local completion-cycle-threshold t)
  (setq-local which-func-functions '(fountain-count-pages))
  (setq-local completion-at-point-functions '(fountain-completion-at-point))
  (setq-local font-lock-extra-managed-props
              '(line-prefix wrap-prefix display invisible))
  (setq-local indent-line-function 'fountain-indent)
  ;; FIXME: This should be temporary. Feels better to ensure appropriate
  ;; case-fold within each function.
  (setq case-fold-search t)
  (setq imenu-case-fold-search nil)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(fountain-init-font-lock nil t))
  (setq font-lock-extend-after-change-region-function
        #'fountain--font-lock-extend-region)
  (add-to-invisibility-spec (cons 'outline t))
  (when fountain-hide-emphasis-markup
    (add-to-invisibility-spec 'fountain-emphasis-markup))
  (when fountain-hide-element-markup
    (add-to-invisibility-spec 'fountain-element-markup))
  (face-remap-add-relative 'default 'fountain)
  (add-hook 'post-self-insert-hook #'fountain--auto-upcase-maybe nil t)
  (declare-function fountain-patch-emacs-bugs "fountain-mode")
  (when (bound-and-true-p fountain-patch-emacs-bugs) (fountain-patch-emacs-bugs)))

(provide 'fountain-mode)

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:

;;; fountain-mode.el ends here
