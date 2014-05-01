Fountain Mode
=============

Fountain Mode aims to be a full-featured screenwriting environment for
GNU Emacs, using the Fountain markup format. For more information on
the Fountain markup format, visit <http://fountain.io>.

![Fountain Mode screenshot][screenshot]

[screenshot]: https://dl.dropboxusercontent.com/u/94472468/fountain-mode-cdn/screenshot.png

Features
--------

- exporting to HTML and PDF
- multiple levels of syntax highlighting for all elements (see below)
- auto-indentation: character, parenthetical, dialog, transition and
  centered text elements (display only, does not modify file contents)
- add/remove continued dialog to successively speaking characters
- templates for inserting synopses, notes and metadata
- navigate by scene heading
- standard commenting (boneyard) behaviour
- everything is customizable, of course

For more information on key-bindings and functions, type `C-h m`. See
the [Fountain Mode wiki][wiki] for ways to extend Fountain Mode.

The following features are not yet supported:

- syntax highlighting for emphasis
- dual dialogue

Feature requests are encouraged.

[wiki]: https://github.com/rnkn/fountain-mode/wiki/ "Fountain Mode wiki"

Requirements
------------

- Emacs 24 (not tested on earlier versions)
- [s.el][] the long lost Emacs string manipulation library
- exporting to PDF requires [Prince][prince]
- to insert UUIDs (useful for using notes as bookmarks) you'll need
  either `uuidgen` CLT (usually pre-installed on OS X and Linux) or
  [uuid.el][] Emacs package

[s.el]: https://github.com/magnars/s.el "s.el"
[prince]: http://www.princexml.com/ "Prince"
[uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

Installation
------------

Fountain Mode is available through [MELPA][]

Alternately, put `fountain-mode.el` and `s.el` in your `load-path` and
add the following line to your `.emacs` or `init.el` file:

    (require 'fountain-mode)

To load Fountain Mode whenever you open a `.fountain` file, also add the
following:

    (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

[MELPA]: http://melpa.milkbox.net "MELPA"

Syntax Highlighting
-------------------

To change the level of syntax highlighting, customize the value of
`font-lock-maximum-decoration`. This can be set indirectly with the
menu, or with `M-x fountain-set-font-lock-decoration` and saved with
`M-x fountain-save-font-lock-decoration`.

History
-------

See [Releases](https://github.com/rnkn/fountain-mode/releases).
