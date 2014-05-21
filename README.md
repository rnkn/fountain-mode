Fountain Mode
=============

Fountain Mode aims to be a full-featured screenwriting environment for
GNU Emacs using the Fountain markup format. For more information on
the Fountain markup format, visit <http://fountain.io>.

![screenshot](https://dl.dropboxusercontent.com/u/94472468/fountain-mode-cdn/screenshot.png)

Features
--------

- support for the Fountain 1.1 specification
- exporting to HTML and PDF (requires [Prince][])
- include or omit a title page
- multiple levels of syntax highlighting for all elements (see below)
- auto-indentation: character, parenthetical, dialog, transition and
  center text elements (display only, does not modify file contents)
- add/remove continued dialog to successively speaking characters
- `occur` navigator for section headings, synopses, notes and scene
  headings
- templates for inserting synopses, notes and metadata
- navigate by scene heading
- suppoort for emphasis (bold, italic, underlined text)
- toggle visibility of emphasis delimiters and escaping characters
- standard commenting (boneyard) behaviour
- everything is customizable, of course

The following features are not yet supported:

- dual dialogue (probably won't be supported because it's stupid)

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`. Bugs and feature requests
are encouraged on the [Issues][] page, or you can email me
directly (email in the source code header).

See the [wiki][] for ways to extend Fountain Mode.

[prince]: http://www.princexml.com/ "Prince"
[issues]: https://github.com/rnkn/fountain-mode/issues/ "Fountain Mode issues"
[wiki]: https://github.com/rnkn/fountain-mode/wiki/ "Fountain Mode wiki"

Requirements
------------

- Emacs 24 (not tested on earlier versions, only tested on Mac OS X
  and Linux, not tested on Windows).
- [s.el][], the long lost Emacs string manipulation library.
- Exporting to PDF requires [Prince][], which is free for personal
  use. Prince adds a removable PDF annotation on the first page; if
  you don't like it, delete the annotation in a PDF application that
  supports editing annotations, or open the PDF and print to PDF,
  which will remove all annotations.
- to insert UUIDs (useful for using notes as linked bookmarks) you'll
  need either `uuidgen` CLT (usually pre-installed on OS X and Linux)
  or [uuid.el][] Emacs package.

[s.el]: https://github.com/magnars/s.el "s.el"
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
