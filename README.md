Fountain Mode
=============

Fountain Mode aims to be a full-featured screenwriting environment for
GNU Emacs using the Fountain markup format. For more information on the
Fountain markup format, visit <http://fountain.io>.

![screenshot](http://files.paulwrankin.com/fountain-mode/screenshot.png)

Pictured: *The Abductors* by Paul W. Rankin in Fountain Mode (using
`tsdh-dark` theme, also running [Olivetti][] minor mode)

[olivetti]: https://github.com/rnkn/olivetti "Olivetti"

Features
--------

- support for the Fountain 1.1 specification (except scene numbers and
  dual dialog)
- export to HTML and PDF (requires [Prince][])
- include or omit a title page
- multiple levels of syntax highlighting for all elements (see below)
- auto-indentation for a kind of WYSIWYG (display only, does not modify
  file contents)
- add/remove automatic (CONT'D) to successively speaking characters
- automatic (MORE) and (CONT'D) when breaking dialog across pages in PDF
  output
- `occur` navigator for section headings, synopses, notes and scene
  headings
- templates for inserting synopses, notes and metadata
- navigate by scene heading
- support for emphasis (bold, italic, underlined text)
- toggle visibility of emphasis delimiters and syntax characters
- support for both official and legacy commenting (boneyard) syntax
- everything is customizable, of course

The following features are not *yet* supported:

- scene numbers
- dual dialog

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`. Bugs and feature requests are
encouraged on the [Issues][] page, or you can email me directly (email
in the source code header). The overlap of Emacs users and screenwriters
is rather small, so any feature request or bug fix will usually be
implemented quickly.

See the [Wiki][] for ways to extend Fountain Mode.

[prince]: http://www.princexml.com "Prince"
[issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"
[wiki]: https://github.com/rnkn/fountain-mode/wiki "Fountain Mode wiki"

Requirements
------------

- Emacs 24.1 (not tested on earlier versions, only tested on Mac OS X
  and Linux, not tested on Windows).
- [s.el][], the long lost Emacs string manipulation library.
- Exporting to PDF requires [Prince][], which is free for personal use.
  Prince adds a removable PDF annotation on the first page; if you don't
  like it, delete the annotation in a PDF application that supports
  editing annotations, or open the PDF and print to PDF, which will
  remove all annotations.
- To insert UUIDs (useful for using notes as linked bookmarks) you'll
  need either `uuidgen` CLT (usually preinstalled on OS X and Linux) or
  [uuid.el][] Emacs package.

[s.el]: https://github.com/magnars/s.el "s.el"
[uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

Installation
------------

*For users on OS X with no experience with Emacs, see the
[Absolute Beginner's Guide (OS X)][beginners guide].*

Fountain Mode is available through [MELPA][] and [MELPA-stable][]. I
encourage installing the stable version.

Alternately, download the [latest release][], move the files into your
`load-path` and add the following line to your `.emacs` or `init.el`
file:

    (require 'fountain-mode)

If you want to use the `develop` branch (not recommended) to stay on
the bleeding-edge, clone the repository in your `load-path` and
require as above:

    git clone https://github.com/rnkn/fountain-mode.git

To load Fountain Mode whenever you open a `.fountain` file, also add the
following:

    (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

[beginners guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
[melpa]: http://melpa.milkbox.net "MELPA"
[melpa-stable]: http://melpa-stable.milkbox.net "MELPA"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

Syntax Highlighting
-------------------

To change the level of syntax highlighting, customize the value of
`font-lock-maximum-decoration`. This can be set indirectly with the
menu, or with `M-x fountain-set-font-lock-decoration` and saved with
`M-x fountain-save-font-lock-decoration`.

History
-------

See [Releases][].

[releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"
