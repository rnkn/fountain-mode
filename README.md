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

- Support for most of the Fountain 1.1 specification (scene numbers and
  dual dialog are forthcoming)
- Auto-align elements for a kind of WYSIWYG (display only, does not
  modify file contents)
- Integration with `outline.el` to toggle visibility of sections and
  scenes (beta, see below)
- Export to HTML and PDF (PDF export requires [Prince][])
- Include or omit a title page
- Navigate by scene heading
- Emphasis (bold, italic, underlined text)
- Toggle visibility of emphasis delimiters and syntax characters
- Multiple levels of syntax highlighting for all elements (see below)
- Add/remove automatic "(CONT'D)" to successively speaking characters
- Automatic "(MORE)" and "(CONT'D)" when breaking dialog across pages in
  PDF output
- Templates for inserting synopses, notes and metadata
- Support for both official and legacy commenting (boneyard) syntax
- Navigator (using `occur`) for section headings, synopses, notes and
  scene headings
- everything is customizable, of course

The following features are not *yet* supported:

- scene numbers
- dual dialog

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`.

See the [Wiki][] on GitHub for ways to extend Fountain Mode.

[prince]: http://www.princexml.com "Prince"
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
[melpa]: http://melpa.org "MELPA"
[melpa-stable]: http://stable.melpa.org "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

Outlining
---------

There are six possible levels of outline subtrees. Section headings
count as the first five levels and scene headings count as the sixth
level, e.g.:

    # section level 1

    ## section level 2

    ### section level 3

    #### section level 4

    ##### section level 5

    ###### not recognized as section level

    INT. LEVEL 6 - DAY

    An obese man (40s) with a large mustard stain on his shirt exits the
    elevator. He holds a hotdog.

Cycle subtree visibility with `TAB`. Cycle global outline visibility
with `<backtab>` (shift-TAB) or `C-u TAB`. More navigation and structure
editing commands are:

- `C-c C-f outline-forward-same-level`
- `C-c C-n outline-next-visible-heading`
- `C-c C-p outline-previous-visible-heading`
- `C-c C-u outline-up-heading`
- `C-c C-v outline-move-subtree-down`
- `C-c C-^ outline-move-subtree-up`
- `C-c C-SPC outline-mark-subtree`

Bugs and Feature Requests
-------------------------

Raise an issue on the [Issues][] page on GitHub, or simply send an email
to the mailing list: <emacs.fountain@librelist.com>.

[issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"

History
-------

See [Releases][] on GitHub.

[releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"
