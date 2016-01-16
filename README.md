Fountain Mode
=============

[![MELPA Stable](http://stable.melpa.org/packages/fountain-mode-badge.svg)](http://stable.melpa.org/#/fountain-mode)
[![MELPA](http://melpa.org/packages/fountain-mode-badge.svg)](http://melpa.org/#/fountain-mode)

Fountain Mode aims to be a full-featured screenwriting environment for
GNU Emacs using the Fountain markup format. For more information on the
Fountain markup format, visit <http://fountain.io>.

![screenshot](http://files.paulwrankin.com/fountain-mode/screenshot.png)

Pictured: *Big Fish* by John August in Fountain Mode (using
`wombat` theme and running [Imenu-list][] and [Olivetti][] minor modes)

[imenu-list]: https://github.com/bmag/imenu-list "imenu-list"
[olivetti]: https://github.com/rnkn/olivetti "Olivetti"

Features
--------

- Support for most of the Fountain 1.1 specification
- Auto-align elements for a kind of WYSIWYG (display only, does not
  modify file contents)
- Integration with `outline` to toggle visibility of sections and
  scenes
- Export to HTML, LaTeX and Final Draft (FDX)
- Include or omit a title page
- Navigate by scene heading
- Emphasis (bold, italic, underlined text)
- Toggle visibility of emphasis delimiters and syntax characters
- Multiple levels of syntax highlighting for all elements
- Add/remove automatic "(CONT'D)" to successively speaking characters
- Automatic "(MORE)" and "(CONT'D)" when breaking dialog across pages in
  PDF output
- Templates for inserting synopses, notes and metadata
- Support for both official and legacy commenting (boneyard) syntax
- Integration with `imenu` (Sections, Scene Headings, Notes)
- Navigator (using `occur`) for section headings, synopses, notes and
  scene headings
- everything is customizable, of course

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type <kbd>C-h m</kbd>.

See the [Wiki][] on GitHub for ways to extend Fountain Mode.

[wiki]: https://github.com/rnkn/fountain-mode/wiki "Fountain Mode wiki"

Requirements
------------

- Emacs 24.4
- [s.el][], the long lost Emacs string manipulation library.
- If you want to use UUIDs (useful for using notes as linked bookmarks) you'll
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
    ###### invalid section level
    INT. LEVEL 6 - DAY

    An obese man (40s) with a large mustard stain on his shirt exits the
    elevator. He holds a hotdog.

Cycle subtree visibility with <kbd>TAB</kbd>. Cycle global outline visibility
with <kbd>S-TAB</kbd> or <kbd>C-u TAB</kbd>. More navigation and structure
editing commands are:

- <kbd>C-c C-f</kbd> `fountain-outline-forward`
- <kbd>C-c C-b</kbd> `fountain-outline-backward`
- <kbd>C-c C-n</kbd> `fountain-outline-next`
- <kbd>C-c C-p</kbd> `fountain-outline-previous`
- <kbd>C-c C-u</kbd> `fountain-outline-up`
- <kbd>C-c C-v</kbd> `fountain-outline-shift-down`
- <kbd>C-c C-^</kbd> `fountain-outline-shift-up`
- <kbd>C-c C-SPC</kbd> `fountain-outline-mark`

Bugs and Feature Requests
-------------------------

Raise an issue on the [Issues][] page on GitHub, or simply send an email
to the mailing list: <emacs.fountain@librelist.com>.

[issues]: https://github.com/rnkn/fountain-mode/issues "Fountain Mode issues"

Roadmap
-------

See [Milestones][] on GitHub.

[milestones]: https://github.com/rnkn/fountain-mode/milestones "Fountain Mode milestones"

History
-------

See [Releases][] on GitHub.

[releases]: https://github.com/rnkn/fountain-mode/releases "Fountain Mode releases"

Todo
---

- [ ] test Emacs 24.4 package require
- [ ] make Emacs 22 (GPL v2) compatible
- [ ] update emacs.fountain@librelist.com
- [ ] add action to font-lock-keywords
- [ ] remove s.el dependencies
- [ ] create proper imenu functions
- [ ] rewrite element predicate function for hard wrapping #37
- [ ] scene heading regexp should include omitted scenes #34
- [ ] support for Mountain include workflow #33
- [ ] support for scene numbers #32
- [ ] support for dual dialogue #31
- [ ] use lisp data model for export #30
- [ ] convert metadata template to skeleton #29
- [ ] setting Action Alignment / Customizing Export Alignment #27
- [ ] forced scene heading syntax character cannot be hidden bug #26
- [ ] MORE dialog does not sit flush with dialog block bug #23
- [ ] visual-line-mode and wrap-prefix bug bug #22
- [ ] export to PDF via LaTeX #20
- [ ] forcing blank lines #18
- [ ] make CSS mobile friendly #17
- [ ] export to ePub #16
- [ ] auto-upcase sluglines #2
- [ ] boneyard and notes interfere with elements bug #1
