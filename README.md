Fountain Mode
=============

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA Stable](https://stable.melpa.org/packages/fountain-mode-badge.svg)](https://stable.melpa.org/#/fountain-mode)
[![MELPA](https://melpa.org/packages/fountain-mode-badge.svg)](https://melpa.org/#/fountain-mode)

Fountain Mode is a complete screenwriting environment for GNU Emacs
using the Fountain markup format. For more information on the Fountain markup
format, visit <http://fountain.io>.

![screenshot](http://files.paulwrankin.com/fountain-mode/screenshot.png)

Pictured: *Big Fish* by John August in Fountain Mode (using [imenu-list][] and
[Olivetti][] minor modes)

[imenu-list]: https://github.com/bmag/imenu-list "imenu-list"
[olivetti]: https://github.com/rnkn/olivetti "Olivetti"

Features
--------

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file contents)
  specific to script format, e.g. screenplay, stageplay or user-defined format
- Export to HTML, LaTeX, Final Draft (FDX), or Fountain
- Export to standalone document or snippet
- Integration with `outline` to fold/cycle visibility of sections and scenes
- Integration with `imenu` (sections, scene headings, notes)
- Intergration with `auto-insert` for title page metadata
- Add/remove automatic continuation string to successively speaking characters
- Navigation by section, scene, character name, or page
- 3 levels of element syntax highlighting
- Automatic loading for `*.fountain` files
- Support for both official and legacy commenting (boneyard) syntax
- Include or omit a title page
- Emphasis (bold, italic, underlined text)
- Toggle visibility of emphasis delimiters and syntax characters
- Everything is customizable

Check out the Nicholl Fellowship sample script exported from Fountain Mode to:

- [HTML](https://rawgit.com/rnkn/mcqueen/master/sample/sample.html)
- [LaTeX](https://www.sharelatex.com/project/54ed9180966959cb7fdbde8e)
- [Final Draft](http://files.paulwrankin.com/fountain-mode/Nicholl%20Fellowship%20sample.fdx)

More information on outlining here: <https://github.com/rnkn/fountain-mode/wiki/Outlining>

Most common features are accessible from the menu. For a full list of functions
and key-bindings, type <kbd>C-h m</kbd>.

For more, see the [Wiki](https://github.com/rnkn/fountain-mode/wiki).

Requirements
------------

- Emacs 24.4
- LaTeX packages for PDF export: `geometry` `fontspec` `titling` `fancyhdr`
  `marginnote` `ulem` `xstring` `oberdiek`

Installation
------------

*For users on OS X with no experience with Emacs, see the
[Absolute Beginner's Guide (OS X)][guide].*

The latest stable release of Fountain Mode is available via
[MELPA-stable](http://stable.melpa.org/#/fountain-mode).

Alternately, download the [latest release][], move the files into your
`load-path` and add the following line to your `.emacs` or `init.el` file:

    (require 'fountain-mode)

If you prefer the latest but perhaps unstable version, install via
[MELPA][], or clone the repository into your `load-path` and require as
above:

    git clone https://github.com/rnkn/fountain-mode.git

[guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(OS-X) "Absolute Beginner's Guide (OS X)"
[melpa]: http://melpa.org/#/fountain-mode "MELPA"
[melpa-stable]: http://stable.melpa.org/#/fountain-mode "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

Bugs and Feature Requests
-------------------------

Please raise an issue on [Issues](https://github.com/rnkn/fountain-mode/issues).

- Emacs currently has a bug with `visual-line-mode` that produces erratic
  navigation behavior when displaying very long lines. More information here:
  <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23879>

Roadmap
-------

See [Milestones](https://github.com/rnkn/fountain-mode/milestones).

History
-------

See [Releases](https://github.com/rnkn/fountain-mode/releases).
