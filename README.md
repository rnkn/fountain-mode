Fountain Mode
=============

[![MELPA Stable](https://stable.melpa.org/packages/fountain-mode-badge.svg)](https://stable.melpa.org/#/fountain-mode)
[![MELPA](https://melpa.org/packages/fountain-mode-badge.svg)](https://melpa.org/#/fountain-mode)

Fountain Mode is a complete screenwriting environment for GNU Emacs
using the Fountain markup format. For more information on the Fountain markup
format, visit <http://fountain.io>.

![screenshot](https://github.com/rnkn/fountain-mode/raw/master/screenshots/01.png)

Pictured: *The Abductors* by Paul W. Rankin in Fountain Mode (using [imenu-list] and
[Olivetti] minor modes)

[imenu-list]: https://github.com/bmag/imenu-list "imenu-list"
[olivetti]: https://github.com/rnkn/olivetti "Olivetti"

Features
--------

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file contents)
  specific to script format, e.g. screenplay, stageplay or user-defined format
- Traditional <kbd>TAB</kbd> writing style for auto-upcasing character names
  (see [Do What I Mean])
- Export to plain text, HTML, LaTeX, Final Draft (FDX), or Fountain
- Export to standalone document or snippet
- Optionally show approximate page count (current page of total pages) in
  mode-line
- Include external files with `{{ include: FILENAME }}`
- Integration with `outline` to fold/cycle visibility of sections and scenes
  (see [Outlining])
- Integration with `imenu` (sections, scene headings, notes)
- Intergration with `auto-insert` for title page metadata
- Add/remove automatic continuation string to successively speaking characters
- Navigation by section, scene, character name, or page
- Optionally display scene numbers in the right margin
- Intelligent insertion of a page breaks
- 3 levels of element syntax highlighting
- Automatic loading for `*.fountain` files
- Include or omit a title page
- Emphasis (bold, italic, underlined text)
- Toggle visibility of emphasis delimiters and syntax characters
- Everything is customizable

Check out the Nicholl Fellowship sample script exported from Fountain Mode to:

- [Plain text](https://gist.github.com/rnkn/edd4fd20e0f6ce2ca1f75e37496e38c9/raw/)
- [HTML](https://rawgit.com/rnkn/mcqueen/master/sample/sample.html)
- [LaTeX](https://www.sharelatex.com/project/54ed9180966959cb7fdbde8e)
- [Final Draft](https://gist.github.com/rnkn/f56934ac723d43c5dec63952dd99dcfd/raw/)

Most common features are accessible from the menu. For a full list of functions
and key-bindings, type <kbd>C-h m</kbd>.

[Do What I Mean]: https://github.com/rnkn/fountain-mode/wiki/Do-What-I-Mean
[Outlining]: https://github.com/rnkn/fountain-mode/wiki/Outlining

For more, see the [Wiki](https://github.com/rnkn/fountain-mode/wiki).

Requirements
------------

- Emacs 24.5
- LaTeX packages for PDF export: `geometry` `fontspec` `titling` `fancyhdr`
  `marginnote` `ulem` `xstring` `oberdiek`

Installation
------------

*For users on OS X with no experience with Emacs, see the
[Absolute Beginner's Guide (macOS)][guide].*

The latest stable release of Fountain Mode is available via
[MELPA-stable](http://stable.melpa.org/#/fountain-mode).

Alternately, download the [latest release], move the files into your
`load-path` and add the following line to your `.emacs` or `init.el` file:

    (require 'fountain-mode)

If you prefer the latest but perhaps unstable version, install via
[MELPA], or clone the repository into your `load-path` and require as
above:

    git clone https://github.com/rnkn/fountain-mode.git

Users of Debian ≥10 or Ubuntu ≥18.04 can install Fountain Mode with the following command:

    sudo apt install elpa-fountain-mode

[guide]: https://github.com/rnkn/fountain-mode/wiki/Absolute-Beginner's-Guide-(macOS) "Absolute Beginner's Guide (macOS)"
[melpa]: https://melpa.org/#/fountain-mode "MELPA"
[melpa-stable]: https://stable.melpa.org/#/fountain-mode "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

Bugs and Feature Requests
-------------------------

Please raise an issue on [Issues](https://github.com/rnkn/fountain-mode/issues).

- Emacs versions prior to 26 have a bug with `visual-line-mode` that produces erratic
  navigation behavior when displaying very long lines. More information here:
  <https://debbugs.gnu.org/23879>

Roadmap
-------

See [Roadmap](https://github.com/rnkn/fountain-mode/projects/2).

History
-------

See [Releases](https://github.com/rnkn/fountain-mode/releases).

Tips
----

Ethereum address `0x209C60afd8aF6c61ac4Dbe340d81D4f789DF64D3`  
Bitcoin Cash address `qp0n3z88mmq06xv9mvkwuur9289qhnyg2gl45tlhxf`
