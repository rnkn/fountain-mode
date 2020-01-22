# Fountain Mode #

[![MELPA stable](https://stable.melpa.org/packages/fountain-mode-badge.svg)][melpa-stable]
[![MELPA](https://melpa.org/packages/fountain-mode-badge.svg)][melpa]

Fountain Mode is a scriptwriting program for GNU Emacs using the
Fountain plain text markup format.

For more information on the Fountain format, visit
<https://fountain.io>.

![screenshot](https://f002.backblazeb2.com/file/pwr-share/fountain-mode.png)

## Features ##

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file
  contents) specific to script format, e.g. screenplay, stageplay or
  user-defined format
- Traditional TAB auto-completion writing style
- Navigation by section, scene, character name, or page
- Integration with `outline` to fold/cycle visibility of sections and
  scenes
- Integration with `imenu` (sections, scene headings, notes)
- Intergration with `auto-insert` for title page metadata
- Automatically add/remove character `(CONT'D)`
- Toggle visibility of emphasis delimiters and syntax characters
- 3 levels of syntax highlighting
- Optionally display scene numbers in the right margin
- Intelligent insertion of a page breaks

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`.

## Requirements ##

- Emacs 24.5

## Exporting ##

Earlier versions of Fountain Mode had export functionality, but this was
never very good and there are several external tools available that better
handle exporting:

- [afterwriting](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md) (JavaScript)
- [Wrap](https://github.com/Wraparound/wrap) (Go)
- [screenplain](https://github.com/vilcans/screenplain) (Python 2)
- [Textplay](https://github.com/olivertaylor/Textplay) (Ruby, requires PrinceXML for PDF)

## Installation ##

The latest stable release of Fountain Mode is available via
[MELPA-stable][] and can be installed with:

    M-x package-install RET fountain-mode RET

Alternately, download the [latest release][], move this file into your
load-path and add to your `init.el` file:

    (require 'fountain-mode)

If you prefer the latest but perhaps unstable version, install via
[MELPA][], or clone the repository into your load-path and require as
above:

    git clone https://github.com/rnkn/fountain-mode.git

Users of Debian >=10 or Ubuntu >=18.04 can install Fountain Mode with:

    sudo apt install elpa-fountain-mode

[melpa]: https://melpa.org/#/fountain-mode "MELPA"
[melpa-stable]: https://stable.melpa.org/#/fountain-mode "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

## History ##

See: <https://github.com/rnkn/fountain-mode/releases>

## Bugs and Feature Requests ##

To report bugs either use <https://github.com/rnkn/fountain-mode/issues>
or send an email to <code@paulwrankin.com>.
