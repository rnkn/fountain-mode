# Fountain Mode #

[![MELPA stable](https://stable.melpa.org/packages/fountain-mode-badge.svg)][melpa-stable]
[![MELPA](https://melpa.org/packages/fountain-mode-badge.svg)][melpa]

Fountain Mode is a scriptwriting program for GNU Emacs using the
Fountain plain text markup format.

For more information on the Fountain format, visit <https://fountain.io>.

![screenshot](https://user-images.githubusercontent.com/1256849/74600084-c1553900-50d7-11ea-9367-c9726e2bffee.png)

Community screenshots: <https://github.com/rnkn/fountain-mode/issues/114>

## Features ##

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file
  contents) specific to script format, e.g. screenplay, stageplay or
  user-defined format
- Traditional TAB auto-completion writing style
- Navigation by section, scene, character name, or page
- Integration with `outline` to fold/cycle visibility of sections and
  scenes
- Integration with `electric-pair-mode` to insert emphasis delimiters
  with single key (i.e. `*` or `_`)
- Integration with `imenu` (sections, scene headings, notes)
- Intergration with `auto-insert` for title page metadata
- Automatically add/remove character `(CONT'D)`
- Toggle syntax highlighting of each element
- Toggle visibility of emphasis and syntax markup
- Optionally display scene numbers in the right margin
- Intelligent insertion of a page breaks

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`.

## Requirements ##

- Emacs 24.5

## Exporting ##

Earlier versions of Fountain Mode had export functionality, but this was
not very good and is better handled via interfacing with external shell
tools, such as:

- [afterwriting](https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md) (JavaScript)
- [Wrap](https://github.com/Wraparound/wrap) (Go)
- [screenplain](https://github.com/vilcans/screenplain) (Python 3)
- [Textplay](https://github.com/olivertaylor/Textplay) (Ruby, requires PrinceXML for PDF)

The option `fountain-export-command-profiles` provides some shell
commands to interface with these tools, but you are encouraged to edit
or completely replace these to suit your own needs. The format is simple
while still allowing for a lot of flexibility.

## Installation ##

The latest stable release of Fountain Mode is available via
[MELPA-stable][]. First, add MELPA-stable to your package archives:

    M-x customize-option RET package-archives RET
    
Insert an entry named `melpa-stable` with the URL `https://stable.melpa.org/packages/`.

You can then find the latest stable version of `fountain-mode` in the
list returned by:

    M-x list-packages RET

If you prefer the latest but perhaps unstable version, do the above
using [MELPA][].

## Advanced Installation ##

Download the [latest release][], move this file into your load-path and
add to your `init.el` file:

    (require 'fountain-mode)

If you wish to contribute to or alter Fountain Mode's code, clone the
repository into your load-path and require as above:

    git clone https://github.com/rnkn/fountain-mode.git

[melpa]: https://melpa.org/#/fountain-mode "MELPA"
[melpa-stable]: https://stable.melpa.org/#/fountain-mode "MELPA-stable"
[latest release]: https://github.com/rnkn/fountain-mode/releases/latest "Fountain Mode latest release"

## History ##

See: <https://github.com/rnkn/fountain-mode/releases>

## Bugs and Feature Requests ##

Report bugs and feature requests at: <https://github.com/rnkn/fountain-mode/issues>
