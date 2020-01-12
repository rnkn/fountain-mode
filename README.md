# Fountain Mode #

Fountain Mode is a scriptwriting program for GNU Emacs using the
Fountain plain text markup format.

For more information on the fountain markup format, visit
<https://fountain.io>.

**n.b. Exporting is depreciated and will be removed in the next major
release. Several external tools are available that better export
Fountain files.**

Screenshot: <https://f002.backblazeb2.com/file/pwr-share/fountain-mode.png>

## Features ##

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file
  contents) specific to script format, e.g. screenplay, stageplay or
  user-defined format
- Navigation by section, scene, character name, or page
- 3 levels of syntax highlighting
- Integration with outline to fold/cycle visibility of sections and
  scenes
- Integration with imenu (sections, scene headings, notes)
- Intergration with auto-insert for title page metadata
- Traditional TAB auto-completion writing style
- Automatically add/remove character "(CONT'D)"
- Export to plain text, HTML, LaTeX, Final Draft (FDX), or Fountain
- Export to standalone document or snippet
- Emphasis (bold, italic, underlined text)
- Include external files with {{ include: FILENAME }}
- Optionally display scene numbers in the right margin
- Intelligent insertion of a page breaks
- Automatic loading for *.fountain files
- Include or omit a title page
- Toggle visibility of emphasis delimiters and syntax characters
- Everything is customizable

Check out the Nicholl Fellowship sample script exported from Fountain
Mode to the following formats:

- plain text: <https://f002.backblazeb2.com/file/pwr-share/Nicholl_Fellowship_sample.txt>
- HTML: <https://f002.backblazeb2.com/file/pwr-share/fountain-export.html>
- Final Draft: <https://f002.backblazeb2.com/file/pwr-share/fountain-export.fdx>
- LaTeX: <https://www.overleaf.com/project/54ed9180966959cb7fdbde8e>

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type C-h m.

## Requirements ##

- Emacs 24.5
- LaTeX packages for PDF export: geometry fontspec titling fancyhdr
  marginnote ulem xstring oberdiek

## Installation ##

The latest stable release of Fountain Mode is available via
[MELPA-stable] and can be installed with:

    M-x package-install RET fountain-mode RET

Alternately, download the [latest release], move this file into your
load-path and add to your .emacs/init.el file:

    (require 'fountain-mode)

If you prefer the latest but perhaps unstable version, install via
[MELPA], or clone the repository into your load-path and require as
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
or send an email to <help@fountain-mode.org>.
