# Fountain Mode #

Fountain Mode is a screenwriting environment for GNU Emacs using the
Fountain markup format. For more information on the Fountain markup
format, visit http://fountain.io.

Screenshot: https://f002.backblazeb2.com/file/pwr-share/fountain-mode.png

## Features ##

- Support for Fountain 1.1 specification
- WYSIWYG auto-align elements (display only, does not modify file contents)
  specific to script format, e.g. screenplay, stageplay or user-defined format
- Navigation by section, scene, character name, or page
- 3 levels of syntax highlighting
- Integration with outline to fold/cycle visibility of sections and scenes
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

Check out the Nicholl Fellowship sample script exported from Fountain Mode to:

- Plain text: https://f002.backblazeb2.com/file/pwr-share/Nicholl_Fellowship_sample.txt
- HTML: https://f002.backblazeb2.com/file/pwr-share/fountain-export.html
- Final Draft: https://f002.backblazeb2.com/file/pwr-share/fountain-export.fdx
- LaTeX: https://www.overleaf.com/project/54ed9180966959cb7fdbde8e

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type C-h m.

## Requirements ##

- Emacs 24.5
- LaTeX packages for PDF export: geometry fontspec titling fancyhdr
  marginnote ulem xstring oberdiek

## Installation ##

Fountain Mode is now part of GNU ELPA and can be installed with M-x
package-install RET fountain-mode RET.

## History ##

See: https://github.com/rnkn/fountain-mode/releases

## Bugs ##

To report bugs, use https://github.com/rnkn/fountain-mode/issues, or M-x
report-emacs-bug RET or send an email to <bug-gnu-emacs@gnu.org> (please include
"fountain" in the subject).

