Fountain Mode
=============

Emacs major mode for editing Fountain-formatted text files.

For more information on the Fountain markup format, visit
<http://fountain.io>.

Features
--------

The following features are supported:

- syntax highlighting for sluglines, forced sluglines, section headings,
  synopses, notes and boneyard elements
- auto-indentation for character, parenthetical, dialogue and transition
  elements (display only, does not modify file contents)
- `S-RET` to upcase the line and insert a newline
- `M-n` and `M-p` to navigate by scene heading
- `C-c C-a` to insert a synopsis under the current scene or section
  heading
- `C-c C-z` to insert a note based on template under the current element
- `C-c C-x i` to insert metadata based on template at the beginning of
  buffer
- both Fountain boneyard syntaxes (follows standard commenting behavior,
  e.g. `M-;`)
- customizable indentation align-to columns, slugline prefixes,
  transition suffixes, and faces
- see [Tips & Tricks][wiki-tips] for ways to extend Fountain Mode

The following features are not yet supported:

- syntax highlighting for all other elements, including emphasis
- menus
- reading metadata
- dual dialogue
- exporting

The feature-set is quite basic at the moment, with many features yet to
be added. Feature requests are encouraged.

It may raise some eyebrows that there is no current support for
exporting to other formats. This is partly because I already use
[Highland][], which is an amazing app. Fountain Mode will be able to
export to other formats one day, but not quite yet.

For exporting, I recommend [Highland][] (OS X) or [Trelby][] (Linux,
Windows). A full list of apps that support the Fountain format is
available here: <http://fountain.io/apps>

[wiki-tips]: https://github.com/rnkn/fountain-mode/wiki/Tips-%26-Tricks "Tips & Tricks"
[Highland]: http://www.quoteunquoteapps.com/highland/ "Highland"
[Trelby]: http://www.trelby.org "Trelby"

Requirements
------------

- Emacs 24 (not tested on earlier versions)
- [s.el][] the long lost Emacs string manipulation library.
- the CLT `uuidgen` is required to insert UUIDs (usually pre-installed
  on OS X and Linux)

[s.el]: https://github.com/magnars/s.el "s.el"

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
