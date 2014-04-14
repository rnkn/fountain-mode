Fountain Mode
=============

Emacs major mode for editing Fountain-formatted text files.

For more information on the Fountain markup format, visit
<http://fountain.io>.

![Fountain Mode screenshot][screenshot]

[screenshot]: https://dl.dropboxusercontent.com/u/94472468/fountain-mode-cdn/screenshot.png

Features
--------

The following features are supported:

- multiple levels of syntax highlighting for all elements
- auto-indentation: character, parenthetical, dialog, transition and
  centered text elements (display only, does not modify file contents)
- add/remove continued dialog to successively speaking characters
- `C-c C-a` to insert a synopsis
- `C-c C-z` to insert a note
- `C-c C-x i` to insert metadata
- synopses, notes and metadata are based on custom templates
- `S-RET` to upcase the line and insert a newline
- `C-M-n` and `C-M-p` to navigate by scene heading
- both Fountain comment/boneyard syntaxes (follows standard commenting
  behavior, e.g. `M-;`)
- command menu
- everything is customizable, of course
- see the [Fountain Mode wiki][wiki] for ways to extend Fountain Mode

The following features are not yet supported:

- syntax highlighting for emphasis
- reading metadata
- dual dialogue
- exporting

Feature requests are encouraged. Exporting is "coming soon."

For exporting, check out the full list of apps that support the Fountain
format here: <http://fountain.io/apps>

[wiki]: https://github.com/rnkn/fountain-mode/wiki/ "Fountain Mode wiki"

Requirements
------------

- Emacs 24 (not tested on earlier versions)
- [s.el][] the long lost Emacs string manipulation library.
- to insert UUIDs (useful for using notes as bookmarks) you'll need
  either `uuidgen` CLT (usually pre-installed on OS X and Linux) or
  [uuid.el][] Emacs package

[s.el]: https://github.com/magnars/s.el "s.el"
[uuid.el]: https://github.com/nicferrier/emacs-uuid "uuid.el"

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
