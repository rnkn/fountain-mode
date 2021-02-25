Fountain Mode
=============

[![MELPA stable](https://stable.melpa.org/packages/fountain-mode-badge.svg)][1]
[![MELPA](https://melpa.org/packages/fountain-mode-badge.svg)][2]

Fountain Mode is a screenwriting program for GNU Emacs using the
Fountain plain text markup format.

For more information on the Fountain format: <https://fountain.io>

![screenshot](screenshots/01.png)


Features
--------

 - Support for Fountain 1.1 specification
 - WYSIWYG auto-align elements (display only, does not modify file
   contents) specific to script format, e.g. screenplay, stageplay or
   user-defined formats
 - Traditional TAB auto-completion writing style
 - Highly accurate pagination calculation
 - Navigation by section, scene, character name, or page
 - Integration with `outline` to fold/cycle visibility of sections,
   scenes and notes
 - Integration with `electric-pair-mode` to insert emphasis delimiters
   with single key (i.e. `*` or `_`)
 - Integration with `imenu` (sections, scene headings, notes)
 - Intergration with `auto-insert` for title page metadata
 - Automatically add/remove character `(CONT'D)`
 - Toggle syntax highlighting of each element
 - Toggle visibility of emphasis and syntax markup
 - Optionally display scene numbers in margins
 - Intelligent insertion of page breaks

Most common features are accessible from the menu. For a full list of
functions and key-bindings, type `C-h m`.


Requirements
------------

 - Emacs 24.4
 - seq 2.20 (part of Emacs 25 and later)


Exporting
---------

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


Installation
------------

The latest stable release of Fountain Mode is available via
[MELPA-stable][1]. First, add MELPA-stable to your package archives:

    M-x customize-option RET package-archives RET
    
Insert an entry named `melpa-stable` with URL:
`https://stable.melpa.org/packages/`

You can then find the latest stable version of `fountain-mode` in the
list returned by:

    M-x list-packages RET

If you prefer the latest but perhaps unstable version, do the above
using [MELPA][2].


Advanced Installation
---------------------

Download the latest tagged release, move this file into your `load-path`
and add to your `init.el` file:

    (require 'fountain-mode)

If you wish to contribute to or alter Fountain Mode's code, clone the
repository into your load-path and require as above:

    git clone https://github.com/rnkn/fountain-mode.git


Bugs and Feature Requests
-------------------------

Send me an email (address in the package header). For bugs, please
ensure you can reproduce with:

    $ emacs -Q -l fountain-mode.el

Known issues are tracked with `FIXME` comments in the source.


[1]: https://stable.melpa.org/#/fountain-mode
[2]: https://melpa.org/#/fountain-mode
