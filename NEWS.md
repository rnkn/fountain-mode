# NEWS

## Version 3.7.3

- Add missing options to Save Options command

## Version 3.7.2

- Fix to ensure page-break after any title page metadata in troff
  export.

## Version 3.7.1

- Fix for breaking page mid-line of element.
- Fix to ensure troff page length is always longer than pagination.

## Version 3.7.0

- Implement internal export to PostScript/PDF requiring only a troff
  program on your system (`fountain-export-troff` bound to `C-c C-e t`).
- Permit `fountain-goto-scene` to accept any string, which will navigate
  to revised scene numbers, e.g. 10A.
- `fountain-add-scene-numbers` can now be called with a prefix argument
  to remove scene numbers.
- `fountain-add-continued-dialog` can now be called with a prefix
  argument to remove continued dialogue.
- Rename `fountain-export` to `fountain-export-command` and bind to `C-c
  C-e e`.
- Add options `fountain-export-title-page`, `fountain-export-number-first-page`
- Improve `fountain-theme` default face size.
- When called with prefix argument `fountain-upcase-line` will only
  prefix the scene with `.` if not already present.
- The info documentation, while not complete, now no longer has missing
  nodes.
- Other bug fixes and improvements.

## Version 3.6.3

- Fix dialogue aligning on Emacs 29.x.
- Improve quoting style in docstrings.
- Only require `dired-x` in Emacs < 29

## Version 3.6.2

- Fix bugs with `fountain-init-font-lock`.
- Add a `font-lock-extend-after-change-region-function` to improve
  fontifying character names.
- Declare `outline` aliases as defined in `fountain-mode`.

## Version 3.6.1

- Optimize Font Lock keywords to use regular expressions instead of
  functions. This dramatically improves performance on slower machines
  but reintroduces the bug where text within comments is fontified.
- Fix a bug where element markup would not be hidden if the element were
  not highlighted or auto-aligned.
- Define `fountain--completion-idle-timer` to quiet compiler warning.
- Update email and copyright.
- Remove GitHub Actions.

## Version 3.6.0

- Implement option to display additional newline before scene headings
  (available in menu as Display Scene Headings Double-Spaced).
- Reimplement fix for bug where text within comments was fontified, e.g.
  `/*comment*/` as italic (this time checking for free variables in
  byte-compile).
- Remove old obsolete option aliases.
- Improve special character syntax classes.
- Some improvements to Info docs (which is now in `/doc` rather than
  `/docs`).

## Version 3.5.3

- Rollback version 3.5.2, which introduced a compilation bug causing
  generation of Font Lock keywords to fail.

## Version 3.5.2

- Fix a longstanding bug where text within comments was fontified, e.g.
  `/*comment*/` as italic.

## Version 3.5.1

- Fix page-count errors with empty buffer.

## Version 3.5.0

- Implement compatibility with `which-function-mode` to display page
  numbers in mode-line, which option added to `fountain-mode-hook`.
- Add commands `fountain-add-continued-dialog` and
  `fountain-remove-continued-dialog`, replacing command
  `fountain-refresh-continued-dialog` and option
  `fountain-add-continued-dialog`
- `TAB` now appropriately indents metadata values
- Add `fountain-outline-hide-sublevels` (alias of
  `outline-hide-sublevels`) on `C-c C-q`, allowing `C-c C-q` to
  quickly collapse all sections, and `M-[N] C-c C-q` to hide
  N-sublevels.
  - These are added to a new "Dialogue" submenu.
- Remove forced page numbers feature, which is not part of Fountain 1.1
  spec.
  - Remove optional args `ASK` and `PAGE-NUM` from
    `fountain-insert-page-break`.
  - Remove page number group from `fountain-page-break-regexp`
  - Remove `fountain-page-number` face.
- Add `%P` in `fountain-note-template` to reposition point.
- Ensure `fountain-note-regexp` omits notes containing blank lines.
- Update pagination when changing page size.
- Add page size & pagination ignore restriction option to menu.
- Fixed an issue where action could be parsed as character/dialogue.
- Fix missing `%n` in `fountain-export-command` format spec.
- Fix `fountain-insert-section-heading` for invisible headings.
- Fix edge case of misidentified character within action.
- Fix `fountain-imenu-elements` option group.
- Fix spelling of "dialogue" in docstrings and comments.
- Fix some missing options from `fountain-save-options` menu item.
- Add `:extend` property to notes face.
- Renamed invisibility property `fountain-syntax-char` to `fountain-element-markup`
- Large rewrite of Font Lock init code to greatly simplify.
  - New implementation of matching action.
  - Remove options for highlighting center text element, treating center
    text as part of action element.
- Make `fountain-fill-*` options into variables and remove option group.
- Set `font-lock-multiline` to t.
- Add `fountain-comment-p` function for more reliably matching comments
  (experimental).

## Version 3.4.1

- Scene numbers now display in both left and right margins (only on
  Emacs 28 and later due to an existing bug).
- Fixed an issue where scene numbers displayed in margins would not
  revert when toggling `fountain-display-scene-numbers-in-margin`.
- Updated copyright, email, URL.

## Version 3.4.0

- Add option `fountain-outline-show-synopses`; when non-nil, show synopses
  following headings when cycling outline visibility.
- Rewrite outline cycling to better work with direction of Emacs outline.
- Better docstring for `fountain-completion-at-point`.
- Rename `fountain-outline-fold-notes` to `fountain-outline-hide-notes`.
- Rename `fountain-beginning-of-scene` to `fountain-outline-beginning`.
- Remove old unneeded commands duplicating `fountain-outline-*`
  functionality:
  - `fountain-backward-scene` (duplicates `fountain-outline-previous`)
  - `fountain-end-of-scene` (duplicates `fountain-outline-next`)
  - `fountain-mark-scene` (duplicates `fountain-outline-mark`)
- Remove interactive command `fountain-forward-scene` and rename function
  to `fountain-move-forward-scene` (matching `fountain-move-forward-page`).
- Name generated Font Lock functions as internal.
- Fix option `:safe` predicates.
- Add export profile for using Makefiles. (Maybe there are crazy people
  who do this?)

## Version 3.3.3

- Fix for adding current incomplete character to completion candidates.
- Rename `fountain-maybe-in-dialog-p` to `fountain-in-dialog-maybe`.
- History is now tracked in a fancy NEWS file.

## Version 3.3.2

- Fix auto-completion of character extensions (e.g. V.O., CONT'D, etc.)
- Known issues are now tracking with FIXME comments in the source
- The html manual, although still in its infancy, is now built
  automatically on the server (or locally with make html-manual)
- The manual is available at https://fountain-mode.org
- Update repository for portability (add a screenshot, no more relying
  on GitHub stuff)

## Version 3.3.1

This release is really just to switch bug reporting from GitHub
issues to email

## Version 3.3.0

### Pagination

Pagination is now a first-class citizen. Page count accuracy should be
good enough to refer between a PDF and your Fountain script within
Emacs. The pagination commands are all located in the menu under
Fountain > Pagination.

Given that counting pages is computationally intensive, pagination is
now handled by applying text properties with command
`fountain-pagination-update` (`C-c C-x p`), which can be triggered with
`fountain-mode-hook`. This applies a `fountain-pagination` text property
of `(PAGE . LENGTH)` to all buffer text, where PAGE is the linear page
count and LENGTH is each page's length in characters.  Counting pages
then checks whether each PAGE property is correctly ordered and LENGTH
is accurate within `fountain-pagination-max-change` (default 150
characters). If either of these checks fails, pagination properties
are recalculated. These properties are now used for all page-related
functions.

All this does require some care on the part of the user in ensuring
some options are set correctly in the group `fountain-pagination`:

- New option `fountain-pagination-double-space-scene-headings` to count
  scene headings as two lines if your export uses double-space scene
  headings.
- New option `fountain-pagination-break-sentences` to if your export
  allows page breaks to occur within sentences.
- New option `fountain-pagination-max-change`. Set this to a lower
  value if you want more accurate pagination, or higher if you don't
  care.
- New commands `fountain-forward-page` and `fountain-backward-page`. These
  navigate by the actual page breaks, not by a page length back/forth
  from point.
- Rename option `fountain-page-ignore-restriction` to
  `fountain-pagination-ignore-restriction`. It's useful to set this option
  to nil if you're working on a narrowed section of your script and want
  the page count to refer only to this.

The title page is considered page 0.

### Other Fixes

- Rewrite of `fountain-get-element` and `fountain-move-forward-page` to
  account for dual dialogue.
- Fix `fountain-export-view` locating the last modified file (was
  previously using access-time).
- When calling an export command that uses the buffer file as input
  but the buffer is not saved, prompt to save buffer first.
- `fountain-export-view` will no longer wait for external viewer to
  terminate.
- Avoid matching transitions (e.g. CUT TO:) as metadata when at
  beginning of buffer.
- Fix for Font Lock not recognizing lowercase scene headings.
- Remove unused `fountain-get-block-bounds` function.

Thanks to https://github.com/zungbang for contributing many fixes to
this release.

## Version 3.2.2

- Improvements to `fountain-export-view` command: will search exec-path
  for suitable shell command to open export output (usually a PDF).
- Lower Emacs requirement to 24.4 (was 24.5).
- Avoid matching notes as metadata at beginning of buffer.
- Allow whitespace ahead of forced scene heading.

## Version 3.2.1

- Fix menu entries for transpose commands.

## Version 3.2.0

- Gone are the three levels of Font Lock highlighting, you can now
  choose to highlight elements individually with
  `fountain-highlight-elements` option accessible via menu > Fountain >
  Syntax Highlighting.
- You can also easily add emphasis markup with a single key by enabling
  `electric-pair-local-mode` via `fountain-mode-hook`, e.g. selecting a word
  and typing "\*" will make that word *italic* (surround with `*asterisks*`),
  and just typing "\*" will insert a pair and place cursor in between. When
  closing the pair, just typing the same emphasis key will skip over the
  emphasis markup.
- Use `transpose-subr` for shifting elements around. These work slightly
  differently to the previous `fountain-shift-*` commands in that
  transposing any block of text will allow moving past a scene/section
  heading, but when point is on a scene/section heading, the previous
  outline subtree functionality remains the same.
- New command `fountain-toggle-highlight-element` to interactively toggle
  element highlight without using the menu.
- Fix compatibility in `fountain-outline-to-indirect-buffer` with Emacs <=
  25.x.
- Remove `fountain-set-font-lock-decoration` command.
- Rewrite of `fountain-init-font-lock` and `fountain--font-lock-keywords`.
- Make `S-TAB` (shift-TAB) compatible with consoles.
- Make `fountain-redisplay-scene-numbers` a little more efficient.
- Appropriately signal errors when calling commands that edit buffer if
  buffer is read-only.
- Use `fountain-*` aliases for outline backwards compatibility with Emacs
  24.x.
- Add `contrib/fountain-theme.el`.
- Add a Makefile to locally check for byte-compile and linting errors.

## Version 3.1.0

- Expand functionality of `fountain-dwim` (bound to `TAB`); when point is:
  - at a blank line within dialogue, insert a parenthetical, or
  - inside a non-empty parenthetical, move to a newline, or
  - inside an empty parenthetical, delete it.
- If `fountain-dwim-insert-next-character` is non-nil, pressing `TAB`
  at the end of dialogue will insert the next (alternately speaking)
  character.
- Character extensions, e.g. (V.O.), (O.C.), are now autocompleted
  according to `fountain-character-extension-list`.
- Add hide markup toggle commands to menu.
- Add some default options to `fountain-mode-hook`.
- Avoid duplicate entries in character autocompletion.
- Use `completing-read` instead of `completing-read-default` to facilitate
  custom completion frameworks.
- Simplify character regular expression.
- Prefer `match-string-no-properties` for efficiency.
- Add file local variables to source.

## Version 3.0.3

- Fixed an issue on Emacs 24.x where Font Lock would fail before first
  scene heading.

## Version 3.0.2

- Added backwards compatibility for Emacs 24.x (which some Linux distros
  are unbelievably still stuck on).
- Added `seq` as dependency for said installations.

## Version 3.0.1

- Make `fountain-init-font-lock` ensure ordering of keywords (fixes an
  issue with scene heading highlight with scene numbers).
- Remove obsolete aliases for `fountain-add-contd-dialog` and
  `fountain-contd-dialog-string` which cause an error for people
  upgrading.

## Version 3.0.0

### Exporting

- Add `fountain-export-command` (`C-c C-e`), which interactively
  prompts for one of `fountain-export-command-profiles`, a list of
  cons-cells of profile name and associated shell command string. The
  command string allows interpolation of various values, most
  importantly `%b` and `%B` for buffer-file-name and the same sans
  extension. This allows infinitely flexible output for all Fountain
  programs, whether they read from STDIN/STDOUT, require the filename,
  etc. and the user can easily specify a different output name. The
  full list of options:
  - `%b` is the buffer-file-name
  - `%B` is the buffer-file-name sans extension
  - `%n` is the user-full-name
  - `%t` is the title (from script metadata)
  - `%a` is the author (from script metadata)
  - `%F` is the current date in ISO format
  - `%x` is the current date in your locale's "preferred" format
- Remove previous Elisp-based export functionality. This was never very
  good and there are several external tools available that better handle
  exporting.
- Add `fountain-export-view` (`C-c C-v`) to open the most recently exported
  file (uses `dired-guess-default`).

### Other Features

- `fountain-note-template` now uses a similar format-spec as
  `fountain-export-command-profiles`:
  - `%u` user-login-name
  - `%n` user-full-name
  - `%e` user-mail-address
  - `%x` date in locale's preferred format
  - `%F` date in ISO format
- `fountain-continued-dialog-string` now always prepends a space
  (reverting v2.8.1 change).
- Add option `fountain-imenu-elements` to choose which elements are
  displayed in Imenu.
- Add `imenu-add-menubar-index` as startup hook option.
- Options `fountain-align-*` are now concs-cells instead of two-element
  lists (may require resetting via Customize for some users).
- Make page length account for dual-dialogue elements.
- Make `fountain-completion-additional-`(characters/locations) case
  sensitive to allow for "@McCLANE".
- Rename `fountain-hide-emphasis-delim` to `fountain-hide-emphasis-markup`.
- Rename `fountain-hide-syntax-chars` to `fountain-hide-element-markup`.
- More things accessible via Fountain menu.
- New faces `fountain-section-heading-`(1--5) mapped the each section
  heading level.
- New variable `fountain-printed-elements` (only affects page length
  calculation).
- Refactor `fountain-init-font-lock` for simplicity.
- Only define `fountain-outline-invisible-p` in Emacs versions < 26
- Use more accurate buffer comparison for
  `fountain-outline-to-indirect-buffer` (`C-c C-x b`) to prevent
  duplicating indirect buffers.
- Use `fountain--*` for internal variables.
- Use `format-string` %S for some messages.
- Revert `fountain-*-contd-*` variables names to `fountain-*-continued-*`.
- Remove parsing code --- no longer using an AST.
- Remove file inclusion code --- this was never part of the 1.1 spec and
  is not supported by most export programs.

## Version 2.8.5

- Ensure `case-fold-search` is locally `t`.
- Fix small scene heading regexp error.

**n.b. Exporting is depreciated and will be removed in the next major
release. Several external tools are available that better export
Fountain files.**

## Version 2.8.4

- Added export depreciation message to `fountain-export-buffer`.
- Added export depreciation note to `fountain-export` group.
- Refactored generation of Font Lock keywords.
- Fixed some regexp bugs.

**n.b. Exporting is depreciated and will be removed in the next major
release. Several external tools are available that better export
Fountain files.**

## Version 2.8.1

- Add `fountain-insert-section` bound to `M-RET`.
- Remove `fountain-completion-additional-characters` as user option (now a
  defvar).
- Remove `fountain-completion-additional-locations` as user option (now a
  defvar).
- Change `fountain-scene-heading-suffix-sep` to regexp to allow "-" or
  "--".
- Rename `fountain-script-format` to `fountain-default-script-format`.
- Fix user options in wrong groups.
- `fountain-continued-dialog-string` now does not force a preceding space
- Options renamed `fountain-*-continued-*` to `fountain-*-contd-*`.
- Options renamed `fountain-pages-*` to `fountain-page-*`.
- `fountain-insert-page-break` no longer enforces prompt, instead
  prompting for forced page number when prefixed with `C-u`.
- `fountain-goto-page-break-point` now ignores non-exported elements
- Fix `fountain-get-page-count` not using appropriate end point.
- Fix `fountain-get-scene-number` incorrect regexp group.

## Version 2.8.0

- When cycling outline visibility, notes now remain folded and can be
  revealed with `TAB`. Customize this with option `fountain-fold-notes`.
- Allow blank lines in notes.
- Only highlight non-printing characters when font-lock decoration is
  set to level 3 (maximum).
- `fountain-set-font-lock-decoration` now uses `read-char-choice` (so
  you don't need to press `RET`).
- Use macros where appropriate instead of functions.
- Added docstrings to all functions/variables.
- Improved most regular expressions.

## Version 2.7.3

Fix infinite loop when reading metadata in some cases (buffer with only
metadata, i.e. no newline, will loop `fountain-read-metadata`).

## Version 2.7.2

Update email, URL, Debian pkg and aesthetics of code comments.

## Version 2.7.1

- Fix `fountain-completion-update-locations` bug.
- Update documentation.
- Use `lm-version` to keep `fountain-version` accurate.

## Version 2.7.0

This version removes a lot of the flaky features that never quite worked
right, in favour of making `fountain-mode` a solid writing environment.

- Scene heading completion now works decently; locations are
  auto-completed, followed by scene suffix (e.g. DAY, NIGHT). These are
  set with `fountain-scene-heading-suffix-list`.
- Add `fountain-scene-heading-suffix-sep`, a string to separate
  locaton/suffix.
- `TAB` now calls `fountain-dwim`, which will toggle section/scene heading
  folding at a heading (when not at EOLP) and trigger auto-completion
  everywhere else.
- Remove auto-completion update funcs from jit-lock-register.
- Remove mode-line page count information.
- Remove page count timer.
- Remove `fountain--edit-line` and associated functions.
- Make `fountain-completion-characters` buffer occurrence-based.
- Remove auto-upcase overlay feature (just use `C-c C-c` or `S-RET`);
  - Remove auto-upcase post-command-hooks,
  - Remove `fountain-auto-upcase-highlight` face.
- Remove `fountain-tab-command` option.

## Version 2.6.2

- Remove redundant option `:group` args.
- Updated copyright year.

## Version 2.6.1

- Fix incorrect use of `fountain-template-key-regexp` instead of
  `fountain-template-regexp`

## Version 2.6.0

### Improvements

- Added functions to shift all elements with `M-up` and `M-down`. These
  functions supersede `fountain-outline-shift-`(up|down) (although those
  functions are still called indirectly when point is at a heading).
  This behaviour can be toggled with `fountain-shift-all-elements`.
- Added Customize set functions to element aligning options to implement
  user options immediately (without needing to recall `fountain-mode`)
- Added `fountain-outline-hide-custom-level` as default hook (removed
  `fountain-outline-startup-level`)
- Removed endnotes feature. This syntax was never adopted by the wider
  Fountain community and introduced weird outline cycling issues. For a
  similar feature, I've created package Side Notes. Please use this instead.
- Respect buffer narrowing when counting pages.
- Added `fountain-script-format` option for default script format, e.g.
  stageplay, teleplay.
- Don't select included file when running `fountain-include-replace-in-region`.
- Improved character parsing to avoid calling other functions.
- Ensure forced scene headings begin with word characters.
- More robust check for interactive use of `fountain-count-pages`.
- Change depreciated `wp` keyword to `text`.

### Bug Fixes

- Fixed an annoying bug where auto-completion would sometimes insert
  "nil".
- Fixed a bug where transitions would not be included in export.
- Fix to prevent unintended auto-upcasing within a folded scene or
  section.
- Added missing "EST" to `fountain-scene-heading-prefix-list`.
- Fixed notes being included in metadata in some cases.
- Removed unused invisible text property on all elements that could
  interfere with some other modes.
- Removed errant obsolete warning for
  `fountain-export-include-title-page`.
- Fixed bug where sections headings could be parsed as characters.
- Fixed missing key-map pointers in docstrings.

### Theme

- I've also published the settings I use for Fountain Mode as
  `contrib/fountain-theme.el`.

## Version 2.5.3

Improvements to opening a scene/section in an indirect buffer:

- Buffer with be named "BUFFER-Heading" (matches Org-Mode's naming).
- Checks that headings are identical based on position rather than name
  (because a script might have many scenes with the same scene heading)
- Add a custom option to control how new windows are opened

Skip version 2.5.2 due to regression.

## Version 2.5.1

- Adds function `fountain-outline-to-indirect-buffer` to clone current
  scene/section in indirect buffer.
- Adds face remapping for default to `fountain` in Fountain buffers,
  allows e.g. setting "Courier Prime" font family just for scripts.
- Replace individual variables for endnotes display into
  `fountain-endnotes-display-alist`.
- Fixes bug where "#" would screw up LaTeX export.
- Remove ability to set `fountain-outline-startup-level` in metadata
  --- use `add-file-local-variable` instead.

## Version 2.5.0

### Autocompletion

Autocomplete scene headings and characters using `completion-at-point`
(bound to `M-TAB`). Completion at beginning of line will insert
character, whereas completion of scene headings only occurs after a
scene heading is recognised, e.g. after `INT.` or `EXT.` or anything
else in `fountain-scene-heading-prefix-list`. Character candidates
will be inserted in the order:

  - previous to previous speaking character within scene (i.e.
	conversation interlocutor),
  - previous speaking character within scene (i.e. same character),
  - other previously speaking characters within scene,
  - next highest *priority* characters in script.

If no characters are speaking within a scene, the next highest
priority character is inserted. Priority does not equate to the
character with the most lines, but rather the characters that occur
most around the text being edited in the current session.

### TAB Command Options

- You can now change the command that `TAB` calls from the menu, or by
  customising `fountain-tab-command`. The options are:
  - Contextual (`fountain-dwim`)
  - Cycle Scene/Section Visibility (`fountain-outline-cycle`)
  - Toggle Auto-Upcasing (`fountain-toggle-auto-upcase`)
  - Auto-Complete (`completion-at-point`)

### And...

- Fix counting comments towards page count.
- Fix an infinite loop that may occur while counting pages with long
  blocks of dialogue.
- Fix an infinite loop that would occur if a page break fell between a
  character and lines.

## Version 2.4.2

- Updated menu Save Options command to save all options set via menu.
- Fix comments in dialogue block fontified as dialogue.
- Fix comments counting towards page count.
- Removed key bindings for removed commands.

## Version 2.4.1

Bug fix when exporting with endnotes.

## Version 2.4.0

This is a large release with over 270 commits.

### New Features

- The biggest user-facing change is probably the addition of
  `fountain-dwim` on the `TAB` key:
  - If point is at a scene heading or section heading, or if prefixed
    with `ARG` (`C-u TAB`) call `fountain-outline-cycle`.
  - If point is at an directive to an included file, call
    `fountain-include-find-file`. This opens the included file in a new
    buffer for editing.
  - Otherwise, upcase the current line and active auto-upcasing. This
    highlights the current line with face `fountain-auto-upcase-highlight`
    and will continue to upcase inserted characters until the command is
    called again (`TAB`) or point moves to a different line (either by
    inserting a new line or point motion). This allows a flexible style
    of entering character names. You may press `TAB` before, during or
    after typing the name to get the same result.
- Another big feature is that Fountain Mode can now give an approximate
  page count:
  - `fountain-count-pages` on `C-c C-x p` will print the current page of
    total pages in the echo area, e.g. "Page 16 of 78".
  - Option `fountain-pages-show-in-mode-line` (nil by default) can be set
    to show the current page of total (e.g. "[16/78]") either with
    automatic or manual update. Automatic update uses an idle timer that
    can be set with `fountain-pages-count-delay`.
- Export to plain text, creating an output like that of scripts on
  https://www.imsdb.com
- Added [Mountain](https://github.com/mjrusso/mountain) style include
  workflow: files can be included with `{{ include: FILENAME.fountain }}`
  where FILENAME is relative to the current file.
- Display scene numbers in the right margin. (Requires a margin width
  greater than 0, e.g. with package Olivetti.
- Added teleplay format with its own aligning and exporting settings.
- A customisable title template for each export format.
- Command `fountain-insert-page-break` (`C-c C-x RET`) will calculate
  appropriate position before point to break page (e.g. never separating
  character names from dialogue, or breaking sentences).
- Section and scene headings in HTML output are now anchored and
  hyperlinked allowing easy bookmarking.
- New faces inherited from more appropriate font-lock faces.

### Improvements

- Menu organisation has been greatly improved.
- When `fountain-auto-upcase-scene-headings` is non-nil, editing a scene
  heading will overlay the line with a highlight.
- Custom options that change buffer display somehow (e.g. alter Font
  Lock keywords) now have set functions to automatically apply their
  updates, without having to call `fountain-mode` again.
- `fountain-goto-scene` and `fountain-goto-page` now call push-mark before
  relocating point, so that pop-mark will return you to where you were.
- Toggling including a title page works for all export formats.
- `fountain-continued-dialog-refresh` is now fast enough to work on the
  whole buffer, and if you’ve changed `fountain-continued-dialog-string`
  it will attempt to remove previous string first.
- Exporting to Final Draft will now recognise when an element starts a
  new page (i.e. page breaks).
- Emphasis delimiters are not fontified on minimum decoration level.
- Keymap now remaps existing bindings to analogous functions (e.g.
  forward-list to `fountain-forward-scene`) to suit users who have already
  have their own bindings for these.
- Incredible parsing speed improvements by using `comment-use-syntax`.
- A rewritten parsing and export model.
- Appropriately naming private variables (e.g. `fountain--outline-cycle`).

### Bug Fixes

- Removed some Font Lock multiline calculations that may have slowed
  things down.
- Fixed some scene heading font issues.
- Always check for CR as well as LF.
- "Go to" is two words.
- Many other small fixes.

## Version 2.3.1

- Updated `fountain-patch-emacs-bugs` to locally disable `show-paren-mode`.
- Release is also GPG signed.

## Version 2.3.0

The gap between master and a stable release was getting a little wide,
so I thought it best to release 2.3.0.

### Endnotes

This release adds an endnotes feature (`fountain-show-or-hide-endnotes`
bound to `M-s e`). This is an experimental feature not supported by most
(any?) other Fountain apps, but will not create any compatibility issues
if you don't use it.

## Version 2.2.2

- Added an option to automatically upcase scene headings.
- Page breaks now export to Final Draft (FDX) files.
- Use `outline-` prefixes and create aliases for Emacs before 25.1
- Fixed a show-stopping bug if autoinsert was loaded before
  `fountain-mode`.
- Rearranged code by functional topics.

## Version 2.2.1

- Fixed forced action being parsed as character.
- Fixed forced action exporting leading "!".
- When using `fountain-upcase-line-and-newline` with an argument (i.e.
  `C-u C-c RET`), only make a forced scene heading if not already
  (prevents adding "..").
- Fixed scene heading gap between heading and scene number being
  fontified on minimum decoration setting.

## Version 2.2.0

### Directives Syntax in Templates

In keeping with Fountain's upcoming "directives" syntax, formatting
templates has changed. Instead of `${key}` you now use `{{key}}`. This
change affects:

- `fountain-export-title-template`
- `fountain-export-contact-template`
- `fountain-export-html-template`
- `fountain-export-tex-template`
- `fountain-export-fdx-template`
- `fountain-export-fountain-template`
- `fountain-note-template`

This may cause some annoyance, but it allows for a very flexible use of
directives in the future. This also allows for recursive template
replacement in LaTeX documents (where "$" is special).

### Further Changes

- Removed `fountain-export-title-format`. Instead, to edit the title
  format, edit `fountain-export-title-template` with normal Fountain
  emphasis syntax, e.g. `_**{{title}}**_` would make it so the title is
  always bold and underlined. Control like this was previously not
  possible.
- Added section headings to LaTeX and HTML export templates.
- Fixed issue where HTML and LaTeX export would ignore
  `fountain-export-title-template`.
- Collapse multiple lines of whitespace in HTML template strings.
- Fixed an issue with LaTeX template strings with line breaks.
- Patch known Emacs bugs with named functions to make debugging easier.
- Fixes an issue where messages "Parsing..." and "Exporting..." would be
  printed on startup.
- Fixes an issue where exporting would fail on Emacs 25.1.

## Version 2.1.5

Patched bug in `outline-invisible-p` with advice override to return
non-nil only if invisible text property is `outline`. This allows the "."
on forced scene headings to be hidden when `fountain-hide-syntax-chars` is
non-nil (toggled with `C-c C-x !`). This advice is future-proof such that
when `outline` if fixed it will still work as expected.

## Version 2.1.4

- Split up the export templates, so you can make edits to one without
  saving every template to your init file. `fountain-export-templates` has
  become:
  - `fountain-export-html-template`
  - `fountain-export-tex-template`
  - `fountain-export-fdx-template`
  - `fountain-export-fountain-template`
- Added export hooks for all formats. These are useful for triggering a
  LaTeX compile or opening an exporting HTML file in your browser.
- Added local variables to make `tex-mode` use XeTeX engine for LaTeX
  export. Unfortunately Emacs will nag you about this being a risky
  local variable, so see
  [Safe File Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html).
- Fixed scene number regular expression. Previously #N would be
  recognised as a scene number when the spec asks for #N#.
- Made the default `fountain-note-template` begin with a space (looks
  neater).
- Fixed obsolete variable warnings.

## Version 2.1.3

- Set default scene heading prefixes to match http://fountain.io
- Fixed `fountain-export-page-size` option mismatch error.
- Fixed `fountain-export-standalone` option mismatch error.
- Fixed exporting to Fountain attempting to overwrite current
  buffer/file.
- Removed s.el dependencies.
- Removed `fountain-insert-template`.
- and just some neatening up doc strings.

## Version 2.1.2

- Fix key binding for GUI/terminal compatibility:
  - `S-RET` for `fountain-upcase-line-and-newline`
  - `S-TAB` for `fountain-outline-cycle-global`
- Integration with auto-insert (replaces `fountain-insert-metadata` with
  skeleton)
- Huge speed boost to parsing export by rewriting `fountain-comment-p` to
  always search forwards.
- Fix character regular expression to allow names starting with numbers
  (e.g. "23B").
- Fix scene number regular expression to look for trailing "#".
- Fix for HTML template: added `viewport` tag.
- Merged `fountain-short-time-format` and `fountain-long-time-format` into
  `fountain-time-format`.
- Prompt to overwrite export buffer contents.
- Automatic saving export file with prompt to overwrite existing file.
- Removed `fountain-metadata-template`.

## Version 2.1.1

- Fixed errant newlines in LaTeX dialogue output.
- Updated file Commentary.

## Version 2.1.0

- Fountain Mode now uses [lexical binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html)
- HTML export template [McQueen](https://github.com/rnkn/mcqueen) is
  now mobile responsive.
- LaTeX export template now indents dialogue to 1 in instead of 1.5 in.
- *.fountain files now automatically load Fountain Mode.
- Emacs page functions now work with Fountain forced page breaks.
- Documentation strings for all functions and variables.
- Bug fix: export templates now include `fountain-export-contact-template`.
- Removed option for using external stylesheet with HTML export.
- Removed omit from scene element property list.
- Removed `fountain-additional-template-replace-functions`.
- Use function quoting for better byte compilation.

## Version 2.0.1

- Fixed "wrong version" problem with Final Draft export template.

## Version 2.0.0

### Exporting

- Export to LaTeX, HTML, Final Draft, Fountain or your very own custom
  formats.
- Separation of the LaTeX template project and the HTML template project.
  - https://www.sharelatex.com/project/54ed9180966959cb7fdbde8e
  - https://github.com/rnkn/mcqueen
- Skip the Lisp export entirely and use `fountain-export-shell` command
  with 'afterwriting or TextPlay or whatever you like.
  - https://github.com/ifrost/afterwriting-labs/blob/master/docs/clients.md
  - https://github.com/olivertaylor/Textplay
- New parsing engine reads Fountain text into Lisp data for further
  magic.
- New export engine allows much faster export—to virtually any format.
- Choice of export to standalone document or a snippet.
- All export templates are totally customizable.

### Not just screenplays

- Auto-align elements to different columns for different formats (e.g.
  screenplay or stageplay), just include format: stageplay or whatever
  in your metadata.
- Action is now a first-class element that can be auto-aligned (like,
  for stageplays).

n.b. the export templates are really geared towards screenplays, so if
you're a playwright and feel lost please get in touch and we'll see what
we can do

### New abilities

- Scene numbers optionally align to the right margin.
- `fountain-upcase-line` with `C-c C-c` (`fountain-continued-dialog-refresh`
  is moved to `C-c C-d`).
- Prefix `fountain-upcase-line` or `fountain-upcase-line-and-newline` with
  `C-u` to make insert "." at the beginning to make a forced scene heading.
- Terminal-friendly key map.
- Align the title page contact info to the left or right.

### Better existing abilities

- Imenu regular expression now show section heading prefixes (e.g.
  "# act II" instead of "act II").
- `fountain-mark-scene` now excludes marking a following outline heading
- For those writers who like to include blank lines _within_ dialogue,
  you should be golden.
- It should now be impossible to get into an endless loop.

### Removing the cruft

- Removed reliance on Prince for PDF export (use LaTeX instead).
- Reduced reliance on s.el.
- Removed `fountain-uuid` and related variables.
- Title page templates now only use a single contact box instead of a
  left and right.

### Notes on upgrading

As this is a major version upgrade, some backwards compatibility is
lost. I've tried to document this as much as possible here:
https://github.com/rnkn/fountain-mode/wiki/Upgrading-From-1.x-to-2.0

If you have any trouble, please get in touch.

## Version 1.5.1

- Adds Emacs 24.4.0 as dependency.
- Adds partial fix for problematic two-space rule.

## Version 1.5.0

- Implemented basic Imenu functionality.

## Version 1.4.5

- Workaround bug where forced scene headings would behave properly in
  outline cycling, but means the syntax character "." cannot be hidden.

## Version 1.4.4

- Optimization (removed use of extremely slow `thingatpt` functions).

## Version 1.4.3

- Major update to integrate with outline, providing structure navigation
  and editing, including:
  - `TAB` cycle outline visibility of the current section/scene
  - `S-TAB` cycle visibility of the entire buffer
  - `C-c C-f` move forward the same outline level
  - `C-c C-b` move backward the same outline level
  - `C-c C-n` move to next section/scene
  - `C-c C-p` move to previous section/scene
  - `C-c C-u` move up outline level
  - `C-c C-v` shift the current section/scene down (swap with follow)
  - `C-c C-^` shift the current section/scene up (swap with previous)
  - `C-c C-SPC` mark the current section/scene
- Added `fountain-outline-startup-level`, the outline visibility level to
  show on startup, which can also be set per-file with the startup-level
  metadata key.
- Added navigation by character:
  - `M-n` move forward character
  - `M-p` move backward character
- Options set in the menu can now be saved _en masse_ with Save Options
  (`custom-save-all`)
- Improved the menu layout.
- Improved element regular expression matching.
- Improved `font-lock extend region` function.
- Improved various navigation functions.
- Fixed scene heading margin-bottom in HTML export (feature screenplays
  should now be about two pages shorter).
- Fixed edge-case errors when working with `font-lock-maximum-decoration`.
- Optimized `fountain-comment-p`.
- Comments now use syntax instead of regular expressions.
- Removed INT/EXT from default `fountain-scene-heading-prefix-list`.
- Other changes to pave the way for an overhaul of the way Fountain Mode
  works with screenplays (exporting, statistics, etc.).

## Version 1.3.4

- Fixed a bug where `font-lock-extra-managed-props` was being set at a
  global instead of buffer-local level.

## Version 1.3.3

- Changed template keys: point is now `$@` and mark is now `$?`.
- Documentation updates.

## Version 1.3.2

- Fixes (_really_ fixes) issue that prevented customizing scene heading
  prefixes and transitions.

## Version 1.3.1

This release introduced a bug that broke everything, fixed in
c3804ae5.

- Changed default face for `fountain-metadata-value` to inherit from
  `font-lock-comment-face`.

## Version 1.3.0

- Added support for automatic "MORE" and "CONT'D" when dialog breaks across
  pages in PDF output. Both of these strings are customizable.
- Adjustable widow and orphan lines for dialog and action. If you prefer
  not to break action or dialog across pages, increase these values
  (default 2).
- Combined `fountain-export.el` into `fountain-mode.el` to prevent
  byte-compile warnings.
- Removed FADE IN: from default list of transitions. Usually the opening
  FADE IN should be left justified, so I found myself always escaping it
  with "!" but I figure it makes more sense to just omit it. You can add
  it back if you like or force the transition with ">".
- Increased maximum element size to 10,000 characters.

## Version 1.2.1

- Fixed issue where very long blocks without blank lines could hang Emacs.

## Version 1.2.0

- A big rewrite of the way elements are handled to boost export speed.
  Elements are now given a `fountain-element` text property instead of
  just a face property. This means that Font Lock won't need to
  refontify the entire buffer on export, so if you've been working on a
  screenplay for a while, Font Lock will have already fontified most of
  the buffer in the background, making exporting take a couple of
  seconds. (On the other hand, if you open a .fountain file and export
  right away, or change any font settings that trigger
  `font-lock-refresh-defaults` then you'll still need to wait.)
- A big change to the faces used. In prior versions, there were
  `fountain-ELEMENT` faces and `fountain-ELEMENT-highlight` faces, but the
  former were mostly the default face. This slowed down Emacs. Now there
  are only `fountain-ELEMENT` faces, and they are applied as per the level
  of `font-lock-maximum-decoration`. For an explanation of which faces are
  used at which level of decoration, see the `fountain-faces` group
  documentation.
- Added support for emphasis (bold, italic, underlined text) and the
  ability to toggle the visibility of the emphasis delimiters.
- Also added a toggle for visibility of escaping characters (e.g. as
  used in forced scene headings, centered text, forced transitions,
  etc.).
- Added the "Navigator" (`occur`) functions to the menu.
- Scene navigation and selection now adheres to section headings. (Not
  sure if this is right way to go?)
- A big cleanup of documentation.
- Added the README content to the header Commentary.
- Combined a few functions for efficiency.

There's currently a workaround for the implementation of visibility
toggling: if the mode starts with `buffer-visibility-spec` set to `t` it
would cause a bunch of things to become invisible (unwanted). If this is
the case the variable is set to nil. However, this is not foolproof, so
I'll continue testing it. If you experience any issues with things
becoming invisible when they shouldn't, please let me know on the
[Issues](https://github.com/rnkn/fountain-mode/issues/) page.

## Version 1.1.1

- Fixed issue where minor modes could interfere with export fontify.
- Fixed issue where refreshing continued dialog could result in endless
  loop.
- Fixed possible issue setting mark and point in templates.
- Character is now HTML `p` element instead of `h3` to prevent PDF readers
  hanging.
- "authors" metadata is now "author" by default to increase compatibility
  with HTML and PDF.

## Version 1.1.0

- Added a navigator for sections, synopses, notes or scene headings
  (`M-s 1`, `M-s 2`, `M-s 3` and `M-s 4` respectively) using occur.
- Option to include or omit a title page, which is useful if you just
  want to export a couple of scenes (by narrowing the buffer restriction
  to the desired selection).
- Title page templates are now split into three blocks: title, left and
  right. You can put whatever you like in these, and use any metadata
  keys you like.
- Added the "forced action" specification, so anything preceded with a "!"
  will be treated as action (the "!" is stripped on export).
- You can now see that Emacs is working away while adding continued
  dialog markers, which is especially useful when applying to an entire
  feature screenplay.
- Renamed "centered" elements to "center" elements.
- Removed some useless custom options for naming buffers that are never
  seen (who needs that?)
- Other minor fixes.

## Version 1.0.0

- The big addition is the ability to export to HTML or PDF (via HTML,
  requires [Prince](https://www.princexml.com/).
- Reading metadata (sets buffer-local variable `fountain-metadata` as an
  association list of key-value pairs of anything given in metadata
  syntax at the beginning of the buffer).
- Title page export with customizable template using metadata values.
- Export scene headings as bold, underlined and/or double-spaced
  (distinct from Font Lock display).
- Customizable default export command.
- Customizable CSS.
- Optionally convert TeX-style quotes to "smart quotes" on export.
- Optionally indent HTML output.
- Templates can now set the point and mark.
- Fixed a whole host of regular expression bugs and inefficiencies.
- Depreciated: there is no longer a distinction between forced scene
  headings and regular, a scene heading is a scene heading.

## Version 0.13.0

- Rather large overhaul of internals, which should make everything a lot
  faster (for the curious, indenting elements is now covered completely
  by Font Lock).
- Fixed a recently introduced bug that would leave trailing whitespace
  when removing continuing dialog markers.
- Some regular expression improvements.
- Some small syntax highlighting changes improvements (to pave the way
  for exporting, which is coming along very soon!).
- Added a `${title}` template key, if you were really hanging out for
  that...
- Unfortunately, due to the internal rewrite, centered text is now not
  _really_ centered :(

## Version 0.12.1

- Improvements to documentation.
- Minor bug fixes and optimizations.

## Version 0.12.0

- Syntax colors have arrived. Almost everything now has a face, and
  these are now mapped to vaguely appropriate Font Lock faces (e.g.
  scene headings: `font-lock-function-face`) so you'll be able to switch
  Emacs themes at whim.
- Syntax highlighting levels. Switch highlighting levels between none,
  minimal and maximum.
- Removed the clean margins feature, which is implemented better in
  [writeroom-mode](https://github.com/joostkremers/writeroom-mode).
- Lots of little bug fixes and optimizations.

This will probably be the last update before version 1.0, in which I'm
hoping to finally have export (to HTML) functionality!

## Version 0.11.0

- Added a menu with the major commands.
- Added clean margins feature. The window margins will automatically
  adjust to display a custom text body width integer or full width.
- You can now add hooks in Customize.
- `visual-line-mode` will turn on by default.
- The function to generate a UUID is now customizable.
- Centered text elements can be displayed centered in the text body or
  indented to a custom integer.
- Comments (boneyard elements) do not interfere with character elements
  (experimental).
- Fixed a _major_ mistake with the default commenting syntax that has
  been present since the beginning (very sorry about this!).
- `C-c C-c` now only adds/removes continued dialog (removed upcasing
  functions).

## Version 0.10.3

- Fixed synopsis regular expression bug.

## Version 0.10.2

- Added missing `s` dependency.
- Bug fixes and optimisations, including minimising use of
  `thing-at-point` and removing all uses of `rx` in favour of regular
  expressions.

## Version 0.10.1

- `C-c C-c` will also upcase the scene heading at point.
- Bug fixes and optimisations.

## Version 0.10.0

- `C-c C-c` will now apply the the continued dialogue marker to characters
  speaking in success within the current region if active, current
  scene, or the entire buffer if prefixed with `C-u`.

## Version 0.9.4

- Cleaning up and optimisation.

## Version 0.9.2

- Changed the way that element indenting is handled, so a regular
  120-page screenplay should load a heck of a lot faster now.
- Bug fixes.

## Version 0.9.1

- Navigate by scene headings (`M-n` and `M-p`).
- Bug fixes for inserting a synopsis.

## Version 0.9.0

- Rewrote many functions to use [s.el](https://github.com/magnars/s.el).
- Inserting a note now uses a custom template.
- Inserting metadata now uses a custom template.
- Scene heading recognition is now faster.

## Version 0.8.2

- Improvements to automatic formatting.
- Removes problematic auto-upcase slugline function introduced in
  4cd6afcb.

## Version 0.8.1

- Changed default key-binding to insert metadata to `C-c C-x i`.

## Version 0.8.0

- Optimised automatic formatting to be much cleaner and faster.
- Bug fixes.

## Version 0.7.3

- Added full boneyard support.
- Added insert note command (`C-c C-z`).
- Bug fixes.

## Version 0.7.0

- Changed comments to boneyard syntax.
- Added notes syntax.
- Bug fixes.

## Version 0.6.0

- Added auto-indentation for character, parenthetical, dialogue and
  transition elements (display only, does not modify file contents).
