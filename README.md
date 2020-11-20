# Emacs Major Mode for EPICS

Author: Jernej Varlec

Version: 0.4.2

## Installation and use

Until a release on MELPA, you have to manually evaluate:
1. open *epics-mode.el* in emacs and run `M-x eval-buffer`.
2. opening a .dbd, .db or .template file will autoload epics-mode.

You can modify settings by using Emacs customization interface (`M-x Customize`).

## Keymaps

* `C-c C-'` - epics-follow-link: Search for a link on the current line and attempt to follow it
* `C-c C-;` - epics-retrace-link: Retrace the last followed link
* `C-c r` - epics-describe-record: Find record type under cursor and display record reference
* `C-c C-r` - epics-describe-record-prompt: Prompt for record type and display record reference
