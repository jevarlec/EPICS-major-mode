# Emacs Major Mode for EPICS

Author: Jernej Varlec

Version: 0.5.0

## Installation

Until a release on MELPA, you have to manually evaluate:
1. open *epics-mode.el* in emacs and run `M-x eval-buffer`.
2. opening a .dbd, .db or .template file will autoload epics-mode.

## Features

Note that most of the features are still work in progress and might be rough around the edges.

### Syntax Colouring

Editing database files is less painful. Strings, macros, asyn, streamdevice, and link parameters all have their own colour.

### Automatic Indentation

The mode will indent every line inside the record block (curly braces). By default it indents by four spaces, but this is customizable. 

### Integrated Record Reference

Effortlessly access record reference pages by simply pointing at the record and issuing the command. You can also open reference page by name, which allows you to open any page in base/html directory.

### Assisted Database Navigation

Implemented right now: You can follow the link fields if they point to a record. This provides quick navigation between linked records. For now this feature is limited and cannot navigate between files.

## Customization

You can modify settings by using Emacs customization interface (`M-x Customize`) and search string "epics". These are the available settings:

* `epics-indent-spaces` - How many spaces should the indentation engine do when indenting. Default value is 4.
* `epics-path-to-base` - Where the base/ folder is located. If set to "env", then epics-mode will attempt to get the path from environment variable 'EPICS_BASE'. Please note that you have to use double quotes when modifying this setting and that you have to run `M-x epics-mode` after any changes. Default value is "env".

## Using the Help Buffer

You can conveniently access the reference files located in your EPICS base/html directory by using the 'epics-open-reference' (`C-c h h`). This will present you with a prompt where it will ask you to provide a file name. Note that the prompt is semi intelligent and will narrow down your options as you type. Press `tab` to display all your options.

You can also open record reference directly by using 'epics-describe-record' (`C-c h r`) while the cursor (more ofter refered to as 'point') is located inside the record block.

Press `q` to close the help buffer after you are done using it.

The help buffer is at the moment only useful for reading the reference files. It does not support links - that is something to look forward to in the future. The formatting will improve over time as well. Still, it does provide a fast way to check the references and the help buffer is searchable.

## Using the Navigation Commands

The mode provides two commands that allow you to quickly navigate between linked records: 'epics-follow-link' (`C-c C-'`), which will find the appropriate record and move point to it. I will also remember where it was before. This is where the second command comes in: 'epics-retrace-link' (`C-c C-;`), which will retrace the steps you took to get where you are now.

These commands are (for now at least) not aware of records residing in a different database file.

## Keymaps

* `C-c C-'` - epics-follow-link: Search for a link on the current line and attempt to follow it
* `C-c C-;` - epics-retrace-link: Retrace the last followed link
* `C-c h r` - epics-describe-record: Find record type under cursor and display record reference
* `C-c h h` - epics-open-reference: Ask user what reference file to open
