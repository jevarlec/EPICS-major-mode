# Emacs Major Mode for EPICS

Author: Jernej Varlec

Version: 0.5.0

## Installation and Uninstallation

NOTE: For best experience, use Emacs version >= 27.

Not yet released on (M)ELPA, you have to manually install:
1. Clone the repo.
2. Open Emacs and run `M-x package-install-file` and point it to the *epics-mode.el*.
3. Add the following to your *init.el*: `(require 'epics-mode)`. Reevaluate or restart Emacs.
4. Opening a .dbd, .db or .template file will autoload epics-mode.

To remove:
1. Delete/comment the line `(require 'epics-mode)` in your *init.el*.
2. Run `M-x package-list-packages`. Search for *epics-mode*, put the point on it and press enter.
3. In the package details buffer, press `delete`.
4. Delete the cloned repo.

## Features

Note that most of the features are still work in progress and might be rough around the edges.

### Syntax Colouring

Editing database files is less painful. Strings, macros, asyn, streamdevice, and link parameters all have their own colour.

### Automatic Indentation

The mode will indent every line inside the record block (curly braces). By default it indents by four spaces, but this is customizable. 

### Integrated Record Reference

Effortlessly access record reference pages by simply pointing at the record and issuing the command. You can also open reference page by name, which allows you to open any page in base/html directory.

### Assisted Database Navigation

You can follow the link fields if they point to a record. This provides quick navigation between linked records. For now this feature is limited and cannot navigate between files.

## Customization

You can modify settings by using Emacs customization interface (`M-x Customize`) and search string "epics". These are the available settings:

* `epics-indent-spaces` - How many spaces should the indentation engine do when indenting. Default value is 4.
* `epics-path-to-base` - Where the base/ folder is located. If set to "From environment variable", then epics-mode will attempt to get the path from environment variable 'EPICS_BASE'. Other choice is to provide a directory. Please note that you have to run `M-x epics-mode` after any changes. Default value is "From environment variable".

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
