# Emacs Major Mode for EPICS

Author: Jernej Varlec

Version: 0.6.0

## Installation, Updating and Uninstallation

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

Note that when you pull changes it might be necessary to run `M-x package-install-file` again.

## Features

Note that most of the features are still work in progress and might be rough around the edges.

### Syntax Colouring

Strings, macros, asyn, streamdevice, and link parameters all have their own colour.

### Indentation

The mode will indent every line inside the record block (curly braces). By default it indents by four spaces, but this is customizable. 

### Integrated Record Reference

Access record reference pages by simply pointing at the record and issuing the command. You can also open reference page by name, which allows you to open any page in base/html directory.

### Assisted Database Navigation

You can follow the link fields if they point to a record. This provides quick navigation between linked records. For now this feature is limited to the current buffer and cannot navigate between files. 

You can also quickly navigate between records and values.

### Code Snippets

Commands are provided to create, modify and remove snippets.

## Customization

You can modify settings by using Emacs customization interface (`M-x Customize`) and search string "epics". These are the available settings:

* `epics-indent-spaces` - How many spaces should the indentation engine do when indenting. Default value is 4.
* `epics-path-to-base` - Where the base/ folder is located. If set to "From environment variable", then epics-mode will attempt to get the path from environment variable 'EPICS_BASE'. Other choice is to provide a directory. Please note that you have to run `M-x epics-mode` after any changes. Default value is "From environment variable".
* `epics-var-dir` - Desired location for persistent variables. Default is "user-emacs-directory/var/epics-mode", where *user-emacs-directory* is usually "~/.emacs.d/".
* `epics-enable-snippets` - Allow expansion of snippets if set to "Yes".
* `epics-always-include-desc` - Always add DESC field when expanding a snippet if set to "Yes".
* `epics-always-include-scan` - Always add SCAN field when expanding a snippet if set to "Yes".

## Using the Help Buffer

You can conveniently access the reference files located in your EPICS base/html directory by using the 'epics-open-reference' (`C-c h h`). This will present you with a prompt where it will ask you to provide a file name. Note that the prompt is semi intelligent and will narrow down your options as you type. Press `tab` to display all your options.

You can also open record reference directly by using 'epics-describe-record' (`C-c h r`) while the cursor (more ofter refered to as 'point') is located inside the record block.

Press `q` to close the help buffer after you are done using it.

The help buffer is at the moment only useful for reading the reference files. It does not support links - that is something to look forward to in the future. The formatting will improve over time as well. Still, it does provide a fast way to check the references and the help buffer is searchable.

## Using the Navigation Commands

Commands 'epics-next-record' (`C-c C-l`) and 'epics-previous-record' (`C-c C-h`) allow for quick navigation between record blocks. Invoking either command will put the point at the record type.

To quickly navigate between values, use commands 'epics-next-value' (`C-c C-j`) and 'epics-previous-value' (`C-c C-k`). This will put the point at the beginning of double quotes, if the value is enclosed with double quotes, or next to the comma.

The mode provides two commands that allow you to quickly navigate between linked records: 'epics-follow-link' (`C-c C-'`), which will find the appropriate record and move point to it. I will also remember where it was before. This is where the second command comes in: 'epics-retrace-link' (`C-c C-;`), which will retrace the steps you took to get where you are now.

These commands are (for now at least) not aware of records residing in a different database file.

## Using Snippets

To use the snippets facility, ensure that `epics-enable-snippets` is set to "Yes". You should use `M-x epics-mode` after changing this variable.

Snippets have the following form:
```
snippet-id record-type field1 field2 ... fieldN
```
For example: `;calc calc desc scan calc inpa inpb`, where *;calc* is the id, *calc* the record type, etc.

To insert a snippet, one would simply write the snippet-id (for example *;calc*) in the buffer and press space, enter, or tab. The above example would expand into:
```
record(calc, "") {
    field(DESC, "")
    field(SCAN, "")
    field(CALC, "")
    field(INPA, "")
    field(INPB, "")
}
```
You could then use the 'epics-next-value' and 'epics-previous-value' to navigate between the values.

### Displaying Available Snippets

Invoking 'epics-show-active-snippet-alist' (`C-c s s`) will display what snippets are available to be used. This buffer is also shown when editing or removing snippets, so it provides reference.

### Adding, Editing, and Removing Snippets

New snippet forms can be defined using 'epics-add-snippet-to-active-alist-maybe' (`C-c s a`). You will be asked to input the desired form, for example *;ai ai dtyp inp hopr lopr*, where ";ai" is the snippet id (this is used to identify which form to expand), "ai" is the record type, and the rest are the fields. The number of fields is not limited.

Using 'epics-edit-snippet' (`C-c s e`) will prompt for the snippet-id, which is used to identify which snippet to edit. The snippet is then fetched to the minibuffer where it can be edited.

'epics-remove-snippet' (`C-c s d`) will prompt for the snippet-id, which is used to identify which snippet to remove. The snippet is then removed from the table.

To remove all snippets, use 'epics-clear-active-snippet-alist'.

### Saving and Loading Snippets

You can save the snippets to 'epics-saved-snippets-file' located in 'epics-var-dir' by using 'epics-save-active-snippet-alist' (`C-c s w`). 

You can also load the saved snippets from the 'epics-saved-snippets-file' with 'epics-load-saved-snippets-to-active-alist' (`C-c s l`).

### Factory Default Snippet Table

A default snippet table is provided as an example. It is automatically saved to the 'epics-saved-snippets-file', so the user can add to in, edit it, or overwrite it completely. You can decide at any point to restore the original table to the file by using 'epics-restore-default-snippet-table' (`C-c s r`).

This default table is also used at the first run of the mode when the initial snippet file is created and populated.

## Keymaps

All keymaps can be displayed with `C-h m`. You can also display help for any command with `C-h f`and inputting command name (e.g. epics-describe-record).

### Help and Reference
* `C-c h r` - epics-describe-record: Find record type under cursor and display record reference
* `C-c h h` - epics-open-reference: Ask user what reference file to open

### Navigation
* `C-c C-'` - epics-follow-link: Search for a link on the current line and attempt to follow it
* `C-c C-;` - epics-retrace-link: Retrace the last followed link
* `C-c C-l` - epics-next-record: Put point at the record type of the next record block
* `C-c C-h` - epics-previous-record: Put point at the record type of the previous record block
* `C-c C-j` - epics-next-value: Put point at the next value
* `C-c C-k` - epics-previous-value: Put point at the previous value

### Snippets
* `C-c s s`- epics-show-active-snippet-alist: Display snippet alist in the help buffer
* `C-c s a`- epics-add-snippet-to-active-alist-maybe: Prompt for snippet to add
* `C-c s e`- epics-edit-snippet: Prompt for id and fetch the corresponding snippet to minibuffer
* `C-c s d`- epics-remove-snippet: Prompt for id and remove the corresponding snippet
* `C-c s w`- epics-save-active-snippet-alist: Save snippets to *epics-saved-snippets-file*
* `C-c s c`- epics-clear-active-snippet-alist: Remove all snippets from active table
* `C-c s r`- epics-restore-default-snippet-table: Restore default snippets to *epics-saved-snippets-file*
* `C-c s l`- epics-load-saved-snippets-to-active-alist: Load snippets from *epics-saved-snippets-file*

