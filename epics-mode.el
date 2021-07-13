;;; epics-mode.el --- EPICS major mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jernej Varlec

;; Author: Jernej Varlec <jernej@varlec.si>
;; Keywords: elisp, epics
;; Version: 0.6.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distribute"d in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Description:

;; A major mode for editing EPICS .db and .template files.
;; Consult README.md for detailed description and features.

;;; Code:

(require 'cl-lib)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.db\\|.template\\|.dbd\\'" . epics-mode))

;; epics group for customization variables and variables themself
(defgroup epics-config nil
  "Customization variables for EPICS mode."
  :group 'programming)

(defcustom epics-indent-spaces 4
  "Setting for desired number of spaces per brace depth. Default
is 4."
  :group 'epics-config
  :type 'number)

(defcustom epics-path-to-base "env"
  "Path where EPICS base is installed.  Run M-x epics-mode after
changing this.  If set to 'env', then epics-mode will try to get
path from environment variables.  Default is 'env'."
  :group 'epics-config
  :type '(choice (const :tag "From environment variables" :value "env")
                 (directory)))

(defcustom epics-var-dir
  (concat user-emacs-directory "var/epics-mode/")
  "Default directory used for persistent variables."
  :group 'epics-config
  :type '(choice (const :tag "user-emacs-directory/var/epics-mode"
                        :value (concat user-emacs-directory "var/epics-mode/"))
                 (directory)))

(defcustom epics-always-include-desc t
  "If set to non-nil, always add a DESC field when expanding a
  snippet. It is set to t by default."
  :group 'epics-config
  :type '(choice (const :tag "Yes"
                        :value t)
                 (const :tag "No"
                        :value nil)))

(defcustom epics-always-include-scan t
  "If set to non-nil, always include SCAN field when expanding a
  snippet. It is set to t by default."
  :group 'epics-config
  :type '(choice (const :tag "Yes"
                        :value t)
                 (const :tag "No"
                        :value nil)))

;; define custom faces
(defface epics-face-shadow
  '((t :inherit shadow))
  "Face name to be used for records and fields.")

(defface epics-face-link-param
  '((t :inherit font-lock-constant-face
       :weight bold))
  "Bold face for link parameters.")

;; syntax highlighting
(defvar-local epics-font-lock-keywords
      (let* (
             ;; define categories of keywords
             (epics-shadow '("record" "field" "path" "addpath" "include" "menu" "choice" "recordtype" "device" "driver" "registrar" "function" "variable" "breaktable" "grecord" "info" "alias"))
             (epics-link-params '("MSS" "NMS" "NPP" "CPP" "MS" "PP" "CA" "CP"))
             (epics-keywords '("All" "Specified" "Mask" "High Signal" "Low Signal" "Median Signal" "YES" "NO" "RUN" "RUNNING" "PAUSE" "PAUSED" "RAW" "Passive" "Event" "I/O Intr" "10 second" "5 second" "2 second" "1 second" ".5 second" ".2 second" ".1 second" "supervisory" "closed_loop" "STRING" "CHAR" "UCHAR" "SHORT" "USHORT" "LONG" "ULONG" "FLOAT" "DOUBLE" "INT64" "UINT64" "ENUM" "NO_ALARM" "MINOR" "MAJOR" "INVALID" "stream" "asynInt32" "asynInt32Average" "asynInt8ArrayIn" "asynInt8ArrayOut" "asynInt16ArrayIn" "asynInt16ArrayOut" "asynInt32ArrayIn" "asynInt32ArrayOut" "asynInt64ArrayIn" "asynInt64ArrayOut" "asynInt32TimeSeries" "asynInt64" "asynInt64TimeSeries" "asynUInt32Digital" "asynFloat64" "asynFloat64Average" "asynFloat32ArrayIn" "asynFloat32ArrayOut" "asynFloat64ArrayIn" "asynFloat64ArrayOut" "asynFloat64TimeSeries" "asynEnum" "asynOctetRead" "asynOctetWrite" "asynOctetWriteRead" "asynOctetWriteBinary" "asynOctetCmdResponse" "READ" "WRITE" "HIHI" "HIGH" "LOLO" "LOW" "STATE" "COS" "COMM" "TIMEOUT" "HWLIMIT" "CALC" "SCAN" "LINK" "SOFT" "BAD_SUB" "UDF" "DISABLE" "SIMM" "READ_ACCESS" "WRITE_ACCESS" "NO CONVERSION" "SLOPE" "LINEAR" "Continue normally" "Don't drive outputs" "Set output to IVOV" "On Change" "Always" "MEDIUM"))

             ;; generate regex string from keyword categories
             (epics-keywords-regexp (regexp-opt epics-keywords t))
             (epics-shadow-regexp (regexp-opt epics-shadow 'words))
             (epics-link-params-regexp (regexp-opt epics-link-params 'words)))

        `(
          ;; apply faces to generated regex
          (,epics-shadow-regexp . 'epics-face-shadow)
          (,epics-link-params-regexp 0 'epics-face-link-param t)
          (,(format "\"%s\"" epics-keywords-regexp) 1 font-lock-keyword-face t)

          ;; define regex for asyn i/o parameters
          (,"\"\\(@\\(asyn\\|asynMask\\)\\((.+?)\\)\\)[A-Za-z0-9_-]*?\"" 1 font-lock-type-face t)

          ;; define regex for streamdevice i/o parameters
          (,"\"\\(@.+?\.proto\\)" 1 font-lock-type-face t)

          ;; define regex for macro highlighting
          (,"$(\\([^ ]+?\\))" 0 font-lock-variable-name-face t))))


;; utility functions and variables
(defvar-local epics--actual-base-dir nil
  "Internal var that holds path to epics base.  This is the
  variable that is actually used internally, not
  epics-path-to-base.")


(defun epics--blank-line-p ()
  "Return t if line is blank."

  (let ((result (string-match-p "^ *$" (thing-at-point 'line t))))
    (if (= result 0)
        t
      nil)))


(defun epics--position-at-next-blank-line ()
  "Position the point at the next blank line and return t when
  successful."

  (interactive)
  (if (or (epics--blank-line-p)
          (= (point) (point-max)))
      t
    (forward-line)
    (epics--position-at-next-blank-line)))


(defun epics--get-base-dir-string ()
  "Return validated base dir string for use in other functions."

  (let ((path (if (equal epics-path-to-base "env")
                  (getenv "EPICS_BASE")
                epics-path-to-base)))

    (if path
        (if (file-directory-p path)
            (progn
              (if (string-suffix-p "/" path)
                  path
                (concat path "/")))
          (message "Invalid base path set: %s" epics-path-to-base))
      (message "EPICS base not found")
      nil)))


(defun epics--string-on-line-p (string &optional inside-strings)
  "Return non-nil if STRING is present on the current line.  Also
searches strings if INSIDE-STRINGS is non-nil."

  (save-excursion
    (beginning-of-line)
      (if (string-match-p string (thing-at-point 'line t))
          (epics--search-forward string inside-strings)
        nil)))


(defun epics--search-forward (string &optional inside-strings inside-comments)
  "Same as `search-forward', except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (epics--search-with #'search-forward string inside-strings inside-comments))


(defun epics--search-backward (string &optional inside-strings inside-comments)
  "Same as `search-backward', except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (epics--search-with #'search-backward string inside-strings inside-comments))


(defun epics--search-with (search-func string &optional inside-strings inside-comments)
  "Use this function with a wrapper! e.g. `epics--search-forward'

Same as desired SEARCH-FUNC, except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (let ((point-after-search (funcall search-func string nil t nil)))

    (cond ((null point-after-search) nil)

          ((epics--inside-comment-p)
           (if inside-comments
               point-after-search
             (epics--search-with search-func string inside-strings inside-comments)))

          ((epics--inside-string-p)
           (if inside-strings
               point-after-search
             (epics--search-with search-func string inside-strings inside-comments)))

          (t point-after-search))))


(defun epics--copy-string-at-word (word del1 del2)
  "Yanks the string located between DEL1 and DEL2, forward of
WORD.  All inputs are strings, return the string or nil if
no match."

  (let (p1 p2 return-string)

    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (not (equal (current-word) word))
          nil
        (skip-chars-forward (concat "^" del1 "\n"))
        (forward-char)
        (setq p1 (point))
        (skip-chars-forward (concat "^" del2 " .\n"))
        (when (equal (following-char) del2)
          (backward-char))
        (setq p2 (point))
        (save-excursion
          (goto-char (point-min))
          (setq return-string (buffer-substring-no-properties p1 p2)))
        return-string))))


(defun epics--get-filenames-in-dir (dir &optional regex)
  "Return a list of files present in DIR.  Optionally provide a
REGEX string to filter files. DIR is a string."

  (directory-files dir nil regex))


(defun epics--get-parent-record-string (del1 del2)
  "Return string between DEL1 and DEL2 pertaining to the record
if point inside record block, nil if not. DEL1 and DEL2 are strings."

  (save-excursion
    (beginning-of-line)
    (unless (epics--blank-line-p)
      (skip-chars-forward " \t")
      (cond ((epics--string-on-line-p "record") (epics--copy-string-at-word "record" del1 del2))
            ((epics--inside-record-block-p nil)
             (search-backward "record")
             (epics--copy-string-at-word "record" del1 del2))
            (t nil)))))


(defun epics--inside-record-block-p (&optional body-only)
  "Return t if point is inside a record block.  If BODY-ONLY is
set to t, the check will ignore the first line that consists of
record type and name."

  (catch 'result
    (unless body-only
      (when (epics--string-on-line-p "record")
        (throw 'result t)))
    (cond ((> (car (syntax-ppss)) 0) t)
          ((epics--string-on-line-p "{") t)
          (t nil))))


(defun epics--position-point-at-value ()
  "Try to find position of value on the line inside a record
block.  Calling this makes sense only after checking that point
is inside a record block."

  (if (epics--string-on-line-p "\"" t)
      (epics--search-forward "\"" t)

    (cond ((looking-at-p ",") (forward-char) (epics--position-point-at-value))
          ((looking-at-p " ")
           (cond ((looking-back ",") (forward-char) (epics--position-point-at-value))
                 ((looking-at ")") t)))
          ((looking-at-p ")") t))))


(defun epics--point-at-value-p ()
  "Check if point is located at the value on the line inside a
record block.  Calling this makes sense only after checking that
point is inside a record block."

  (if (epics--inside-string-p)
      t
    (save-excursion
      (backward-char 2)
      (if (looking-at-p ",")
          t
        nil))))


(defun epics--point-after-value-p ()
  "Check if point is located after a value on the line inside a
record block.  Calling this makes sense only after checking that
point is inside a record block."

  (interactive)
  (let ((current-pos (point))
        (after-value-pos nil))

    (save-excursion
      (beginning-of-line)
      (epics--search-forward ")")
      (backward-char)
      (if (>= current-pos (point))
          t
        nil))))


(defun epics--inside-string-p ()
  "Return t if point in string."

  (save-excursion
    (when (= (point) (line-beginning-position))
      (forward-char))
    (if (null (nth 3 (syntax-ppss)))
        nil
      t)))


(defun epics--inside-comment-p ()
  "Return t if point in comment."

  (save-excursion
    (when (= (point) (line-beginning-position))
      (forward-char))
    (if (null (nth 4 (syntax-ppss)))
        nil
      t)))


(defun epics--inside-comment-string-p ()
  "Return t if point in comment or string."

  (if (or (epics--inside-comment-p)
          (epics--inside-string-p))
      t
    nil))


(defun epics--check-line-contents (&optional N)
  "Check whether the Nth line in front or back is comment,
record, or blank.

Checks the line in front of point by default or if N ==
1. Negative number checks lines backwards. If N == 0 it checks
current line.

Return value is a symbol 'blank, 'record, 'comment, or nil."

  (save-excursion
    (unless (equal N 0)
      (forward-line N))
    (cond ((epics--blank-line-p) 'blank)
          ((epics--inside-record-block-p) 'record)
          ((epics--inside-comment-p) 'comment)
          (t nil))))


(defun epics--print-data-to-file (data filename)
  "Write DATA as lisp object to file FILENAME. DATA can be any
symbol or sexpression, FILENAME is a string."

  (with-temp-file filename
    (prin1 data (current-buffer))))


(defun epics--read-data-from-file (filename)
  "Read and return lisp objects from file FILENAME, which is a
string."

  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))


;; record adding/deleting
(defun epics-add-record ()
  "Insert record body near point, if point is not inside
comment."

  (when (= 0 (buffer-size))
    (insert "\n")
    (beginning-of-buffer))

  (interactive)
  (let ((check-line (epics--check-line-contents 0)))

    ;; comment handling can be done better,
    ;; but this will do for now
    (if (equal check-line 'comment)
        (message "Cannot insert record in comment")

      (when (equal check-line 'record)
        (progn
          (epics--search-forward "}")
          (insert "\n")))

      (when (< 1 (line-number-at-pos))
        (save-excursion
          (previous-line)
          (unless (epics--blank-line-p)
            (end-of-line)
            (insert "\n"))))

      (insert "record(, \"\") {\n")
      (when epics-always-include-desc
        (insert "field(DESC, \"\")\n"))
      (when epics-always-include-scan
        (insert "field(SCAN, \"\")\n"))
      (insert "}")

      (save-excursion
        (if (= (point) (point-max))
            (insert "\n\n")
          (next-line)
          (unless (epics--blank-line-p)
            (previous-line)
            (end-of-line)
            (insert "\n"))))

      (let ((p1 (point))
            (p2 (epics--search-backward "record")))
        (indent-region p2 p1))

      (epics--search-forward "("))))


(defun epics-delete-record ()
  "Remove record at point."

  (interactive)
  (if (epics--inside-record-block-p)
      (progn
        (epics--search-forward "}")

        (let ((p1 (+ 1 (point)))
              (p2 (epics--search-backward "record")))
          (kill-region p1 p2))

        (when (epics--blank-line-p)
          (cond ((and (equal (epics--check-line-contents) 'blank)
                     (equal (epics--check-line-contents -1) 'blank))
                 (kill-line 2))
                ((or (equal (epics--check-line-contents) 'blank)
                     (equal (epics--check-line-contents -1) 'blank))
                 (kill-line)))))

    (message "Point not in record!")))


;; epics reference functions
(defun epics--render-help-file (html-file)
  "Render the HTML-FILE in the help buffer if possible.
HTML-FILE is a string containing absolute path to desired html
file."

  (let ((buf-name "EPICS Reference")
        (dom))

    (if (file-readable-p html-file)
        (progn
          (with-temp-buffer
            (insert-file-contents html-file)
            (goto-char (point-min))
            (re-search-forward "^$")
            (setq dom (libxml-parse-html-region
                       (point)
                       (point-max))))
          (with-output-to-temp-buffer buf-name
            (switch-to-buffer-other-window buf-name)
            (if (null dom)
                (message "Error parsing document: %s" html-file)
              (shr-insert-document dom)
              t)))
      (message "Cannot access html-file %s" html-file)
      nil)))


(defun epics-open-reference ()
  "Prompt user to select what reference file to open, then render
it in a help buffer."

  (interactive)
  (let* ((help-dir (concat epics--actual-base-dir "html/"))
         (choice-list (epics--get-filenames-in-dir help-dir "[A-Za-z0-9-_]*\.html"))
         (choice (ido-completing-read "Choose reference to read:" choice-list)))
    (epics--render-help-file (concat help-dir choice))))


(defun epics-describe-record ()
  "Open the record reference for the record at point."

  (interactive)
  (let ((record (epics--get-parent-record-string "(" ",")))
    (if (null record)
        (message "Point not in record!")
      (epics--render-help-file (concat epics--actual-base-dir
                                       "html/"
                                       record
                                       "Record.html")))))


;; navigation functions and variables
(defvar-local epics-followed-links-history nil)


(defun epics-retrace-link ()
  "Pop from history the last record a link was followed from and
return to it."

  (interactive)
  (if (null epics-followed-links-history)
      (message "No record to return to!")
    (goto-char (point-min))
    (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" (car epics-followed-links-history))
                           nil
                           t)
    (setq epics-followed-links-history (cdr epics-followed-links-history))
    (message "Links followed history: %s" epics-followed-links-history)))


(defun epics-follow-link ()
  "Try to find a link to a record on the current line and follow
it."

  (interactive)
  (let ((link (epics--copy-string-at-word "field" "\"" "\""))
        (pos nil))

    (save-excursion
      (goto-char (point-min))
      (setq pos (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" link)
                                       nil
                                       t)))
    (if (null pos)
        (message "Not a link or record not found.")
      (unless (equal (car epics-followed-links-history)
                     (epics--get-parent-record-string "\"" "\""))
        (setq epics-followed-links-history (cons (epics--get-parent-record-string "\"" "\"")
                                                 epics-followed-links-history)))
      (goto-char pos)
      (message "Following %s" link))))


(defun epics-next-record ()
  "Find the next record block and set the point to the record
type."

  (interactive)
  (end-of-line)
  (when (epics--inside-record-block-p)
    (search-forward "}"))
  (epics--search-forward "record")
  (epics--search-forward "("))


(defun epics-previous-record ()
  "Find the previous record block and set the point to the record
type."

  (interactive)
  (beginning-of-line)
  (when (epics--inside-record-block-p)
    (if (epics--string-on-line-p "record")
        (previous-line)
      (epics--search-backward "record")
      (previous-line)))
  (epics--search-backward "record")
  (epics--search-forward "("))


(defun epics-next-value ()
  "Position the point at the next possible value."

  (interactive)
  (cond ((epics--blank-line-p) (epics--search-forward "record") (epics-next-value))
        ((epics--inside-string-p) (next-line) (beginning-of-line) (epics-next-value))
        ((epics--inside-record-block-p)
         (if (or (epics--point-after-value-p)
                 (epics--point-at-value-p))
             (progn
               (next-line)
               (beginning-of-line)
               (epics-next-value))
           (beginning-of-line)
           (epics--search-forward ",")
           (epics--position-point-at-value)))
        (t (next-line) (epics-next-value))))


(defun epics-previous-value ()
  "Position the point at the previous possible value."

  (interactive)
  (cond ((epics--blank-line-p) (epics--search-backward "}") (epics-previous-value))
        ((epics--inside-string-p) (previous-line) (end-of-line) (epics-previous-value))
        ((epics--inside-record-block-p)
         (if (epics--point-after-value-p)
             (progn
               (beginning-of-line)
               (epics--search-forward ",")
               (epics--position-point-at-value))
           (previous-line)
           (end-of-line)
           (epics-previous-value)))
        (t (previous-line) (epics-previous-value))))


;; indentation function
(defun epics--calc-indent ()
  "Calculate the depth of indentation for the current line"
  
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (let* ((depth (car (syntax-ppss)))
             (base (* epics-indent-spaces depth)))
        (unless (zerop depth)
          (setq indent base)
          (when (looking-at "\\s)")
            (setq indent (- base 4))))))
    indent))


(defun epics-indent-line ()
  "Indent the line based on brace depth"

  (let ((indent (epics--calc-indent)))
    (unless (or (null indent)
                (zerop indent))
      (unless (= indent (current-column))
        (beginning-of-line)
        (delete-horizontal-space)
        (skip-chars-forward " \t")
        (indent-to indent)))))


;; syntax table
(defvar epics-mode-syntax-table nil "Syntax table for 'epics-mode'.")

;; set # as a comment symbol
(setq epics-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; # is a comment until \n
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        synTable))


(defvar epics-mode-map nil "Keymap for epics-mode")
(progn
  (setq epics-mode-map (make-sparse-keymap))
  ;; help
  (define-key epics-mode-map (kbd "C-c h r") #'epics-describe-record)
  (define-key epics-mode-map (kbd "C-c h h") #'epics-open-reference)
  ;; navigation
  (define-key epics-mode-map (kbd "C-c C-'") #'epics-follow-link)
  (define-key epics-mode-map (kbd "C-c C-;") #'epics-retrace-link)
  (define-key epics-mode-map (kbd "C-c C-j") #'epics-next-value)
  (define-key epics-mode-map (kbd "C-c C-k") #'epics-previous-value)
  (define-key epics-mode-map (kbd "C-c C-l") #'epics-next-record)
  (define-key epics-mode-map (kbd "C-c C-h") #'epics-previous-record)
  ;; add/remove record
  (define-key epics-mode-map (kbd "C-c a") #'epics-add-record)
  (define-key epics-mode-map (kbd "C-c d") #'epics-delete-record))


(define-derived-mode epics-mode prog-mode "EPICS"
  "Major mode for editing EPICS .db and .template files."

  ;; initial setup
  (setq-local epics--actual-base-dir (epics--get-base-dir-string))

  (unless (file-accessible-directory-p epics-var-dir)
    (make-directory epics-var-dir t))

  ;; enable syntax highlighting
  (setq-local font-lock-defaults '((epics-font-lock-keywords)))

  ;; comment-dwim functionality
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; epics indentation function
  (setq-local indent-line-function #'epics-indent-line))


(provide 'epics-mode)

;;; epics-mode.el ends here
