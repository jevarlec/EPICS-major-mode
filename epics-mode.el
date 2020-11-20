;;; epics-mode.el --- EPICS major mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jernej Varlec

;; Author: Jernej Varlec <jernej@varlec.si>
;; Keywords: elisp, epics
;; Version: 0.4.2

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

;; A major mode for editing EPICS .db and .template files
;; Features syntax highlighting and (eventually) code snippets.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.db\\|.template\\|.dbd\\'" . epics-mode))

;; epics group for customization variables and variables themself
(defgroup epics-config nil "Customization variables for EPICS mode")

(defcustom epics-indent-spaces 4
  "Setting for desired number of spaces per brace depth. Default is 4."
  :group 'epics-config)

(defcustom epics-path-to-base "/opt/epics/base/"
  "Path where EPICS base is installed."
  :group 'epics-config)

;; define custom faces
(defface epics-mode-face-shadow
  '((t :inherit shadow))
  "Face name to be used for records and fields")

;; syntax highlighting
(setq epics-font-lock-keywords
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
          (,epics-shadow-regexp . 'epics-mode-face-shadow)
          (,epics-link-params-regexp 0 font-lock-constant-face t)
          (,(format "\"%s\"" epics-keywords-regexp) 1 font-lock-variable-name-face t)

          ;; define regex for asyn i/o parameters
          (,"\"\\(@\\(asyn\\|asynMask\\)\\((.+?)\\)\\)[A-Za-z0-9_-]*?\"" 1 font-lock-type-face t)

          ;; define regex for streamdevice i/o parameters
          (,"\"\\(@.+?\.proto\\)" 1 font-lock-type-face t)

          ;; define regex for macro highlighting
          (,"$(\\([^ ]+?\\))" 0 font-lock-warning-face t))))

;; epics utility functions
(defvar-local epics-followed-links-history nil)

(defun epics--copy-thing-at-hook (hook del1 del2)
  "Yanks a thing located between DEL1 and DEL2, forward of HOOK.
All inputs should be strings, returns the thing or nil if no match."
  (let (p1 p2 string)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (not (equal (current-word) hook))
          nil
        (skip-chars-forward (concat "^" del1 "\n"))
        (forward-char)
        (setq p1 (point))
        (skip-chars-forward (concat "^" del2 " .\n"))
        (when (equal (following-char) "\"")
          (backward-char))
        (setq p2 (point))
        (save-excursion
          (beginning-of-buffer)
          (setq string (buffer-substring-no-properties p1 p2)))
        string))))

(defun epics-describe-record ()
  "Find the record name on the current line, find the associated reference page and
display it in a help buffer. Return t if successfull, nil if not."
  (interactive)
  (let ((buf-name "EPICS - record reference")
        (help-file nil)
        (record nil)
        (dom nil))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (unless (equal (current-word) "record")
        (search-backward "record"))
      (setq record (epics--copy-thing-at-hook "record"
                                              "("
                                              ","))
      (if (null record)
          (progn
            (message "No record found!")
            nil)
        (with-output-to-temp-buffer buf-name
          (setq help-file (concat epics-path-to-base
                                  "html/"
                                  record
                                  "Record.html"))
          (switch-to-buffer-other-window buf-name)
          (if (file-readable-p help-file)
              (progn
                (with-temp-buffer
                  (insert-file-contents help-file)
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (delete-region (point) (point-min))
                  (setq dom (libxml-parse-html-region
                             (point-min)
                             (point-max))))
                (if (null dom)
                    (message "Error parsing document: %s" help-file)
                  (message "Opening %s record reference" record)
                  (shr-insert-document dom)
                  t))
            (message "Cannot access file %s" help-file)
            nil))))))

(defun epics-retrace-link ()
  "Pop from history the last record a link was followed from and return to it"
  (interactive)
  (if (null epics-followed-links-history)
      (message "No record to return to!")
    (beginning-of-buffer)
    (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" (car epics-followed-links-history))
                           nil
                           t)
    (setq epics-followed-links-history (cdr epics-followed-links-history))
    (message "Links followed history: %s" epics-followed-links-history)))

(defun epics-follow-link ()
  "Try to find a link to a record on the current line and follow it"
  (interactive)

  (defun epics--get-parent-record-name ()
    (save-excursion
      (search-backward "record")
      (epics--copy-thing-at-hook "record"
                                 "\""
                                 "\"")))

  (let ((link (epics--copy-thing-at-hook "field"
                                         "\""
                                         "\""))
        (pos nil))
    (save-excursion
      (beginning-of-buffer)
      (setq pos (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" link)
                                       nil
                                       t)))
    (if (null pos)
        (message "Not a link or record not found.")
      (unless (equal (car epics-followed-links-history)
                     (epics--get-parent-record-name))
        (setq epics-followed-links-history (cons (epics--get-parent-record-name)
                                                 epics-followed-links-history)))
      (goto-char pos)
      (message "Following %s" link))))

;; indentation function
(defun epics-indent-line ()
  "Indent the line based on brace depth"

  (defun epics-calc-indent ()
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

  (let ((indent (epics-calc-indent)))
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

(defun epics-inside-comment-string-p ()
  "Return non-nil if inside comment or string"
  (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))

(defvar epics-mode-map nil "Keymap for epics-mode")
(progn
  (setq epics-mode-map (make-sparse-keymap))
  (define-key epics-mode-map (kbd "C-c C-'") #'epics-follow-link)
  (define-key epics-mode-map (kbd "C-c C-;") #'epics-retrace-link)
  (define-key epics-mode-map (kbd "C-c r") #'epics-describe-record))

(define-derived-mode epics-mode prog-mode "EPICS"
  "Major mode for editing EPICS .db and .template files."

  ;; enable syntax highlighting
  (setq font-lock-defaults '((epics-font-lock-keywords)))

  ;; comment-dwim functionality
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; epics indentation function
  (setq-local indent-line-function #'epics-indent-line))

(provide 'epics-mode)

;;; epics-mode.el ends here
