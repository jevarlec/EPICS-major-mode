;;; epics-mode.el --- EPICS major mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jernej Varlec

;; Author: Jernej Varlec <jernej@varlec.si>
;; Keywords: elisp, epics
;; Version: 0.3.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Description:

;; A major mode for editing EPICS .db and .template files
;; Features syntax highlighting and (eventually) code snippets.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.db\\|.template\\|.dbd\\'" . epics-mode))

;; define custom faces
(defface epics-mode-face-shadow
  '((t :inherit shadow))
  "Face name to be used for records and fields"
  :group 'epics-mode)

;; syntax highlighting
(setq epics-font-lock-keywords
      (let* (
             ;; define categories of keywords
             (epics-shadow '("record" "field" "path" "addpath" "include" "menu" "choice" "recordtype" "device" "driver" "registrar" "function" "variable" "breaktable" "grecord" "info" "alias"))
             (epics-link-params '("MSS" "NMS" "NPP" "CPP" "MS" "PP" "CA" "CP"))

             ;; define premade regex string for keywords
             (epics-keywords-regexp "\"\\(\\.\\(?:[125] second\\)\\|1\\(?:0? second\\)\\|2 second\\|5 second\\|Al\\(?:l\\|ways\\)\\|BAD_SUB\\|C\\(?:ALC\\|HAR\\|O\\(?:MM\\|S\\)\\|ontinue normally\\)\\|D\\(?:ISABLE\\|OUBLE\\|on't drive outputs\\)\\|E\\(?:NUM\\|vent\\)\\|FLOAT\\|H\\(?:I\\(?:GH\\|HI\\)\\|WLIMIT\\|igh Signal\\)\\|I\\(?:/O Intr\\|N\\(?:T64\\|VALID\\)\\)\\|L\\(?:IN\\(?:EAR\\|K\\)\\|O\\(?:LO\\|NG\\|W\\)\\|ow Signal\\)\\|M\\(?:AJOR\\|EDIUM\\|INOR\\|ask\\|edian Signal\\)\\|NO\\(?: CONVERSION\\|_ALARM\\)?\\|On Change\\|P\\(?:AUSED?\\|assive\\)\\|R\\(?:AW\\|EAD\\(?:_ACCESS\\)?\\|UN\\(?:NING\\)?\\)\\|S\\(?:CAN\\|HORT\\|IMM\\|LOPE\\|OFT\\|T\\(?:ATE\\|RING\\)\\|et output to IVOV\\|pecified\\)\\|TIMEOUT\\|U\\(?:CHAR\\|DF\\|INT64\\|LONG\\|SHORT\\)\\|WRITE\\(?:_ACCESS\\)?\\|YES\\|asyn\\(?:Enum\\|Float\\(?:32Array\\(?:In\\|Out\\)\\|64\\(?:A\\(?:rray\\(?:In\\|Out\\)\\|verage\\)\\|TimeSeries\\)?\\)\\|Int\\(?:16Array\\(?:In\\|Out\\)\\|32\\(?:A\\(?:rray\\(?:In\\|Out\\)\\|verage\\)\\|TimeSeries\\)?\\|64\\(?:Array\\(?:In\\|Out\\)\\|TimeSeries\\)?\\|8Array\\(?:In\\|Out\\)\\)\\|Octet\\(?:CmdResponse\\|Read\\|Write\\(?:Binary\\|Read\\)?\\)\\|UInt32Digital\\)\\|closed_loop\\|s\\(?:tream\\|upervisory\\)\\)\"")

             ;; generate regex string from keyword categories
             (epics-shadow-regexp (regexp-opt epics-shadow 'words))
             (epics-link-params-regexp (regexp-opt epics-link-params 'words)))

        `(
          ;; apply faces to generated regex
          (,epics-shadow-regexp . 'epics-mode-face-shadow)
          (,epics-link-params-regexp 0 font-lock-constant-face t)
          (,epics-keywords-regexp 1 font-lock-variable-name-face t)

          ;; define regex for i/o parameters
          (,"\"\\(@\\(asyn\\|asynMask\\)\\((.+?)\\)\\|@.+?\.proto\\)[A-Za-z0-9]*?\"" 1 font-lock-type-face t)

          ;; define regex for macro highlighting
          (,"$(\\([^ ]+?\\))" 0 font-lock-warning-face t))))

(defvar epics-mode-syntax-table nil "Syntax table for 'epics-mode'.")

;; set # as a comment symbol
(setq epics-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; # is a comment until \n
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        synTable))

(define-derived-mode epics-mode fundamental-mode "EPICS"
  "Major mode for editing EPICS .db and .template files."

  ;; enable syntax highlighting
  (setq font-lock-defaults '((epics-font-lock-keywords)))

  ;; comment-dwim functionality
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(provide 'epics-mode)

;;; epics-mode.el ends here
