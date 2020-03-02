;;; epics-mode.el --- EPICS major mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jernej Varlec

;; Author: Jernej Varlec <jernej@varlec.si>
;; Keywords: elisp, epics
;; Version: 0.1

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

;; define custom faces
(defface epics-mode-face-italic
  '((t :inherit shadow
       :slant italic))
  "Face name to be used for records and fields"
  :group 'epics-mode)

;; syntax highlighting
(setq epics-font-lock-keywords
      (let* (
             ;; define categories of keywords
             (epics-italic '("record" "field" "path" "addpath" "include" "menu" "choice" "recordtype" "device" "driver" "registrar" "function" "variable" "breaktable" "grecord" "info" "alias"))
             (epics-link-params '("NMS" "NPP" "CPP" "MS" "PP" "CA" "CP"))

             ;; generate regex string from keyword categories
             (epics-italic-regexp (regexp-opt epics-italic 'words))
             (epics-link-params-regexp (regexp-opt epics-link-params 'words)))

        `(
          ;; apply faces to generated regex
          (,epics-italic-regexp . 'epics-mode-face-italic)
          (,epics-link-params-regexp 0 font-lock-constant-face t)

          ;; define regex for menuChoices
          (,"\\s\"\\(\\.\\(?:[125] second\\)\\|1\\(?:0? second\\)\\|2 second\\|5 second\\|All\\|CHAR\\|DOUBLE\\|E\\(?:NUM\\|vent\\)\\|FLOAT\\|High Signal\\|I\\(?:/O Intr\\|NVALID\\)\\|L\\(?:ONG\\|ow Signal\\)\\|M\\(?:AJOR\\|INOR\\|ask\\|edian Signal\\)\\|NO\\(?:_ALARM\\)?\\|Passive\\|RAW\\|S\\(?:HORT\\|TRING\\|pecified\\)\\|U\\(?:CHAR\\|LONG\\|SHORT\\)\\|YES\\|asyn\\(?:Enum\\|Float64\\|Int\\(?:32\\|64\\)\\|UInt32Digital\\)\\|closed_loop\\|s\\(?:tream\\|upervisory\\)\\)\\s\"" 0 font-lock-variable-name-face t)

          ;; define regex for macro highlighting
          (,"$(\\([^ ]+?\\))" 0 font-lock-warning-face t)

          ;; define regex for i/o parameters
          (,"\\s\"@\\(asyn\\|asynMask\\)(.+?)[^)\" ]*\\|@.+\.proto" 0 font-lock-type-face t)
          )))

(defvar epics-mode-syntax-table nil "Syntax table for 'epics-mode'.")

;; set # as a comment symbol
(setq epics-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; # is a comment until \n
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        synTable))

(define-derived-mode epics-mode fundamental-mode "EPICS-mode"
  "Major mode for editing EPICS .db and .template files."

  ;; enable syntax highlighting
  (setq font-lock-defaults '((epics-font-lock-keywords)))

  ;; comment-dwim functionality
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  )

(provide 'epics-mode)

;;; epics-mode.el ends here
