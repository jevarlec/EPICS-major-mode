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
(defface epics-mode-face-record-in
  '((t :foreground "#dc322f"
       :weight extra-bold))
  "Face name to be used for input records"
  :group 'epics-mode)

(defface epics-mode-face-record-out
  '((t :foreground "#b58900"
       :weight extra-bold))
  "Face name to be used for output records"
  :group 'epics-mode)

(defface epics-mode-face-record-gen
  '((t :inherit font-lock-builtin-face
       :weight extra-bold))
  "Face name to be used for general records"
  :group 'epics-mode)

(defface epics-mode-face-italic
  '((t :inherit shadow
       :slant italic))
  "Face name to be used for records and fields"
  :group 'epics-mode)

;; syntax highlighting
(setq epics-font-lock-keywords
      (let* (
             ;; define categories of keywords
             (epics-italic '("record" "field"))
             (epics-links '("INPA" "INPB" "INPC" "INPD" "INPE" "INPF" "INPG" "INPH" "INPI" "INPJ" "INPK" "INPL" "OUTA" "OUTB" "OUTC" "OUTD" "OUTE" "OUTF" "OUTG" "OUTH" "OUTI" "OUTJ" "OUTK" "OUTL" "OUTM" "OUTN" "OUTO" "OUTP" "INP" "OUT" "DOL"))
             (epics-scan '("FLNK" "SCAN" "SDIS" "PHAS" "PINI" "EVNT" "LNK0" "LNK1" "LNK2" "LNK3" "LNK4" "LNK5" "LNK6" "LNK7" "LNK8" "LNK9" "LNKA" "LNKB" "LNKC" "LNKD" "LNKE" "LNKF"))
             (epics-link-params '("NMS" "NPP" "MS" "PP"))
             (epics-alarm '("NSEV" "SEVR" "STAT" "NSTA"))
             (epics-records-gen '("permissive" "subArray" "compress" "dfanout" "fanout" "event" "state" "calc" "aSub" "sub" "seq" "sel"))
             (epics-records-in '("waveform" "stringin" "mbbiDirect" "longin" "int64in" "mbbi" "ai" "bi"))
             (epics-records-out '("stringout" "mbboDirect" "longout" "calcout" "int64out" "mbbo" "ao" "bo"))

             ;; generate regex string from keyword categories
             (epics-italic-regexp (regexp-opt epics-italic 'words))
             (epics-links-regexp (regexp-opt epics-links 'words))
             (epics-scan-regexp (regexp-opt epics-scan 'words))
             (epics-link-params-regexp (regexp-opt epics-link-params 'words))
             (epics-alarm-regexp (regexp-opt epics-alarm 'words))
             (epics-records-gen-regexp (regexp-opt epics-records-gen 'words))
             (epics-records-in-regexp (regexp-opt epics-records-in 'words))
             (epics-records-out-regexp (regexp-opt epics-records-out 'words)))

        `(
          ;; apply faces to generated regex
          (,epics-italic-regexp . 'epics-mode-face-italic)
          (,epics-records-gen-regexp . 'epics-mode-face-record-gen)
          (,epics-records-in-regexp . 'epics-mode-face-record-in)
          (,epics-records-out-regexp . 'epics-mode-face-record-out)
          (,epics-links-regexp . font-lock-type-face)
          (,epics-link-params-regexp 0 font-lock-constant-face t)
          (,epics-scan-regexp . font-lock-constant-face)
          (,epics-alarm-regexp . font-lock-warning-face)

          ;; define regex for macro highlighting
          (,"$(\\([^(]+?\\))" 0 font-lock-warning-face t)
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
