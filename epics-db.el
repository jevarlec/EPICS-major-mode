;;; epics-db.el --- EPICS major mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jernej Varlec

;; Author: Jernej Varlec <jernej@varlec.si>
;; Keywords: elisp, epics
;; Version: 0.7.0

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
(require 'epics-util)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.db\\|.template\\|.dbd\\'" . epics-db-mode))

;; epics-db customization vars
(defcustom epics-db-always-include-desc t
  "If set to non-nil, always add a DESC field when expanding a
  snippet. It is set to t by default."
  :group 'epics-config
  :type '(choice (const :tag "Yes"
                        :value t)
                 (const :tag "No"
                        :value nil)))

(defcustom epics-db-always-include-scan t
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

;; syntax highlighting and keywords
(defvar-local epics-db-link-param-keywords '("MSS" "NMS" "NPP" "CPP" "MS" "PP" "CA" "CP"))

(defvar-local epics-db-type-keywords '("record" "field" "path" "addpath" "include" "menu" "choice" "recordtype" "device" "driver" "registrar" "function" "variable" "breaktable" "grecord" "info" "alias"))

(defvar epics-db-general-keywords '("All" "Specified" "Mask" "High Signal" "Low Signal" "Median Signal" "YES" "NO" "RUN" "RUNNING" "PAUSE" "PAUSED" "RAW" "Passive" "Event" "I/O Intr" "10 second" "5 second" "2 second" "1 second" ".5 second" ".2 second" ".1 second" "supervisory" "closed_loop" "STRING" "CHAR" "UCHAR" "SHORT" "USHORT" "LONG" "ULONG" "FLOAT" "DOUBLE" "INT64" "UINT64" "ENUM" "NO_ALARM" "MINOR" "MAJOR" "INVALID" "stream" "asynInt32" "asynInt32Average" "asynInt8ArrayIn" "asynInt8ArrayOut" "asynInt16ArrayIn" "asynInt16ArrayOut" "asynInt32ArrayIn" "asynInt32ArrayOut" "asynInt64ArrayIn" "asynInt64ArrayOut" "asynInt32TimeSeries" "asynInt64" "asynInt64TimeSeries" "asynUInt32Digital" "asynFloat64" "asynFloat64Average" "asynFloat32ArrayIn" "asynFloat32ArrayOut" "asynFloat64ArrayIn" "asynFloat64ArrayOut" "asynFloat64TimeSeries" "asynEnum" "asynOctetRead" "asynOctetWrite" "asynOctetWriteRead" "asynOctetWriteBinary" "asynOctetCmdResponse" "READ" "WRITE" "HIHI" "HIGH" "LOLO" "LOW" "STATE" "COS" "COMM" "TIMEOUT" "HWLIMIT" "CALC" "SCAN" "LINK" "SOFT" "BAD_SUB" "UDF" "DISABLE" "SIMM" "READ_ACCESS" "WRITE_ACCESS" "NO CONVERSION" "SLOPE" "LINEAR" "Continue normally" "Don't drive outputs" "Set output to IVOV" "On Change" "Always" "MEDIUM"))

(defvar-local epics-db-field-keywords '("NAME" "DESC" "SCAN" "PINI" "PHAS" "EVNT" "PRIO" "DISV" "DISA" "DISS" "LSET" "LCNT" "PACT" "FLNK" "SPVT" "STAT" "SEVR" "ACKS" "ACKT" "UDF" "RSET" "DSET" "DPVT" "TPRO" "BKPT" "ASG" "ASP" "DISP" "DTYP" "MLOK" "MLIS" "PPN" "PPNR" "PUTF" "RDES" "RPRO" "TIME" "TSE" "TSEL" "EGU" "INP"  "RVAL" "VAL"  "SIMM" "SIML" "SVAL" "SIOL" "SIMS" "SDLY" "SSCN" "OUT" "OVAL" "RBV" "DOL" "OMSL" "IVOA" "IVOV" "ROFF" "ASLO" "AOFF" "LINR" "ESLO" "EOFF" "EGUL" "EGUF" "SMOO" "PREC" "HOPR" "LOPR" "DRVH" "DRVL" "HIHI" "LOLO" "HIGH" "LOW" "HHSV" "HSV" "LLSV" "LSV" "HYST" "LALM" "AFTC" "ADEL" "MDEL" "ALST" "MLST" "ORAW" "OIF" "PVAL" "OROC" "ORBV" "INIT" "PBRK" "LBRK" "OMOD" "SDIS" "PROC" "NSTA" "NSEV" "UDFS" "VAL" "INP" "NELM" "FTVL" "NORD" "BPTR" "MPST" "APST" "HASH" "AFVL" "INAM" "LFLG" "SUBL" "SNAM" "ONAM" "SADR" "CADR" "BRSV" "EFLG" "INPA" "INPB" "INPC" "INPD" "INPE" "INPF" "INPG" "INPH" "INPI" "INPJ" "INPK" "INPL" "INPM" "INPN" "INPO" "INPP" "INPQ" "INPR" "INPS" "INPT" "INPU" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "FTA" "FTB" "FTC" "FTD" "FTE" "FTF" "FTG" "FTH" "FTI" "FTJ" "FTK" "FTL" "FTM" "FTN" "FTO" "FTP" "FTQ" "FTR" "FTS" "FTT" "FTU" "NOA" "NOB" "NOC" "NOD" "NOE" "NOF" "NOG" "NOH" "NOI" "NOJ" "NOK" "NOL" "NOM" "NON" "NOO" "NOP" "NOQ" "NOR" "NOS" "NOT" "NOU" "NEA" "NEB" "NEC" "NED" "NEE" "NEF" "NEG" "NEH" "NEI" "NEJ" "NEK" "NEL" "NEM" "NEN" "NEO" "NEP" "NEQ" "NER" "NES" "NET" "NEU" "OUTA" "OUTB" "OUTC" "OUTD" "OUTE" "OUTF" "OUTG" "OUTH" "OUTI" "OUTJ" "OUTK" "OUTL" "OUTM" "OUTN" "OUTO" "OUTP" "OUTQ" "OUTR" "OUTS" "OUTT" "OUTU" "VALA" "VALB" "VALC" "VALD" "VALE" "VALF" "VALG" "VALH" "VALI" "VALJ" "VALK" "VALL" "VALM" "VALN" "VALO" "VALP" "VALQ" "VALR" "VALS" "VALT" "VALU" "OVLA" "OVLB" "OVLC" "OVLD" "OVLE" "OVLF" "OVLG" "OVLH" "OVLI" "OVLJ" "OVLK" "OVLL" "OVLM" "OVLN" "OVLO" "OVLP" "OVLQ" "OVLR" "OVLS" "OVLT" "OVLU" "FTVA" "FTVB" "FTVC" "FTVD" "FTVE" "FTVF" "FTVG" "FTVH" "FTVI" "FTVJ" "FTVK" "FTVM" "FTVN" "FTVO" "FTVP" "FTVQ" "FTVR" "FTVS" "FTVT" "FTVU" "NOVA" "NOVB" "NOVC" "NOVD" "NOVE" "NOVF" "NOVG" "NOVH" "NOVI" "NOVJ" "NOVK" "NOVL" "NOVM" "NOVN" "NOVO" "NOVP" "NOVQ" "NOVR" "NOVS" "NOVT" "NOVU" "NEVA" "NEVB" "NEVC" "NEVD" "NEVE" "NEVF" "NEVG" "NEVH" "NEVI" "NEVJ" "NEVK" "NEVL" "NEVM" "NEVN" "NEVO" "NEVP" "NEVQ" "NEVR" "NEVS" "NEVT" "NEVU" "ONVA" "ONVB" "ONVC" "ONVD" "ONVE" "ONVF" "ONVG" "ONVH" "ONVI" "ONVJ" "ONVK" "ONVL" "ONVM" "ONVN" "ONVO" "ONVP" "ONVQ" "ONVR" "ONVS" "ONVT" "ONVU" "ZSV" "OSV" "COSV" "ZNAM" "MASK" "RPVT" "WDPT" "CALC" "CLCV" "INAV" "INBV" "INCV" "INDV" "INEV" "INFV" "INGV" "INHV" "INIV" "INJV" "INKV" "INLV" "OUTV" "OOPT" "ODLY" "DLYA" "DOPT" "OCAL" "OCLV" "OEVT" "EPVT" "LA" "LB" "LC" "LD" "LE" "LF" "LG" "LH" "LI" "LJ" "LK" "LL" "POVL" "RPCL" "ORPC" "RES" "ALG" "NSAM" "IHIL" "ILIL" "OFF" "NUSE" "OUSE" "SPTR" "WPTR" "CVB" "INX" "SELM" "SELN" "SELL" "OFFS" "SHFT" "LNK0" "LNK1" "LNK2" "LNK3" "LNK4" "LNK5" "LNK6" "LNK7" "LNK8" "LNK9" "LNKA" "LNKB" "LNKC" "LNKD" "LNKE" "LNKF" "CSTA" "CMD" "ULIM" "LLIM" "WDTH" "SGNL" "SVL" "WDOG" "MCNT" "SDEL" "SIZV" "LEN" "OLEN" "NOBT" "B0" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9" "BA" "BB" "BC" "BD" "BE" "BF" "ZRVL" "TWVL" "THVL" "FRVL" "FVVL" "SXVL" "SVVL" "EIVL" "NIVL" "TEVL" "ELVL" "TVVL" "TTVL" "FFVL" "ZRST" "ONST" "TWST" "THST" "FRST" "FVST" "SXST" "SVST" "EIST" "NIST" "TEST" "ELST" "TVST" "TTST" "FTST" "FFST" "ZRSV" "ONSV" "TWSV" "THSV" "FRSV" "FVSV" "SXSV" "SVSV" "EISV" "NISV" "TESV" "ELSV" "TVSV" "TTSV" "FTSV" "FFSV" "UNSV" "SDEF" "LABL" "WFLG" "OFLG" "FMT" "IVLS" "INP0" "INP1" "INP2" "INP3" "INP4" "INP5" "INP6" "INP7" "INP8" "INP9" "NVL" "NLST" "OLDN" "DLY0" "DOL0" "DO0" "DLY1" "DOL1" "DO1" "DLY2" "DOL2" "DO2" "DLY3" "DOL3" "DO3" "DLY4" "DOL4" "DO4" "DLY5" "DOL5" "DO5" "DLY6" "DOL6" "DO6" "DLY7" "DOL7" "DO7" "DLY8" "DOL8" "DO8" "DLY9" "DOL9" "DO9" "DOLA" "DOA" "DLYB" "DOLB" "DOB" "DLYC" "DOLC" "DOC" "DLYD" "DOLD" "DOD" "DLYE" "DOLE" "DOE" "DLYF" "DOLF" "DOF" "MALM" "INDX" "BUSY" "RARM" "VERS" "PRE" "OPRE" "SM" "OSM" "AR" "OAR" "AFT" "OAFT" "PPV" "RPV" "DPV" "TPV" "OPPV" "ORPV" "ODPV" "OTPV" "SP" "OSP" "EP" "OEP" "NP" "ONP" "SC" "OSC" "AQT" "OAQT" "MP" "IMP" "ACT" "IACT" "LOAD" "OLOAD" "GO" "OGO" "STEP" "LSTP" "SMSG" "CMND" "ALRT" "MPTS" "EXSC" "XSC" "PXSC" "KILL" "WAIT" "WCNT" "AWCT" "WTNG" "AWAIT" "AAWAIT" "DATA" "REFD" "NPTS" "FPTS" "FFO" "CPT" "BCPT" "DPT" "PCPT" "PASM" "TOLP" "TLAP" "ATIME"))

(defvar-local epics-db-record-keywords '("aai" "aao" "ai" "ao" "aSub" "bi" "bo" "calc" "calcout" "compress" "dfanout" "event" "fanout" "histogram" "longin" "longout" "int64in" "int64out" "mbbi" "mbbiDirect" "mbbo" "mbboDirect" "permissive" "printf" "sel" "seq" "state" "stringin" "stringout" "lsi" "lso" "subArray" "sub" "waveform"))

(defvar-local epics-db-font-lock-keywords
      (let ((epics-keywords-regexp (regexp-opt epics-db-general-keywords t))
            (epics-shadow-regexp (regexp-opt epics-db-type-keywords 'words))
            (epics-link-params-regexp (regexp-opt epics-db-link-param-keywords 'words)))

        `((,epics-shadow-regexp . 'epics-face-shadow)
          (,epics-link-params-regexp 0 'epics-face-link-param t)
          (,(format "\"%s\"" epics-keywords-regexp) 1 font-lock-keyword-face t)

          ;; define regex for asyn i/o parameters
          (,"\"\\(@\\(asyn\\|asynMask\\)\\((.+?)\\)\\).*?\"" 1 font-lock-type-face t)

          ;; define regex for streamdevice i/o parameters
          (,"\"\\(@.+?\.proto\\)" 1 font-lock-type-face t)

          ;; define regex for macro highlighting
          (,"$(\\([^ ]+?\\))" 0 font-lock-variable-name-face t))))


;; utility functions and variables
(defvar-local epics--actual-base-dir nil
  "Internal var that holds path to epics base.  This is the
  variable that is actually used internally, not
  epics-path-to-base.")


;; database utilities
(defun epics-db--get-parent-record-string (del1 del2)
  "Return string between DEL1 and DEL2 pertaining to the record
if point inside record block, nil if not. DEL1 and DEL2 are strings."

  (save-excursion
    (beginning-of-line)
    (unless (epics-util--blank-line-p)
      (skip-chars-forward " \t")
      (cond ((epics-util--string-on-line-p "record") (epics-util--copy-string-at-word "record" del1 del2))
            ((epics-db--inside-record-block-p nil)
             (search-backward "record")
             (epics-util--copy-string-at-word "record" del1 del2))
            (t nil)))))


(defun epics-db--inside-record-block-p (&optional body-only)
  "Return t if point is inside a record block.  If BODY-ONLY is
set to t, the check will ignore the first line that consists of
record type and name."

  (catch 'result
    (unless body-only
      (when (epics-util--string-on-line-p "record")
        (throw 'result t)))
    (cond ((> (car (syntax-ppss)) 0) t)
          ((epics-util--string-on-line-p "{") t)
          (t nil))))


(defun epics-db--position-point-at-value ()
  "Try to find position of value on the line inside a record
block.  Calling this makes sense only after checking that point
is inside a record block."

  (if (epics-util--string-on-line-p "\"" t)
      (epics-util--search-forward "\"" t)

    (cond ((looking-at-p ",") (forward-char) (epics-db--position-point-at-value))
          ((looking-at-p " ")
           (cond ((looking-back ",") (forward-char) (epics-db--position-point-at-value))
                 ((looking-at ")") t)))
          ((looking-at-p ")") t))))


(defun epics-db--point-at-value-p ()
  "Check if point is located at the value on the line inside a
record block.  Calling this makes sense only after checking that
point is inside a record block."

  (if (epics-util--inside-string-p)
      t
    (save-excursion
      (backward-char 2)
      (if (looking-at-p ",")
          t
        nil))))


(defun epics-db--point-after-value-p ()
  "Check if point is located after a value on the line inside a
record block.  Calling this makes sense only after checking that
point is inside a record block."

  (interactive)
  (let ((current-pos (point))
        (after-value-pos nil))

    (save-excursion
      (beginning-of-line)
      (epics-util--search-forward ")")
      (backward-char)
      (if (>= current-pos (point))
          t
        nil))))


;; record adding/deleting
(defun epics-db-add-record ()
  "Insert record body near point, if point is not inside
comment."

  (when (= 0 (buffer-size))
    (insert "\n")
    (beginning-of-buffer))

  (interactive)
  (let ((check-line (epics-util--check-line-contents 0)))

    ;; comment handling can be done better,
    ;; but this will do for now
    (if (equal check-line 'comment)
        (message "Cannot insert record in comment")

      (when (equal check-line 'record)
        (progn
          (epics-util--search-forward "}")
          (insert "\n")))

      (when (< 1 (line-number-at-pos))
        (save-excursion
          (previous-line)
          (unless (epics-util--blank-line-p)
            (end-of-line)
            (insert "\n"))))

      (insert "record(, \"\") {\n")
      (when epics-db-always-include-desc
        (insert "field(DESC, \"\")\n"))
      (when epics-db-always-include-scan
        (insert "field(SCAN, \"\")\n"))
      (insert "}")

      (save-excursion
        (if (= (point) (point-max))
            (insert "\n\n")
          (next-line)
          (unless (epics-util--blank-line-p)
            (previous-line)
            (end-of-line)
            (insert "\n"))))

      (let ((p1 (point))
            (p2 (epics-util--search-backward "record")))
        (indent-region p2 p1))

      (epics-util--search-forward "("))))


(defun epics-db-delete-record ()
  "Remove record at point."

  (interactive)
  (if (epics-db--inside-record-block-p)
      (progn
        (epics-util--search-forward "}")

        (let ((p1 (+ 1 (point)))
              (p2 (epics-util--search-backward "record")))
          (kill-region p1 p2))

        (when (epics-util--blank-line-p)
          (cond ((and (equal (epics-util--check-line-contents) 'blank)
                     (equal (epics-util--check-line-contents -1) 'blank))
                 (kill-line 2))
                ((or (equal (epics-util--check-line-contents) 'blank)
                     (equal (epics-util--check-line-contents -1) 'blank))
                 (kill-line)))))

    (message "Point not in record!")))


;; epics reference functions
(defun epics-db-open-reference ()
  "Prompt user to select what reference file to open, then render
it in a help buffer."

  (interactive)
  (let* ((help-dir (concat epics--actual-base-dir "html/"))
         (choice-list (epics-util--get-filenames-in-dir help-dir "[A-Za-z0-9-_]*\.html"))
         (choice (ido-completing-read "Choose reference to read:" choice-list)))
    (epics-util--render-help-file (concat help-dir choice))))


(defun epics-db-describe-record ()
  "Open the record reference for the record at point."

  (interactive)
  (let ((record (epics-db--get-parent-record-string "(" ",")))
    (if (null record)
        (message "Point not in record!")
      (epics-util--render-help-file (concat epics--actual-base-dir
                                       "html/"
                                       record
                                       "Record.html")))))


;; navigation functions and variables
(defvar-local epics-db-followed-links-history nil)


(defun epics-db-retrace-link ()
  "Pop from history the last record a link was followed from and
return to it."

  (interactive)
  (if (null epics-db-followed-links-history)
      (message "No record to return to!")
    (goto-char (point-min))
    (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" (car epics-db-followed-links-history))
                           nil
                           t)
    (setq epics-db-followed-links-history (cdr epics-db-followed-links-history))
    (message "Links followed history: %s" epics-db-followed-links-history)))


(defun epics-db-follow-link ()
  "Try to find a link to a record on the current line and follow
it."

  (interactive)
  (let ((link (epics-util--copy-string-at-word "field" "\"" "\""))
        (pos nil))

    (save-excursion
      (goto-char (point-min))
      (setq pos (search-forward-regexp (format "record.+?\\([a-z ]+?\"%s\"\\)" link)
                                       nil
                                       t)))
    (if (null pos)
        (message "Not a link or record not found.")
      (unless (equal (car epics-db-followed-links-history)
                     (epics-db--get-parent-record-string "\"" "\""))
        (setq epics-db-followed-links-history (cons (epics-db--get-parent-record-string "\"" "\"")
                                                 epics-db-followed-links-history)))
      (goto-char pos)
      (message "Following %s" link))))


(defun epics-db-next-record ()
  "Find the next record block and set the point to the record
type."

  (interactive)
  (end-of-line)
  (when (epics-db--inside-record-block-p)
    (search-forward "}"))
  (epics-util--search-forward "record")
  (epics-util--search-forward "("))


(defun epics-db-previous-record ()
  "Find the previous record block and set the point to the record
type."

  (interactive)
  (beginning-of-line)
  (when (epics-db--inside-record-block-p)
    (if (epics-util--string-on-line-p "record")
        (previous-line)
      (epics-util--search-backward "record")
      (previous-line)))
  (epics-util--search-backward "record")
  (epics-util--search-forward "("))


(defun epics-db-next-value ()
  "Position the point at the next possible value."

  (interactive)
  (cond ((epics-util--blank-line-p) (epics-util--search-forward "record") (epics-db-next-value))
        ((epics-util--inside-string-p) (next-line) (beginning-of-line) (epics-db-next-value))
        ((epics-db--inside-record-block-p)
         (if (or (epics-db--point-after-value-p)
                 (epics-db--point-at-value-p))
             (progn
               (next-line)
               (beginning-of-line)
               (epics-db-next-value))
           (beginning-of-line)
           (epics-util--search-forward ",")
           (epics-db--position-point-at-value)))
        (t (next-line) (epics-db-next-value))))


(defun epics-db-previous-value ()
  "Position the point at the previous possible value."

  (interactive)
  (cond ((epics-util--blank-line-p) (epics-util--search-backward "}") (epics-db-previous-value))
        ((epics-util--inside-string-p) (previous-line) (end-of-line) (epics-db-previous-value))
        ((epics-db--inside-record-block-p)
         (if (epics-db--point-after-value-p)
             (progn
               (beginning-of-line)
               (epics-util--search-forward ",")
               (epics-db--position-point-at-value))
           (previous-line)
           (end-of-line)
           (epics-db-previous-value)))
        (t (previous-line) (epics-db-previous-value))))


;; syntax table
(defvar epics-db-syntax-table nil "Syntax table for 'epics-db'.")

;; set # as a comment symbol
(setq epics-db-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; # is a comment until \n
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        synTable))


(defvar epics-db-mode-map nil "Keymap for epics-db")
(progn
  (setq epics-db-mode-map (make-sparse-keymap))
  ;; help
  (define-key epics-db-mode-map (kbd "C-c h r") #'epics-db-describe-record)
  (define-key epics-db-mode-map (kbd "C-c h h") #'epics-db-open-reference)
  ;; navigation
  (define-key epics-db-mode-map (kbd "C-c C-'") #'epics-db-follow-link)
  (define-key epics-db-mode-map (kbd "C-c C-;") #'epics-db-retrace-link)
  (define-key epics-db-mode-map (kbd "C-c C-j") #'epics-db-next-value)
  (define-key epics-db-mode-map (kbd "C-c C-k") #'epics-db-previous-value)
  (define-key epics-db-mode-map (kbd "C-c C-l") #'epics-db-next-record)
  (define-key epics-db-mode-map (kbd "C-c C-h") #'epics-db-previous-record)
  ;; add/remove record
  (define-key epics-db-mode-map (kbd "C-c a") #'epics-db-add-record)
  (define-key epics-db-mode-map (kbd "C-c d") #'epics-db-delete-record))


(define-derived-mode epics-db-mode prog-mode "EPICS-DB"
  "Major mode for editing EPICS .db and .template files."

  ;; initial setup
  (setq-local epics--actual-base-dir (epics-util--get-base-dir-string))

  (unless (file-accessible-directory-p epics-var-dir)
    (make-directory epics-var-dir t))

  ;;imenu
  (setq-local imenu-generic-expression
              '(("Record" "^record\\s-*(\\(.+\\))" 1)
                ("Alias" "^\\s-*alias(\\(.+\\))" 1)
                ("Info" "^\\s-*info(\\(.+\\))" 1)))

  ;; enable syntax highlighting
  (setq-local font-lock-defaults '((epics-font-lock-keywords)))

  ;; comment-dwim functionality
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; epics indentation function
  (setq-local indent-line-function #'epics-util-indent-line))


(provide 'epics-db)

;;; epics-db.el ends here
