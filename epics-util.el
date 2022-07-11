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
  "Path where EPICS base is installed.  Run M-x epics-db after
changing this.  If set to 'env', then epics-db will try to get
path from environment variables.  Default is 'env'."
  :group 'epics-config
  :type '(choice (const :tag "From environment variables" :value "env")
                 (directory)))

(defcustom epics-var-dir
  (concat user-emacs-directory "var/epics-db/")
  "Default directory used for persistent variables."
  :group 'epics-config
  :type '(choice (const :tag "user-emacs-directory/var/epics-db"
                        :value (concat user-emacs-directory "var/epics-db/"))
                 (directory)))


;; utilities
(defun epics-util--blank-line-p ()
  "Return t if line is blank."

  (let ((result (string-match-p "^ *$" (thing-at-point 'line t))))
    (if (= result 0)
        t
      nil)))


(defun epics-util--position-at-next-blank-line ()
  "Position the point at the next blank line and return t when
  successful."

  (interactive)
  (if (or (epics-util--blank-line-p)
          (= (point) (point-max)))
      t
    (forward-line)
    (epics-util--position-at-next-blank-line)))


(defun epics-util--get-base-dir-string ()
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


(defun epics-util--string-on-line-p (string &optional inside-strings)
  "Return non-nil if STRING is present on the current line.  Also
searches strings if INSIDE-STRINGS is non-nil."

  (save-excursion
    (beginning-of-line)
    (if (string-match-p string (thing-at-point 'line t))
        (epics-util--search-forward string inside-strings)
      nil)))


(defun epics-util--regexp-on-line-p (regexp &optional inside-strings)
  ""

  (save-excursion
    (beginning-of-line)
    (if (string-match-p regexp (thing-at-point 'line t))
        (epics-util--search-with #'search-forward-regexp regexp inside-strings nil)
      nil)))


(defun epics-util--search-forward (string &optional inside-strings inside-comments)
  "Same as `search-forward', except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (epics-util--search-with #'search-forward string inside-strings inside-comments))


(defun epics-util--search-backward (string &optional inside-strings inside-comments)
  "Same as `search-backward', except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (epics-util--search-with #'search-backward string inside-strings inside-comments))


(defun epics-util--search-with (search-func string &optional inside-strings inside-comments)
  "Use this function with a wrapper! e.g. `epics-util--search-forward'

Same as desired SEARCH-FUNC, except it provides options to do
search INSIDE-STRINGS or INSIDE-COMMENTS if set to t.  By default
it does not search the comments or strings."

  (let ((point-after-search (funcall search-func string nil t nil)))

    (cond ((null point-after-search) nil)

          ((epics-util--inside-comment-p)
           (if inside-comments
               point-after-search
             (epics-util--search-with search-func string inside-strings inside-comments)))

          ((epics-util--inside-string-p)
           (if inside-strings
               point-after-search
             (epics-util--search-with search-func string inside-strings inside-comments)))

          (t point-after-search))))


(defun epics-util--copy-string-at-word (word del1 del2)
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


(defun epics-util--get-filenames-in-dir (dir &optional regex)
  "Return a list of files present in DIR.  Optionally provide a
REGEX string to filter files. DIR is a string."

  (directory-files dir nil regex))


(defun epics-util--inside-string-p ()
  "Return t if point in string."

  (save-excursion
    (when (= (point) (line-beginning-position))
      (forward-char))
    (if (null (nth 3 (syntax-ppss)))
        nil
      t)))


(defun epics-util--inside-comment-p ()
  "Return t if point in comment."

  (save-excursion
    (when (= (point) (line-beginning-position))
      (forward-char))
    (if (null (nth 4 (syntax-ppss)))
        nil
      t)))


(defun epics-util--inside-comment-string-p ()
  "Return t if point in comment or string."

  (if (or (epics-util--inside-comment-p)
          (epics-util--inside-string-p))
      t
    nil))


(defun epics-util--check-line-contents (&optional N)
  "Check whether the Nth line in front or back is comment,
record, or blank.

Checks the line in front of point by default or if N ==
1. Negative number checks lines backwards. If N == 0 it checks
current line.

Return value is a symbol 'blank, 'record, 'comment, or nil."

  (save-excursion
    (unless (equal N 0)
      (forward-line N))
    (cond ((epics-util--blank-line-p) 'blank)
          ((epics-db--inside-record-block-p) 'record)
          ((epics-util--inside-comment-p) 'comment)
          (t nil))))


(defun epics-util--print-data-to-file (data filename)
  "Write DATA as lisp object to file FILENAME. DATA can be any
symbol or sexpression, FILENAME is a string."

  (with-temp-file filename
    (prin1 data (current-buffer))))


(defun epics-util--read-data-from-file (filename)
  "Read and return lisp objects from file FILENAME, which is a
string."

  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))


(defun epics-util--render-help-file (html-file)
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


;; indentation function
(defun epics-util--calc-indent ()
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


(defun epics-util-indent-line ()
  "Indent the line based on brace depth"

  (let ((indent (epics-util--calc-indent)))
    (unless (or (null indent)
                (zerop indent))
      (unless (= indent (current-column))
        (beginning-of-line)
        (delete-horizontal-space)
        (skip-chars-forward " \t")
        (indent-to indent)))))


(provide 'epics-util)
