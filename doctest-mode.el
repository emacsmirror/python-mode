;;; doctest-mode.el --- Major mode for editing Python doctest files

;; Copyright (C) 2004-2007  Edward Loper

;; Author:     Edward Loper
;; Maintainer: edloper@alum.mit.edu
;; Created:    Aug 2004
;; Keywords:   python doctest unittest test docstring

(defconst doctest-version "0.4"
  "`doctest-mode' version number.")

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;; This is a major mode for editing text files that contain Python
;; doctest examples.  Doctest is a testing framework for Python that
;; emulates an interactive session, and checks the result of each
;; command.  For more information, see the Python library reference:
;; <http://docs.python.org/lib/module-doctest.html>

;; Known bugs:
;; - Some places assume prompts are 4 chars (but they can be 3
;;   if they're bare).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup doctest nil
  "Support for the Python doctest framework"
  :group 'languages
  :prefix "doctest-")

(defcustom doctest-default-margin 4
  "The default pre-prompt margin for doctest examples."
  :type 'integer
  :group 'doctest)

(defcustom doctest-avoid-trailing-whitespace t
  "If true, then delete trailing whitespace when inserting a newline."
  :type 'boolean
  :group 'doctest)

(defcustom doctest-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok "/var/tmp")
	(funcall ok  ".")
	(error (concat "Couldn't find a usable temp directory -- "
		       "set `doctest-temp-directory'"))))	 
  "Directory used for temporary files created when running doctest.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'doctest)

(defcustom hide-example-source nil
  "If true, then don't display the example source code for each 
failure in the results buffer."
  :type 'boolean
  :group 'doctest)

(defcustom doctest-python-command "python"
  "Shell command used to start the python interpreter"
  :type 'string
  :group 'doctest)

(defcustom doctest-results-buffer-name "*doctest-output*"
  "The name of the buffer used to store the output of the doctest
command."
  :type 'string
  :group 'doctest)

(defcustom doctest-optionflags '()
  "Option flags for doctest"
  :group 'doctest
  :type '(repeat (choice (const :tag "Select an option..." "")
                         (const :tag "Normalize whitespace"
                                "NORMALIZE_WHITESPACE")
                         (const :tag "Ellipsis"
                                "ELLIPSIS")
                         (const :tag "Don't accept True for 1"
                                "DONT_ACCEPT_TRUE_FOR_1")
                         (const :tag "Don't accept <BLANKLINE>"
                                "DONT_ACCEPT_BLANKLINE")
                         (const :tag "Ignore Exception detail"
                                "IGNORE_EXCEPTION_DETAIL")
                         (const :tag "Report only first failure"
                                "REPORT_ONLY_FIRST_FAILURE")
                         )))

(defcustom doctest-async t
  "If true, then doctest will be run asynchronously."
  :type 'boolean
  :group 'doctest)

(defcustom doctest-trim-exceptions t
  "If true, then any exceptions inserted by doctest-replace-output
will have the stack trace lines trimmed."
  :type 'boolean
  :group 'doctest)

(defcustom doctest-highlight-strings t
  "If true, then highlight strings.  If you find that doctest-mode
is responding slowly when you type, turning this off might help."
  :type 'boolean
  :group 'doctest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface doctest-prompt-face
  '((((class color) (background dark))
     (:foreground "#68f"))
    (t (:foreground "#226")))
  "Face for Python prompts in doctest examples."
  :group 'doctest)

(defface doctest-output-face
  '((((class color) (background dark))
     (:foreground "#afd"))
    (t (:foreground "#262")))
  "Face for the output of doctest examples."
  :group 'doctest)

(defface doctest-output-marker-face
  '((((class color) (background dark))
     (:foreground "#0f0"))
    (t (:foreground "#080")))
  "Face for markers in the output of doctest examples."
  :group 'doctest)

(defface doctest-output-traceback-face
  '((((class color) (background dark))
     (:foreground "#f88"))
    (t (:foreground "#622")))
  "Face for traceback headers in the output of doctest examples."
  :group 'doctest)

(defface doctest-results-divider-face
  '((((class color) (background dark))
     (:foreground "#08f"))
    (t (:foreground "#00f")))
  "Face for dividers in the doctest results window."
  :group 'doctest)

(defface doctest-results-loc-face
  '((((class color) (background dark))
     (:foreground "#0f8"))
    (t (:foreground "#084")))
  "Face for location headers in the doctest results window."
  :group 'doctest)

(defface doctest-results-header-face
  '((((class color) (background dark))
     (:foreground "#8ff"))
    (t (:foreground "#088")))
  "Face for sub-headers in the doctest results window."
  :group 'doctest)

(defface doctest-results-selection-face
  '((((class color) (background dark))
     (:foreground "#ff0" :background "#008"))
    (t (:background "#088" :foreground "#fff")))
  "Face for selected failure's location header in the results window."
  :group 'doctest)

(defface doctest-selection-face
  '((((class color) (background dark))
     (:foreground "#ff0" :background "#00f" :bold t))
    (t (:foreground "#f00")))
  "Face for selected example's prompt"
  :group 'doctest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doctest-prompt-re
  "^\\(?:\\([ \t]*\\)\\(>>> ?\\|[.][.][.] ?\\)\\([ \t]*\\)\\)"
  "Regular expression for doctest prompts.  It defines three groups:
the pre-prompt margin; the prompt; and the post-prompt indentation.")

(defconst doctest-open-block-re
  "[^\n]+:[ \t]*\\(#.*\\)?$"
  "Regular expression for a line that opens a block")

(defconst doctest-close-block-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)\\b"
  "Regular expression for a line that closes a block")

(defconst doctest-example-source-re 
  "^Failed example:\n\\(\n\\|    [^\n]*\n\\)+")

(defconst doctest-results-divider-re
  "^\\([*]\\{60,\\}\\)$")

(defconst doctest-results-loc-re
  "^File \"[^\"]+\", line \\([0-9]+\\), in [^\n]+")

(defconst doctest-pre-py24-results-loc-re
  "^from line #\\([0-9]+\\) of [^\n]*")

(defconst doctest-results-header-re
  "^\\([^ \n\t].+:\\|Expected nothing\\|Got nothing\\)$")

(defconst doctest-traceback-header-re
  "^[ \t]*Traceback (\\(most recent call last\\|innermost last\\)):")

(defconst doctest-pre-py24-results-re
  "^from line #"
  "Regular expression that will match in the results buffer if we're
using an old (pre-python2.4) version of doctest.")

;; nb: There's a bug in Python's traceback.print_exception function
;; which causes SyntaxError exceptions to be displayed incorrectly;
;; which prevents this regexp from matching.  But there shouldn't be
;; too many people testing for SyntaxErrors, so I won't worry about
;; it.
(defconst doctest-traceback-re
  (let ((nonprompt
         ;; This matches any non-blank line that doesn't start with
         ;; a prompt (... or >>>).
         (concat 
          "\\(?:[.][.][^.\n]\\|[>][>][^>\n]\\|"
          "[.][^.\n]\\|[>][^>\n]\\|[^.>\n \t]\\)[^\n]*")))
    (concat
     "^\\(\\([ \t]*\\)Traceback "
     "(\\(?:most recent call last\\|innermost last\\)):\n\\)"
     "\\(?:\\2[ \t]+[^ \t\n][^\n]*\n\\)*"
     "\\(\\(?:\\2" nonprompt "\n\\)"
        "\\(?:\\2[ \t]*" nonprompt "\n\\)*\\)"))
  "Regular expression that matches a complete exception traceback.
It contains three groups: group 1 is the header line; group 2 is
the indentation; and group 3 is the exception message.")
  
(defconst doctest-blankline-re
  "^[ \t]*<BLANKLINE>"
  "Regular expression that matches blank line markers in doctest
output.")

(defconst doctest-outdent-re
  (concat "\\(" (mapconcat 'identity
			   '("else:"
			     "except\\(\\s +.*\\)?:"
			     "finally:"
			     "elif\\s +.*:")
			   "\\|")
	  "\\)")
  "Regular expression for a line that should be outdented.  Any line
that matches `doctest-outdent-re', but does not follow a line matching
`doctest-no-outdent-re', will be outdented.")

;; It's not clear to me *why* the behavior given by this definition of
;; doctest-no-outdent-re is desirable; but it's basically just copied
;; from python-mode.
(defconst doctest-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
	      (list "try:"
		    "except\\(\\s +.*\\)?:"
		    "while\\s +.*:"
		    "for\\s +.*:"
		    "if\\s +.*:"
		    "elif\\s +.*:"
                    "\\(return\\|raise\\|break\\|continue\\|pass\\)[ \t\n]"
		    )
	      "\\|")
	  "\\)")
  "Regular expression matching lines not to outdent after.  Any line
that matches `doctest-outdent-re', but does not follow a line matching
`doctest-no-outdent-re', will be outdented.")

(defconst doctest-script
  (concat "from doctest import *\n"
          "import sys\n"
          "doc = open('%s').read()\n"
          "if sys.version_info[:2] >= (2,4):\n"
          "    test = DocTestParser().get_doctest(doc, {}, '%s', '%s', 0)\n"
          "    r = DocTestRunner(optionflags=%s)\n"
          "    r.run(test)\n"
          "else:\n"
          "    Tester(globs={}).runstring(doc, '%s')\n"
          "print\n";; <- so the buffer won't be empty
          )
  "Python script used to run doctest.  It takes the following arguments,
supplied by `format': TEMPFILE, BUFFER-NAME, BUFFER-FILE-NAME, FLAGS,
BUFFER-FILE-NAME.")

(defconst doctest-keyword-re
  (let* ((kw1 (mapconcat 'identity
                         '("and"      "assert"   "break"   "class"
                           "continue" "def"      "del"     "elif"
                           "else"     "except"   "exec"    "for"
                           "from"     "global"   "if"      "import"
                           "in"       "is"       "lambda"  "not"
                           "or"       "pass"     "print"   "raise"
                           "return"   "while"    "yield"
                           )
                         "\\|"))
         (kw2 (mapconcat 'identity
                         '("else:" "except:" "finally:" "try:")
                         "\\|"))
         (kw3 (mapconcat 'identity
                         '("ArithmeticError" "AssertionError"
                           "AttributeError" "DeprecationWarning" "EOFError"
                           "Ellipsis" "EnvironmentError" "Exception" "False"
                           "FloatingPointError" "FutureWarning" "IOError"
                           "ImportError" "IndentationError" "IndexError"
                           "KeyError" "KeyboardInterrupt" "LookupError"
                           "MemoryError" "NameError" "None" "NotImplemented"
                           "NotImplementedError" "OSError" "OverflowError"
                           "OverflowWarning" "PendingDeprecationWarning"
                           "ReferenceError" "RuntimeError" "RuntimeWarning"
                           "StandardError" "StopIteration" "SyntaxError"
                           "SyntaxWarning" "SystemError" "SystemExit"
                           "TabError" "True" "TypeError" "UnboundLocalError"
                           "UnicodeDecodeError" "UnicodeEncodeError"
                           "UnicodeError" "UnicodeTranslateError"
                           "UserWarning" "ValueError" "Warning"
                           "ZeroDivisionError" "__debug__"
                           "__import__" "__name__" "abs" "apply" "basestring"
                           "bool" "buffer" "callable" "chr" "classmethod"
                           "cmp" "coerce" "compile" "complex" "copyright"
                           "delattr" "dict" "dir" "divmod"
                           "enumerate" "eval" "execfile" "exit" "file"
                           "filter" "float" "getattr" "globals" "hasattr"
                           "hash" "hex" "id" "input" "int" "intern"
                           "isinstance" "issubclass" "iter" "len" "license"
                           "list" "locals" "long" "map" "max" "min" "object"
                           "oct" "open" "ord" "pow" "property" "range"
                           "raw_input" "reduce" "reload" "repr" "round"
                           "setattr" "slice" "staticmethod" "str" "sum"
                           "super" "tuple" "type" "unichr" "unicode" "vars"
                           "xrange" "zip")
                         "\\|"))
         (pseudokw (mapconcat 'identity
                              '("self" "None" "True" "False" "Ellipsis")
                              "\\|"))
         (string (concat "'\\(?:\\\\[^\n]\\|[^\n']*\\)'" "\\|"
                         "\"\\(?:\\\\[^\n]\\|[^\n\"]*\\)\""))
         (brk "\\(?:[ \t(]\\|$\\)"))
    (concat
     ;; Comments (group 1)
     "\\(#.*\\)"
     ;; Function & Class Definitions (groups 2-3)
     "\\|\\b\\(class\\|def\\)"
     "[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     ;; Builtins preceeded by '.'(group 4)
     "\\|[.][\t ]*\\(" kw3 "\\)"
     ;; Keywords & builtins (group 5)
     "\\|\\b\\(" kw1 "\\|" kw2 "\\|"
     kw3 "\\|" pseudokw "\\)" brk
     ;; Decorators (group 6)
     "\\|\\(@[a-zA-Z_][a-zA-Z0-9_]*\\)"
     ))
  "A regular expression used for syntax highlighting.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colorization support (font-lock mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define the font-lock keyword table.
(defconst doctest-font-lock-keywords
  `(
    ;; The following pattern colorizes source lines.  In particular,
    ;; it first matches prompts, and then looks for any of the
    ;; following matches *on the same line* as the prompt.  It uses
    ;; the form:
    ;;
    ;;   (MATCHER MATCH-HIGHLIGHT
    ;;            (ANCHOR-MATCHER nil nil MATCH-HIGHLIGHT))
    ;;
    ;; See the variable documentation for font-lock-keywords for a
    ;; description of what each of those means.
    ("^[ \t]*\\(>>>\\|\\.\\.\\.\\)"
     (1 'doctest-prompt-face)
     (doctest-source-matcher
      nil nil
      (1 'font-lock-comment-face t t)         ; comments
      (2 'font-lock-keyword-face t t)         ; def/class
      (3 'font-lock-type-face t t)            ; func/class name
      ;; group 4 (builtins preceeded by '.') gets no colorization.
      (5 'font-lock-keyword-face t t)         ; keywords & builtins
      (6 'font-lock-preprocessor-face t t)    ; decorators
      (7 'font-lock-string-face t t)          ; strings
      ))
      
    ;; The following pattern colorizes output lines.  In particular,
    ;; it uses doctest-output-line-matcher to check if this is an
    ;; output line, and if so, it colorizes it, and any special
    ;; markers it contains.
    (doctest-output-line-matcher
     (0 'doctest-output-face t)
     ("\\.\\.\\." (beginning-of-line) (end-of-line)
      (0 'doctest-output-marker-face t))
     (,doctest-blankline-re (beginning-of-line) (end-of-line)
                            (0 'doctest-output-marker-face t))
     (doctest-traceback-line-matcher (beginning-of-line) (end-of-line)
                                     (0 'doctest-output-traceback-face t))
     (,doctest-traceback-header-re (beginning-of-line) (end-of-line)
                                   (0 'doctest-output-traceback-face t))
     )

    ;; A PS1 prompt followed by a non-space is an error.
    ("^[ \t]*\\(>>>[^ \t\n][^\n]*\\)" (1 'font-lock-warning-face t))
    )
  "Expressions to highlight in Doctest mode.")

(defun doctest-output-line-matcher (limit)
  "A `font-lock-keyword' MATCHER that returns t if the current 
line is the expected output for a doctest example, and if so, 
sets `match-data' so that group 0 spans the current line."
  ;; The real work is done by find-doctest-output-line.
  (when (find-doctest-output-line limit)
    ;; If we found one, then mark the entire line.
    (beginning-of-line)
    (re-search-forward "[^\n]*" limit)))

(defun doctest-traceback-line-matcher (limit)
  "A `font-lock-keyword' MATCHER that returns t if the current line is
the beginning of a traceback, and if so, sets `match-data' so that
group 0 spans the entire traceback.  n.b.: limit is ignored."
  (beginning-of-line)
  (when (looking-at doctest-traceback-re)
      (goto-char (match-end 0))
      t))

(defun doctest-source-matcher (limit)
  "A `font-lock-keyword' MATCHER that returns t if the current line
contains a Python source expression that should be highlighted
after the point.  If so, it sets `match-data' to cover the string
literal.  The groups in `match-data' should be interpreted as follows:

  Group 1: comments
  Group 2: def/class
  Group 3: function/class name
  Group 4: builtins preceeded by '.'
  Group 5: keywords & builtins
  Group 6: decorators
  Group 7: strings
"
  (let ((matchdata nil))
    ;; First, look for string literals.
    (when doctest-highlight-strings
      (save-excursion
        (when (doctest-string-literal-matcher limit)
          (setq matchdata
                (list (match-beginning 0) (match-end 0)
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      (match-beginning 0) (match-end 0))))))
    ;; Then, look for other keywords.  If they occur before the
    ;; string literal, then they take precedence.
    (save-excursion
      (when (and (re-search-forward doctest-keyword-re limit t)
                 (or (null matchdata)
                     (< (match-beginning 0) (car matchdata))))
        (setq matchdata (match-data))))
    (when matchdata
      (set-match-data matchdata)
      (goto-char (match-end 0))
      t)))
          
(defun doctest-string-literal-matcher (limit &optional debug)
  "A `font-lock-keyword' MATCHER that returns t if the current line
contains a string literal starting at or after the point.  If so, it
expands `match-data' to cover the string literal.  This matcher uses
`doctest-statement-info' to collect information about strings that
continue over multiple lines.  It therefore might be a little slow for
very large statements."
  (let* ((stmt-info (doctest-statement-info))
         (quotes (reverse (nth 5 stmt-info)))
         (result nil))
    (if debug (warn "quotes %s" quotes))
    (while (and quotes (null result))
      (let* ((quote (pop quotes))
             (start (car quote))
             (end (min limit (or (cdr quote) limit))))
        (if debug (warn "quote %s-%s pt=%s lim=%s" start end (point) limit))
        (when (or (and (<= (point) start) (< start limit))
                  (and (< start (point)) (< (point) end)))
          (setq start (max start (point)))
          (set-match-data (list start end))
          (if debug (warn "marking string %s" (match-data)))
          (goto-char end)
          (setq result t))))
    result))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source code editing & indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctest-indent-source-line (&optional dedent-only)
  "Re-indent the current line, as doctest source code.  I.e., add a
prompt to the current line if it doesn't have one, and re-indent the
source code (to the right of the prompt).  If `dedent-only' is true,
then don't increase the indentation level any."
  (interactive "*")
  (let ((indent-end nil))
    (save-excursion
      (beginning-of-line)
      (let ((new-indent (doctest-current-source-line-indentation dedent-only))
            (new-margin (doctest-current-source-line-margin))
            (line-had-prompt (looking-at doctest-prompt-re)))
        ;; Delete the old prompt (if any).
        (when line-had-prompt
          (goto-char (match-end 1))
          (delete-char 4))
        ;; Delete the old indentation.
        (delete-backward-char (skip-chars-forward " \t"))
        ;; If it's a continuation line, or a new PS1 prompt,
        ;; then copy the margin.
        (when (or new-indent (not line-had-prompt))
          (beginning-of-line)
          (delete-backward-char (skip-chars-forward " \t"))
          (insert-char ?\  new-margin))
        ;; Add the new prompt.
        (insert-string (if new-indent "... " ">>> "))
        ;; Add the new indentation
        (if new-indent (insert-char ?\  new-indent))
        (setq indent-end (point))))
    ;; If we're left of the indentation end, then move up to the
    ;; indentation end.
    (if (< (point) indent-end) (goto-char indent-end))))

(defun doctest-current-source-line-indentation (&optional dedent-only)
  "Return the post-prompt indent to use for this line.  This is an
integer for a continuation lines, and nil for non-continuation lines."
  (save-excursion
    ;; Examine the previous doctest line (if present).
    (let* ((prev-stmt-info (doctest-prev-statement-info))
           (prev-stmt-indent (nth 0 prev-stmt-info)) 
           (continuation-indent (nth 1 prev-stmt-info))
           (prev-stmt-opens-block (nth 2 prev-stmt-info))
           (prev-stmt-closes-block (nth 3 prev-stmt-info))
           (prev-stmt-blocks-outdent (nth 4 prev-stmt-info))
           )
      ;; Examine this doctest line.
      (let* ((curr-line-indent 0)
             (curr-line-outdented nil))
        (beginning-of-line)
        (when (looking-at doctest-prompt-re)
          (setq curr-line-indent (- (match-end 3) (match-beginning 3)))
          (goto-char (match-end 3)))
        (setq curr-line-outdented (and (looking-at doctest-outdent-re)
                                       (not prev-stmt-blocks-outdent)))
        ;; Compute the overall indent.
        (let ((indent (or continuation-indent 
                          (+ prev-stmt-indent
                             (if curr-line-outdented -4 0)
                             (if prev-stmt-opens-block 4 0)
                             (if prev-stmt-closes-block -4 0)))))
          ;; If dedent-only is true, then make sure we don't indent.
          (when dedent-only 
            (setq indent (min indent curr-line-indent)))
          ;; If indent=0 and we're not outdented, then set indent to
          ;; nil (to signify the start of a new source example).
          (when (and (= indent 0)
                     (not (or curr-line-outdented continuation-indent)))
            (setq indent nil))
          ;; Return the indentation.
          indent)))))

(defun doctest-prev-statement-info (&optional debug)
  (save-excursion
    (forward-line -1)
    (doctest-statement-info debug)))

(defun doctest-statement-info (&optional debug)
  "Collect info about the previous statement, and return it as a list:

  (INDENT, CONTINUATION, OPENS-BLOCK, CLOSES-BLOCK, BLOCKS-OUTDENT,
   QUOTES)

INDENT -- The indentation of the previous statement (after the prompt)

CONTINUATION -- If the previous statement is incomplete (e.g., has an
open paren or quote), then this is the appropriate indentation
level; otherwise, it's nil.

OPENS-BLOCK -- True if the previous statement opens a Python control
block.

CLOSES-BLOCK -- True if the previous statement closes a Python control
block.

BLOCKS-OUTDENT -- True if the previous statement should 'block the
next statement from being considered an outdent.

QUOTES -- A list of (START . END) pairs for all quotation strings.
"
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (while (and (on-doctest-source-line "...") (= (forward-line -1) 0)))
      (cond
       ;; If there's no previous >>> line, then give up.
       ((not (on-doctest-source-line ">>>"))
        '(0 nil nil nil nil))
       
       ;; If there is a previous statement, walk through the source
       ;; code, checking for operators that may be of interest.
       (t 
        (beginning-of-line)
        (let* ((quote-mark nil) (nesting 0) (indent-stack '())
               (stmt-indent 0)
               (stmt-opens-block nil)
               (stmt-closes-block nil)
               (stmt-blocks-outdent nil)
               (quotes '())
               (elt-re (concat "\\\\[^\n]\\|"
                               "(\\|)\\|\\[\\|\\]\\|{\\|}\\|"
                               "\"\"\"\\|\'\'\'\\|\"\\|\'\\|"
                               "#[^\n]*\\|" doctest-prompt-re)))
          (while (re-search-forward elt-re end t)
            (let* ((elt (match-string 0))
                   (elt-first-char (substring elt 0 1)))
              (if debug (warn "Debug: %s" elt))
              (cond
               ;; Close quote -- set quote-mark back to nil.  The
               ;; second case is for cases like: '  '''
               (quote-mark
                (cond
                 ((equal quote-mark elt)
                  (setq quote-mark nil)
                  (setcdr (car quotes) (point)))
                 ((equal quote-mark elt-first-char)
                  (setq quote-mark nil)
                  (setcdr (car quotes) (point))
                  (backward-char 2))))
               ;; Prompt -- check if we're starting a new stmt.  If so,
               ;; then collect various info about it.
               ((string-match doctest-prompt-re elt)
                (when (and (null quote-mark) (= nesting 0))
                  (let ((indent (- (match-end 3) (match-end 2))))
                    (unless (looking-at "[ \t]*\n")
                      (setq stmt-indent indent)
                      (setq stmt-opens-block
                            (looking-at doctest-open-block-re))
                      (setq stmt-closes-block
                            (looking-at doctest-close-block-re))
                      (setq stmt-blocks-outdent
                            (looking-at doctest-no-outdent-re))))))
               ;; Open paren -- increment nesting, and update indent-stack.
               ((string-match "(\\|\\[\\|{" elt-first-char)
                (let ((elt-pos (point))
                      (at-eol (looking-at "[ \t]*\n"))
                      (indent 0))
                  (save-excursion 
                    (re-search-backward doctest-prompt-re)
                    (if at-eol
                        (setq indent (+ 4 (- (match-end 3) (match-end 2))))
                      (setq indent (- elt-pos (match-end 2))))
                    (push indent indent-stack)
                (setq nesting (+ nesting 1)))
               ;; Close paren -- decrement nesting, and pop indent-stack.
               ((string-match ")\\|\\]\\|}" elt-first-char)
                (setq indent-stack (cdr indent-stack))
                (setq nesting (max 0 (- nesting 1))))
               ;; Open quote -- set quote-mark.
               ((string-match "\"\\|\'" elt-first-char)
                (push (cons (- (point) (length elt)) nil) quotes)
                (setq quote-mark elt)))))
        
          (let* ((continuation-indent
                 (cond
                  (quote-mark 0)
                  ((> nesting 0) (if (null indent-stack) 0 (car indent-stack)))
                  (t nil)))
                 (result 
                  (list stmt-indent continuation-indent
                        stmt-opens-block stmt-closes-block
                        stmt-blocks-outdent quotes)))
            (if debug (warn "Debug: %s" result))
            result)))))))
  

(defun doctest-current-source-line-margin ()
  "Return the pre-prompt margin to use for this source line.  This is
copied from the most recent source line, or set to
`doctest-default-margin' if there are no preceeding source lines."
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (while (and (not (on-doctest-source-line))
                (re-search-backward doctest-prompt-re nil t)))
    (if (looking-at doctest-prompt-re)
        (- (match-end 1) (match-beginning 1))
      doctest-default-margin)))

(defun doctest-electric-backspace ()
  "Delete the preceeding character, level of indentation, or
prompt.  

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the first non-whitespace character
following an indented source line's prompt, then reduce the
indentation to the next multiple of 4; and update the source line's
prompt, when necessary.

Otherwise, if point is at the first non-whitespace character
following an unindented source line's prompt, then remove the
prompt (converting the line to an output line or text line).

Otherwise, if point is at the first non-whitespace character of a
line, the delete the line's indentation.

Otherwise, delete the preceeding character.
"
  (interactive "*")
  (cond 
   ;; Beginning of line: delete preceeding newline.
   ((bolp) (backward-delete-char 1))
      
   ;; First non-ws char following prompt: dedent or remove prompt.
   ((and (looking-at "[^ \t\n]\\|$") (doctest-looking-back doctest-prompt-re))
    (let* ((prompt-beg (match-beginning 2))
	   (indent-beg (match-beginning 3)) (indent-end (match-end 3))
	   (old-indent (- indent-end indent-beg))
	   (new-indent (* (/ (- old-indent 1) 4) 4)))
      (cond
       ;; Indented source line: dedent it.
       ((> old-indent 0)
	(goto-char indent-beg)
	(delete-region indent-beg indent-end)
	(insert-char ?\  new-indent)
	;; Change prompt to PS1, when appropriate.
	(when (and (= new-indent 0) (not (looking-at doctest-outdent-re)))
	  (delete-backward-char 4)
	  (insert-string ">>> ")))
       ;; Non-indented source line: remove prompt.
       (t
	(goto-char indent-end)
	(delete-region prompt-beg indent-end)))))

   ;; First non-ws char of a line: delete all indentation.
   ((and (looking-at "[^ \n\t]\\|$") (doctest-looking-back "^[ \t]+"))
    (delete-region (match-beginning 0) (match-end 0)))

   ;; Otherwise: delete a character.
   (t
    (backward-delete-char 1))))

(defun doctest-newline-and-indent ()
  "Insert a newline, and indent the new line appropriately.

If the current line is a source line containing a bare prompt,
then clear the current line, and insert a newline.

Otherwise, if the current line is a source line, then insert a
newline, and add an appropriately indented prompt to the new
line.

Otherwise, if the current line is an output line, then insert a
newline and indent the new line to match the example's margin.

Otherwise, insert a newline.

If `doctest-avoid-trailing-whitespace' is true, then clear any
whitespace to the left of the point before inserting a newline.
"
  (interactive "*")
  ;; If we're avoiding trailing spaces, then delete WS before point.
  (if doctest-avoid-trailing-whitespace
      (delete-char (- (skip-chars-backward " \t"))))     
  (cond 
   ;; If we're on an empty prompt, delete it.
   ((on-empty-doctest-source-line)
    (delete-region (match-beginning 0) (match-end 0))
    (insert-char ?\n 1))
   ;; If we're on a doctest line, add a new prompt.
   ((on-doctest-source-line)
    (insert-char ?\n 1)
    (doctest-indent-source-line))
   ;; If we're in doctest output, indent to the margin.
   ((on-doctest-output-line)
    (insert-char ?\n 1)
    (insert-char ?\  (doctest-current-source-line-margin)))
   ;; Otherwise, just add a newline.
   (t (insert-char ?\n 1))))

(defun doctest-electric-colon ()
  "Insert a colon, and dedent the line when appropriate."
  (interactive "*")
  (insert-char ?: 1)
  (when (on-doctest-source-line)
    (doctest-indent-source-line t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctest-execute-buffer-with-diff ()
  "Run doctest on the current buffer, and display the results in the 
*doctest-output* buffer, using the diff format."
  (interactive)
  (doctest-execute-buffer t))

(defun doctest-execute-buffer (&optional diff)
  "Run doctest on the current buffer, and display the results in the 
*doctest-output* buffer."
  (interactive)
  (doctest-execute-region (point-min) (point-max) diff))
  
(defun doctest-execute-region (start end &optional diff)
  "Run doctest on the current buffer, and display the results in the 
*doctest-output* buffer."
  (interactive "r")
  ;; If it's already running, give the user a chance to restart it.
  (when (doctest-process-live-p doctest-async-process)
    (when (y-or-n-p "Doctest is already running.  Restart it? ")
      (doctest-cancel-async-process)
      (message "Killing doctest...")))
  (cond
   ((and doctest-async (doctest-process-live-p doctest-async-process))
    (message "Can't run two doctest processes at once!"))
   (t
    (setq doctest-results-buffer (get-buffer-create
                                  doctest-results-buffer-name))
    (let* ((temp (concat (doctest-temp-name) ".py"))
           (tempfile (expand-file-name temp doctest-temp-directory))
           (cur-buf (current-buffer))
           (in-buf (get-buffer-create "*doctest-input*"))
           ;(beg (point-min)) (end (point-max))
           (script (format doctest-script tempfile (buffer-name)
                           (buffer-file-name) (doctest-optionflags diff)
                           (buffer-file-name))))
      ;; Write buffer to a file.
      (save-excursion
        (goto-char (min start end))
        (let ((lineno (doctest-line-number)))
          (set-buffer in-buf)
          ;; Add blank lines, to keep line numbers the same:
          (dotimes (n (- lineno 1)) (insert-string "\n"))
          ;; Add the selected region
          (insert-buffer-substring cur-buf start end)
          ;; Write it to a file
          (write-file tempfile)
          ;; Dispose of the buffer
          (kill-buffer in-buf)))
      ;; Clear out the results buffer, if it contains anything;
      ;; and set its mode.
      (save-excursion
        (set-buffer doctest-results-buffer)
        (toggle-read-only 0)
        (delete-region (point-min) (point-max))
        (doctest-results-mode)
        (setq doctest-source-buffer cur-buf)
        )
      ;; Add markers to examples, and record what line number each one
      ;; starts at.  That way, if the input buffer is edited, we can
      ;; still find corresponding examples in the output.
      (doctest-mark-examples)
      ;; Run doctest
      (cond (doctest-async
             ;; Asynchronous mode:
             (let ((process (start-process "*doctest*" doctest-results-buffer
                                           doctest-python-command
                                           "-c" script)))
               (setq doctest-async-process-tempfile tempfile)
               (setq doctest-async-process process)
               (set-process-sentinel process 'doctest-async-process-sentinel)
               (display-buffer doctest-results-buffer)
               (doctest-update-mode-line ":running")
               (message "Running doctest...")))
            (t
             ;; Synchronous mode:
             (call-process doctest-python-command nil
                           doctest-results-buffer t "-c" script)
             (doctest-handle-output)
             (delete-file tempfile)))))))

(defun doctest-handle-output ()
  "This function, which is called after the 'doctest' process spawned
by doctest-execute-buffer has finished, checks the
doctest-results-buffer.  If that buffer is empty, it reports no errors
and deletes it; if that buffer is not empty, it reports that errors
occured, displays the buffer, and runs doctest-postprocess-results."
  ;; If any tests failed, display them.
  (cond ((not (buffer-live-p doctest-results-buffer))
         (message "Results buffer not found!"))
        ((> (buffer-size doctest-results-buffer) 1)
         (display-buffer doctest-results-buffer)
         (doctest-postprocess-results)
         (let ((num (length doctest-example-markers)))
           (message "%d doctest example%s failed!" num
                    (if (= num 1) "" "s"))))
        (t
         (display-buffer doctest-results-buffer)
         (let ((w (get-buffer-window doctest-results-buffer t)))
           (when w (delete-window w)))
         (message "All doctest examples passed!"))))
         
(defun doctest-async-process-sentinel (process state)
  "A process sentinel, called when the asynchronous doctest process
completes, which calls doctest-handle-output."
  ;; Check to make sure we got the process we're expecting.  On
  ;; some operating systems, this will end up getting called twice
  ;; when we use doctest-cancel-async-process; this check keeps us
  ;; from trying to clean up after the same process twice (since we
  ;; set doctest-async-process to nil when we're done).
  (when (equal process doctest-async-process)
    (cond ((not (buffer-live-p doctest-results-buffer))
           (message "Results buffer not found!"))
          ((equal state "finished\n")
           (doctest-handle-output))
          ((equal state "killed\n")
           (message "Doctest killed."))
          (t
           (message "Doctest failed -- %s" state)
           (display-buffer doctest-results-buffer)))
    (doctest-update-mode-line "")
    (when doctest-async-process-tempfile
      (delete-file doctest-async-process-tempfile)
      (setq doctest-async-process-tempfile nil))
    (setq doctest-async-process nil)))

(defun doctest-cancel-async-process ()
  "If a doctest process is running, then kill it."
  (interactive "")
  (when (doctest-process-live-p doctest-async-process)
    ;; Update the modeline
    (doctest-update-mode-line ":killing")
    ;; Kill the process.
    (kill-process doctest-async-process)
    ;; Run the sentinel.  (Depending on what OS we're on, the sentinel
    ;; may end up getting called once or twice.)
    (doctest-async-process-sentinel doctest-async-process "killed\n")
    ))

(defun doctest-postprocess-results ()
  (save-excursion
    (set-buffer doctest-results-buffer)
    ;; Check if we're using an old doctest version.
    (goto-char (point-min))
    (when (re-search-forward doctest-pre-py24-results-re nil t)
      (setq doctest-results-are-pre-py24 t)
      (goto-char (point-min)))
    ;; Turn on read-only mode.
    (toggle-read-only t))
  
  (doctest-filter-example-markers)
  (if hide-example-source
      (hide-example-source))
  (if (eq (current-buffer) doctest-source-buffer)
      (doctest-next-failure 1)))

(defun doctest-results-loc-re ()
  (if doctest-results-are-pre-py24
      doctest-pre-py24-results-loc-re
    doctest-results-loc-re))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctest-mark-examples ()
  "Add a marker at the beginning of every (likely) example in the
input buffer; and create a list, `doctest-example-markers',
which maps from markers to the line numbers they originally occured
on.  This will allow us to find the corresponding example in the
doctest output, even if the input buffer is edited."
  (setq doctest-example-markers '())
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ *>>> " nil t)
      (backward-char 4)
      (push (cons (point-marker) (doctest-line-number))
            doctest-example-markers))))

(defun doctest-filter-example-markers ()
  "Remove any entries from `doctest-example-markers' that do not
correspond to a failed example."
  (let ((filtered nil) (markers doctest-example-markers))
    (save-excursion
      (set-buffer doctest-results-buffer)
      (goto-char (point-max))
      (while (re-search-backward (doctest-results-loc-re) nil t)
        (let ((lineno (string-to-int (match-string 1))))
          (when doctest-results-are-pre-py24
            (setq lineno (+ lineno 1)))
          (while (and markers (< lineno (cdar markers)))
            (setq markers (cdr markers)))
          (if (and markers (= lineno (cdar markers)))
              (push (car markers) filtered)
            (warn "Example expected on line %d but not found %s" lineno
                  markers)))))
    (setq doctest-example-markers filtered)))
                       
(defun doctest-prev-example-marker ()
  "Helper for doctest-replace-output: move to the preceeding example
marker, and return the corresponding 'original' lineno.  If none is
found, return nil."
  (let ((lineno nil)
        (pos nil))
    (save-excursion
      (end-of-line)
      (when (re-search-backward "^\\( *\\)>>> " nil t)
        (goto-char (match-end 1))
        (dolist (marker-info doctest-example-markers)
          (when (= (marker-position (car marker-info)) (point))
            (setq lineno (cdr marker-info))
            (setq pos (point))))))
    (unless (null lineno)
      (goto-char pos)
      lineno)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctest-next-failure (count)
  "Move to the top of the next failing example, and highlight the
example's failure description in *doctest-output*."
  (interactive "p")
  (cond
   ((and doctest-async (doctest-process-live-p doctest-async-process))
    (message "Wait for doctest to finish running!"))
   ((not (buffer-live-p doctest-results-buffer))
    (message "Run doctest first! (C-c C-c)"))
   (t
    (let ((marker nil) (orig-window (selected-window))
          (results-window (display-buffer doctest-results-buffer)))
      (save-excursion
        (set-buffer doctest-results-buffer)
        ;; Switch to the results window (so its point gets updated)
        (if results-window (select-window results-window))
        ;; Pick up where we left off.
        ;; (nb: doctest-selected-failure is buffer-local)
        (goto-char (or doctest-selected-failure (point-min)))
        ;; Skip past anything on *this* line.
        (if (>= count 0) (end-of-line) (beginning-of-line))
        ;; Look for the next failure
        (if (>= count 0)
            (re-search-forward (doctest-results-loc-re) nil t count)
          (re-search-backward (doctest-results-loc-re) nil t (- count)))
        ;; We found a failure:
        (when (match-string 1)
          (let ((old-selected-failure doctest-selected-failure))
            (beginning-of-line)
            ;; Extract the line number for the doctest file.
            (let ((orig-lineno (string-to-int (match-string 1))))
              (when doctest-results-are-pre-py24
                (setq orig-lineno (+ orig-lineno 1)))
              (dolist (marker-info doctest-example-markers)
                (when (= orig-lineno (cdr marker-info))
                  (setq marker (car marker-info)))))
              
            ;; Store our position for next time.
            (beginning-of-line)
            (setq doctest-selected-failure (point))
            ;; Update selection.
            (doctest-fontify-line old-selected-failure)
            (doctest-fontify-line doctest-selected-failure))))
      
      ;; Return to the original window
      (select-window orig-window)

      (cond
       ;; We found a failure -- move point to the selected failure.
       (marker
        (goto-char (marker-position marker))
        (beginning-of-line))
       ;; We didn't find a failure, but there is one -- wrap.
       ((> (length doctest-example-markers) 0)
        (if (>= count 0) (doctest-first-failure) (doctest-last-failure)))
       ;; We didn't find a failure -- alert the user.
       (t (message "No failures found!")))))))

(defun doctest-prev-failure (count)
  "Move to the top of the previous failing example, and highlight
the example's failure description in *doctest-output*."
  (interactive "p")
  (doctest-next-failure (- count)))

(defun doctest-first-failure ()
  (interactive)
  (if (buffer-live-p doctest-results-buffer)
      (save-excursion
        (set-buffer doctest-results-buffer)
        (let ((old-selected-failure doctest-selected-failure))
          (setq doctest-selected-failure (point-min))
          (doctest-fontify-line old-selected-failure))))
  (doctest-next-failure 1))

(defun doctest-last-failure ()
  (interactive)
  (if (buffer-live-p doctest-results-buffer)
      (save-excursion
        (set-buffer doctest-results-buffer)
        (let ((old-selected-failure doctest-selected-failure))
          (setq doctest-selected-failure (point-max))
          (doctest-fontify-line old-selected-failure))))
  (doctest-next-failure -1))

(defun hide-example-source ()
  "Delete the source code listings from the results buffer (since it's
easy enough to see them in the original buffer)"
  (save-excursion
    (set-buffer doctest-results-buffer)
    (toggle-read-only 0)
    (goto-char (point-min))
    (while (re-search-forward doctest-example-source-re nil t)
      (replace-match "" nil nil))
    (toggle-read-only t)))

(defun doctest-results-next-header ()
  (if (re-search-forward (concat doctest-results-header-re "\\|"
                                 doctest-results-divider-re) nil t)
      (let ((result (match-string 0)))
        (if (string-match doctest-results-header-re result)
            result
          nil))
    nil))

(defun doctest-select-failure ()
  "Move to the top of the currently selected example, and select that
example in the source buffer.  Intended for use in the results
buffer."
  (interactive)
  (set-buffer doctest-results-buffer)
  (re-search-backward doctest-results-divider-re)
  (let ((old-selected-failure doctest-selected-failure))
    (setq doctest-selected-failure (point))
    (doctest-fontify-line old-selected-failure))
  (pop-to-buffer doctest-source-buffer)
  (doctest-next-failure 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doctest-replace-output ()
  "Move to the top of the closest example, and replace its output
with the 'got' output from the *doctest-output* buffer.  An error is
displayed if the chosen example is not listed in *doctest-output*, or
if the 'expected' output for the example does not exactly match the
output listed in the source buffer.  The user is asked to confirm the
replacement."
  (interactive)
  ;; Move to the beginning of the example.
  (cond
   ((and doctest-async (doctest-process-live-p doctest-async-process))
    (message "Wait for doctest to finish running!"))
   ((not (buffer-live-p doctest-results-buffer))
    (message "Run doctest first! (C-c C-c)"))
   ((save-excursion (set-buffer doctest-results-buffer)
                    doctest-results-are-pre-py24)
    (error "doctest-replace-output requires python 2.4+."))
   (t
    (save-excursion
      (let* ((orig-buffer (current-buffer))
             ;; Find an example, and look up its original lineno.
             (lineno (doctest-prev-example-marker))
             ;; Find the example's indentation.
             (prompt-indent (doctest-line-indentation)))
        
        ;; Switch to the output buffer, and look for the example.
        ;; If we don't find one, complain.
        (cond
         ((null lineno) (message "Doctest example not found"))
          (t
           (set-buffer doctest-results-buffer)
           (goto-char (point-min))
           (let ((output-re (format "^File .*, line %s," lineno)))
             (when (not (re-search-forward output-re nil t))
               (message "This doctest example did not fail")
               (setq lineno nil)))))

        ;; If we didn't find an example, give up.
        (when (not (null lineno))
                   
          ;; Get the output's 'expected' & 'got' texts.
          (let ((doctest-got nil) (doctest-expected nil) (header nil))
            (while (setq header (doctest-results-next-header))
              (cond
               ((equal header "Failed example:")
                t)
               ((equal header "Expected nothing")
                (setq doctest-expected ""))
               ((equal header "Expected:")
                (unless (re-search-forward "^\\(\\(    \\).*\n\\)*" nil t)
                  (error "Error parsing doctest output"))
                (setq doctest-expected (doctest-replace-regexp-in-string
                                        "^    " prompt-indent
                                        (match-string 0))))
               ((equal header "Got nothing")
                (setq doctest-got ""))
               ((or (equal header "Got:") (equal header "Exception raised:"))
                (unless (re-search-forward "^\\(\\(    \\).*\n\\)*" nil t)
                  (error "Error parsing doctest output"))
                (setq doctest-got (doctest-replace-regexp-in-string
                                   "^    " prompt-indent (match-string 0))))
               ((string-match "^Differences" header)
                (error (concat "doctest-replace-output can not be used "
                               "with diff style output")))
               (t (error "Unexpected header %s" header))))

            ;; Go back to the source buffer.
            (set-buffer orig-buffer)
          
            ;; Skip ahead to the output.
            (beginning-of-line)
            (unless (re-search-forward "^ *>>>.*")
              (error "Error parsing doctest output"))
            (re-search-forward "\\(\n *\\.\\.\\..*\\)*\n?")
            (if (not (looking-at "^"))
                (insert-string "\n"))

            ;; Check that the output matches.
            (let ((start (point)) end)
              (cond ((re-search-forward "^ *\\(>>>.*\\|$\\)" nil t)
                     (setq end (match-beginning 0)))
                    (t
                     (goto-char (point-max))
                     (insert-string "\n")
                     (setq end (point-max))))
              (when (and doctest-expected
                         (not (equal (buffer-substring start end)
                                     doctest-expected)))
                (error (concat "This example's output has been modified "
                               "since doctest was last run.")))
              (setq doctest-expected (buffer-substring start end))
              (goto-char end))

            ;; Trim exceptions
            (when (and doctest-trim-exceptions
                       (string-match doctest-traceback-re
                                     doctest-got))
              (let ((s1 0) (e1 (match-end 1))
                    (s2 (match-beginning 2)) (e2 (match-end 2))
                    (s3 (match-beginning 3)) (e3 (length doctest-got)))
                (setq doctest-got
                    (concat (substring doctest-got s1 e1)
                            (substring doctest-got s2 e2) "  . . .\n"
                            (substring doctest-got s3 e3)))))
              
            ;; Confirm it with the user.
            (let ((confirm-buffer (get-buffer-create "*doctest-confirm*")))
              (set-buffer confirm-buffer)
              ;; Erase anything left over in the buffer.
              (delete-region (point-min) (point-max))
              ;; Write a confirmation message
              (if (equal doctest-expected "")
                  (insert-string "Replace nothing\n")
                (insert-string (concat "Replace:\n" doctest-expected)))
              (if (equal doctest-got "")
                  (insert-string "With nothing\n")
                (insert-string (concat "With:\n" doctest-got)))
              (let ((confirm-window (display-buffer confirm-buffer)))
                ;; Shrink the confirm window.
                (shrink-window-if-larger-than-buffer confirm-window)
                ;; Return to the original buffer.
                (set-buffer orig-buffer)
                ;; Match the old expected region.
                (when doctest-expected
                    (search-backward doctest-expected))
                (when (equal doctest-expected "") (backward-char 1))
                ;; Get confirmation & do the replacement
                (cond ((y-or-n-p "Ok to replace? ")
                       (when (equal doctest-expected "") (forward-char 1))
                       (replace-match doctest-got t t)
                       (message "Replaced."))
                      (t
                       (message "Replace cancelled.")))
                ;; Clean up our confirm window
                (kill-buffer confirm-buffer)
                (delete-window confirm-window))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doctest Results Mode (output of doctest-execute-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doctest-results-font-lock-keywords
  `((,doctest-results-divider-re 
     (0 'doctest-results-divider-face))
    (,doctest-results-loc-re 
     (0 'doctest-results-loc-face))
    (,doctest-results-header-re 
     (0 'doctest-results-header-face))
    (doctest-results-selection-matcher 
     (0 'doctest-results-selection-face t))))

(defun doctest-results-selection-matcher (limit)
  "Matches from `doctest-selected-failure' to the end of the
line.  This is used to highlight the currently selected failure."
  (when (and doctest-selected-failure
	     (<= (point) doctest-selected-failure)
	     (< doctest-selected-failure limit))
    (goto-char doctest-selected-failure)
    (re-search-forward "[^\n]+" limit)))

;; Register the font-lock keywords (xemacs)
(put 'doctest-results-mode 'font-lock-defaults 
     '(doctest-results-font-lock-keywords))

;; Register the font-lock keywords (gnu emacs)
(defvar font-lock-defaults-alist nil) ; in case we're in xemacs
(setq font-lock-defaults-alist
      (append font-lock-defaults-alist
              `((doctest-results-mode 
		 doctest-results-font-lock-keywords 
		 nil nil nil nil))))

(defvar doctest-selected-failure nil
  "The location of the currently selected failure.
 (local variable for doctest-results buffers)")

(defvar doctest-source-buffer nil
  "The buffer that spawned this one.
 (local variable for doctest-results buffers)")

(defvar doctest-results-are-pre-py24 nil
  "True if the results are generated by an old version of doctest")

(defconst doctest-results-mode-map 
  (let ((map (make-keymap)))
    (define-key map [return] 'doctest-select-failure)
    map) 
  "Keymap for doctest-results-mode.")

;; Define the mode
(define-derived-mode doctest-results-mode text-mode "Doctest Results"
  "A major mode used to display the results of running doctest.
See `doctest-mode'.

\\{doctest-results-mode-map}
"
  ;; Enable font-lock mode.
  (if (featurep 'font-lock) (font-lock-mode 1))
  ;; Keep track of which failure is selected
  (set (make-local-variable 'doctest-selected-failure) nil)
  (set (make-local-variable 'doctest-results-are-pre-py24) nil)
  ;; Display doctest-mode-line-process on the modeline.
  (setq mode-line-process 'doctest-mode-line-process)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-doctest-source-line (&optional prompt)
  "Return true if the current line is a source line.  The optional
argument prompt can be used to specify which type of source
line (... or >>>)."
  (save-excursion
    (beginning-of-line)
    ;; Check if we're looking at a prompt (of the right type).
    (when (and (looking-at doctest-prompt-re)
               (or (null prompt)
                   (equal prompt (substring (match-string 2) 0 3))))
      ;; Scan backwards to make sure there's a >>> somewhere.  Otherwise,
      ;; this might be a '...' in the text or in an example's output.
      (while (looking-at "^[ \t]*[.][.][.]")
        (forward-line -1))
      (looking-at "^[ \t]*>>>"))))

(defun on-empty-doctest-source-line ()
  "Return true if the current line contains a bare prompt."
  (save-excursion
    (beginning-of-line)
    (and (on-doctest-source-line)
         (looking-at (concat doctest-prompt-re "$")))))

(defun on-doctest-output-line ()
  "Return true if the current line is an output line."
  (save-excursion
    (beginning-of-line)
    ;; The line must not be blank or a source line.
    (when (not (or (on-doctest-source-line) (looking-at "[ \t]*$")))
      ;; The line must follow a source line, with no intervening blank
      ;; lines.
      (while (not (or (on-doctest-source-line) (looking-at "[ \t]*$")
                      (= (point) (point-min))))
        (forward-line -1))
      (on-doctest-source-line))))

(defun find-doctest-output-line (&optional limit)
  "Move forward to the next doctest output line (staying within
the given bounds).  Return the character position of the doctest
output line if one was found, and false otherwise."
  (let ((found-it nil) ; point where we found an output line
	(limit (or limit (point-max)))) ; default value for limit
    (save-excursion
      ;; Keep moving forward, one line at a time, until we find a
      ;; doctest output line.
      (while (and (not found-it) (< (point) limit) (not (eobp)))
	(if (and (not (eolp)) (on-doctest-output-line))
	    (setq found-it (point))
	  (forward-line))))
    ;; If we found a doctest output line, then go to it.
    (if found-it (goto-char found-it))))

(defun doctest-line-indentation ()
  "Helper for doctest-replace-output: return the whitespace indentation
at the beginning of this line."
  (save-excursion
    (end-of-line)
    (re-search-backward "^\\( *\\)" nil t)
    (match-string 1)))

(defun doctest-optionflags (&optional diff)
  (let ((flags "0"))
    (dolist (flag doctest-optionflags)
      (setq flags (concat flags "|" flag)))
    (if diff (concat flags "|" "REPORT_UDIFF") flags)))
  
(defun doctest-version ()
  "Echo the current version of `doctest-mode' in the minibuffer."
  (interactive)
  (message "Using `doctest-mode' version %s" doctest-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Many of these are defined for compatibility reasons.  E.g., if
;; they are available under different names depending on the emacs
;; version.

(defvar doctest-serial-number 0) ;used if broken-temp-names.
(defun doctest-temp-name ()
  (if (memq 'broken-temp-names features)
      (let
	  ((sn doctest-serial-number)
	   (pid (and (fboundp 'emacs-pid) (emacs-pid))))
	(setq doctest-serial-number (1+ doctest-serial-number))
	(if pid
	    (format "doctest-%d-%d" sn pid)
	  (format "doctest-%d" sn)))
    (make-temp-name "doctest-")))

(defun doctest-looking-back (regexp)
  "Return True if the text before point matches the given regular
expression.  Like looking-at except backwards and slower.  (This
is available as `looking-back' in GNU emacs and
`looking-at-backwards' in XEmacs, but it's easy enough to define
from scratch such that it works under both.)"
  (save-excursion
    (let ((orig-pos (point)))
      ;; Search backwards for the regexp.
      (if (re-search-backward regexp nil t)
	  ;; Check if it ends at the original point.
	  (= orig-pos (match-end 0))))))

(defun doctest-fontify-line (charpos)
  "Run font-lock-fontify-region on the line containing the given
position."
  (if (and charpos (functionp 'font-lock-fontify-region))
      (save-excursion
        (goto-char charpos)
        (let ((beg (progn (beginning-of-line) (point)))
              (end (progn (end-of-line) (point))))
          (font-lock-fontify-region beg end)))))
  
(defun doctest-replace-regexp-in-string (regexp replacement text)
  "Return the result of replacing all mtaches of REGEXP with
REPLACEMENT in TEXT.  (Since replace-regexp-in-string is not available
under all versions of emacs, and is called different names in
different versions, this compatibility function will emulate it if
it's not available."
  (let ((start 0) (repl-len (length replacement)))
    (while (string-match regexp text start)
      (setq start (+ (match-beginning 0) repl-len 1))
      (setq text (replace-match replacement t nil text)))
    text))

(defun doctest-line-number ()
  "Compatibility function: Equivalent to `line-number' or
`line-number-at-pos'."
  (cond ((functionp 'line-number)
         (line-number))
        ((functionp 'line-number-at-pos)
         (line-number-at-pos))
        (t
         (1+ (count-lines 1
               (save-excursion (progn (beginning-of-line) (point))))))))

(defun doctest-process-live-p (process)
  (if (null process)
      nil
    (equal (process-status process) 'run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We do *NOT* currently use this, because it applies too
;; indiscrimanantly.  In particular, we don't want "'" and '"' treated
;; as quote marks on text lines.  But there's no good way to prevent
;; it.
(defvar doctest-syntax-alist nil
  "Syntax alist used in `doctest-mode' buffers.")
(setq doctest-syntax-alist '((?\( . "()") (?\[ . "(]") (?\{ . "(}")
			     (?\) . ")(") (?\] . ")[") (?\} . "){")
			     (?\$ . "." ) (?\% . "." ) (?\& . "." )
			     (?\* . "." ) (?\+ . "." ) (?\- . "." )
			     (?\/ . "." ) (?\< . "." ) (?\= . "." )
			     (?\> . "." ) (?\| . "." ) (?\_ . "w" )
			     (?\' . "\"") (?\" . "\"") (?\` . "$" )
			     (?\# . "<" ) (?\n . ">" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doctest-mode-map 
  (let ((map (make-keymap)))
    (define-key map [backspace] 'doctest-electric-backspace)
    (define-key map [return] 'doctest-newline-and-indent)
    (define-key map [tab] 'doctest-indent-source-line)
    (define-key map ":" 'doctest-electric-colon)
    (define-key map "\C-c\C-v" 'doctest-version)
    (define-key map "\C-c\C-c" 'doctest-execute-buffer)
    (define-key map "\C-c\C-d" 'doctest-execute-buffer-with-diff)
    (define-key map "\C-c\C-n" 'doctest-next-failure)
    (define-key map "\C-c\C-p" 'doctest-prev-failure)
    (define-key map "\C-c\C-a" 'doctest-first-failure)
    (define-key map "\C-c\C-e" 'doctest-last-failure)
    (define-key map "\C-c\C-z" 'doctest-last-failure)
    (define-key map "\C-c\C-r" 'doctest-replace-output)
    (define-key map "\C-c|" 'doctest-execute-region)
    map) 
  "Keymap for doctest-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Register the font-lock keywords (xemacs)
(put 'doctest-mode 'font-lock-defaults '(doctest-font-lock-keywords
                                         nil nil nil nil))

;; Register the font-lock keywords (gnu emacs)
(when (boundp 'font-lock-defaults-alist)
  (setq font-lock-defaults-alist
        (append font-lock-defaults-alist
                `((doctest-mode doctest-font-lock-keywords
                                nil nil nil nil)))))
  
(defvar doctest-results-buffer nil
  "The output buffer for doctest-mode")

(defvar doctest-async-process nil
  "The process object created by the asynchronous doctest process")

(defvar doctest-async-process-tempfile nil
  "The name of the tempfile created by the asynchronous doctest process")

(defvar doctest-mode-line-process ""
  "A string displayed on the modeline, to indicate when doctest is
running asynchronously.")

(defun doctest-update-mode-line (value)
  (setq doctest-mode-line-process
        value)
  (force-mode-line-update t))

(defvar doctest-example-markers nil
  "A list mapping markers to the line numbers at which they appeared
in the buffer at the time doctest was last run.  This is used to find
'original' line numbers, which can be used to search the doctest
output buffer.")

;; Use doctest mode for files ending in .doctest
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

;;;###autoload
(define-derived-mode doctest-mode text-mode "Doctest"
  "A major mode for editing text files that contain Python
doctest examples.  Doctest is a testing framework for Python that
emulates an interactive session, and checks the result of each
command.  For more information, see the Python library reference:
<http://docs.python.org/lib/module-doctest.html>

`doctest-mode' defines three kinds of line, each of which is
treated differently:

  - 'Source lines' are lines consisting of a Python prompt
    ('>>>' or '...'), followed by source code.  Source lines are
    colored (similarly to `python-mode') and auto-indented.

  - 'Output lines' are non-blank lines immediately following
    source lines.  They are colored using several doctest-
    specific output faces.

  - 'Text lines' are any other lines.  They are not processed in
    any special way.

\\{doctest-mode-map}
"
  ;; Enable auto-fill mode.
  (auto-fill-mode 1)

  ;; Enable font-lock mode.
  (if (featurep 'font-lock) (font-lock-mode 1))
  
  ;; Register our indentation function.
  (set (make-local-variable 'indent-line-function) 
       'doctest-indent-source-line)

  ;; Display doctest-mode-line-process on the modeline.
  (setq mode-line-process 'doctest-mode-line-process)

  )

(provide 'doctest-mode)
;;; doctest-mode.el ends here
