;;; py-bug-numbered-tests.el --- run single tests according to bug number

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path default-directory)

(require 'thingatpt-utils-base)
(require 'thing-at-point-utils)
(require 'beg-end)
(require 'ar-comment-lor)

;; (require 'python-components-mode)

(setq bug-numbered-tests
      (if (featurep 'xemacs)
          (list
           'bullet-lists-in-comments-lp:328782-test
           'fill-paragraph-problems-lp:710373-test
           'nested-indents-lp:328775-test
           'previous-statement-lp:637955-test)
        (list
         'mark-block-region-lp:328806-test
         'nested-dictionaries-indent-lp:328791-test
         'triple-quoted-string-dq-lp:302834-test
         'fore-00007F-breaks-indentation-lp:328788-test
         'dq-in-tqs-string-lp:328813-test
         'beg-end-of-defun-lp:303622-test
         'bullet-lists-in-comments-lp:328782-test
         'fill-paragraph-problems-lp:710373-test
         'nested-indents-lp:328775-test
         'previous-statement-lp:637955-test
         'multiline-assignment-indentation-lp:629916-test
         'indentation-of-continuation-lines-lp:691185-test
         'goto-beginning-of-tqs-lp:735328-test
         'class-treated-as-keyword-lp:709478-test)))

(defun py-run-bug-numbered-tests (&optional arg)
  (interactive "p")
  (dolist (ele bug-numbered-tests)
    (funcall ele arg)))

(defun py-bug-numbered-tests-intern (testname &optional arg teststring)
  (if arg
      (progn
        (set-buffer (get-buffer-create (prin1-to-string testname)))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (python-mode)
        (funcall testname)
        (message "%s" (concat (prin1-to-string testname) " test passed")))
    (with-temp-buffer
      (insert teststring)
      (funcall testname))))


(defun sexp-commands-lp:328778-test (&optional arg load-branch-function)
  "Reported by Montanaro on 2003-08-05
\[ ... ]
 You can kill balanced expressions on a
 particular line but it's not possible to remove the
 whole of an 'if' or 'while' block."
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):                          
    grammar = \"kant.xml\"                
    try:                                
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:          
        usage() 
        sys.exit(2) 
    for opt, arg in opts:                
        if opt in (\"-h\", \"--help\"):      
            usage() 
            sys.exit() 
        elif opt == '-d':                
            global _debug 
            _debug = 1 
        elif opt in (\"-g\", \"--grammar\"): 
            grammar = arg 
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'sexp-commands-lp:328778 arg teststring)))

(defun sexp-commands-lp:328778 ()
  (let ((size (buffer-size)))
    (goto-char (point-min))
    (forward-line 15)
    (py-kill-clause)
    (assert (< (buffer-size) size) nil "sexp-commands-lp:328778 test failed")
    (assert (eq (buffer-size) 526) nil "sexp-commands-lp:328778 test failed")
    (kill-line 1)
    (indent-according-to-mode)
    (forward-line -4)
    (py-kill-block)
    (assert (eq (buffer-size) 324) nil "sexp-commands-lp:328778 test failed")
    ))

(defun nested-dictionaries-indent-lp:328791-test (&optional arg load-branch-function)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    d = {'a':{'b':3,
              'c':4}}
"))
    (py-bug-numbered-tests-intern 'nested-dictionaries-indent-lp:328791 arg teststring)))

(defun nested-dictionaries-indent-lp:328791 ()
  (goto-char (point-min))
  (forward-line 2)
  (assert (eq 14 (py-compute-indentation t))))

(defun mark-block-region-lp:328806-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "def f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
  (py-bug-numbered-tests-intern 'mark-block-region-lp:328806 arg teststring)))

(defun mark-block-region-lp:328806 ()
  (forward-line -2)
  (py-mark-block)
  (assert (< (region-beginning) (region-end)) nil "mark-block-region-lp:328806 test failed!"))

(defun flexible-indentation-lp:328842-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
\(long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list

packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)

\( long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list
"
))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'flexible-indentation-lp:328842 arg teststring)))

(defun flexible-indentation-lp:328842 ()
    (goto-char (point-min))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq 1 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 16 (current-indentation)) nil "flexible-indentation-lp:328842 test failed"))

(defun beg-end-of-defun-lp:303622-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
class f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
  (py-bug-numbered-tests-intern 'beg-end-of-defun-lp:303622 arg teststring)))

(defun beg-end-of-defun-lp:303622 ()
  (end-of-defun)
  (assert (eq 292 (point)) nil "beg-end-of-defun-lp:303622 test failed!")
  (beginning-of-defun)
  (assert (eq 2 (point)) nil "beg-end-of-defun-lp:303622 test failed!"))

(defun dq-in-tqs-string-lp:328813-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''
"))
    (py-bug-numbered-tests-intern 'dq-in-tqs-string-lp:328813 arg teststring)))

(defun dq-in-tqs-string-lp:328813 ()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (goto-char 78)
  (let ((erg (get-char-property (point) 'face)))
    (message "%s" erg)
    (insert "\"")
    (font-lock-fontify-buffer)
    (message "%s" erg)
    (message "%s" (get-char-property (point) 'face))
    (assert (eq erg (get-char-property (point) 'face)) nil "dq-in-tqs-string-lp:328813 test failed ")
    (goto-char 122)))

(defun fill-paragraph-problems-lp:710373-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
(let ((teststring "
    \"\"\"
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    \"\"\"
"))
  (fill-paragraph-problems-lp:710373-test-intern arg teststring)))

(defun fill-paragraph-problems-lp:710373-test-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (let ((tmp-dir "/tmp/")
        (fpp-exec-buffer "fill-paragraph-problems-lp:710373")
        (diff-buffer "fpp-lp:710373-old"))
    (set-buffer (get-buffer-create diff-buffer))
    (erase-buffer)
    (fundamental-mode)
    (insert teststring)
    (write-file (concat tmp-dir diff-buffer))
    (if arg
        (progn
          (set-buffer (get-buffer-create fpp-exec-buffer))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert teststring)
          (fundamental-mode)
          (fill-paragraph-problems-lp:710373-test-base tmp-dir fpp-exec-buffer diff-buffer))
      (with-temp-buffer
        (insert teststring)
        (fill-paragraph-problems-lp:710373-test-base tmp-dir fpp-exec-buffer diff-buffer)))))
  
(defun fill-paragraph-problems-lp:710373-test-base (tmp-dir fpp-exec-buffer diff-buffer)
    (goto-char 48)
  ;; the following lines work when called from edebug
  ;;  (message "%s" (get-text-property 6 'syntax-table))
  ;;  (assert (eq 15 (car (get-text-property 6 'syntax-table))))
  ;;  (assert (eq 6 (nth 8 (parse-partial-sexp (point-min) (point)))))
  (if (functionp 'py-fill-paragraph)
      (py-fill-paragraph)
    (python-fill-paragraph))
  (write-file (concat tmp-dir fpp-exec-buffer))
  (diff (concat tmp-dir fpp-exec-buffer) (concat tmp-dir diff-buffer) "-u")
  (if (featurep 'xemacs)
      (progn
        (set-buffer "*Diff Output*")
        (switch-to-buffer (current-buffer)))
    (set-buffer "*Diff*")
    (sit-for 1)
    (assert (numberp (progn (goto-char (point-min))(search-forward "no differences" nil t 1))) t)
    (message "%s" "fill-paragraph-problems-lp:710373 passed")))
  
(defun triple-quoted-string-dq-lp:302834-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-numbered-tests-intern 'triple-quoted-string-dq-lp:302834 arg teststring)))

(defun triple-quoted-string-dq-lp:302834 ()
        (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (insert "\"")
      (font-lock-fontify-buffer)
      (assert (eq erg (get-char-property (point) 'face)) "Being stuck inside triple-quoted-string. Did not reach beginning of class."))
    )

(defun multiline-assignment-indentation-lp:629916-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])
"))
    (py-bug-numbered-tests-intern 'multiline-assignment-indentation-lp:629916 arg teststring)))


(defun multiline-assignment-indentation-lp:629916 ()
    (goto-char (point-min))
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 27 (current-indentation)) nil "multiline-assignment-indentation-lp:629916 test failed")
  (end-of-line)
  (search-backward "[")
  (newline)
  (indent-according-to-mode)
  (assert (eq 27 (current-indentation)) nil "multiline-assignment-indentation-lp:629916 test failed")
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 28 (current-indentation)) nil "multiline-assignment-indentation-lp:629916 test failed")
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 28 (current-indentation)) nil "multiline-assignment-indentation-lp:629916 test failed"))

(defun previous-statement-lp:637955-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-numbered-tests-intern 'previous-statement-lp:637955 arg teststring)))

(defun previous-statement-lp:637955 ()
  (beginning-of-line)
  (py-previous-statement)
  (assert (eq 31 (point)) nil "Being stuck inside triple-quoted-string 637955 test. Did not reach beginning of class.")
  )

(defun nested-indents-lp:328775-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"
elif x < 0:
    print \"x is negative\"
"))
    (py-bug-numbered-tests-intern 'nested-indents-lp:328775 arg teststring)))

(defun nested-indents-lp:328775 ()
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (assert (eq 4 (py-compute-indentation t)) nil "nested-indents-lp:328775 test failed!")
  (goto-char 41)
  (assert (eq 8 (py-compute-indentation t)) nil "nested-indents-lp:328775 test failed!")
  (forward-line 1)
  (assert (eq 4 (py-compute-indentation t)) nil "nested-indents-lp:328775 test failed!")
  )

(defun bullet-lists-in-comments-lp:328782-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring))
    (bullet-lists-in-comments-lp:328782-test-intern arg teststring)))

(defun bullet-lists-in-comments-lp:328782-test-intern (&optional arg teststring)
  (set-buffer (get-buffer-create "bullet-lists-in-comments-lp:328782-test"))
  (erase-buffer)
  ;;     (with-temp-buffer
  (insert "
## * If the filename is a directory and not a Maildir nor
##   an MH Mailbox, it will be processed as a Mailbox --this bug named here: bullet-lists-in-comments-lp:328782.htm--
##   directory consisting of just .txt and .lorien files.
")
  (when arg (switch-to-buffer (current-buffer)))
    (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (goto-char 100)
  (if (functionp 'py-fill-paragraph)
      (py-fill-paragraph)
    (python-fill-paragraph)
    ))

(defun indentation-of-continuation-lines-lp:691185-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "    def f(val):
        # current behavior - indent to just after the first space
        a_verry_loonng_variable_nammmee = \\
                                        val
"))
    (py-bug-numbered-tests-intern 'indentation-of-continuation-lines-lp:691185 arg teststring)))

(defun indentation-of-continuation-lines-lp:691185 ()
  (goto-char (point-min))
  (forward-line 3)
  (indent-according-to-mode)
  (assert (eq 10 (current-indentation)) nil "indentation-of-continuation-lines-lp:691185-test failed!"))

(defun goto-beginning-of-tqs-lp:735328-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class Foo(object):
\"\"\"
This docstring isn't indented, test should pass anyway.
\"\"\"
"))
    (py-bug-numbered-tests-intern 'goto-beginning-of-tqs-lp:735328 arg teststring)))

(defun goto-beginning-of-tqs-lp:735328 ()
  (goto-char (point-min))
  (forward-line 4)
  (indent-according-to-mode)
  (assert (eq 4 (current-column)) nil "goto-beginning-of-tqs-lp:735328-test failed")
  )

(defun class-treated-as-keyword-lp:709478-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo = [
    T.div(
        T.tabl(*trows),

        CLASS='blok',)
]
"))
    (py-bug-numbered-tests-intern 'class-treated-as-keyword-lp:709478 arg teststring)))

(defun class-treated-as-keyword-lp:709478 ()
  (font-lock-fontify-buffer)
  (goto-char 64)
  (assert (eq (get-char-property (point) 'face) 'font-lock-string-face) nil "class-treated-as-keyword-lp:709478d test failed")
  (goto-char 57)
  (assert (not (get-char-property (point) 'face)) nil "class-treated-as-keyword-lp:709478-test failed")
  )


(defun fore-00007F-breaks-indentation-lp:328788-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class a:
    def __init__(self):
        self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
            self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                    self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'fore-00007F-breaks-indentation-lp:328788 arg teststring)))

(defun fore-00007F-breaks-indentation-lp:328788 ()
    (goto-char (point-min))
    (forward-line 1)
    (indent-according-to-mode)
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation (point-min) (point))) nil "fore-00007F-breaks-indentation test failed") 
    (indent-according-to-mode)
    (forward-line 1)
    (indent-according-to-mode)
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation (point-min) (point))) nil "fore-00007F-breaks-indentation test failed")
    (indent-according-to-mode)
    (forward-line 1)
    (indent-according-to-mode)
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation (point-min) (point))) nil "fore-00007F-breaks-indentation test failed")
    (indent-according-to-mode)
    (forward-line 1)
    (indent-according-to-mode)
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation (point-min) (point))) nil "fore-00007F-breaks-indentation test failed")
    (indent-according-to-mode)
    (forward-line 1)
    (indent-according-to-mode)
    )

(provide 'py-bug-numbered-tests)
;;; py-bug-numbered-tests.el ends here


