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
         'dq-in-tqs-string-lp:328813-test
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

(defun nested-dictionaries-indent-lp:328791-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring "
    d = {'a':{'b':3,
              'c':4}}
"))
    (nested-dictionaries-indent-lp:328791-intern arg teststring)))

(defun nested-dictionaries-indent-lp:328791-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (if arg
      (progn
        (set-buffer (get-buffer-create "nested-dictionaries-indent-lp:328791"))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (nested-dictionaries-indent-lp:328791-base))
    (with-temp-buffer
      (insert teststring)
      (nested-dictionaries-indent-lp:328791-base))))

(defun nested-dictionaries-indent-lp:328791-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 2)
  (assert (eq 14 (py-compute-indentation t)))
  (message "%s" "nested-dictionaries-indent-lp:328791-test passed"))

(defun mark-block-region-lp:328806-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring "def f():
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
  (mark-block-region-lp:328806-intern arg teststring)))

(defun mark-block-region-lp:328806-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (if arg
      (progn
        (set-buffer (get-buffer-create "mark-block-region-lp:328806"))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (mark-block-region-lp:328806-base))
    (with-temp-buffer
      (insert teststring)
      (mark-block-region-lp:328806-base))))

(defun mark-block-region-lp:328806-base ()
  (python-mode)
  (forward-line -2)
  (py-mark-block)
  (assert (< (region-beginning) (region-end)) nil "mark-block-region-lp:328806 test failed!")
  (message "%s" "mark-block-region-lp:328806 test passed"))

(defun dq-in-tqs-string-lp:328813-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring "
# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''
"))
    (dq-in-tqs-string-lp:328813-test-intern arg teststring)))

(defun dq-in-tqs-string-lp:328813-test-intern (&optional arg teststring)
  (set-buffer (get-buffer-create "tqs-string-lp:328813-test"))
  (erase-buffer)
  (insert teststring)
  (when arg (switch-to-buffer (current-buffer)))
  (dq-in-tqs-string-lp:328813-test-base))

(defun dq-in-tqs-string-lp:328813-test-base ()
  (python-mode)
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (goto-char 78)
  (lexical-let ((erg (face-at-point)))
    (insert "\"")
    (assert (ar-triplequoted-in-p-atpt) nil "tqs-string-lp:328813 failed: In triplequoted string not recognised!")
    (font-lock-fontify-buffer)
    (assert (eq erg (face-at-point)) "Being stuck inside triple-quoted-string 328813 test. "))
  (goto-char 122)
  (assert (ar-triplequoted-in-p-atpt) nil "tqs-string-lp:328813 failed: In triplequoted at point 122 not recognised!")
  (when arg (message "%s" "dq-in-tqs-string-lp:328813-test passed")))

(defun fill-paragraph-problems-lp:710373-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
(lexical-let ((teststring "
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
  (python-mode)
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
  (lexical-let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (triple-quoted-string-dq-lp:302834-test-intern arg teststring)))

(defun triple-quoted-string-dq-lp:302834-test-intern (&optional arg teststring)
  (with-temp-buffer
    (insert teststring)
    (when arg (switch-to-buffer (current-buffer)))
    (python-mode)
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (lexical-let ((erg (face-at-point)))
      (insert "\"")
      (assert (ar-triplequoted-dq-in-p-atpt) nil "In triplequoted string not recognised!")
      (font-lock-fontify-buffer)
      (assert (eq erg (face-at-point)) "Being stuck inside triple-quoted-string. Did not reach beginning of class."))
    (when arg (message "%s" "triple-quoted-string-dq-lp:302834-test passed"))))

(defun multiline-assignment-indentation-lp:629916-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring "foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])
"))
    (multiline-assignment-indentation-lp:629916-intern arg teststring)))

(defun multiline-assignment-indentation-lp:629916-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (if arg
      (progn
        (set-buffer (get-buffer-create "multiline-assignment-indentation-lp:629916-test"))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (multiline-assignment-indentation-lp:629916-base))
    (with-temp-buffer
      (insert teststring)
      (multiline-assignment-indentation-lp:629916-base))))

(defun multiline-assignment-indentation-lp:629916-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 27 (current-indentation)) nil "multiline-assignment-indentation-lp:629916-test fails")
  (end-of-line)
  (search-backward "[")
  (newline)
  (indent-according-to-mode)
  (assert (eq 27 (current-indentation)) nil "multiline-assignment-indentation-lp:629916-test fails")
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 28 (current-indentation)) nil "multiline-assignment-indentation-lp:629916-test fails")
  (forward-line 1)
  (indent-according-to-mode)
  (assert (eq 28 (current-indentation)) nil "multiline-assignment-indentation-lp:629916-test fails")
  (message "%s" "multiline-assignment-indentation-lp:629916-test passed"))

(defun previous-statement-lp:637955-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring))
    (previous-statement-lp:637955-test-intern arg teststring)))

(defun previous-statement-lp:637955-test-intern (&optional arg teststring)
  (set-buffer (get-buffer-create "previous-statement-lp:637955-test"))
  (erase-buffer)
  (insert "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"")
  (when arg (switch-to-buffer (current-buffer)))
  (beginning-of-line)
  (py-previous-statement)
  (assert (eq 31 (point)) nil "Being stuck inside triple-quoted-string 637955 test. Did not reach beginning of class.")
  (when arg (message "%s" "previous-statement-lp:637955-test passed")))

(defun nested-indents-lp:328775-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring))
    (nested-indents-lp:328775-test-intern arg teststring)))

(defun nested-indents-lp:328775-test-intern (&optional arg teststring)
  (set-buffer (get-buffer-create "nested-indents-lp:328775"))
  (erase-buffer)
  (when arg (switch-to-buffer (current-buffer)))
  (insert "
if x > 0:
    for i in range(100):
        print i
")
  (save-excursion (insert "else:
    print \"All done\""))
  (when arg (switch-to-buffer (current-buffer)))
  (python-mode)
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (assert (eq 4 (py-compute-indentation t)) nil "nested-indents-lp:328775-test fails!")
  (indent-according-to-mode)
  (goto-char (point-max))
  (save-excursion (insert "\nelif x < 0:
    print \"x is negative\""))
  (assert (eq 8 (py-compute-indentation t)) nil "nested-indents-lp:328775-test fails!")
  (forward-line 1)
  (assert (eq 0 (py-compute-indentation t)) nil "nested-indents-lp:328775-test fails!")
  (message "%s" "nested-indents-lp:328775-test passed"))

(defun bullet-lists-in-comments-lp:328782-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring))
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
  (python-mode)
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
  (lexical-let ((teststring "    def f(val):
        # current behavior - indent to just after the first space
        a_verry_loonng_variable_nammmee = \\
                                        val
"))
    (indentation-of-continuation-lines-lp:691185-test-intern arg teststring)))

(defun indentation-of-continuation-lines-lp:691185-test-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (if arg
      (progn
        (set-buffer (get-buffer-create "indentation-of-continuation-lines-lp:691185-test"))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (indentation-of-continuation-lines-lp:691185-test-base))
    (with-temp-buffer
      (insert teststring)
      (indentation-of-continuation-lines-lp:691185-test-base))))

(defun indentation-of-continuation-lines-lp:691185-test-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 3)
  (indent-according-to-mode)
  (assert (eq 10 (current-indentation)) nil "indentation-of-continuation-lines-lp:691185-test failed!")
  (message "%s"  "indentation-of-continuation-lines-lp:691185-test passed"))

(defun goto-beginning-of-tqs-lp:735328-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
(lexical-let ((teststring "class Foo(object):
\"\"\"
This docstring isn't indented, test should pass anyway.
\"\"\"
"))
  (goto-beginning-of-tqs-lp:735328-test-intern arg teststring)))

(defun goto-beginning-of-tqs-lp:735328-test-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (if arg
      (progn
        (set-buffer (get-buffer-create "goto-beginning-of-tqs-lp:735328-test"))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (goto-beginning-of-tqs-lp:735328-test-base))
    (with-temp-buffer
      (insert teststring)
      (goto-beginning-of-tqs-lp:735328-test-base))))

(defun goto-beginning-of-tqs-lp:735328-test-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 4)
  (indent-according-to-mode)
  (assert (eq 4 (current-column)) nil "goto-beginning-of-tqs-lp:735328-test failed")
  (message "goto-beginning-of-tqs-lp:735328-test passed"))

(defun class-treated-as-keyword-lp:709478-test (&optional arg load-branch-function)
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (lexical-let ((teststring "foo = [
    T.div(
        T.tabl(*trows),

        CLASS='blok',)
]
"))
    (class-treated-as-keyword-lp:709478-intern arg teststring)))

(defun class-treated-as-keyword-lp:709478-intern (&optional arg teststring)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
    (if arg
        (progn
          (set-buffer (get-buffer-create "class-treated-as-keyword-lp:709478"))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert teststring)
          (fundamental-mode)
          (class-treated-as-keyword-lp:709478-base))
      (with-temp-buffer
        (insert teststring)
        (class-treated-as-keyword-lp:709478-base))))

(defun class-treated-as-keyword-lp:709478-base ()
  (python-mode)
  (font-lock-fontify-buffer)
  (goto-char 64)
  (assert (eq (get-char-property (point) 'face) 'font-lock-string-face) nil "class-treated-as-keyword-lp:709478d- test failed")
  (goto-char 57)
  (assert (not (get-char-property (point) 'face)) nil "class-treated-as-keyword-lp:709478-test failed")
  (message "%s"  "class-treated-as-keyword-lp:709478-test passed"))


(provide 'py-bug-numbered-tests)
;;; py-bug-numbered-tests.el ends here

