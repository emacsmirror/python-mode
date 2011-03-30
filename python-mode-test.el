;;; python-mode-test.el --- tests for Emacs python-mode.el

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: lisp, languages

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

;; A couple of test cases for python-mode.el

;;; Code:

(setq python-mode-tests
      (if (featurep 'xemacs)
          (list
           'py-beginning-of-block-or-clause-test)
        (list
         'py-beginning-of-block-test
         'py-end-of-block-test
         'py-beginning-of-block-or-clause-test
         'py-end-of-block-or-clause-test
         'py-beginning-of-def-test
         'py-end-of-def-test
         'py-beginning-of-def-or-class-test
         'py-end-of-def-or-class-test)))

(defun py-run-tests (&optional arg)
  (interactive "p")
  (dolist (ele python-mode-tests)
    (funcall ele arg)))

(defun py-beginning-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-numbered-tests-intern 'py-beginning-of-block-base arg teststring)))

(defun py-beginning-of-block-base ()
  (goto-char (point-max))
  (py-beginning-of-block)
  (assert (looking-at "if") nil "py-beginning-of-block test failed"))

(defun py-end-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'py-end-of-block-base arg teststring)))

(defun py-end-of-block-base ()
  (py-beginning-of-block)
  (py-end-of-block)
  (assert (eq (point) 556) nil "py-end-of-block test failed"))

(defun py-beginning-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])"
                    ))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-numbered-tests-intern 'py-beginning-of-block-or-clause-base arg teststring)))

(defun py-beginning-of-block-or-clause-base ()
    (goto-char (point-max))
    (py-beginning-of-block-or-clause)
    (assert (looking-at "if") nil "py-beginning-of-block-or-clause test failed"))


(defun py-end-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'py-end-of-block-or-clause-base arg teststring)))

(defun py-end-of-block-or-clause-base ()
    (py-beginning-of-block-or-clause)
    (py-end-of-block-or-clause)
    (assert (eq (point) 556) nil "py-end-of-block-or-clause test failed"))

(defun py-beginning-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'py-beginning-of-def-base arg teststring)))

(defun py-beginning-of-def-base ()
  (py-beginning-of-def)
  (assert (eq (point) 238) nil "py-beginning-of-def test failed")
    )

(defun py-end-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'py-end-of-def-base arg teststring)))

(defun py-end-of-def-base ()
    (py-beginning-of-def)
    (py-end-of-def)
    (assert (eq (point) 556) nil "py-end-of-def test failed")  
    )

(defun py-beginning-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-numbered-tests-intern 'py-beginning-of-def-or-class-base arg teststring)))

(defun py-beginning-of-def-or-class-base ()
  (py-beginning-of-def-or-class t)
  (assert (eq (point) 1) nil "py-beginning-of-def-or-class test failed"))

(defun py-end-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-numbered-tests-intern 'py-end-of-def-or-class-base arg teststring)))

(defun py-end-of-def-or-class-base ()
  (py-beginning-of-def-or-class t)
  (py-end-of-def-or-class t)
  (assert (eq (point) 556) nil "py-end-of-def-or-class test failed"))

(provide 'python-mode-test)
;;; python-mode-test.el ends here
