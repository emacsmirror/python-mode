;;; py-shell-completion-tests.el --- Test completion for available Python shell

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Commentary: Edit `py-test-pyshellname-list' before
;; running this test-builder or give a list of shells as
;; arguments

;;; Code:

(setq python-mode-shell-complete-tests
      (list

       'python-shell-complete-test
       'usr-bin-python-shell-complete-test
       'usr-bin-python2.7-shell-complete-test
       'arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test
       'usr-bin-python3-shell-complete-test
       'usr-bin-python3.1-shell-complete-test
       'ipython-shell-complete-test
       'usr-bin-ipython-shell-complete-test
       'arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test))

(defun py-run-shell-complete-tests ()
  (interactive)
  (dolist (ele python-mode-shell-complete-tests)
    (funcall ele)))

(defun python-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "python" nil "/"))
    (switch-to-buffer (current-buffer)) 
    (goto-char (point-max))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-python-test failed")
    (when py-verbose-p (message "%s" "py- python-test passed"))))


(defun usr-bin-python-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "/usr/bin/python" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-usr-bin-python-test failed")
    (when py-verbose-p (message "%s" "py- usr-bin-python-test passed"))))


(defun usr-bin-python2.7-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "/usr/bin/python2.7" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-usr-bin-python2.7-test failed")
    (when py-verbose-p (message "%s" "py- usr-bin-python2.7-test passed"))))


(defun arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "~/arbeit/python/epdfree/epd_free-7.2-2-rh5-x86/bin/python2.7" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-test failed")
    (when py-verbose-p (message "%s" "py- arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-test passed"))))


(defun usr-bin-python3-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "/usr/bin/python3" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-usr-bin-python3-test failed")
    (when py-verbose-p (message "%s" "py- usr-bin-python3-test passed"))))


(defun usr-bin-python3.1-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "/usr/bin/python3.1" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-usr-bin-python3.1-test failed")
    (when py-verbose-p (message "%s" "py- usr-bin-python3.1-test passed"))))


(defun ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "ipython" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-ipython-test failed")
    (when py-verbose-p (message "%s" "py- ipython-test passed"))))


(defun usr-bin-ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "/usr/bin/ipython" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-usr-bin-ipython-test failed")
    (when py-verbose-p (message "%s" "py- usr-bin-ipython-test passed"))))


(defun arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil nil "~/arbeit/python/epd_free-7.1-2-rh5-x86/bin/ipython" nil "/"))
    (goto-char (point-max))
    (comint-send-input)
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "pri")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at "print") nil "py-arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-test failed")
    (when py-verbose-p (message "%s" "py- arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-test passed"))))



(provide 'py-shell-completion-tests)
;;; py-shell-completion-tests ends here
 
