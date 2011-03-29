#!/bin/bash
 # --

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# batch-script tests Emacs python-mode
#
# Code:

# Edit the vars pointing to the directories/files
# holding your python-mode for test

# assumes python-mode files in current directory

# the path 
PDIR=`pwd`

# python-mode file to load
if [ -s "python-components-mode.el" ];
    then
    PYTHONMODE="python-components-mode.el"
    else
    PYTHONMODE="python-mode.el"
fi

# file holding the tests
TESTFILE="py-bug-numbered-tests.el"

$HOME/emacs/src/emacs -Q --batch --eval "(message (emacs-version))" --eval "(when (featurep 'python-mode)(unload-feature 'python-mode t))" --eval "(add-to-list 'load-path \"$PDIR/\")" -load "$PDIR/$PYTHONMODE" -load "$PDIR/$TESTFILE" \
--funcall nested-dictionaries-indent-lp:328791-test \
--funcall triple-quoted-string-dq-lp:302834-test \
--funcall fore-00007F-breaks-indentation-lp:328788-test \
--funcall dq-in-tqs-string-lp:328813-test \
--funcall flexible-indentation-lp:328842-test \
--funcall beg-end-of-defun-lp:303622-test \
--funcall bullet-lists-in-comments-lp:328782-test \
--funcall nested-indents-lp:328775-test \
--funcall exceptions-not-highlighted-lp:473525-test \
--funcall previous-statement-lp:637955-test \
--funcall inbound-indentation-multiline-assignement-lp:629916-test \
--funcall indentation-of-continuation-lines-lp:691185-test \
--funcall goto-beginning-of-tqs-lp:735328-test \
--funcall class-treated-as-keyword-lp:709478-test \
--funcall backslashed-continuation-line-indent-lp:742993-test \
--funcall py-decorators-face-lp:744335-test

# fail as batch-skripts, but work from inside
# --funcall mark-block-region-lp:328806-test \
# fill-paragraph-problems-lp:710373-test

# -load "$HOME/werkstatt/sh-beg-end.el" \
