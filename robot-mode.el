;;; robot-mode.el --- Major-mode for Robot Framework files -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 26 Nov 2020
;; Keywords: languages files
;; URL: https://github.com/kopoli/robot-mode
;; Version: 0.8.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ### Description

;; A Robot Framework major mode for Emacs. Robot Framework is a framework for
;; acceptance testing.

;; - https://robotframework.org
;; - https://robotframework.org/robotframework/latest/RobotFrameworkUserGuide.html

;; This major mode provides the following:
;; - Syntax highlighting.
;; - Indentation.
;; - Alignment of keyword contents.
;; - Line continuation in Robot Framework syntax.
;; - A helper for adding necessary spaces between arguments.

;; #### Alignment of keyword contents

;; Align the contents of a keyword, test or task with `C-c C-a'. It changes the
;; following code:

;;     Example Keyword
;;         [Documentation]    Documents the keyword
;;         [Arguments]    ${arg1}    ${arg2}
;;         Log    ${arg1}            ${arg2}

;; To:

;;     Example Keyword
;;         [Documentation]    Documents the keyword
;;         [Arguments]        ${arg1}    ${arg2}
;;         Log                ${arg1}    ${arg2}

;; #### Line continuation

;; Insert a newline, indentation, ellipsis and necessary spaces at current
;; point with `C-c C-j'. For example (| denotes the cursor):

;;     Another Keyword
;;         [Documentation]    A very long text| that describes the keyword.

;; To:

;;     Another Keyword
;;         [Documentation]    A very long text
;;         ...    |that describes the keyword.

;; #### Add spacing for an argument

;; Robot framework separates arguments to keywords with 2 or more spaces. The
;; `C-c C-SPC' sets the whitespace amount around point to exactly
;; `robot-mode-argument-separator'. For example (| denotes the cursor):

;;     Example Keyword
;;         [Arguments]    ${first}|${second}

;; To:

;;     Example Keyword
;;         [Arguments]    ${first}    |${second}

;; ### Limitations

;; - Currently supports only the Space separated format:
;;   https://robotframework.org/robotframework/latest/RobotFrameworkUserGuide.html#space-separated-format
;; - Does NOT support the Pipe separated format or the reStructuredText
;;   format.

;; ### Notable changes

;; Version 0.8.0

;; - Add `robot-mode-retain-point-on-indent' option to retain point position
;;   when indenting. Currently it is disabled by default, but may be enabled
;;   in the future.
;; - Prevent indent toggling from interfering with TAB completion when
;;   `tab-always-indent' is set to 'complete and the above point retention is
;;   used.
;; - Fix several indent toggling bugs:
;;   - Set `electric-indent-inhibit'.
;;   - Disable when aligning by using variable `robot-mode-indent-toggle'.
;; - Fix syntax highlighting and indentation when a control structure word is
;;   the first word in a keyword.
;; - Fix various other syntax highlighting bugs.

;; Version 0.7.0

;; - Add control structure indentation (IF/WHILE/FOR/TRY etc.).

;;; Code:

(require 'align)

(defgroup robot nil
  "Major mode for editing Robot Framework files."
  :prefix "robot-mode-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/kopoli/robot-mode")
  :link '(url-link :tag "Documentation" "https://robotframework.org/robotframework/latest/RobotFrameworkUserGuide.html"))

(defcustom robot-mode-basic-offset standard-indent
  "The amount of indentation for test and keyword steps."
  :type 'integer
  :group 'robot
  :safe 'integerp)

(defcustom robot-mode-argument-separator standard-indent
  "The amount of spaces between different arguments to keywords."
  :type 'integer
  :group 'robot
  :safe 'integerp)

(defcustom robot-mode-retain-point-on-indent nil
  "If the `point' position is after the indentation, retain it when
indenting a line. Otherwise move `point' always `back-to-indentation'."
  :type 'boolean
  :group 'robot
  :safe 'booleanp)

(defvar robot-mode-font-lock-keywords
  '(("^\\*.*" . font-lock-keyword-face)
    ("\\[\\sw+\\]" . font-lock-constant-face)
    ("^\\s-*\\(\\.\\.\\.\\)" . (1 font-lock-constant-face))
    ("[@$&%]{[^}]*}" . font-lock-variable-name-face)
    ("^\\(Library\\|Resource\\|\\(Suite\\|Task\\|Test\\) \\(Setup\\|Teardown\\|Template\\|Timeout\\)\\|Variables\\):?\\s-*\\(.*?\\)\\(\\s-\\{2,\\}\\|$\\)"
     (1 font-lock-preprocessor-face) (4 font-lock-constant-face append))
    ("^\\(Documentation\\|\\(Force \\|Default \\)Tags\\|Metadata\\):?\\s-*\\(.*\\)"
     (1 font-lock-preprocessor-face) (3 font-lock-doc-face))
    ("[@$&%]{\\([+-]?\\(0[xbo]\\)?[0-9.a-f]+\\|true\\|false\\|None\\|null\\|EMPTY\\|SPACE\\)}" . font-lock-constant-face)
    ("\\([$]{{[^}]*}}\\)\\|^\\s-+\\(IF\\|ELSE IF\\|ELSE\\|END\\|FOR\\|WHILE\\|TRY\\|EXCEPT\\|RETURN\\|BREAK\\|CONTINUE\\|FINALLY\\)\\(\\s-\\{2,\\}\\|$\\)"
     . font-lock-builtin-face)
    ("^[[:alnum:]]+.*$" . font-lock-function-name-face))
  "Default `font-lock-keywords' for Robot mode.")

(defvar robot-mode-syntax-table
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (syntax-table))
  "Syntax table for Robot mode.")

(defvar robot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'robot-mode-align-region-or-defun)
    (define-key map (kbd "C-c C-j") #'robot-mode-split-continuation)
    (define-key map (kbd "C-c C-SPC") #'robot-mode-add-argument)
    map)
  "Key map for Robot mode.")

(defun robot-mode-syntax-propertize (start end)
  "Propertize text between START and END."
  (funcall
   (syntax-propertize-rules
    ;; Single space between non-space characters is part of the symbol syntax
    ("[[:alnum:]]\\( \\)[[:alnum:]]" (1 "_")))
   start end))

(defvar robot-mode-indent-toggle t
  "Should the indentation be removed when already indented.")

(defun robot-mode--back-to-previous-line ()
  "Move point to the previous non-empty, non-comment line."
  (beginning-of-line)
  (re-search-backward "^\\s-*[^#[:space:][:cntrl:]]+" nil t)
  (back-to-indentation))

(defun robot-mode-indent-line ()
  "Indent current line in Robot mode.

Used as `indent-line-function' of the mode."
  (interactive)
  (let* ((indent 0)
	 ;; Get the current section
	 (section
	  (downcase (or (save-excursion
			 (re-search-backward "^\\s-*\\*+\\s-*\\([a-zA-Z ]+\\)" nil t)
			 (match-string-no-properties 1)) "")))

	 ;; The non-indented contents of previous non-empty line
	 previous-line

	 ;; The amount of indent of previous non-empty line
	 (previous-indent
	  (save-excursion
	    (robot-mode--back-to-previous-line)
	    (setq previous-line (buffer-substring-no-properties
				 (point) (line-end-position)))
	    (current-indentation)))

	 ;; The non-indented contents of the current line
	 current-line

	 ;; The indentation level of the current line
	 (current-indent
	  (save-excursion
	    (back-to-indentation)
	    (setq current-line (buffer-substring-no-properties
				(point) (line-end-position)))
	    (current-indentation))))

    (setq indent
	  (cond ((or
		  ;; Don't indent if not in the below sections
		  (not (string-match "task.*\\|test case.*\\|keyword.*" section))
		  ;; Don't indent the section line
		  (string-match "^\\*" current-line)
		  ;; Don't indent the line after a section line
		  (string-match "^\\*" previous-line))
		 0)

		;; If the current line contains an inline IF, don't increase indent
		((string-match "^\\s-*IF\\s-\\{2,\\}[^[:space:]]+\\s-\\{2,\\}[^#[:space:]]" previous-line)
		 previous-indent)

		;; If previous line contains control structures, increase the
		;; indentation level
		((string-match "^\\s-*\\(IF\\|ELSE IF\\|ELSE\\|FOR\\|WHILE\\|TRY\\|EXCEPT\\|FINALLY\\)\\(\\s-\\{2,\\}.*\\|\\s-*$\\)" previous-line)
		 (+ previous-indent robot-mode-basic-offset))

		;; Decrease indentation on control structures that end a block
		((string-match"\\(END\\|ELSE IF\\|ELSE\\|EXCEPT\\|FINALLY\\)" current-line)
		 (max robot-mode-basic-offset (- previous-indent robot-mode-basic-offset)))

		;; If previous line is indented, indent to that level
		((> previous-indent 0)
		 previous-indent)

		;; Otherwise indent to basic offset
		(t
		 robot-mode-basic-offset)))

    ;; Toggle indentation if the line is already indented
    (when (and robot-mode-indent-toggle
	       (> indent 0)
	       (= indent current-indent)
	       ;; Prevent toggling if point is retained, TAB also does
	       ;; completion, point is after indentation and previous command
	       ;; was also an indentation.
	       (not (and robot-mode-retain-point-on-indent
			 (eq tab-always-indent 'complete)
			 (> (current-column) current-indent)
			 (equal this-command last-command))))
      (setq indent 0))

    (when (not robot-mode-retain-point-on-indent)
      (back-to-indentation))

    ;; Save point if point retention is enabled and point is after indentation
    (if (and (> (current-column) current-indent)
	     robot-mode-retain-point-on-indent)
	(save-excursion
	  (indent-line-to indent))
      (indent-line-to indent))))

(defun robot-mode-beginning-of-defun ()
  "Move the point to the beginning of the current defun.

Defuns are the steps of a keyword, test or task. This is used as
`beginning-of-defun-function' of the mode."
  (re-search-backward "^[[:graph:]]" nil t))

(defun robot-mode-end-of-defun ()
  "Move the point to the end of the current defun.

Defuns are the steps of a keyword, test or task. This is used as
`end-of-defun-function' of the mode."
  (end-of-line)
  (if (re-search-forward "^[[:graph:]]" nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun robot-mode-align (beg end)
  "Align the contents of the region between BEG and END."
  (interactive
   (list (region-beginning) (region-end)))

  ;; Align only with spaces
  (let ((align-to-tab-stop nil))
    (align-regexp beg end "\\(\\s-\\s-+\\)"  1 robot-mode-argument-separator t))
  (let ((robot-mode-indent-toggle nil))
    (indent-region beg end)))

(defun robot-mode-align-defun ()
  "Align the contents current defun."
  (interactive)
  (let ((beg (save-excursion
		(beginning-of-defun)
		(forward-line)
		(point)))
	(end (save-excursion
	       (end-of-defun)
	       (point))))
    (robot-mode-align beg end)))

(defun robot-mode-align-region-or-defun ()
  "Call `robot-mode-align' if region is active, otherwise `robot-mode-align-defun'."
  (interactive)
  (if (region-active-p)
      (robot-mode-align (region-beginning) (region-end))
    (robot-mode-align-defun)))

(defun robot-mode-split-continuation ()
  "Split current line at point and continue in the next line.

Prefix the continuation with indentation, ellipsis and spacing."
  (interactive)
  ;; If point is between the indentation and beginning of line add the
  ;; ellipsis to the previous line. Otherwise add to the next line.
  (if (not (<= (line-beginning-position)
	       (point)
	       (save-excursion
		 (back-to-indentation) (point))))
      (progn
	(delete-horizontal-space)
	(newline))
    (beginning-of-line)
    (newline)
    (forward-line -1))
  (insert "...")
  (insert (make-string robot-mode-argument-separator ? ))
  (indent-region (line-beginning-position) (line-end-position)))

(defun robot-mode-add-argument ()
  "Add exactly `robot-mode-argument-separator' spaces to point."
  (interactive)
  (delete-horizontal-space)
  (insert (make-string robot-mode-argument-separator ? )))

;;;###autoload
(define-derived-mode robot-mode prog-mode "Robot"
  "Major mode for editing Robot framework files.

\\{robot-mode-map}"

  (setq-local indent-line-function #'robot-mode-indent-line)
  (setq-local electric-indent-inhibit t)
  (setq-local font-lock-defaults '(robot-mode-font-lock-keywords nil t))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local beginning-of-defun-function #'robot-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'robot-mode-end-of-defun)
  (setq-local syntax-propertize-function #'robot-mode-syntax-propertize)
  ;; Due to to the symbol syntax expanding to single space between non-space
  ;; characters, broaden the whitespace definition of isearch to include
  ;; literal spaces. This fixes the isearch-forward for strings containing
  ;; spaces.
  (setq-local search-whitespace-regexp "\\(\\s-\\| \\)+")
  (setq-local outline-regexp "^\\*\\|^\\sw"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(robot\\|resource\\)\\'" . robot-mode))

(provide 'robot-mode)

;;; robot-mode.el ends here
