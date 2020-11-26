;;; robot-mode.el --- Robot framework major-mode

;; Copyright (C) 2020 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 26 Nov 2020
;; Keywords: major-mode
;; Version: 0.1.0

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defgroup robot-mode nil
  ""
  )

(defcustom robot-mode-basic-offset standard-indent
  "")

(defvar robot-mode-font-lock-keywords
  '(("#.*" . font-lock-comment-face)
    ("^\\*.*" . font-lock-keyword-face)
    ("\\[\\sw+\\]" . font-lock-constant-face)
    ("\\.\\.\\." . font-lock-constant-face)
    ("^\\(Library\\|Resource\\|Suite Setup\\)\s-*\\(.*\\)"
     (1 font-lock-preprocessor-face t) (2 font-lock-constant-face t))
    ("^\\(Documentation\\|Tags\\)\s-*\\(.*\\)"
     (1 font-lock-preprocessor-face t) (2 font-lock-string-face t))
    ("[@$&%]{\\([0-9]+\\|true\\|false\\)}" . font-lock-constant-face)
    ("[@$&%]{[^}]*}" . font-lock-variable-name-face)
    ("^[^ \t].*" . font-lock-function-name-face)
    )
  "")

(defvar robot-mode-syntax-table
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (syntax-table))
  "")

(defvar robot-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map )
    map)
  "")

(defun robot-mode-indent-line ()
  (interactive)
  (let* ((indent 0)
	 (section
	  (downcase (or (save-excursion
			 (re-search-backward "^\\s-*\\*+\\s-*\\([a-zA-Z ]+\\)" nil t)
			 (match-string-no-properties 1)) "")))
	 (back-to-previous-line
	  (lambda ()
	    (beginning-of-line)
	    (re-search-backward "^\\s-*[[:print:]]" nil t)
	    (back-to-indentation)))
	 (previous-indent
	  (save-excursion
	    (funcall back-to-previous-line)
	    ;; (mssage "LINE STRING %d %s" (point) (buffer-substring (point) (line-end-position)))
	    (- (point) (line-beginning-position)))))

    ;; (message "PREVINDENT %s" previous-indent)

    ;; (message "Section on %s" section)

    (cond ((not (string-match "task.*\\|test case.*\\|keyword.*" section))
	   ;; indent only lines in the above section
	   (setq indent 0))

	  ;; header line should not be indented
	  ((save-excursion
	     (back-to-indentation)
	     (looking-at "\\*"))
	   (setq indent 0))

	  ((= previous-indent 0)
	   (save-excursion
	     (funcall back-to-previous-line)
	     (setq indent
		   ;; If the previous line is not a header
		   (cond ((not (looking-at "^\\*"))
			  robot-mode-basic-offset)
			 (t 0)))))
	  (t
	   ;; If previous line is indented, indent to that level
	   (setq indent previous-indent)))

    ;; Toggle indentation if the line is already indented
    (when (and (> indent 0)
	       (= indent (- (point) (line-beginning-position)))
	       ;; (= (point) (line-end-position))

	       )
      (setq indent 0))

    (back-to-indentation)
    (delete-region (line-beginning-position)  (point))
    (indent-to indent)))

;; DEBUG
;; TEST command: emacs -Q --load ./robot-mode.el -- testfile.robot
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(resource\\|robot\\)\\'" . robot-mode))

;;;###autoload
(define-derived-mode robot-mode prog-mode "Robot"
  "Major mode for editing Robot framework files

\\{robot-mode-map}"

  (setq-local indent-line-function #'robot-mode-indent-line)
  (setq-local font-lock-defaults '(robot-mode-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local outline-regexp "^\\*\\*\\*\\|^\\sw"))

;;; robot-mode.el ends here
