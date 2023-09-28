;;; robot-indent.el --- robot mode for emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Samuel Dawant

;; Author: Samuel Dawant <samueld@mailo.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Gather all utils for robot mode

;;; Code:

(defun robot-utils-current-header-block ()
  "Get the current header-block name"
  (save-excursion
    (re-search-backward "[*][*][*]  *\\([^*]+\\)  *[*][*][*]" nil t)
    (match-string 1)))

(defun robot-utils-get-line-alignment (&optional number)
  "Return list of all arguments START and END column number in the form of (START . END)"
  (save-excursion
    (forward-line (or number 0))
    (while (or (looking-at "[[:space:]]*\\(#.*\\)*$") (bobp)) ;; skip empty or comments lines
      (forward-line (or number -1)))
    (if (looking-at "[^ ]")
      nil
      (let ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
            (next-col 0))
        (mapcar
         (lambda (arg)
           (setq next-col (string-match (regexp-quote arg) current-line next-col))
           (cons next-col (match-end 0)))
         (split-string current-line "   *" t))))))


(provide 'robot-utils)
;;; robot-utils.el ends here
