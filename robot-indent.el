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

;; Heavily inspired by gdscript mode indent functions which are based on python mode ones

;;; Code:

(defvar robot-defun-regexp "^[[:alnum:]]+.*$"
  "Regexp matching the line that define a keyword or a test case")

(defvar robot-header-regexp
  (rx line-start "*" (* ascii) line-end)
  "Regexp matching the heaer")

(defvar robot-special-keyword-regex
  (rx line-start
      (* (or space "\t"))
      (or "..." (and "[" (+ word) "]"))
      (* ascii))
  "Regexp for special [keywords]")

(defvar robot-block-start-regex
  (rx line-start
      (* (or space "\t"))
      (or "TRY"
       (and
         (or "WHILE" "IF" "FOR")
         (or (>= 2 " ") "\t") (* ascii)))
      line-end)
  "Regexp Matching end of block")

(defvar robot-dedenter-block-regex
  (rx line-start
      (* (or space "\t"))
      (or (or "FINALLY" "ELSE") (and (or "EXCEPT" "ELSE IF") (+ (or space "\t")) (* ascii)))
      (* (or space "\t"))
      line-end)
  "Regexp Matching dedenter of block")

(defvar robot-block-end-regex
  (rx line-start (+ (or space "\t")) "END" (* (or space "\t")) line-end)
  "Regexp Matching end of block")

(defun robot-indent-line-function ()
  "`indent-line-function' for Gdscript mode.
When the variable `last-command' is equal to `indent-for-tab-command'
it cycles possible indentation levels from right to left."
  (robot-indent-line
   (and (equal this-command 'indent-for-tab-command)
        (eq last-command this-command))))

(defun robot-indent-line (&optional previous)
  "Internal implementation of `robot-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (robot-indent--calculate-indentation previous)))
    (when follow-indentation-p
      (back-to-indentation))))

(defun robot-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
             (= (current-indentation) (current-column)))
    (let* ((ril robot-mode-indent-level)
           (previous-column (* ril (1- (/ (current-column) ril )))))
      (while (and (not (bolp))
              (> (current-column) previous-column))
        (delete-char -1)))
    t))

(defun robot-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (robot-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(defun robot-indent--get-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:no-indent
 - When in Settings or Variables headers
:at-header (no-indent)
 - When point in a header
:in-settings-block (no-indent)
 - When point inside the settings block
:in-variables-block (no-indent)
 - When point inside the settings block

:after-defun
 - Point is after a keyword or test case defun
:at-special-keyword
 - Point is in a line that define a special keyword (with square brackets) or ...
 - START is the position of the previous line indentation
:at-block-end
 - Point is at line starting a block starter
 - START is the position of the previous line indentation
:at-block-end
 - Point is at line starting a block ender
 - START is the position of the previous line indentation
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block
 - Point is on a line starting a dedenter block.
 - START is the position where the previous line starts."

  (let ((current-header (robot-indent--get-current-header-block))
        (previous-line-start (save-excursion
                               (back-to-indentation)
                               (skip-chars-backward " \t\n")
                               (beginning-of-line)
                               (point))))
    (cond
     ;; Beginning of buffer.
     ((= (line-number-at-pos) 1)
      (cons :no-indent 0))
     ((robot-indent--check-line robot-header-regexp)
      (cons :at-header 0))
     ((string-equal-ignore-case current-header "variables")
      (cons :in-variables-block 0))
     ((string-equal-ignore-case current-header "settings")
      (cons :in-settings-block 0))
     ;; After defun, Keyword or Test Case
     ((robot-indent--check-line robot-defun-regexp -1)
      (cons :after-defun previous-line-start))
     ;; After block start
     ((or
       (robot-indent--check-line robot-dedenter-block-regex -1)
       (robot-indent--check-line robot-block-start-regex -1))
      (cons :after-block-start previous-line-start))
     ;; At special [keyword] or "..."
     ((robot-indent--check-line robot-special-keyword-regex)
      (cons :at-special-keyword previous-line-start))
     ;; At block start
     ((robot-indent--check-line robot-block-start-regex)
      (cons :at-block-start previous-line-start))
     ;; At block end
     ((robot-indent--check-line robot-block-end-regex)
      (cons :at-block-end previous-line-start))
     ;; At dendeter block
     ((robot-indent--check-line robot-dedenter-block-regex)
      (cons :at-dedenter-block previous-line-start))
     ;; After normal line, comment or ender (default case).
     ((save-excursion
        (back-to-indentation)
        (skip-chars-backward " \t\n")
        (if (bobp)
            (cons :no-indent 0)
          (beginning-of-line)
          (skip-chars-forward " \t")
          (cons :after-line (point))))))))

(defun robot-indent--calculate-indentations ()
  "Internal implementation of `robot-indent--calculate-indentation'.
Return the list of possible indentations based on context.
Note that it return a list only on the after-line context"
  (save-excursion
    (pcase (robot-indent--get-context)
      (`(,(or :no-indent
              :at-header
              :in-variables-block
              :in-settings-block) . ,_) (prog-first-column)) ; usually 0
      (`(,(or :after-defun ;; Add one indentation level
              :after-block-start)
         . ,start)
       (goto-char start)
       (+ (current-indentation) robot-mode-indent-level))
      (`(,(or :at-special-keyword
              :at-continuing-line
              :at-block-start)
         . ,start)
       ;; Copy previous indentation
       (goto-char start)
       (current-indentation))
      (`(:after-line . ,start)
       ;; Copy previous indentation but permit to go on the first level
       (goto-char start)
       (list 0 (current-indentation)))
      (`(,(or :at-block-end  ;; Remove one indentation level
              :at-dedenter-block)
         . ,start)
       (goto-char start)
       (- (current-indentation) robot-mode-indent-level)))))

(defun robot-indent--calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let ((indentations (robot-indent--calculate-indentations)))
    (if (listp indentations)
        (if previous
            (robot-indent--previous-indentation indentations (current-indentation))
          (if (and indentations)
              (apply #'max indentations)
            (prog-first-column)))
      indentations)))

(defun robot-indent--previous-indentation (indentations indentation)
  "Return previous indentation from INDENTATIONS relative to INDENTATION."
  (let* ((indentations (sort (copy-sequence indentations) #'>))
         (default (car indentations)))
    (catch 'return
      (dolist (i indentations)
        (when (funcall #'< i indentation)
          (throw 'return i)))
      default)))

(defun robot-indent--get-current-header-block ()
  "Get the current header-block name"
  (save-excursion
    (re-search-backward "[*][*][*]  *\\([^*]+\\)  *[*][*][*]" nil t)
    (match-string 1)))

(defun robot-indent--check-line (regexp &optional num)
  "Check if the current line match the regexp.
Use `num' to check previous or next lines as `forward-line' argument"
  (save-excursion
    (forward-line (or num 0))
    (when num
      (while (and (looking-at "[[:space:]]*\\(#.*\\)*$")
                 (not (bobp)) (not (eobp))) ;; skip empty or comments lines
        (forward-line (or num -1))))
    (string-match
     regexp (buffer-substring (line-beginning-position) (line-end-position)))))


(provide 'robot-indent)
;;; robot-indent.el ends here
