;;; robot-mode-test.el --- Tests for robot-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 20 Jul 2024
;; Keywords: languages files
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(require 'ert)
(require 'ert-x)
(require 'ert-font-lock)

(setq debug-on-error nil)

(ert-font-lock-deftest-file robot-mode-test--correct-font-lock
    "Test that the font lock works on an example file."
  robot-mode
  "robot-mode-test-correct-font-lock.robot")

(ert-deftest robot-mode-test--indentation ()
  (ert-test-erts-file (ert-resource-file "robot-mode-test-indentation.erts")))

(ert-deftest robot-mode-test--operation ()
  (ert-test-erts-file (ert-resource-file "robot-mode-test-operation.erts")))

(ert-deftest robot-mode-test--imenu ()
  (ert-test-erts-file (ert-resource-file "robot-mode-test-imenu.erts")))

;;; robot-mode-test.el ends here
