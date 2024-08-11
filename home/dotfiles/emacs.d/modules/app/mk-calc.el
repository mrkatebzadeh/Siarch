;;; mk-calc.el --- Calc  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr.katebzadeh@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

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

(use-package calc
  :commands calc
  :defer  t)

;;; calc
(with-eval-after-load 'calc
  (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
				(MiB "1024 * KiB" "Mega Byte")
				(KiB "1024 * B" "Kilo Byte")
				(B nil "Byte")
				(Gib "1024 * Mib" "Giga Bit")
				(Mib "1024 * Kib" "Mega Bit")
				(Kib "1024 * b" "Kilo Bit")
				(b "B / 8" "Bit"))
	math-units-table nil))

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "c" 'calc)




(provide 'mk-calc)
;;; mk-calc.el ends here
