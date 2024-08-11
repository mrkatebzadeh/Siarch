;;; mk-jekyl.el --- Jekyll -*- lexical-binding: t; -*-

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

(use-package hyde
  :commands (hyde)
  :init
  (defun mk-hyde ()
    "Helper function"
    (interactive)
    (hyde)))

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "j" 'mk-hyde)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps '(hyde/hyde-mode hyde-mode-map)
 "n" 'hyde/new-post
 "c" 'hyde/hyde-commit-post
 "g" 'hyde/load-posts
 "d" 'hyde/delete-post
 "p" 'hyde/promote-to-post
 "q" 'hyde/quit
 "s" 'hyde/serve
 "x" 'hyde/stop-serve
 "o" 'hyde/open-post-maybe)




(provide 'mk-jekyl)
;;; mk-jekyl.el ends here
