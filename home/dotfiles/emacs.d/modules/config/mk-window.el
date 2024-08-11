;;; mk-window.el --- Window  -*- lexical-binding: t; -*-

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

(use-package ace-window
  :defer t
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package imenu
  :defer t)

(use-package imenu-list
  :defer t
  :init
  (setq imenu-list-focus-after-activation t
	imenu-list-auto-resize t))

(use-package zoom
  :defer t
  :init
					;  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package darkroom
  :defer t)

;; transparency
(defun mk-toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 50) '(100 . 100)))))


;;; config

(windmove-default-keybindings)
(winner-mode 1)

;;; bindings

(general-define-key
 :prefix "SPC w"
 :states '(normal visual motion)
 :keymaps 'override
 "v" 'evil-window-vsplit
 "s" 'evil-window-split
 "d" 'evil-window-delete
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right)

(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "i" 'imenu-list
 "I" 'helm-imenu
 "d" 'darkroom-mode
 "t" 'mk-toggle-transparency
 "z" 'zoom-mode)

(provide 'mk-window)
;;; mk-window.el ends here
