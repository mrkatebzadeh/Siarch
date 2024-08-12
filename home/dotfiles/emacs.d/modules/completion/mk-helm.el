;;; mk-helm.el --- Helm  -*- lexical-binding: t; -*-

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

(use-package helm
  :disabled
  :defer t
  :commands (helm-find-files
	     helm-show-kill-ring
	     helm-recentf
	     helm-tramp
	     helm-tramp-quit)
  :config
  (helm-mode 1)
  (setq helm-ff-skip-boring-files t))


(with-eval-after-load 'helm
(customize-set-variable 'helm-ff-lynx-style-map t)
(add-to-list 'display-buffer-alist
	     `(,(rx bos "*helm" (* not-newline) "*" eos)
	       (display-buffer-in-side-window)
	       (inhibit-same-window . t)
	       (window-height . 0.35)))
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action))




;;; helm-tramp
(use-package helm-tramp
  :after helm
  :defer t)

;;; Files
(general-define-key
 :prefix "SPC f"
 :states '(normal visual motion)
 :keymaps 'override
 "K" 'helm-show-kill-ring
 "R" 'helm-recentf
 "t" 'helm-tramp
 "T" 'helm-tramp-quit
 "f" 'helm-find-files)

(provide 'mk-helm)
;;; mk-helm.el ends here
