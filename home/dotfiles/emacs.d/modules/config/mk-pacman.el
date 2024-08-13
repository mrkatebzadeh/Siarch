;;; mk-pacman.el --- PACMAN -*- lexical-binding: t; -*-

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

(when (string= mk-completion "featured")
(use-package pacman
  :defer t
  :ensure nil
  :commands (pacman))

(setq commands '(("Pacman" . pacman)
		 ("Install" . pacman-mode-install)
		 ("Search" . pacman-mode-search)
		 ("Details" . pacman-mode-package-details)
		 ("Sync" . pacman-mode-sync)
		 ("Update" . pacman-mode-update)
		 ("Remove" . pacman-mode-remove)
		 ("List" . pacman-mode-list-installed-packages)))

(setq pacman-helm-source
      `((name . "Pacman commands")
	(candidates . ,(mapcar 'car commands))
	(action . (lambda (candidate)
		    (funcall-interactively (cdr (assoc candidate commands)))))))

(defun mk-helm-pacman ()
  "Pacman interface"
  (interactive)
  (require 'pacman)
  (helm :sources '(pacman-helm-source)))

;;; bindings

(leader
  "ap" 'mk-helm-pacman
  )

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'pacman-mode-map
 "i" 'pacman-mode-install
 "s" 'pacman-mode-search
 "y" 'pacman-mode-sync
 "d" 'pacman-mode-package-details
 "u" 'pacman-mode-update
 "r" 'pacman-mode-remove
 "l" 'pacman-mode-list-installed-packages
 "q" 'pacman-mode-kill-buffer)
)

(provide 'mk-pacman)
;;; mk-pacman.el ends here
