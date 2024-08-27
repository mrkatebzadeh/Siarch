;;; mk-docker.el --- Docker -*- lexical-binding: t; -*-

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

(use-package dockerfile-mode
  :ensure t
  :defer t
  :delight "δ "
  :mode "Dockerfile\\'")

(use-package docker
  :ensure t
  :defer t
  :commands (docker))

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "D" 'docker)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'docker-container-mode-map
 "a" 'docker-container-attach
 "c" 'docker-container-cp
 "d" 'docker-container-diff
 "i" 'docker-container-inspect
 "k" 'docker-container-kill
 "L" 'docker-container-logs
 "l" 'docker-container-ls
 "p" 'docker-container-pause
 "n" 'docker-container-rename
 "R" 'docker-container-restart
 "r" 'docker-container-rm
 "s" 'docker-container-start
 "S" 'docker-container-stop
 "P" 'docker-container-unpause)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'docker-image-mode-map
 "i" 'docker-image-inspect
 "f" 'docker-image-pull
 "p" 'docker-image-push
 "r" 'docker-image-rm
 "l" 'docker-image-ls
 "U" 'docker-image-run
 "t" 'docker-image-tag)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'docker-network-mode-map
 "r" 'docker-network-rm
 "l" 'docker-network-ls)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'docker-volume-mode-map
 "r" 'docker-volume-rm
 "l" 'docker-volume-ls)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'dockerfile-mode-map
 "b" 'docker-compose-build
 "C" 'docker-compose-config
 "c" 'docker-compose-create
 "d" 'docker-compose-down
 "e" 'docker-compose-exec
 "L" 'docker-compose-logs
 "f" 'docker-compose-pull
 "p" 'docker-compose-push
 "r" 'docker-compose-remove
 "R" 'docker-compose-restart
 "U" 'docker-compose-run
 "s" 'docker-compose-start
 "S" 'docker-compose-stop
 "u" 'docker-compose-up)



(provide 'mk-docker)
;;; mk-docker.el ends here
