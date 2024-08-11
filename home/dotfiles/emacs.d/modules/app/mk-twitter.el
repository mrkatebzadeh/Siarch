;;; mk-twitter.el --- Twitter  -*- lexical-binding: t; -*-

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

(use-package twittering-mode
  :commands twit
  :defer t
  :custom
  (twittering-private-info-file (concat org-directory "/keys/twittering-mode.gpg"))
  :config
  (setq twittering-use-master-password t)
  (setq twittering-initial-timeline-spec-string
	'(":home"))
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 300)
  (setq twittering-url-show-status nil))

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "t" 'twit)


(provide 'mk-twitter)
;;; mk-twitter.el ends here
