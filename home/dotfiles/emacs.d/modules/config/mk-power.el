;;; mk-power.el --- Power  -*- lexical-binding: t; -*-

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

(setq helm-power-source
      '((name . "Managing power in Emacs.")
        (candidates . ("Cancel" "Shutdown" "Reboot" "Lock"))
        (action . (lambda (candidate)
		    (cond
		     ((equal candidate "Shutdown") (shell-command "sudo shutdown now"))
		     ((equal candidate "Reboot") (shell-command "sudo shutdown -r now"))
		     ((equal candidate "Lock") (shell-command "slock"))
		     ((equal candidate "Cancel") (message "Canceled!"))
		     (t (message "Invalid!")))))))

(defun helm-power ()
  "Prompt for power management."
  (interactive)
  (helm :sources '(helm-power-source)))

;;; bindigs
(general-define-key
 :prefix "SPC q"
 :states '(normal visual motion)
 :keymaps 'override
 "z" 'helm-power)

(provide 'mk-power)
;;; mk-power.el ends here
