;;; mk-theme.el --- Theme -*- lexical-binding: t; -*-

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


;; catppuccin-theme
(use-package catppuccin-theme
  :defer t
  :ensure t
  )

(defcustom mk-theme-var nil
  "Variable which sets the default startup theme as light or dark.
Also allows for toggling of the themes. Is set to 'light' by
'mk-theme-light' and 'dark' by 'mk-theme-dark'.
Defaults to nil."
  :group 'mk
  :type 'string)


(defun mk-refresh-theme ()
  "Function to load catppuccin theme"
  (interactive)
  (load-theme 'catppuccin :no-confirm)
  )

(defun mk-toggle-theme ()
  "Function to interactively toggle between light and dark mk themes.
Requires both to be loaded in order to work."
  (interactive)
  (cond ((string= mk-theme-var "light")
         (mk-theme-set-dark))
        ((string= mk-theme-var "dark")
         (mk-theme-set-light))
        (t nil))
  (mk-refresh-theme))


(defun mk-theme-set-dark ()
  "Apply dark Mk theme base."
  (setq catppuccin-flavor 'frappe)
  ;; to allow for toggling of the themes.
  (setq mk-theme-var "dark")
  )

(defun mk-theme-set-light ()
  "Apply light Mk theme base."

  (setq catppuccin-flavor 'latte)
  ;; to allow for toggling of the themes.
  (setq mk-theme-var "light")
  )

(mk-theme-set-dark)
(call-interactively 'mk-refresh-theme)


(leader
  "tT" 'mk-toggle-theme)

(provide 'mk-theme)
;;; mk-theme.el ends here
