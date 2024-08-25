;;; mk-siarch.el --- Siarch -*- lexical-binding: t; -*-

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

;;

(defvar siarch-dir (expand-file-name ".siarch" (getenv "HOME"))
  "Directory where the siarch script is located.")

(defvar siarch-hostname-params
  '((SiAir . "macbookair")
    (shiraz . "shiraz"))
  "Mapping of hostnames to parameters for ./siarch.sh.")

(defun mk-siarch-rebuild ()
  "Run ./siarch.sh with different arguments based on the hostname."
  (interactive)
  (let* ((hostname (string-trim (shell-command-to-string "hostname")))
         (param (cdr (assoc (intern hostname) siarch-hostname-params)))
         (command (when param
                    (concat "cd " siarch-dir " && ./siarch.sh " param)))
         (buf "*Siarch-output*"))
    (if command
        (progn
          ;; Create or reuse the buffer
          (let ((existing-buffer (get-buffer buf)))
            (when existing-buffer
              (kill-buffer existing-buffer)))
          (async-shell-command command buf)

          (let ((win (get-buffer-window buf)))
            (unless win
              (setq win (split-window (selected-window) nil 'below))

	      )
            (set-window-buffer win buf)
            (with-selected-window win
              (setq mode-line-format nil)
              (fit-window-to-buffer win)
              (set-window-dedicated-p win t))))
      (message "Hostname not recognized: %s" hostname))))

(defun mk-siarch-cancel ()
  "Kill the *Siarch-output* buffer and its window."
  (interactive)
  (let ((buf "*Siarch-output*"))
    (if (get-buffer buf)
        (progn
          (let ((win (get-buffer-window buf)))
            (when win
              (delete-window win)))
          (kill-buffer buf))
      (message "Buffer *Siarch-output* does not exist."))))

(leader
  "csb" 'mk-siarch-rebuild
  "csc" 'mk-siarch-cancel)

(provide 'mk-siarch)
;;; mk-siarch.el ends here
