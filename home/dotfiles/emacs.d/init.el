;;; init.el --- init file -*- lexical-binding: t; -*-

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

;; A bare-boned config template. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Path vars
;;(setq user-emacs-directory (file-name-directory load-file-name))
(defvar mk-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar mk-vars-file (concat mk-emacs-dir "mk-vars.el")
  "The MK's vars files. Must end with a slash.")

(defvar mk-core-file (concat mk-emacs-dir "mk-core.el")
  "The root directory of MK's core files. Must end with a slash.")

(defvar mk-key-file (concat mk-emacs-dir "mk-key.el")
  "The root directory of MK's key configs. Must end with a slash.")

(defvar mk-modules-dir (concat mk-emacs-dir "modules/")
  "The root directory for MK's modules. Must end with a slash.")

(defvar mk-lisp-dir (concat mk-emacs-dir "site-lisp/")
  "The root directory of MK's external files. Must end with a slash.")

(defvar mk-ui-file (concat mk-emacs-dir "mk-ui.el")
  "The root directory of MK's UI files. Must end with a slash.")

(defvar mk-backup-dir (concat mk-emacs-dir ".backups/")
  "The root directory of MK's backup files. Must end with a slash.")

(defvar mk-cache-dir (concat mk-emacs-dir ".cache/")
  "The root directory of MK's cache files. Must end with a slash.")

(defvar mk-autosave-dir (concat mk-emacs-dir ".autosave/")
  "The root directory of MK's autosave files. Must end with a slash.")

(defvar mk-eshell-dir (concat mk-emacs-dir ".eshell/")
  "The root directory of MK's eshell files. Must end with a slash.")

(defvar mk-desktop-dir (concat mk-emacs-dir ".desktop/")
  "Directory to save desktop sessions.")

(defvar mk-completion "light"
  "Completion frameworks: light -> vertico/consult/corf, featured -> helm/company ")

(setq org-directory     "~/Dropbox/org")

(message "Starting MK")
;;; Speed up startup
(eval-when-compile (require 'cl-lib))

(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;;; Basic configs
(setq warning-minimum-level :emergency)
(setq eshell-directory-name mk-eshell-dir)
(setq pcache-directory (concat mk-cache-dir "/var/pcache"))
(setq transient-history-file (concat mk-cache-dir "/transient/history.el"))
(setq srecode-map-save-file (concat mk-cache-dir "/srecode-map.el"))
(setq projectile-cache-file (concat mk-cache-dir "/projectile.cache"))
					; stop creating backup~ files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

(let ((backup-dir mk-backup-dir)
      (auto-saves-dir mk-autosave-dir))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
(setq custom-file (concat mk-backup-dir "custom.el"))
(load custom-file 'noerror)

;; Load directory function
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun load-modules (dir)
  (mapcar 'load (directory-files-recursively dir "")))

(add-to-list 'load-path mk-lisp-dir)

(load mk-vars-file)
(message "Vars has been loaded.")

(load mk-core-file)
(message "Core has been loaded.")

(load mk-key-file)
(message "Key has been loaded.")

;;; Load Theme
(load mk-ui-file)
;;; Load modules
(load-modules mk-modules-dir)
;;; run server
(require 'server)
(unless (server-running-p)
  (server-start))
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/usr/local/texlive/2019basic/bin/x86_64-darwin/")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(add-to-list 'exec-path "/run/current-system/sw/bin")
(add-to-list 'exec-path "~/.nix-profile/bin/")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2019basic/bin/x86_64-darwin/"))
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
(setenv "PATH" (concat (getenv "PATH") ":~/.nix-profile/bin/"))


;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil
                     (lambda ()
                       "Clean up gc."
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))
(provide 'init)

;;; init.el ends here
