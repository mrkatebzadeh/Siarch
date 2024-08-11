;;; mk-rss.el --- RSS -*- lexical-binding: t; -*-

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

(use-package elfeed
  :defer t
  :config
  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  )

(use-package elfeed-org
  :after elfeed
  :defer t
  :init (elfeed-org)
  :config
  (setq rmh-elfeed-org-files  (list (concat org-directory "/feed/emacs.org")
				    (concat org-directory "/feed/research.org"))))

;; add a star
(defun mk-elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
(defun mk-elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;shortcut to jump to starred bookmark
(defun mk-elfeed-show-starred ()
  (interactive)
  (bookmark-jump "elfeed-starred"))

;;searches
(defun mk-elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun mk-elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defun mk-elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

(defun mk-elfeed-show-network ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-network"))

;; makes sure elfeed reads index from disk before launching
(defun mk-elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun mk-elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "e" 'elfeed)

(with-eval-after-load 'elfeed
  (general-define-key
   :prefix "SPC l"
   :states 'normal
   :keymaps 'elfeed-search-mode-map
   "a" 'elfeed-show-all
   "b" '(:ignore t :which-key "bookmarks")
   "be" 'mk-elfeed-show-emacs
   "bd" 'mk-elfeed-show-daily
   "bn" 'mk-elfeed-show-network
   "bs" 'mk-elfeed-show-starred
   "q" 'mk-elfeed-save-db-and-bury
   "s" 'mk-elfeed-star
   "S" 'mk-elfeed-unstar
   "u" 'elfeed-update))

(provide 'mk-rss)
;;; mk-rss.el ends here
