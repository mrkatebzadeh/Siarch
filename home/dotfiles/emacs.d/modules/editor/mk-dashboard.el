;;; mk-dashboard.el --- Dashboard -*- lexical-binding: t; -*-

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

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "[-< True happiness can be found when two contrary powers cooperate together >-]")
  (dashboard-startup-banner (concat mk-emacs-dir "logo.txt"))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '(projects bookmarks recents))
  (initial-scratch-message nil)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  )

(defcustom mk-dashboard-homepage-footer-url
  "https://github.com/emacs-dashboard/emacs-dashboard"
  "URL to use for `dashboard-insert-homepage-footer'."
  :type '(string)
  :group 'dashboard)
(defcustom mk-dashboard-set-widget-binding t
  "If non-nil show keybindings in shortmenu widgets."
  :type 'boolean
  :group 'dashboard)

(defcustom mk-dashboard-shortmenu-functions
  `((recents   . recentf)
    (bookmarks . bookmark-jump)
    (projects
     . ,(if (eq dashboard-projects-backend 'project-el)
            'project-switch-project
          'projectile-switch-project))
    (agenda    . org-agenda))
  "Functions to me used by shortmenu widgets.
Possible values for list-type are: `recents', `bookmarks', `projects',
`agenda' ,`registers'."
  :type  '(alist :key-type symbol :value-type function)
  :group 'dashboard)

;;
;; Faces
;;

(defface mk-dashboard-bindings-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for shortmenu widgets bindings."
  :group 'dashboard)

;;
;; Widget functions
;;

(defun mk-dashboard-insert-homepage-footer ()
  "Insert a homepage footer to go `mk-dashboard-homepage-footer-url'."
  (widget-create 'item
                 :tag dashboard-footer-icon
                 :action
                 (lambda (&rest _)
                   (browse-url mk-dashboard-homepage-footer-url))
                 :mouse-face 'highlight
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]")
  (dashboard-center-text (- (point) 1) (point))
  (insert "\n"))

(defun mk-dashboard-insert-project-shortmenu (&rest _)
  "Insert project shortmenu widget."
  (let* ((fn (alist-get 'projects mk-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'projects dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	 )
    (if dashboard-display-icons-p
        (insert (format "%-3s" icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Open project")
                   :action (lambda (&rest _)
                             (call-interactively
			      (alist-get 'projects mk-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if mk-dashboard-set-widget-binding
        (insert (propertize (substitute-command-keys fn-keymap)
                            'face
                            'mk-dashboard-bindings-face)))))

(defun mk-dashboard-insert-org-agenda-shortmenu (&rest _)
  "Insert `org-agenda' shortmenu widget."
  (let* ((fn (alist-get 'agenda mk-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'agenda dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	 )
    (if dashboard-display-icons-p
        (insert (format "%-3s" icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Open org-agenda")
                   :action (lambda (&rest _)
                             (call-interactively
			      (alist-get 'agenda mk-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if mk-dashboard-set-widget-binding
        (insert (propertize (substitute-command-keys fn-keymap)
                            'face
                            'mk-dashboard-bindings-face)))))

(defun mk-dashboard-insert-bookmark-shortmenu (&rest _)
  "Insert bookmark shortmenu widget."
  (let* ((fn (alist-get 'bookmarks mk-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'bookmarks dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	 )
    (if dashboard-display-icons-p
        (insert (format "%-3s" icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Jump to bookmark")
                   :action (lambda (&rest _)
                             (call-interactively
			      (alist-get 'bookmarks mk-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if mk-dashboard-set-widget-binding
        (insert (propertize (substitute-command-keys fn-keymap)
                            'face
                            'mk-dashboard-bindings-face)))))

(defun mk-dashboard-insert-recents-shortmenu (&rest _)
  "Insert recent files short menu widget."
  (let* ((fn (alist-get 'recents mk-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'recents dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	 )
    (if dashboard-display-icons-p
	(insert (format "%-3s" icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Recently opened files")
                   :action (lambda (&rest _)
                             (call-interactively
			      (alist-get 'recents mk-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if mk-dashboard-set-widget-binding
	(insert (propertize (substitute-command-keys fn-keymap)
                            'face
                            'mk-dashboard-bindings-face)))))
(provide 'mk-dashboard)
;;; mk-dashboard.el ends here
