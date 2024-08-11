;;; mk-buffer.el --- Buffer  -*- lexical-binding: t; -*-

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

(use-package avy
  :defer t)

(use-package undo-tree
  :defer t)

(use-package smart-hungry-delete
  :ensure t
  :defer nil)

(use-package expand-region
  :defer t)

(use-package paren-face
  :defer t)

(use-package evil-surround
  :defer t
  :config
  (global-evil-surround-mode 1))

(use-package nlinum-relative
  :defer t
  :init
  (nlinum-relative-setup-evil))

(use-package emojify
  :defer t)

(use-package google-this
  :defer t)

(use-package fixmee
  :defer t
  :after button-lock
  :init
  (global-fixmee-mode 1))

(use-package aggressive-indent
  :defer t
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

(use-package electric-operator
  :defer t
  :delight
  :hook (python-mode . electric-operator-mode))

(use-package rainbow-mode
  :defer t
  :delight
  :hook (prog-mode))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package format-all
  :defer t)

(use-package hl-todo
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package rainbow-delimiters
  :defer t)

(use-package highlight-indent-guides
  :defer t
  :config
  (set-face-background 'highlight-indent-guides-odd-face "dimgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))


;;; config
(defalias 'list-buffers 'ibuffer-other-window)

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun mk-protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'mk-protected-buffers)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Files" (mode . dired-mode))
	       ("Org" (name . "^.*org$"))
	       ("Web" (or (mode . web-mode) (mode . js2-mode)))
	       ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
	       ("Mail" (name . "\*mu4e\*"))
	       ("IRC" (mode . erc-mode))
	       ("Programming" (or
			       (mode . python-mode)
			       (mode . cc-mode)
			       (mode . c++-mode)))
	       ("Emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))
	       ))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

(avy-setup-default)

(with-eval-after-load 'undo-tree
  (global-undo-tree-mode))

(with-eval-after-load 'smart-hungry-delete
  (smart-hungry-delete-add-default-hooks))

(fset 'yes-or-no-p 'y-or-n-p)

(with-eval-after-load 'nlinum-relative
  (setq nlinum-relative-redisplay-delay 0
	nlinum-relative-current-symbol ""
	nlinum-relative-offset 1))

(with-eval-after-load 'emojify
  (add-hook 'after-init-hook #'global-emojify-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun mk-man()
  "Load and run man"
  (interactive)
  (require 'man)
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
  (man))

;; highlight matches
(show-paren-mode 1)

;; highlight-indent-guides
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character))


;;; bindings
(general-define-key
 :prefix "SPC b"
 :states '(normal visual motion)
 :keymaps 'override
 "d" 'kill-current-buffer
 "D" 'kill-buffer
 "b" 'helm-buffers-list
 "B" 'ibuffer
 "w" 'evil-write
 "u" 'undo-tree-visualize)

(general-define-key
 :prefix "SPC /"
 :states '(normal visual motion)
 :keymaps 'override
 "e" 'er/expand-region
 "a" 'avy-goto-char)

(with-eval-after-load 'smart-hungry-delete
  (general-define-key
   :prefix "<backspace>"
   :states '(insert)
   :keymaps 'override
   "" 'smart-hungry-delete-backward-char))

(general-define-key
 :prefix "SPC h"
 :states '(normal visual motion)
 :keymaps 'override
 "g" 'google-this
 "G" 'google-this-search
 "m" 'mk-man)

(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "h" 'highlight-indent-guides-mode
 "p" 'smartparens-mode
 "n" 'nlinum-relative-toggle
 "r" 'rainbow-delimiters-mode)


(provide 'mk-buffer)
;;; mk-buffer.el ends here
