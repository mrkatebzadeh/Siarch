;;; mk-key.el --- Key -*- lexical-binding: t; -*-

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

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t
	evil-default-cursor t
	evil-want-integration nil
	evil-want-keybinding nil)
  ;; This has to be before we invoke evil-mode due to:
  ;; https://github.com/cofi/evil-leader/issues/10
  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode))
  (evil-mode 1))

;; evil-collection
(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;; Display visual hint on evil edit operations
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; general
(use-package general
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  (general-auto-unbind-keys)
  )


;; which-key
(use-package which-key
  :ensure t
  :defer t
  :init
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

(general-create-definer leader
  :states '(normal visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "A-SPC")

;; Redo
(evil-set-undo-system 'undo-redo)
;; Esc
;;;(global-set-key [escape] 'keyboard-quit)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; leader file
(leader
  "" '(nil :which-key "My lieutenant general prefix")
  "f" '(:ignore t :which-key "Files")
  "c" '(:ignore t :which-key "Config Files")
  "o" '(:ignore t :which-key "Org")
  "a" '(:ignore t :which-key "Applications")
  "g" '(:ignore t :which-key "Magit")
  "m" '(:ignore t :which-key "EMMS")
  "l" '(:ignore t :which-key "Local Bindings")
  "b" '(:ignore t :which-key "Buffers")
  "h" '(:ignore t :which-key "Help!")
  "v" '(:ignore t :which-key "Volume")
  "w" '(:ignore t :which-key "Windows")
  "q" '(:ignore t :which-key "Quit")
  "t" '(:ignore t :which-key "Toggles")

  "x" 'execute-extended-command
  )

;; Exit/restart/reboot/shutdown
(leader
  "qq" 'kill-emacs
  "qQ" 'delete-frame)


(provide 'mk-key)
;;; mk-key.el ends here
