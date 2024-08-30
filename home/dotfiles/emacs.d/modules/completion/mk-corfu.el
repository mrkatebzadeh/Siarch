;;; mk-corfu.el --- Corfu -*- lexical-binding: t; -*-

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

(when (string= mk-completion "light")
  (use-package corfu
    :defer t
    :ensure t
    :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
    :general
    (:keymaps 'corfu-map
	      :states 'insert
	      "C-n" #'corfu-next
	      "<tab>" #'corfu-next
	      "C-p" #'corfu-previous
	      "<escape>" #'corfu-quit
	      "<return>" #'corfu-insert
	      "H-SPC" #'corfu-insert-separator
	      ;; "SPC" #'corfu-insert-separator ; Use when `corfu-quit-at-boundary' is non-nil
	      "M-d" #'corfu-show-documentation
	      "C-g" #'corfu-quit
	      "M-l" #'corfu-show-location)
    :custom
    ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
    ;; want to perform completion
    (tab-always-indent 'complete)
    (completion-cycle-threshold nil)      ; Always show candidates in menu

    (corfu-auto nil)
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.25)

    (corfu-min-width 80)
    (corfu-max-width corfu-min-width)     ; Always have the same width
    (corfu-count 14)
    (corfu-scroll-margin 4)
    (corfu-cycle nil)

    ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
    ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
    ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
    ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
    ;; configuration already has pre-prepared). Necessary for manual corfu usage with
    ;; orderless, otherwise first component is ignored, unless `corfu-separator'
    ;; is inserted.
    (corfu-quit-at-boundary nil)
    (corfu-separator ?\s)            ; Use space
    (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
    (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
    (corfu-preselect-first nil)        ; Preselect first candidate?

    ;; Other
    (corfu-echo-documentation t)        ; Already use corfu-doc
    (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
    :init
    (global-corfu-mode)
    (corfu-popupinfo-mode)
    :config
    ;; Enable Corfu more generally for every minibuffer, as long as no other
    ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
    ;; completion UI. From
    ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
    (defun corfu-enable-always-in-minibuffer ()
      "Enable Corfu in the minibuffer if Vertico/Mct are not active."
      (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                  (bound-and-true-p vertico--input))
	(setq-local corfu-auto nil)       ; Ensure auto completion is disabled
	(corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

    ;; Setup lsp to use corfu for lsp completion
    (defun kb/corfu-setup-lsp ()
      "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))))

  (use-package nerd-icons-corfu
    :ensure t
    :after corfu
    :config
    (setq erd-icons-corfu-mapping
	  '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
	    (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
	    ;; ...
	    (t :style "cod" :icon "code" :face font-lock-warning-face)))
    ;; Remember to add an entry for `t', the library uses that as default.
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (use-package cape
    :defer t
    :ensure t
    ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
    ;; Press C-c p ? to for help.
    :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
    ;; Alternatively bind Cape commands individually.
    ;; :bind (("C-c p d" . cape-dabbrev)
    ;;        ("C-c p h" . cape-history)
    ;;        ("C-c p f" . cape-file)
    ;;        ...)
    :init
    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-hook 'completion-at-point-functions #'cape-history)
    ;; ...
    )


;;; Toggles
  (leader
    "ta" 'global-corfu-mode)
  )

(provide 'mk-corfu)
;;; mk-corfu.el ends here
