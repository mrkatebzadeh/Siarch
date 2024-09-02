;;; mk-lsp.el --- LSP -*- lexical-binding: t; -*-

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

;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-keep-workspace-alive nil)
  (setq read-process-output-max (* 1024 1024))
  :custom
  (lsp-prefer-flymake nil)
  (lsp-session-file (concat mk-backup-dir "lsp-session-v1"))
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'rustic-mode-hook #'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls))
  )

(use-package lsp-ui
  :ensure t
  :demand t
  :after lsp-mode
  :commands (lsp-ui-peek-find-definitions
	     lsp-ui-peek-find-implementation
	     lsp-ui-peek-find-references)
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 15
	lsp-ui-doc-max-width 150
        lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-enable t
	lsp-ui-doc-show-with-mouse t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-update-mode "line"
	lsp-ui-sideline-show-hover nil)
  )

(when (string= mk-completion "featured")
  (use-package company-lsp
    :ensure t
    :defer t
    :after (company lsp-mode)
    :init
    (defvar company-lsp-enable-recompletion t)
    (defvar company-lsp-async t)
    :config
    (setq company-backends '(company-lsp company-yasnippet)))
  )

(use-package dap-mode
    :ensure t
    :defer t
    :after lsp-mode
    :config
    (dap-mode -1)
    (dap-ui-mode -1)
    :bind
    (:map dap-mode-map
	  (("<f12>" . dap-debug)
	   ("<f6>" . dap-breakpoint-condition)
	   ("<f8>" . dap-continue)
	   ("<f9>" . dap-next)
	   ("<M-f11>" . dap-step-in)
	   ("C-M-<f11>" . dap-step-out)
	   ("<f7>" . dap-breakpoint-toggle))))

(leader
  "d" '(:ignore t :which-key "Debug")
  "dd" 'dap-debug
  "dB" 'dap-breakpoint-condition
  "dc" 'dap-continue
  "dn" 'dap-next
  "di" 'dap-step-in
  "do" 'dap-step-out
  "db" 'dap-breakpoint-toggle
  )

(use-package format-all
  :ensure t
  :defer t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
		  ("Rust"     (rustfmt))
		  ("Nix"     (nixpkgs-fmt))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

(leader
  "ld" 'lsp-ui-peek-find-definitions
  "lD" 'lsp-ui-peek-find-implementation
  "lr" 'lsp-ui-peek-find-references
  "lf" 'format-all-mode
  "lk" 'lsp-ui-doc-glance)

(provide 'mk-lsp)
;;; mk-lsp.el ends
