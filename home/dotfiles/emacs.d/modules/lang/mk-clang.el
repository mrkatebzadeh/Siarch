;;; mk-clang.el --- Clang  -*- lexical-binding: t; -*-

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

(use-package srefactor
  :defer t
  :config
  (semantic-mode 1))

(use-package ccls
  :defer t
  :after projectile
  :preface
  (defun mk-load-ccls-functions ()
    (setq ccls-sem-highlight-method 'font-lock)
    ;; alternatively, (setq ccls-sem-highlight-method 'overlay)
    (setq ccls-extra-init-params '(:completion (:detailedLabel t)))
    ;; For rainbow semantic highlighting
    (ccls-use-default-rainbow-sem-highlight)

    (defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
    (defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
    (defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
    (defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
    (defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
    (defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

    ;; References w/ Role::Role
    (defun ccls/references-read () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :role 8)))

    ;; References w/ Role::Write
    (defun ccls/references-write ()
      (interactive)
      (lsp-ui-peek-find-custom "textDocument/references"
			       (plist-put (lsp--text-document-position-params) :role 16)))

    ;; References w/ Role::Dynamic bit (macro expansions)
    (defun ccls/references-macro () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :role 64)))

    ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
    (defun ccls/references-not-call () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :excludeRole 32)))

    ;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
    ;; (ccls/base 1) direct bases
    ;; (ccls/derived 1) direct derived
    ;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
    ;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
    ;; (ccls/member 0) => member variables / variables in a namespace
    ;; (ccls/vars 1) => field
    ;; (ccls/vars 2) => local variable
    ;; (ccls/vars 3) => field or local variable. 3 = 1 | 2


    ;; References whose filenames are under this project
    (lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))
    )
  (defun mk-ccls ()
    (require 'ccls)
    (require 'company-c-headers)
    (lsp)
    (mk-load-ccls-functions))
  :hook ((c-mode c-common-mode c++-mode objc-mode) . mk-ccls)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
	   projectile-project-root-files-top-down-recurring)))

(use-package google-c-style
  :hook ((c++-mode) . google-set-c-style))
					;  (c-mode-common . google-make-newline-indent))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :defer t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))
(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))
(use-package clang-format
  :defer t
  :hook ((c-mode cc-mode c-common-mode c++-mode) . clang-format-buffer-smart-on-save))

(use-package disaster
  :defer t
  :commands (disaster disaster-objdump))

(use-package makefile-executor
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package company-c-headers
  :defer t
  :config
  (push 'company-c-headers company-backends))

(use-package cpp-auto-include
  :defer t
  :ensure nil
  :commands (cpp-auto-include)
  :load-path (lambda () (concat mk-lisp-dir "/cpp-auto-include")))

;;; config

(setq-default c-default-style "linux")

(defvar cmake-project-dir nil)
(defvar cmake-build-dir nil)
(defvar cmake-build-command nil)
(defvar cmake-make-command nil)
(defvar cmake-bclean-command nil)
(defvar cmake-mclean-command nil)

(defun cmake-find-project ()
  "Finds the directory of the project for cmake."
  (interactive)
  (setq cmake-project-dir (projectile-project-root))
  (setq cmake-build-dir (concat cmake-project-dir "build"))
  (setq cmake-make-command
	(concat "cd " cmake-build-dir " && make"))
  (setq cmake-build-command
	(concat "cd " cmake-build-dir " && cmake .. && cp compile_commands.json .."))
  (setq cmake-bclean-command
	(concat "cd " cmake-build-dir " && rm -rf *"))
  (setq cmake-mclean-command
	(concat "cd " cmake-build-dir " && make clean"))
  )

(defun cmake-build ()
  (interactive)
  (let* (
	 (buildbuffer (concat "*build*"))
	 (buildprocess (concat "buildprocess")))
    (start-process-shell-command buildprocess
				 buildbuffer
				 cmake-build-command)
    (with-current-buffer buildbuffer
      (display-buffer (current-buffer))
      (evil-force-normal-state)
      (read-only-mode -1)
      (kill-region (point-min) (point-max))
      (read-only-mode t)
      (require 'shell)
      (shell-mode)
      (set-process-filter (get-buffer-process buildbuffer) 'comint-output-filter))
    )
  (message cmake-build-command))

(defun cmake-make ()
  (interactive)
  (let* (
	 (makebuffer (concat "*make*"))
	 (makeprocess (concat "makeprocess")))
    (start-process-shell-command makeprocess
				 makebuffer
				 cmake-make-command)
    (with-current-buffer makebuffer
      (display-buffer (current-buffer))
      (evil-force-normal-state)
      (read-only-mode -1)
      (kill-region (point-min) (point-max))
      (read-only-mode t)
      (require 'shell)
      (shell-mode)
      (set-process-filter (get-buffer-process makebuffer) 'comint-output-filter))
    )
  (message cmake-make-command))

(defun cmake-build-clean ()
  (interactive)
  (shell-command cmake-bclean-command)
  (message cmake-bclean-command))

(defun cmake-make-clean ()
  (interactive)
  (shell-command cmake-mclean-command)
  (message cmake-mclean-command))

(defun cmake-objdump-disaster (file-name)
  (require 'disaster)
  (let* ((objdump-cmd (format "%s %s" disaster-objdump (shell-quote-argument file-name)))
	 (buf (set-buffer (generate-new-buffer objdump-cmd))))
    (shell-command objdump-cmd buf)
    (read-only-mode)
    (asm-mode)
    (disaster--shadow-non-assembly-code)
    (switch-to-buffer-other-window buf)))

(defun cmake-find-obj-files ()
  (interactive)
  (let* ((exec-files (seq-filter 'file-readable-p
                                 (directory-files-recursively
                                  cmake-build-dir ".+\.o[bj]?$")))
         (base-buffer-name (file-name-base (buffer-name)))
         (calc-dist (lambda (fn) (cons fn
                                       (string-distance
                                        base-buffer-name
                                        (file-name-base fn)))))
         (cdr-< (lambda (a b) (< (cdr a) (cdr b))))
         (distances (sort (mapcar calc-dist exec-files) cdr-<)))
    (mapcar 'car distances)))

(defun cmake-obj-files-source ()
  (interactive)
  (require 'seq)
  `((name . "Object file to objdump")
    (candidates . ,(cmake-find-obj-files))
    (action . (lambda (sel) (cmake-objdump-disaster sel)))))

(defun cmake-objdump ()
  (interactive)
  (helm :sources (cmake-obj-files-source)))

(add-hook 'c-mode-common-hook 'cmake-find-project)

(setq mk-cc-dap-is-active nil)

(defun mk-cc-dap()
  "Active dap-mode and load dap-lldb"
  (interactive)

  (if mk-cc-dap-is-active
      (progn
	(setq mk-cc-dap-is-active nil)
	(set-window-configuration global-config-editing)
	(dap-ui-mode -1)
	(dap-mode -1)
	(message "DAP mode is disabled."))
    (progn
      (setq global-config-editing (current-window-configuration))
      (let (
	    (c-buffer (window-buffer (selected-window))))
	(require 'dap-mode)
	(require 'dap-gdb-lldb)
	(setq dap-breakpoints-file
	      (concat (projectile-project-root) "/.dap-breakpoints"))
	(dap-mode)
	(dap-ui-mode)
	(dap-ui-locals)
	(dap-ui-breakpoints)
	(other-window 2)
	(setq mk-cc-dap-is-active t)
	(message "DAP mode is enabled.")))))

(setq gdb-many-windows nil)

(defun mk-set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* (
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                             'below)) ;; left bottom
         )
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)
    ))
(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let (
        (c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (mk-set-gdb-layout c-buffer))
  )
(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))
;;; bindings

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps '(c-mode-map c++-mode-map)
 "g" '(:ignore t :which-key "goto")
 "gc" 'ccls/callee
 "gC" 'ccls/caller
 "gm" 'ccls/member
 "f" '(:ignore t :which-key "find")
 "fr" 'lsp-ui-peek-find-references
 "fd" 'lsp-ui-peek-find-implementation
 "fD" 'lsp-ui-peek-find-definitions
 "r"  'lsp-rename
 "h"  'cpp-auto-include
 "F"  'clang-format-buffer
 "i"  'lsp-ui-imenu
 "d"  'cmake-objdump
 "D"  'mk-cc-dap
 "G"  'gdb
 "m"  'cmake-make
 "b"  'cmake-build
 "M"  'cmake-make-clean
 "B"  'cmake-build-clean
 "R"  'lsp-restart-workspace
 "H"  'lsp-symbol-highlight
 "s"  'srefactor-refactor-at-point)


(provide 'mk-clang)
;;; mk-clang.el ends here
