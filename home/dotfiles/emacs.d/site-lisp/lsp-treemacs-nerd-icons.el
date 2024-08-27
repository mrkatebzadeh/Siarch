;;; lsp-treemacs-nerd-icons.el --- Emacs Nerd Font Icons for lsp-tremacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Velnbur
;;
;; Author: Velnbur <kyrylobaybula@gmail.com>
;; Maintainer: Velnbur <kyrylobaybula@gmail.com>
;; Version: 0.0.1
;; Keywords: files, icons, treemacs, lsp, lsp-treemacs, nerd-icons
;; Homepage: https://github.com/velnbur/lsp-treemacs-nerd-icons
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `nerd-icons' theme for `lsp-treemacs'
;;
;;; Code:

(require 'treemacs)
(require 'nerd-icons)

(cl-defmacro lsp-treemacs-nerd-icons--create-symbol-icon! (&key symbol extensions)
  "Add a nerd-icon for treemacs theme for `SYMBOL' with `EXTENSIONS'.

Requires the `SYMBOL' to be a string or a symbol. `EXTESION' a
string, symbol, or property list of them. If `EXTENSION' is nil
then `SYMBOL' will be used as an extension."
  `(treemacs-create-icon
    :icon (format "%s" (nerd-icons-codicon ,(format "nf-cod-symbol_%s" symbol)))
    :extensions ,(if extensions
                     (if (listp extensions)
                         extensions
                       (list extensions))
                   (list symbol))))

(defmacro lsp-treemacs-nerd-icons--create-symbol-icons! (pairs)
  "Create an icons for treemacs them from `PAIRS'.

If `PAIRS' is a property list, each elemnt should be a symbol,
cons cell. There are two possible usages:

  1. SYMBOL - `SYMBOL' is a string or symbol.
  2. (SYMBOL . EXTENSION) - `SYMBOL' is a string or symbol and
     `EXTENSIONS' is a string, symbol, or property list of them."
  `(progn
     ,@(mapcar (lambda (pair)
                 (cond
                  ((listp pair)
                   (let ((symbol (car pair))
                         (extensions (cdr pair)))
                     `(lsp-treemacs-nerd-icons--create-symbol-icon!
                       :symbol ,symbol
                       :extensions ,extensions)))
                  (t `(lsp-treemacs-nerd-icons--create-symbol-icon!
                       :symbol ,pair
                       :extensions nil))))
               pairs)))

(treemacs-create-theme "nerd-icons-ext"
  :extends "nerd-icons"
  :config
  (progn
    (treemacs-create-icon
     :icon (format "%s" (nerd-icons-codicon "nf-cod-file"))
     :extensions (document))

    (treemacs-create-icon
     :icon (format "%s" (nerd-icons-octicon "nf-oct-project"))
     :extensions (project))

    (treemacs-create-icon
     :icon (format "%s" (nerd-icons-octicon "nf-oct-package"))
     :extensions (package))

    (treemacs-create-icon
     :icon (format "%s" (nerd-icons-mdicon "nf-md-function"))
     :extensions (method function))

    (treemacs-create-icon
     :icon (format "%s " (nerd-icons-devicon "nf-dev-rust"))
     :extensions ("rs"))

    ;; General icons
    (lsp-treemacs-nerd-icons--create-symbol-icons!
     (namespace
      "class"
      ;; method
      property
      field
      (enum . (enumerator enum))
      interface
      constant
      string
      numeric
      (boolean . boolean-data)
      (enum_member . (enumMember enum-member enumitem))
      (structure . struct)
      event
      operator))))

(provide 'lsp-treemacs-nerd-icons)
;;; lsp-treemacs-nerd-icons.el ends here
