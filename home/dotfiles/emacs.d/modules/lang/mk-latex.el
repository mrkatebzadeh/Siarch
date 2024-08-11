;;; mk-latex.el --- LaTeX  -*- lexical-binding: t; -*-

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

(use-package auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t
	TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (TeX-source-correlate-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
	      ;; (add-to-list 'TeX-view-program-selection
	      ;; '(output-pdf "Zathura"))
	      ;; (add-to-list 'TeX-view-program-selection
	      ;; '(output-pdf "zathura"))
	      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		    TeX-source-correlate-start-server t)
	      (setq-default TeX-master nil)
              (setq reftex-plug-into-AUCTeX t
                    TeX-PDF-mode t
		    TeX-command-force ""
		    TeX-source-correlate-method 'synctex
		    TeX-source-correlate-start-server t
		    TeX-command-default "latex -synctex=1"
		    ))))

(use-package latex-preview-pane
  :defer t)

(use-package reftex
  :defer t)

(use-package helm-bibtex
  :defer t)

(use-package auctex-latexmk
  :defer t
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :defer t
  :after (auctex company))

;;; config

(with-eval-after-load 'auctex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook 'TeX-command-master nil t)))
					; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

  ;; 3 Make C-c-c not ask, just do the default action. Add C-c-a for asking
  ;; If non-nil, TeX-command-query will return the value of this variable instead
  ;; of quering the user.

  (add-hook 'LaTeX-mode-hook
	    '(lambda()
	       (define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask
		 (lambda (arg) (interactive "P")
		   (let ((TeX-command-force nil))
		     (TeX-command-master arg)))))))

(with-eval-after-load 'reftex
  (setq reftex-cite-prompt-optional-args t))


(with-eval-after-load 'helm-bibtex
  (setq bibtex-completion-bibliography (concat org-directory "/ref/master.bib")
        bibtex-completion-library-path (concat org-directory "/ref/files")
        bibtex-completion-notes-path   (concat org-directory "/ref/notes.org")
	helm-bibtex-bibliography bibtex-completion-bibliography
	helm-bibtex-library-path bibtex-completion-library-path
	;; using bibtex path reference to pdf file
	bibtex-completion-pdf-field "File"
	helm-bibtex-default-action 'bibtex-completion-insert-citation))

(with-eval-after-load 'auctex-latexmk
  (auctex-latexmk-setup))

(with-eval-after-load 'company-auctex
  (company-auctex-init))

(defun latex-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file))

;;; bindings

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'LaTeX-mode-map
 "b" 'latex-compile
 "c" 'TeX-command-master
 "\\" 'TeX-insert-macro
 "-" 'TeX-recenter-output-buffer
 "%" 'TeX-comment-or-uncomment-paragraph
 ";" 'TeX-comment-or-uncomment-region
 "a" 'TeX-command-run-all
 "k" 'TeX-kill-job
 "l" 'TeX-recenter-output-buffer
 "i" '(:ignore t :which-key "insert")
 "ii" 'LaTeX-insert-item
 "im" 'TeX-insert-macro
 "v" 'TeX-view
 "hd" 'TeX-doc
 "*" 'LaTeX-mark-section
 "." 'LaTeX-mark-environment
 "c" 'LaTeX-close-environment
 "e" 'LaTeX-environment
 "s" 'LaTeX-section
 "f" '(:ignore t :which-key "fill")
 "fe" 'LaTeX-fill-environment
 "fp" 'LaTeX-fill-paragraph
 "fr" 'LaTeX-fill-region
 "fs" 'LaTeX-fill-section
 "p" '(:ignore t :which-key "preview")
 "pb" 'preview-buffer
 "pc" 'preview-clearout
 "pd" 'preview-document
 "pe" 'preview-environment
 "pf" 'preview-cache-preamble
 "pp" 'preview-at-point
 "pr" 'preview-region
 "ps" 'preview-section
 "r" '(:ignore t :which-key "reftex")
 "rc" 'reftex-citation
 "rg" 'reftex-grep-document
 "ri" 'reftex-index-selection-or-word
 "rI" 'reftex-display-index
 "r TAB" 'reftex-index
 "rl" 'reftex-label
 "rp" 'reftex-index-phrase-selection-or-word
 "rP" 'reftex-index-visit-phrases-buffer
 "rr" 'reftex-reference
 "rs" 'reftex-search-document
 "rt" 'reftex-toc
 "rT" 'reftex-toc-recenter
 "rv" 'reftex-view-crossref
 "rb" 'helm-bibtex)

(provide 'mk-latex)
;;; mk-latex.el ends here
