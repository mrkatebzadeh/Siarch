;;; mk-docs.el --- Docs  -*- lexical-binding: t; -*-

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

(use-package markdown-mode
  :defer t
  :delight "μ "
  :mode ("\\.markdown\\'" "\\.md\\'")
  :custom (markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode
  :defer t
  :after markdown-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 75))

(use-package csv-mode
  :defer t)

(use-package elisp-mode
  :defer t
  :ensure nil
  :delight "ξ ")

(use-package eldoc
  :defer t
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package toml-mode
  :defer t)

(use-package systemd
  :defer t
  :mode
  ("\\.service\\'" "\\.timer\\'" "\\.target\\'" "\\.mount\\'"
   "\\.automount\\'" "\\.slice\\'" "\\.socket\\'" "\\.path\\'"
   "\\.netdev\\'" "\\.network\\'" "\\.link\\'"))

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :config
  (setq mouse-wheel-follow-mouse t
	pdf-view-resize-factor 1.10))



;;; config

;;; bindigs
(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'markdown-mode-map
 ;; Movement
 "{"   'markdown-backward-paragraph
 "}"   'markdown-forward-paragraph
 ;; Completion, and Cycling
 "]"   'markdown-complete
 ;; Indentation
 ">"   'markdown-indent-region
 "<"   'markdown-exdent-region
 ;; Buffer-wide commands
 "c" '(:ignore t :which-key "commands")
 "c]"  'markdown-complete-buffer
 "cc"  'markdown-check-refs
 "ce"  'markdown-export
 "cm"  'markdown-other-window
 "cn"  'markdown-cleanup-list-numbers
 "co"  'markdown-open
 "cp"  'markdown-preview
 "cv"  'markdown-export-and-preview
 "cw"  'markdown-kill-ring-save
 ;; headings
 "h" '(:ignore t :which-key "headings")
 "hi"  'markdown-insert-header-dwim
 "hI"  'markdown-insert-header-setext-dwim
 "h1"  'markdown-insert-header-atx-1
 "h2"  'markdown-insert-header-atx-2
 "h3"  'markdown-insert-header-atx-3
 "h4"  'markdown-insert-header-atx-4
 "h5"  'markdown-insert-header-atx-5
 "h6"  'markdown-insert-header-atx-6
 "h!"  'markdown-insert-header-setext-1
 "h@"  'markdown-insert-header-setext-2
 ;; Insertion of common elements
 "i" '(:ignore t :which-key "insert")
 "-"   'markdown-insert-hr
 "if"  'markdown-insert-footnote
 "ii"  'markdown-insert-image
 "ik"  'spacemacs/insert-keybinding-markdown
 "iI"  'markdown-insert-reference-image
 "il"  'markdown-insert-link
 "iL"  'markdown-insert-reference-link-dwim
 "iw"  'markdown-insert-wiki-link
 "iu"  'markdown-insert-uri
 ;; Element removal
 "k"   'markdown-kill-thing-at-point
 ;; List editing
 "li"  'markdown-insert-list-item
 ;; region manipulation
 "f" '(:ignore t :which-key "region")
 "xb"  'markdown-insert-bold
 "xi"  'markdown-insert-italic
 "xc"  'markdown-insert-code
 "xC"  'markdown-insert-gfm-code-block
 "xq"  'markdown-insert-blockquote
 "xQ"  'markdown-blockquote-region
 "xp"  'markdown-insert-pre
 "xP"  'markdown-pre-region
 ;; Following and Jumping
 "N"   'markdown-next-link
 "f"   'markdown-follow-thing-at-point
 "P"   'markdown-previous-link
 "<RET>" 'markdown-jump)

(with-eval-after-load 'pdf-tools
  (general-define-key
   :prefix "SPC l"
   :states '(normal visual motion)
   :keymaps 'pdf-view-mode-map
   ;; Slicing image
   "s" '(:ignore t :which-key "slicing")
   "sm" 'pdf-view-set-slice-using-mouse
   "sb" 'pdf-view-set-slice-from-bounding-box
   "sr" 'pdf-view-reset-slice
   ;; Annotations
   "a" '(:ignore t :which-key "annotations")
   "aD" 	'pdf-annot-delete
   "at" 	'pdf-annot-attachment-dired
   "ah" 	'pdf-annot-add-highlight-markup-annotation
   "al" 	'pdf-annot-list-annotations
   "am" 	'pdf-annot-add-markup-annotation
   "ao" 	'pdf-annot-add-strikeout-markup-annotation
   "as" 	'pdf-annot-add-squiggly-markup-annotation
   "at" 	'pdf-annot-add-text-annotation
   "au" 	'pdf-annot-add-underline-markup-annotation
   ;; Fit image to window
   "f" '(:ignore t :which-key "fit")
   "fw" 'pdf-view-fit-width-to-window
   "fh" 'pdf-view-fit-height-to-window
   "fp" 'pdf-view-fit-page-to-window
   ;; Other
   "o" 'pdf-occur
   "p" 'pdf-misc-print-document
   "O" 'pdf-outline
   "n" 'pdf-view-midnight-minor-mode))



(provide 'mk-docs)
;;; mk-docs.el ends here
