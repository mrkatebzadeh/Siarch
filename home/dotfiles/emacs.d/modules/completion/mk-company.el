;;; mk-company.el --- Company  -*- lexical-binding: t; -*-

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

(when (string= mk-completion "featured")
(use-package company
  :defer t
  :config
  (setq company-frontends (delete  'company-tng-frontend  company-frontends))
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("<tab>" . company-complete-common-or-cycle)
	("RET"  . company-complete-selection)
	:map company-search-map
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  )

(use-package company-box
  :defer t
  :after company
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50))

(defun company-box-icons--elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((sym (intern candidate)))
      (cond ((fboundp sym) 'Function)
	    ((featurep sym) 'Module)
	    ((facep sym) 'Color)
	    ((boundp sym) 'Variable)
	    ((symbolp sym) 'Text)
	    (t . nil)))))

(with-eval-after-load 'all-the-icons
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-fileicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
	`((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
	  (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
	  (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	  (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	  (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	  (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
	  (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
	  (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
	  (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
	  (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
	  (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
	  (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
	  (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
	  (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
	  (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
	  (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
	  (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
	  (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
	  (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
	  (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
	  (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
	  (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
	  (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
	  (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
	  (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
	  (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
	  (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))

(use-package company-quickhelp
  :defer t
  :after company
  :defines company-quickhelp-delay
  :bind (:map company-active-map
	      ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 0.8))

(with-eval-after-load 'company
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 3
	company-show-numbers t
	company-tooltip-align-annotations 't))

;;; company
(general-define-key
 :prefix "C-SPC"
 :states '(insert motion)
 :keymaps 'override
 "" 'company-complete-common-or-cycle)

;;; Toggles
(leader
  "ta" 'global-company-mode)
)


(provide 'mk-company)
;;; mk-company.el ends here
