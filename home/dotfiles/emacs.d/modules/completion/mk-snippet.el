;;; mk-snippet.el --- Snippet  -*- lexical-binding: t; -*-

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

(use-package yasnippet
  :defer t
  :commands (yas-minor-mode)
  :hook ((prog-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :defer t)

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-minor-mode)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :defer t
  :init
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (define-auto-insert "\\.c?$" ["default-c" autoinsert-yas-expand])
  (define-auto-insert "\\.el?$" ["default-el" autoinsert-yas-expand])
  (define-auto-insert "\\.h?$" ["default-h" autoinsert-yas-expand])
  (define-auto-insert "\\.html?$" "default-html"))

(use-package abbrev
  :ensure nil
  :delight
  :hook (text-mode . abbrev-mode)
  :custom (abbrev-file-name (concat mk-emacs-dir "abbrev_defs"))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "y" 'yas-global-mode)


(provide 'mk-snippet)
;;; mk-snippet.el ends here
