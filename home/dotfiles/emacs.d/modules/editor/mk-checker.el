;;; mk-checker.el --- Checker  -*- lexical-binding: t; -*-

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

(use-package flymake
  :ensure nil
  :config ; (Optional) For fix bad icon display (Only for left margin)
  (advice-add #'flymake--indicator-overlay-spec
              :filter-return
              (lambda (indicator)
                (concat indicator
                        (propertize " "
                                    'face 'default
                                    'display `((margin left-margin)
                                               (space :width 5))))))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error ,(nerd-icons-faicon "nf-fa-remove_sign") compilation-error)
     (warning ,(nerd-icons-faicon "nf-fa-warning") compilation-warning)
     (note ,(nerd-icons-faicon "nf-fa-circle_info") compilation-info))))

(use-package flycheck
  :defer t)

(use-package flyspell
  :defer t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package flyspell-correct-helm
  :after (flyspell helm)
  :defer t
  :init (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package langtool
  :defer t
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :delight
  :custom
  (langtool-default-language "en")
  (langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (langtool-java-bin "/usr/bin/java")
  (langtool-mother-tongue "en-US"))

;;; bindings
(leader
  "ts" 'flyspell-mode
  "tl" 'langtool-check
  "tc" 'global-flycheck-mode)


(provide 'mk-checker)
;;; mk-checker.el ends here
