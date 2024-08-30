;;; mk-org.el --- Org -*- lexical-binding: t; -*-

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

(use-package org
  ;; :ensure org-contrib
  :defer t
  :pin gnu
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-agenda-files
	(append
	 (file-expand-wildcards (concat org-directory "/agenda/*.org")))
	org-agenda-window-setup (quote current-window)
	org-deadline-warning-days 7
	org-agenda-span (quote fortnight)
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
	org-agenda-todo-ignore-deadlines (quote all)
	org-agenda-todo-ignore-scheduled (quote all)
	org-agenda-sorting-strategy (quote
				     ((agenda deadline-up priority-down)
				      (todo priority-down category-keep)
				      (tags priority-down category-keep)
				      (search category-keep)))
	org-default-notes-file (concat org-directory "/agenda/notes.org")
	org-capture-templates
	'(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(when (string= mk-completion "featured")
  (use-package helm-org
    :ensure t
    :after helm
    :defer t)
  )

(use-package ox-reveal
  :ensure t
  :defer t)

(use-package htmlize
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t
  :defer t)

(use-package org-ref
  :ensure t
  :defer t
  :init
  (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
        org-ref-default-bibliography   (list (concat org-directory "/ref/master.bib"))
        org-ref-pdf-directory          (concat org-directory "/ref/files/"))
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
  (setq interleave-org-notes-dir-list `(,(concat org-directory "/ref/files"))))

;;; evil-org
(use-package evil-org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects)))))

(use-package org-agenda
  :defer t
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-window-setup 'current-window))

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "◉" "○" "✸" "✿")))

(use-package org-contacts
  :ensure nil
  :defer t
  :after org
  :custom (org-contacts-files '((concat org-directory "/agenda/contacts.org"))))

(use-package org-faces
  :ensure nil
  :after org
  :defer t
  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "cyan" :weight bold))
     ("SOMEDAY" . (:foreground "gray" :weight bold))
     ("TODO" . (:foreground "green" :weight bold))
     ("WAITING" . (:foreground "red" :weight bold)))))

(use-package org-crypt
  :ensure nil
  :after org
  :defer t
  :custom (org-crypt-key "mr.katebzadeh@gmail.com"))

(use-package org-journal
  :defer t
  :after org
  :preface
  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))
  :custom
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-dir (format (concat org-directory "/journal/")
			   (format-time-string "%Y")))
  (org-journal-enable-encryption t)
  (org-journal-file-format "%Y%m%d")
  (org-journal-time-format ""))

(use-package org-gcal
  :ensure t
  :defer t
  :config
  (load-library "~/Dropbox/org/keys/gcal.el.gpg"))

(use-package org-drill
  :defer t
  :ensure nil)

(defun mk-org-drill ()
  "Load and run org-drill"
  (interactive)
  (require 'org-drill))

(use-package org-tvdb
  :defer t
  :ensure nil ; remove this if available through melpa
  :config
  (load-library "~/Dropbox/org/keys/tvdb.el.gpg")
  :commands (org-tvdb-insert-todo-list
	     org-tvdb-add-season
	     org-tvdb-add-series
	     org-tvdb-mark-series-watched
	     org-tvdb-mark-season-watched
	     org-tvdb-update-series
	     org-tvdb-update-season))

(use-package ox-moderncv
  :defer t
  :ensure nil
  :load-path (lambda () (concat mk-lisp-dir "/org-cv/")))

(defun mk-org-export()
  "Load required packages for exporting org file"
  (interactive)
  (require 'ox-moderncv)
  (require 'ox-reveal))

;;; config
(with-eval-after-load 'org
  (require 'org-id)
  (setq org-ref-open-pdf-function
	(lambda (fpath)
	  (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))

  (setq mk-secret-dir (concat org-directory "/keys/"))
  (setq org-todo-keywords '((sequence "TODO(t)"
				      "STARTED(s)"
				      "WAITING(w@/!)"
				      "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
			    (sequence "TOBUY"
				      "TOSHRINK"
				      "TOCUT"
				      "TOSEW" "|" "DONE(x)")
			    (sequence "TOWATCH"
				      "UNRELEASED"
				      "RELEASED" "|" "WATCHED(w)" "BREAK(b)")
			    (sequence "TODO"
				      "DOING"
				      "TESTING"
				      "ALMOST" "|" "DONE(x)")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))

  ;; org-beamer
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       ;; beamer class, for presentations
	       '("beamer"
		 "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

		 ("\\section{%s}" . "\\section*{%s}")

		 ("\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}"
		  "\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

	       '("letter"
		 "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-latex-packages-alist
	(quote (("" "color" t)
		("" "minted" t)
		("" "parskip" t)
		("" "tikz" t)))))

(with-eval-after-load 'org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(with-eval-after-load 'ox-reveal
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
	org-reveal-mathjax t))

(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
	 (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end)))

(defun mk-helm-ref ()
  "Prompt for switching libraries."
  (interactive)
  (require 'org-ref)
  (helm :sources '(mk-helm-libraries-source)))


(with-eval-after-load 'org-ref
  (defun mk-set-libraries (library)
    "Set paths according to the selected library."
    (cond
     ((equal candidate "Research")
      (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/ref/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/ref/files/")
	    bibtex-completion-bibliography (concat org-directory "/ref/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ref/files")
	    bibtex-completion-notes-path   (concat org-directory "/ref/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "Ebooks")
      (setq org-ref-bibliography-notes     (concat org-directory "/ebooks/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/ebooks/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/ebooks/files/")
	    bibtex-completion-bibliography (concat org-directory "/ebooks/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ebooks/files")
	    bibtex-completion-notes-path   (concat org-directory "/ebooks/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "PDFs")
      (setq org-ref-bibliography-notes     (concat org-directory "/pdfs/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/pdfs/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/pdfs/files/")
	    bibtex-completion-bibliography (concat org-directory "/pdfs/master.bib")
	    bibtex-completion-library-path (concat org-directory "/pdfs/files")
	    bibtex-completion-notes-path   (concat org-directory "/pdfs/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     (t (message "Invalid!"))))
  (setq mk-helm-libraries-source
	'((name . "Select a library.")
	  (candidates . ("Research" "Ebooks" "PDFs"))
	  (action . (lambda (candidate)
		      (mk-set-libraries candidate)))))

  (defun my-orcb-key ()
    "Replace the key in the entry, also change the pdf file name if it exites."
    (let ((key (funcall org-ref-clean-bibtex-key-function
			(bibtex-generate-autokey))))
      ;; first we delete the existing key
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)

      (setq old-key (match-string 2));;store old key

      (if (match-beginning bibtex-key-in-head)
	  (delete-region (match-beginning bibtex-key-in-head)
			 (match-end bibtex-key-in-head)))
      ;; check if the key is in the buffer
      (when (save-excursion
	      (bibtex-search-entry key))
	(save-excursion
	  (bibtex-search-entry key)
	  (bibtex-copy-entry-as-kill)
	  (switch-to-buffer-other-window "*duplicate entry*")
	  (bibtex-yank))
	(setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
      (insert key)
      (kill-new key)

      (save-excursion
	"update pdf names and notes items"
	;; rename the pdf after change the bib item key
	(my-update-pdf-names old-key key)
	;; renmae the notes item after change the bib item key
	(my-update-notes-item old-key key))

      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)))
  ;; define a function that update the pdf file names before change the key of a bib entry
  (defun my-update-pdf-names (old-key new-key)
    (let ((old-filename (concat org-ref-pdf-directory old-key ".pdf"))
	  (new-filename (concat org-ref-pdf-directory new-key ".pdf" )))
      (if (file-exists-p old-filename)
	  (rename-file old-filename new-filename))))
  ;; define a function that update the notes items before change the key of bib entry
  (defun my-update-notes-item (old-key new-key)
    "update a notes item of a old-key by a new-key in case the bib item is changed"

    (set-buffer (find-file-noselect org-ref-bibliography-notes))
    ;; move to the beginning of the buffer
    (goto-char (point-min))
    ;; find the string and replace it
    (let ((newcite new-key)
	  (regstr old-key))

      (while (re-search-forward regstr nil t)

	(delete-region (match-beginning 0)
		       (match-end 0))
	(insert newcite))

      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)
      (kill-buffer)))
  (add-hook 'org-ref-clean-bibtex-entry-hook 'my-orcb-key))

(defun mk-open-bib-file()
  (interactive)
  (find-file (car org-ref-default-bibliography)))

(defun mk-open-note-file()
  (interactive)
  (find-file org-ref-bibliography-notes))

;;; bindings
(general-define-key
 :prefix "SPC o"
 :states '(normal visual motion)
 :keymaps 'override
 "a" 'org-agenda
 "e" 'mk-org-export
 "o" 'org-mode
 "c" 'org-capture
 "r" '(:ignore t :which-key "org-ref")
 "rs" 'mk-helm-ref
 "ri" 'org-ref-helm-insert-cite-link
 "rl" 'helm-bibtex
 "rd" 'doi-utils-add-bibtex-entry-from-doi
 "rn" 'mk-open-note-file
 "ro" 'mk-open-bib-file
 "j" '(:ignore t :which-key "org-journal")
 "jt" 'org-journal-new-entry
 "jy" 'journal-file-yesterday
 "l" 'org-store-link)


(evil-define-key 'normal bibtex-mode-map
  (kbd "C-j") 'org-ref-bibtex-next-entry
  (kbd "C-k") 'org-ref-bibtex-previous-entry
  "gj" 'org-ref-bibtex-next-entry
  "gk" 'org-ref-bibtex-previous-entry)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'bibtex-mode-map
 ;; Navigation
 "j" 'org-ref-bibtex-next-entry
 "k" 'org-ref-bibtex-previous-entry

 ;; Open
 "b" 'org-ref-open-in-browser
 "n" 'org-ref-open-bibtex-notes
 "p" 'org-ref-open-bibtex-pdf

 ;; Misc
 "h" 'org-ref-bibtex-hydra/body
 "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
 "s" 'org-ref-sort-bibtex-entry

 ;; Lookup utilities
 "l" '(:ignore t :which-key "lookup")
 "la" 'arxiv-add-bibtex-entry
 "lA" 'arxiv-get-pdf-add-bibtex-entry
 "ld" 'doi-utils-add-bibtex-entry-from-doi
 "li" 'isbn-to-bibtex
 "lp" 'pubmed-insert-bibtex-from-pmid)

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'org-mode-map
 "c" 'org-todo
 "s" 'org-schedule
 "C" '(:ignore t :which-key "org-crypt")
 "Ce" 'org-encrypt-entry
 "CE" 'org-encrypt-entries
 "Cd" 'org-decrypt-entry
 "CD" 'org-decrypt-entries
 "'" 'org-edit-special
 "d" 'org-deadline
 "D" 'org-insert-drawer
 "e" '(:ignore t :which-key "export")
 "ee" 'org-export-dispatch
 "f" 'org-set-effort
 "P" 'org-set-property
 ":" 'org-set-tags

 "b" 'org-tree-to-indirect-buffer
 "A" 'org-archive-subtree
 "l" 'org-open-at-point
 "T" 'org-show-todo-tree

 "." 'org-time-stamp
 "!" 'org-time-stamp-inactive

 ;; headings
 "h" '(:ignore t :which-key "headings")
 "hi" 'org-insert-heading-after-current
 "hI" 'org-insert-heading
 "hs" 'org-insert-subheading

 ;; More cycling options (timestamps, headlines, items, properties)
 "L" 'org-shiftright
 "H" 'org-shiftleft
 "J" 'org-shiftdown
 "K" 'org-shiftup

 ;; Change between TODO sets
 "C-S-l" 'org-shiftcontrolright
 "C-S-h" 'org-shiftcontrolleft
 "C-S-j" 'org-shiftcontroldown
 "C-S-k" 'org-shiftcontrolup

 ;; Subtree editing
 "S" '(:ignore t :which-key "subtree")
 "Sl" 'org-demote-subtree
 "Sh" 'org-promote-subtree
 "Sj" 'org-move-subtree-down
 "Sk" 'org-move-subtree-up

 ;; tables
 "t" '(:ignore t :which-key "table")
 "ta" 'org-table-align
 "tb" 'org-table-blank-field
 "tc" 'org-table-convert
 "td" '(:ignore t :which-key "delete")
 "tdc" 'org-table-delete-column
 "tdr" 'org-table-kill-row
 "te" 'org-table-eval-formula
 "tE" 'org-table-export
 "th" 'org-table-previous-field
 "tH" 'org-table-move-column-left
 "ti" '(:ignore t :which-key "insert")
 "tic" 'org-table-insert-column
 "tih" 'org-table-insert-hline
 "tiH" 'org-table-hline-and-move
 "tir" 'org-table-insert-row
 "tI" 'org-table-import
 "tj" 'org-table-next-row
 "tJ" 'org-table-move-row-down
 "tK" 'org-table-move-row-up
 "tl" 'org-table-next-field
 "tL" 'org-table-move-column-right
 "tn" 'org-table-create
 "tN" 'org-table-create-with-table.el
 "tr" 'org-table-recalculate
 "ts" 'org-table-sort-lines
 "tt" '(:ignore t :which-key "toggles")
 "ttf" 'org-table-toggle-formula-debugger
 "tto" 'org-table-toggle-coordinate-overlays
 "tw" 'org-table-wrap-region
 )

(general-define-key
 :prefix "SPC K"
 :states '(normal visual motion emacs)
 :keymaps 'org-agenda-mode-map
 "h" '(:ignore t :which-key "headings")
 "ht" 'org-agenda-todo
 "hk" 'org-agenda-kill
 "hr" 'org-agenda-refile
 "hA" 'org-agenda-archive-default
 "hT" 'org-agenda-set-tags
 "hp" 'org-agenda-priority

 ;; Visit entry
 "SPC" 'org-agenda-show-and-scroll-up
 "<tab>" 'org-agenda-goto
 "TAB" 'org-agenda-goto
 "RET" 'org-agenda-switch-to
 "o"   'link-hint-open-link

 ;; Date
 "d" '(:ignore t :which-key "date")
 "ds" 'org-agenda-schedule
 "dd" 'org-agenda-deadline
 "dt" 'org-agenda-date-prompt
 "+" 'org-agenda-do-date-later
 "-" 'org-agenda-do-date-earlier

 ;; View
 "v" '(:ignore t :which-key "view")
 "vd" 'org-agenda-day-view
 "vw" 'org-agenda-week-view
 "vt" 'org-agenda-fortnight-view
 "vm" 'org-agenda-month-view
 "vy" 'org-agenda-year-view
 "vn" 'org-agenda-later
 "vp" 'org-agenda-earlier
 "vr" 'org-agenda-reset-view

 ;; Toggle mode
 "t" '(:ignore t :which-key "toggle")
 "tf" 'org-agenda-follow-mode
 "tl" 'org-agenda-log-mode
 "ta" 'org-agenda-archives-mode
 "tr" 'org-agenda-clockreport-mode
 "td" 'org-agenda-toggle-diary

 ;; Filter
 "f" '(:ignore t :which-key "filter")
 "ft" 'org-agenda-filter-by-tag
 "fr" 'org-agenda-filter-by-tag-refine
 "fc" 'org-agenda-filter-by-category
 "fh" 'org-agenda-filter-by-top-headline
 "fx" 'org-agenda-filter-by-regexp
 "fd" 'org-agenda-filter-remove-all

 ;; Clock
 "c" '(:ignore t :which-key "clock")
 "ci" 'org-agenda-clock-in
 "co" 'org-agenda-clock-out
 "ck" 'org-agenda-clock-cancel
 "cj" 'org-agenda-clock-goto

 ;; Other
 "q" 'org-agenda-quit
 "gr" 'org-agenda-redo
 "." 'org-agenda-goto-today
 "gd" 'org-agenda-goto-date)

(eval-after-load 'org-agenda
  '(progn
     (evil-set-initial-state 'org-agenda-mode 'normal)
     (evil-define-key 'normal org-agenda-mode-map
       (kbd "<RET>") 'org-agenda-switch-to
       (kbd "\t") 'org-agenda-goto

       "q" 'org-agenda-quit
       "r" 'org-agenda-redo
       "S" 'org-save-all-org-buffers
       "gj" 'org-agenda-goto-date
       "gJ" 'org-agenda-clock-goto
       "gm" 'org-agenda-bulk-mark
       "go" 'org-agenda-open-link
       "s" 'org-agenda-schedule
       "+" 'org-agenda-priority-up
       "," 'org-agenda-priority
       "-" 'org-agenda-priority-down
       "y" 'org-agenda-todo-yesterday
       "n" 'org-agenda-add-note
       "t" 'org-agenda-todo
       ":" 'org-agenda-set-tags
       ";" 'org-timer-set-timer
       "I" 'helm-org-task-file-headings
       "i" 'org-agenda-clock-in-avy
       "O" 'org-agenda-clock-out-avy
       "u" 'org-agenda-bulk-unmark
       "x" 'org-agenda-exit
       "j"  'org-agenda-next-line
       "k"  'org-agenda-previous-line
       "vt" 'org-agenda-toggle-time-grid
       "va" 'org-agenda-archives-mode
       "vw" 'org-agenda-week-view
       "vl" 'org-agenda-log-mode
       "vd" 'org-agenda-day-view
       "vc" 'org-agenda-show-clocking-issues
       "g/" 'org-agenda-filter-by-tag
       "o" 'delete-other-windows
       "gh" 'org-agenda-holiday
       "gv" 'org-agenda-view-mode-dispatch
       "f" 'org-agenda-later
       "b" 'org-agenda-earlier
       "c" 'helm-org-capture-templates
       "e" 'org-agenda-set-effort
       "n" nil  ; evil-search-next
       "{" 'org-agenda-manipulate-query-add-re
       "}" 'org-agenda-manipulate-query-subtract-re
       "A" 'org-agenda-toggle-archive-tag
       "." 'org-agenda-goto-today
       "0" 'evil-digit-argument-or-evil-beginning-of-line
       "<" 'org-agenda-filter-by-category
       ">" 'org-agenda-date-prompt
       "F" 'org-agenda-follow-mode
       "D" 'org-agenda-deadline
       "H" 'org-agenda-holidays
       "J" 'org-agenda-next-date-line
       "K" 'org-agenda-previous-date-line
       "L" 'org-agenda-recenter
       "P" 'org-agenda-show-priority
       "R" 'org-agenda-clockreport-mode
       "Z" 'org-agenda-sunrise-sunset
       "T" 'org-agenda-show-tags
       "X" 'org-agenda-clock-cancel
       "[" 'org-agenda-manipulate-query-add
       "g\\" 'org-agenda-filter-by-tag-refine
       "]" 'org-agenda-manipulate-query-subtract)))
(provide 'mk-org)
;;; mk-org.el ends here
