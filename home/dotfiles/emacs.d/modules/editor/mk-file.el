;;; mk-file.el --- File  -*- lexical-binding: t; -*-

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
  (use-package projectile
    :ensure t
    :defer t
    :commands (projectile-project-root)
    :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (use-package helm-projectile
    :ensure t
    :defer t
    :commands (helm-projectile-switch-project
	       helm-projectile-find-file
	       helm-projectile-find-file-in-known-projects
	       helm-projectile-recentf
	       helm-projectile-ag)
    :config
    (helm-projectile-on)
    (setq projectile-use-git-grep t))
  )

(when (string= mk-completion "light")
  (use-package consult-project-extra
    :ensure t
    :ensure t)

  (use-package project-x
    :after project
    :config
    (add-hook 'project-find-functions 'project-x-try-local 90)
    (add-hook 'kill-emacs-hook 'project-x--window-state-write)
    (setq project-switch-commands #'project-x-windows)
    )

  (defun mk-project-close ()
    "Close all buffers associated with the current project."
    (interactive)
    (let ((project (project-current t)))
      (if project
          (let* ((project-root (project-root project))
		 (buffers (project-buffers project)))
            (dolist (buffer buffers)
              (when (string-prefix-p project-root (or (buffer-file-name buffer) ""))
		(kill-buffer buffer)))
            (message "Closed all buffers for project: %s" project-root))
	(message "No project found."))))

  )

(use-package recentf
  :ensure t
  :defer t
  :init
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
					 (recentf-mode)
					 (recentf-track-opened-file))))
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         (expand-file-name mk-backup-dir)
                         (expand-file-name mk-local-dir)
                         (expand-file-name org-directory)
                         (expand-file-name (concat mk-emacs-dir "emms/"))
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 15)
  (recentf-save-file (concat mk-backup-dir "recentf"))
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package docker-tramp
  :ensure t
  :defer t)

(defun mk-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-init 'dired)
  (let ((args (list "-aBhl" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " ")))
  (evil-define-key 'normal dired-mode-map (kbd "/") 'dired-narrow
    (kbd "P") 'peep-dired
    (kbd "t") 'dired-subtree-insert
    (kbd "T") 'dired-subtree-remove
    (kbd "q") 'mk-kill-dired-buffers)
  (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
    (kbd "<backspace>") 'peep-dired-scroll-page-up
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  :init
  (setq dired-auto-revert-buffer t
	dired-dwim-target t
	dired-hide-details-hide-symlink-targets nil
	;; Always copy/delete recursively
	dired-recursive-copies  'always
	dired-recursive-deletes 'top
	;; Where to store image caches
	image-dired-dir (concat mk-cache-dir "image-dired/")
	image-dired-db-file (concat image-dired-dir "db.el")
	image-dired-gallery-dir (concat image-dired-dir "gallery/")
	image-dired-temp-image-file (concat image-dired-dir "temp-image")
	image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

(use-package diredfl
  :ensure t
  :defer t
  :after dired
  :config
  (diredfl-global-mode))

(use-package peep-dired
  :ensure t
  :after dired
  :defer t
  :init
  (setq peep-dired-cleanup-on-disable t
	peep-dired-cleanup-eagerly t
	peep-dired-enable-on-directories t
	peep-dired-ignored-extensions '("mkv" "iso" "mp4")))

(use-package dired-narrow
  :ensure t
  :after dired
  :defer t)

(use-package dired-subtree
  :ensure t
  :after dired
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal treemacs-mode-map
    (kbd "d") 'treemacs-delete-file
    (kbd "a") 'treemacs-create-file
    (kbd "<") 'treemacs-decrease-width
    (kbd ">") 'treemacs-increase-width
    (kbd "H") 'treemacs-toggle-show-dotfiles
    (kbd "<tab>") 'treemacs-RET-action
    (kbd "RET") 'treemacs-rename-file
    (kbd "r") 'treemacs-rename-file)
  (treemacs-project-follow-mode 1)
  (treemacs-peek-mode 1)
  (treemacs-load-all-the-icons-with-workaround-font "FiraCode Nerd Font")
  )

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :defer t
  )

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  :defer t
  )

(when (string= mk-completion "featured")
  (use-package treemacs-projectile
    :after (treemacs projectile)
    :defer t))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :defer t)

(use-package treemacs-icons-dired
  :ensure t
  :after (treemacs dired)
  :defer t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  :defer t)

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(use-package direnv
  :config
  (direnv-mode))

(use-package inheritenv
  :ensure t
  )
;;; config
(with-eval-after-load 'projectile
  (setq projectile-globally-ignored-directories
        '(".bzr"
          ".ensime_cache"
          ".eunit"
          ".fslckout"
          ".git"
          ".hg"
          ".idea"
          ".stack-work"
          ".svn"
          ".tox"
          ".clangd"
          ".ccls-cache"
          "READONLY"
          "_FOSSIL_"
          "_darcs"
          "blaze-bin"
          "blaze-genfiles"
          "blaze-google3"
          "blaze-out"
          "blaze-testlogs"
          "node_modules"
          "third_party"
	  "backup"
          "vendor"))
  (setq projectile-completion-system 'helm
	projectile-enable-caching t
	projectile-switch-project-action 'helm-projectile-find-file)
  (projectile-global-mode))

(with-eval-after-load 'treemacs
  (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
	treemacs-deferred-git-apply-delay      0.5
	treemacs-display-in-side-window        t
	treemacs-eldoc-display                 t
	treemacs-file-event-delay              5000
	treemacs-file-follow-delay             0.01
	treemacs-follow-after-init             t
	treemacs-git-command-pipe              ""
	treemacs-goto-tag-strategy             'refetch-index
	treemacs-indentation                   2
	treemacs-indentation-string            " "
	treemacs-is-never-other-window         nil
	treemacs-max-git-entries               5000
	treemacs-missing-project-action        'ask
	treemacs-no-png-images                 nil
	treemacs-no-delete-other-windows       t
	treemacs-project-follow-cleanup        nil
	treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	treemacs-recenter-distance             0.1
	treemacs-recenter-after-file-follow    nil
	treemacs-recenter-after-tag-follow     nil
	treemacs-recenter-after-project-jump   'always
	treemacs-recenter-after-project-expand 'on-distance
	treemacs-show-cursor                   nil
	treemacs-show-hidden-files             nil
	treemacs-silent-filewatch              nil
	treemacs-silent-refresh                nil
	treemacs-sorting                       'alphabetic-asc
	treemacs-space-between-root-nodes      t
	treemacs-tag-follow-cleanup            nil
	treemacs-tag-follow-delay              0.05
	treemacs-width                         28)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))


(defun mk-rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.
When NEW-FILENAME is not specified, asks user for a new name.
Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (message "File '%s' successfully renamed to '%s'" short-name (file-name-nondirectory new-name)))))))

;; from magnars
(defun mk-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
		 (recentf-add-file new-name)
		 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun mk-delete-file (filename &optional ask-user)
  "Remove specified file or directory.
Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.
When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename))))

(defun mk-delete-file-confirm (filename)
  "Remove specified file or directory after users approval.
FILENAME is deleted using `mk-delete-file' function.."
  (interactive "f")
  (funcall-interactively #'mk-delete-file filename t))

;; from magnars
(defun mk-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun mk-sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))


(defun mk-delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun mk-ace-delete-window (&optional arg)
  "Ace delete window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (mk-kill-this-buffer arg)))
     (aw-delete-window window))))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun mk-kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun mk-ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (mk-kill-this-buffer arg))))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun mk-kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun mk-toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
	 (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
	     (if dedicated "no longer " "")
	     (buffer-name))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun mk-show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun mk-new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; http://stackoverflow.com/a/10216338/4869
(defun mk-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun mk-copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun mk-dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun mk-unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun mk-copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;; from http://www.emacswiki.org/emacs/WordCount
(defun mk-count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words
        alist_words_compare
        (formated "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formated 0 -2)))
        (message "No words.")))
    words))

;;; bindings
(leader
  "p" '(:ignore t :which-key "Projects"))

(when (string= mk-completion "light")
  (leader
    "fg" 'consult-git-grep
    )
  (leader
    "pa" 'consult-project-extra-find-other-window
    "pc" 'mk-project-close
    "pf" 'project-find-file
    "pF" 'consult-project-extra-find
    "pb" 'project-switch-to-buffer
    "pd" 'project-find-dir
    "pp" 'project-switch-project
    "ps" 'project-x-window-state-save
    "pl" 'project-x-window-state-load
    )
  )

(when (string= mk-completion "featured")
  (leader
    "fg" 'helm-projectile-grep
    )
  (leader
    "pi" 'projectile-invalidate-cache
    "pz" 'projectile-cache-current-file
    "pa" 'helm-projectile-find-other-file
    "pb" 'helm-projectile-switch-to-buffer
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file
    "pF" 'helm-projectile-find-file-in-known-projects
    "pg" 'helm-projectile-find-file-dwim
    "pp" 'helm-projectile-switch-project
    "pr" 'helm-projectile-recentf
    ))

(leader
  "ad" 'dired)

(leader
  "tf" 'treemacs)

(leader
  "fe" 'treemacs
  "fR" '(:ignore t :which-key "rename")
  "fRf" 'mk-rename-file
  "fRb" 'mk-rename-current-buffer-file
  "fd" '(:ignore t :which-key "delete")
  "fdf" 'mk-delete-file-confirm
  "fdb" 'mk-delete-current-buffer-file
  "fdw" 'mk-delete-window
  "fda" 'mk-ace-delete-window
  "fk" '(:ignore t :which-key "kill")
  "fkb" 'mk-kill-this-buffer
  "fka" 'mk-ace-kill-this-buffer
  "fko" 'mk-kill-other-buffers
  "fD" 'mk-toggle-current-window-dedication
  "fs" 'mk-sudo-edit
  "fF" 'mk-show-and-copy-buffer-filename
  "fn" 'mk-new-empty-buffer
  "fy" 'mk-copy-whole-buffer-to-clipboard
  "fp" 'mk-copy-clipboard-to-whole-buffer
  "fC" '(:ignore t :which-key "convert")
  "fCu" 'mk-dos2unix
  "fCd" 'mk-unix2dos
  "fc" 'mk-copy-file
  "fa" 'mk-count-words-analysis)


(provide 'mk-file)
;;; mk-file.el ends here
