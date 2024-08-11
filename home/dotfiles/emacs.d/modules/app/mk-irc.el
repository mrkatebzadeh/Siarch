;;; mk-irc.el --- IRC -*- lexical-binding: t; -*-

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

;;;###autoload
(defun mk-erc-browse-last-url ()
  "Searchs backwards through an ERC buffer, looking for a URL. When a URL is
     found, it prompts you to open it."
  (interactive)
  (save-excursion
    (let ((ffap-url-regexp "\\(https?://\\)."))
      (ffap-next-url t t))))

;;;###autoload
(defun mk-erc-count-users ()
  "Displays the number of users and ops connected on the current channel."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (let ((channel (erc-default-target)))
	(if (and channel (erc-channel-p channel))
	    (let ((hash-table (with-current-buffer (erc-server-buffer)
				erc-server-users))
		  (users 0)
		  (ops 0))
	      (maphash (lambda (k v)
			 (when (member (current-buffer)
				       (erc-server-user-buffers v))
			   (incf users))
			 (when (erc-channel-user-op-p k)
			   (incf ops)))
		       hash-table)
	      (message "%d users (%s ops) are online on %s" users ops channel))
	  (user-error "The current buffer is not a channel")))
    (user-error "You must first be connected on IRC")))

;;;###autoload
(defun mk-erc-get-ops ()
  "Displays the names of ops users on the current channel."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (let ((channel (erc-default-target)))
	(if (and channel (erc-channel-p channel))
	    (let (ops)
	      (maphash (lambda (nick cdata)
			 (if (and (cdr cdata)
				  (erc-channel-user-op (cdr cdata)))
			     (setq ops (cons nick ops))))
		       erc-channel-users)
	      (if ops
		  (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
		(message "There are no ops users online on %s" channel)))
	  (user-error "The current buffer is not a channel")))
    (user-error "You must first be connected on IRC")))

;;;###autoload
(defun mk-erc-notify (nickname message)
  "Displays a notification message for ERC."
  (let* ((channel (buffer-name))
	 (nick (erc-hl-nicks-trim-irc-nick nickname))
	 (title (if (string-match-p (concat "^" nickname) channel)
		    nick
		  (concat nick " (" channel ")")))
	 (msg (s-trim (s-collapse-whitespace message))))
    (alert (concat nick ": " msg) :title title)))

;;;###autoload
(defun mk-erc-preprocess (string)
  "Avoids channel flooding."
  (setq str (string-trim (replace-regexp-in-string "\n+" " " str))))

;;;###autoload
(defun mk-erc-reset-track-mode ()
  "Resets ERC track mode."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update)
  (erc-modified-channels-display)
  (force-mode-line-update))

;;;###autoload
(defun mk-erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (erc :server "irc.freenode.net" :port 6667 :nick "mrkatebzadeh")))

(use-package erc
  :defer t
  :hook (;(ercn-notify . mk-erc-notify)
         (erc-send-pre . mk-erc-preprocess))
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "#bash" "##c++"
                                  "#emacs" "#i3" "#latex" "#org-mode" "#python"
                                  "#dwm" "#voidlinux" "#arista" "vim" "suckless"
				  "cisco" "networking" "nm" "bash" "docker" "linux")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format "%n on %t (%m)")
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'bury)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
					;(add-to-list 'erc-modules 'notifications)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package erc-hl-nicks
  :defer t
  :after erc)
(use-package erc-image
  :defer t
  :after erc)

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "i" 'mk-erc-start-or-switch)

(provide 'mk-irc)
;;; mk-irc.el ends here
