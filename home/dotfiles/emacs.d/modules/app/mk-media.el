;;; mk-media.el --- Media  -*- lexical-binding: t; -*-

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

;;; emms
(use-package emms
  :defer  t)

(use-package emms-info-mediainfo
  :defer t)

(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq mpc-host "localhost:6600"))

;;;###autoload
(defun mk-mpd-start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mk-mpd-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

;;;###autoload
(defun mk-mpd-kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

;;;###autoload
(defun mk-mpd-update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

;;;###autoload
(defun mk-emms-start-mpd ()
  "Run EMMS and MPD"
  (interactive)
  (mk-mpd-start-music-daemon)
  (emms))

;;; volume
(setq volume-step "10%")

(defun volume-increase ()
  "Increase the volume by volume-setp"
  (interactive)
  (shell-command (concat "amixer set Master " volume-step "+")))

(defun volume-decrease ()
  "Decrease the volume by volume-setp"
  (interactive)
  (shell-command (concat "amixer set Master " volume-step "-")))

(defun volume-toggle ()
  "Mute/unmute the volume."
  (interactive)
  (shell-command "amixer set Master toggle"))

(defun volume-max ()
  "Set the volume to 100%."
  (interactive)
  (shell-command "amixer set Master 100%"))

(general-define-key
 :prefix "SPC m"
 :states '(normal visual motion)
 :keymaps 'override
 "a" 'mk-emms-start-mpd
 "n" 'emms-player-mpd-next
 "p" 'emms-player-mpd-previous
 "s" 'emms-player-mpd-play
 "e" 'emms-player-mpd-pause
 "u" 'mk-mpd-update-database
 "q" 'mk-mpd-kill-music-daemon)

(general-define-key
 :prefix "SPC v"
 :states '(normal visual motion)
 :keymaps 'override
 "i" 'volume-increase
 "d" 'volume-decrease
 "t" 'volume-toggle
 "m" 'volume-max)



(provide 'mk-media)
;;; mk-media.el ends here
