;;; pomodoro.el --- Pomodoro Technique in Emacs

;; Author: Syohei Yoshida(syohex@gmail.com)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup pomodoro nil
  "Pomodoro in Emacs"
  :prefix "pomodoro:"
  :group 'pomodoro)

(defcustom pomodoro:file "~/.emacs.d/pomodoro.org"
  "Pomodoro check file"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:work-time 25
  "Work minitus"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:rest-time 5
  "Rest minutes"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:long-rest-time 30
  "Rest minutes"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:mode-line-work-sign "●"
  "String in mode line at work"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:mode-line-rest-sign "●"
  "String in mode line at rest"
  :group 'pomodoro
  :type 'string)

(defface pomodoro:work-face
  '((t (:foreground "red")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro:rest-face
  '((t (:foreground "blue")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro:timer-face
  '((t (:weight bold)))
  "mode-line-face"
  :group 'pomodoro)

(defvar pomodoro:timer nil)

(defvar pomodoro:work-count 0)

(defvar pomodoro:current-state 'working
  "Pomodoro statement flag, working or rest")

(defvar pomodoro:remainder-seconds 0)

(defvar pomodoro:finish-work-hook nil)
(defvar pomodoro:finish-rest-hook nil)
(defvar pomodoro:long-rest-hook nil)

(defmacro pomodoro:set-state (mode)
  `(setq pomodoro:current-state ,mode))

(defmacro pomodoro:reset-remainder-time (time)
  `(setq pomodoro:remainder-seconds (* ,time 60)))

(defun pomodoro:switch-to-long-rest ()
  (run-hooks 'pomodoro:long-rest-hook)
  (pomodoro:reset-remainder-time pomodoro:long-rest-time))

(defun pomodoro:long-rest-p ()
  (zerop (mod pomodoro:work-count 4)))

(defun pomodoro:switch-to-rest ()
  (pomodoro:set-state 'rest)
  (find-file pomodoro:file)
  (incf pomodoro:work-count)
  (run-hooks 'pomodoro:finish-work-hook)
  (cond ((pomodoro:long-rest-p)
         (pomodoro:switch-to-long-rest))
        (t
         (pomodoro:reset-remainder-time pomodoro:rest-time))))

(defvar pomodoro:mode-line "")

(defun pomodoro:time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun pomodoro:propertize-sign ()
  (if (eq pomodoro:current-state 'working)
      (propertize pomodoro:mode-line-work-sign 'face 'pomodoro:work-face)
    (propertize pomodoro:mode-line-rest-sign 'face 'pomodoro:rest-face)))

(defun pomodoro:propertize-mode-line ()
  (unless (string= pomodoro:mode-line "")
    (concat (pomodoro:propertize-sign)
            (propertize pomodoro:mode-line 'face 'pomodoro:timer-face))))

(defun pomodoro:set-mode-line ()
  (setq pomodoro:mode-line
        (pomodoro:time-to-string pomodoro:remainder-seconds)))

(defun pomodoro:expire ()
  (if (eq pomodoro:current-state 'working)
      (pomodoro:switch-to-rest)
    (progn
      (run-hooks 'pomodoro:finish-rest-hook)
      (run-with-timer 0 nil 'pomodoro:stop))))

(defun pomodoro:tick ()
  (let ((remainder-seconds (1- pomodoro:remainder-seconds)))
    (if (< remainder-seconds 0)
        (pomodoro:expire)
      (decf pomodoro:remainder-seconds))
    (pomodoro:set-mode-line)
    (pomodoro:propertize-mode-line)
    (force-mode-line-update)))

(defun pomodoro:set-remainder-second (minutes)
  (setq pomodoro:remainder-seconds (* 60 minutes)))

(defun pomodoro:clear-mode-line ()
  (setq pomodoro:mode-line "")
  (force-mode-line-update))

(defun pomodoro:current-time-to-string ()
  (format-time-string "%m:%d" (current-time)))

(defvar pomodoro:last-work-time nil
  "Last time of pomodoro work(format 'Month:Day')")

(defun pomodoro:last-work-today-p ()
  (or (not pomodoro:last-work-time)
      (string= pomodoro:last-work-time (pomodoro:current-time-to-string))))

(defun pomodoro:today-work-count ()
  (interactive)
  (message "Today's Pomodoro Count is %d !!" pomodoro:work-count))

(defun pomodoro:start (arg)
  (interactive "P")
  (if pomodoro:timer
    (error "Already start timer!!"))
  (if (consp current-prefix-arg)
      (setq arg (string-to-int (read-string "How long pomodoro time >> "))))
  (when (not (pomodoro:last-work-today-p))
    (message "Reset Pomodoro Count")
    (setq pomodoro:work-count 0))
  (setq pomodoro:last-work-time (pomodoro:current-time-to-string))
  (pomodoro:set-state 'working)
  (pomodoro:set-remainder-second (or arg pomodoro:work-time))
  (setq pomodoro:timer (run-with-timer 0 1 'pomodoro:tick)))

(defun pomodoro:stop (&optional do-reset)
  (interactive)
  (pomodoro:set-state nil)
  (if do-reset
      (setq pomodoro:work-count 0))
  (cancel-timer pomodoro:timer)
  (setq pomodoro:timer 'nil)
  (pomodoro:clear-mode-line))

(defun pomodoro:reset ()
  (interactive)
  (pomodoro:stop t))

(defvar pomodoro:set-mode-line-p nil)

(unless pomodoro:set-mode-line-p
  (setq-default mode-line-format
                (cons '(:eval (concat (pomodoro:propertize-mode-line)))
                      mode-line-format))
  (setq pomodoro:set-mode-line-p t))

(provide 'pomodoro)
;;; pomodoro.el ends here
