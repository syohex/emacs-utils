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

(require 'notifications)

(defgroup pomodoro nil
  "Pomodoro in Emacs"
  :prefix "pomodoro-"
  :group 'pomodoro)

(defcustom pomodoro:work-time 25
  "Work minitus"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:rest-time 5
  "Rest minutes"
  :group 'pomodoro
  :type 'integer)

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

(defvar pomodoro:current-state 'working
  "Pomodoro statement flag, working or rest")

(defvar pomodoro:remainder-seconds 0)

(defmacro pomodoro:set-mode (mode)
  `(setq pomodoro:current-state ,mode))

(defun pomodoro:switch-mode ()
  (if (eq pomodoro:current-state 'working)
      (pomodoro:set-mode 'rest)
    (pomodoro:set-mode 'working)))

(defmacro pomodoro:reset-remainder-time (time)
  `(setq pomodoro:remainder-seconds (* ,time 60)))

(defun pomodoro:reset-time ()
  (if (eq pomodoro:current-state 'working)
      (pomodoro:reset-remainder-time pomodoro:rest-time)
    (pomodoro:reset-remainder-time pomodoro:work-time)))

(defvar pomodoro:mode-line "")

(defvar pomodoro:mode-line-sign "●"
  "Show which is working or resting now")

(defvar pomodoro:finish-work-hook nil)
(defvar pomodoro:finish-rest-hook nil)

(add-hook 'pomodoro:finish-work-hook
          (lambda ()
            (notifications-notify :title "Pomodoro"
                                  :body "Work is finished"
                                  :urgency 'critical)))

(add-hook 'pomodoro:finish-rest-hook
          (lambda ()
            (notifications-notify :title "Pomodoro"
                                  :body "Break time is finished"
                                  :urgency 'critical)))

(defun pomodoro:time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun pomodoro:propertize-sign ()
  (if (eq pomodoro:current-state 'working)
      (propertize pomodoro:mode-line-sign 'face 'pomodoro:work-face)
    (propertize pomodoro:mode-line-sign 'face 'pomodoro:rest-face)))

(defun pomodoro:propertize-mode-line ()
  (unless (string= pomodoro:mode-line "")
    (concat (pomodoro:propertize-sign)
            (propertize pomodoro:mode-line 'face 'pomodoro:timer-face))))

(defun pomodoro:set-mode-line ()
  (setq pomodoro:mode-line
        (pomodoro:time-to-string pomodoro:remainder-seconds)))

(defun pomodoro:tick ()
  (let ((remainder-seconds (1- pomodoro:remainder-seconds)))
    (cond ((< remainder-seconds 0)
           (pomodoro:switch-mode)
           (pomodoro:reset-time)
           (run-hook 'pomodoro:finish-work-hook))
          (t (decf pomodoro:remainder-seconds)))
    (pomodoro:set-mode-line)
    (force-mode-line-update)))

(defun pomodoro:set-remainder-second (times)
  (setq pomodoro:remainder-seconds (* 60 times pomodoro:work-time)))

(defun pomodoro:start (arg)
  (interactive "p")
  (pomodoro:set-remainder-second arg)
  (setq pomodoro:timer (run-with-timer 0 1 'pomodoro:tick)))

(defun pomodoro:stop ()
  (interactive)
  (cancel-timer pomodoro:timer)
  (setq pomodoro:mode-line "")
  (force-mode-line-update))

(setq-default mode-line-format
              (cons '(:eval (concat (pomodoro:propertize-mode-line)))
                    mode-line-format))

(provide 'pomodoro)
;;; pomodoro.el ends here
