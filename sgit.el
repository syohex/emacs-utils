;;; sgit.el --- My own git utilities

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01
;; Package-Requires: ((helm "1.0"))

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

;;; Code:

(require 'helm)
(require 'diff)

(defgroup sgit nil
  "Ack command with helm interface"
  :prefix "sgit:"
  :group 'vc)

(defvar sgit:buffer "*sgit*"
  "Name of the buffer where is execute command")

(defun sgit:exec (cmd &optional mode-func)
  (when (get-buffer sgit:buffer)
    (kill-buffer (get-buffer sgit:buffer)))
  (let ((buf (get-buffer-create sgit:buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((ret (call-process-shell-command cmd nil t)))
        (unless (zerop ret)
          (error (format "Failed '%s'" cmd)))
        (goto-char (point-min))
        (when mode-func
          (funcall mode-func))
        (setq buffer-read-only t)
        (pop-to-buffer buf)))))

(defun sgit:top-directory ()
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error (format "Failed '%s'" cmd)))
      (goto-char (point-min))
      (file-name-as-directory
       (buffer-substring-no-properties (point) (line-end-position))))))

(defun sgit:prompt (git-cmd &optional option)
  (read-string "> " (format "git %s %s " git-cmd (or option ""))))

(defun sgit:file-name (file)
  (file-relative-name file (sgit:top-directory)))

(defun sgit:default-target (cmd)
  (cond ((string= cmd "status") ".")
        (t (buffer-file-name))))

(defun sgit:target-path (cmd)
  (cond ((equal current-prefix-arg '(16))  "")
        ((equal current-prefix-arg '(4)) ".")
        (t (sgit:default-target cmd))))

(defun sgit:git-cmd (git-cmd &optional mode-func)
  (let ((cmd (format "git %s %s" git-cmd (sgit:target-path git-cmd))))
    (sgit:exec cmd mode-func)))

(defun sgit:status ()
  (interactive)
  (sgit:git-cmd "status"))

(defun sgit:log ()
  (interactive)
  (sgit:git-cmd "log" #'sgit:git-log-mode))

(defun sgit:diff ()
  (interactive)
  (sgit:git-cmd "diff" #'diff-mode))

(defface sgit:git-log-commit-header
  '((t (:foreground "yellow" :weight bold)))
  "Face of commit header"
  :group 'sgit)

(define-generic-mode sgit:git-log-mode
  nil
  nil
  ;; highlight setting
  '(("commit\\s-+[0-9a-zA-Z]+" . 'sgit:git-log-commit-header))
  nil
  nil
  "Major mode for 'git log'")

(provide 'sgit)

;;; sgit.el ends here
