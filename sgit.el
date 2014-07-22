;;; sgit.el --- My own git utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(eval-when-compile
  (defvar git-gutter-mode))

(require 'cl-lib)
(require 'diff)

(declare-function git-gutter "git-gutter")

(defgroup sgit nil
  "Simple git utils"
  :prefix "sgit:"
  :group 'vc)

(defvar sgit:buffer " *sgit*"
  "Name of the buffer where is execute command")

(defun sgit:exec (cmd &optional mode-func)
  (when (get-buffer sgit:buffer)
    (kill-buffer (get-buffer sgit:buffer)))
  (let ((buf (get-buffer-create sgit:buffer)))
    (with-current-buffer buf
      (read-only-mode -1)
      (view-mode -1)
      (erase-buffer))
    (set-process-sentinel
     (start-process-shell-command "sgit" buf cmd)
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (if (string= (buffer-string) "")
               (message "No Changes")
             (goto-char (point-min))
             (when mode-func
               (funcall mode-func))
             (view-mode +1)
             (read-only-mode +1)
             (pop-to-buffer (current-buffer)))))))))

(defun sgit:prompt (git-cmd &optional option)
  (read-string "> " (format "git %s %s " git-cmd (or option ""))))

(defun sgit:file-name ()
  (cl-case major-mode
    (dired-mode (dired-get-filename nil t))
    (otherwise (buffer-file-name))))

(defun sgit:git-cmd (subcmd &optional mode-func)
  (let ((cmd (format "git --no-pager %s %s"
                     subcmd
                     (expand-file-name (sgit:file-name)))))
    (sgit:exec cmd mode-func)))

;;;###autoload
(defun sgit:status ()
  (interactive)
  (sgit:git-cmd (concat "status" " .")))

;;;###autoload
(defun sgit:log ()
  (interactive)
  (let ((cmd (if current-prefix-arg
                 "log -p --stat "
               "log")))
    (sgit:git-cmd cmd)))

;;;###autoload
(defun sgit:diff ()
  (interactive)
  (let ((cmd (if current-prefix-arg
                 "diff --cached"
               "diff")))
    (sgit:git-cmd cmd 'diff-mode)))

;;;###autoload
(defun sgit:intent-to-add ()
  (interactive)
  (save-buffer)
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (unless (zerop (call-process "git" nil nil nil "add" "-N" file))
      (error "Failed: 'git add -N %s'" file))
    (message "Success: Staging %s" file))
  (when git-gutter-mode
    (git-gutter)))

(provide 'sgit)

;;; sgit.el ends here
