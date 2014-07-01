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

(require 'cl-lib)
(require 'diff)

(declare-function helm "helm")
(declare-function helm-candidate-buffer "helm")
(declare-function helm-attrset "helm")

(defgroup sgit nil
  "Simple git utils"
  :prefix "sgit:"
  :group 'vc)

(defvar sgit:buffer "*sgit*"
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

(defun sgit:top-directory ()
  (with-temp-buffer
    (unless (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
      (error "Failed: 'git rev-parse --show-toplevel'"))
    (goto-char (point-min))
    (file-name-as-directory
     (buffer-substring-no-properties (point) (line-end-position)))))

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

(defvar sgit:grep-history nil)

(defun sgit:grep-init ()
  (let ((cmd (read-string "> "
                          (concat "git grep -n "
                                  (substring-no-properties
                                   (or (thing-at-point 'symbol)
                                       "")))
                          'sgit:grep-history)))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: '%s'" cmd))
      (when (zerop (length (buffer-string)))
        (error "No output: '%s'" cmd)))))

(defvar sgit-source-grep
  '((name . "sgit git grep")
    (init . sgit:grep-init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun sgit:grep ()
  (interactive)
  (let ((default-directory (sgit:top-directory)))
    (helm :sources '(sgit-source-grep) :buffer "*sgit-grep*")))

(defface sgit:git-log-commit-header
  '((t (:foreground "yellow" :weight bold)))
  "Face of commit header"
  :group 'sgit)

(provide 'sgit)

;;; sgit.el ends here
