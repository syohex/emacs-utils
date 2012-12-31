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

(defun sgit:git-cmd (git-cmd mode-func)
  (let* ((file (buffer-file-name))
         (cmd (or (and current-prefix-arg
                       (sgit:prompt git-cmd (sgit:file-name file)))
                  (format "git %s %s" git-cmd file))))
    (sgit:exec cmd mode-func)))

(defun sgit:log ()
  (interactive)
  (sgit:git-cmd "log" #'sgit:git-log-mode))

(defun sgit:diff ()
  (interactive)
  (sgit:git-cmd "diff" #'diff-mode))

(defun helm-c-sgit-grep-init ()
  (let ((precmd (sgit:prompt "grep" "-n"))
        (file (or (and current-prefix-arg ".")
                  (sgit:file-name (buffer-file-name)))))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((cmd (format "%s %s" precmd file))
             (ret (call-process-shell-command cmd nil t)))
        (unless (zerop ret)
          (error (format "Failed '%s'" cmd)))))))

(defvar helm-c-sgit-grep-source
  '((name . "helm sgit")
    (init . helm-c-sgit-grep-init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

(defun sgit:grep ()
  (interactive)
  (helm :sources '(helm-c-sgit-grep-source)
        :buffer (get-buffer-create sgit:buffer)))

(defvar helm-git-file-buffer "*helm git*")

(defun helm-c-git-exec (cmd &optional mode-func)
  (when (get-buffer helm-git-file-buffer)
    (kill-buffer (get-buffer helm-git-file-buffer)))
  (with-current-buffer (get-buffer-create helm-git-file-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error (format "Failed %s" cmd)))
      (when mode-func
        (funcall mode-func))
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun helm-c-git-log (candidate)
  (let* ((option (or (and current-prefix-arg "-p --stat") ""))
         (cmd (format "git log %s %s" option candidate)))
    (helm-c-git-exec cmd)
    (pop-to-buffer helm-git-file-buffer)))

(defun helm-c-git-diff (candidate)
  (let ((cmd (format "git diff %s" candidate)))
    (helm-c-git-exec cmd #'diff-mode)
    (pop-to-buffer helm-git-file-buffer)))

(define-helm-type-attribute 'git-file
  `((action
     ("Find file" . helm-find-many-files)
     ("Git Log" . helm-c-git-log)
     ("Git Diff" . helm-c-git-diff)
     ("Open dired in file's directory" . helm-c-open-dired)))
  "Type for Files in Git Repos")

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
