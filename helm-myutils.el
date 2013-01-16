;;; helm-myutils.el --- my own helm utilities

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01

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
(require 'sgit)

(defvar helm-myutils:git-action-buffer "*helm git*")

(defun helm-myutils:git-exec (cmd &optional mode-func)
  (helm-aif (get-buffer helm-myutils:git-action-buffer)
    (kill-buffer it))
  (with-current-buffer (get-buffer-create helm-myutils:git-action-buffer)
    (let ((ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error (format "Failed %s" cmd)))
      (when mode-func
        (funcall mode-func))
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun helm-myutils:action-git-common (cmd &optional mode-func)
  (helm-myutils:git-exec cmd mode-func)
  (pop-to-buffer (get-buffer helm-myutils:git-action-buffer)))

(defun helm-myutils:action-git-log (candidate)
  (let* ((option (or (and current-prefix-arg "-p --stat") ""))
         (cmd (format "git log %s %s" option candidate)))
    (helm-myutils:action-git-common cmd #'sgit:git-log-mode)))

(defun helm-myutils:action-git-diff (candidate)
  (let ((cmd (format "git diff %s" candidate)))
    (helm-myutils:action-git-common cmd #'diff-mode)))

;; List files in git repos
(defun helm-c-sources-git-project (pwd)
  (loop for elt in
        '(("Modified files" . "--modified")
          ("Untracked files" . "--others --exclude-standard")
          ("All controlled files in this project" . nil))
        for title  = (format "%s (%s)" (car elt) pwd)
        for option = (cdr elt)
        for cmd    = (format "git ls-files %s" (or option ""))
        collect
        `((name . ,title)
          (init . (lambda ()
                    (unless (and (not ,option) (helm-candidate-buffer))
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (call-process-shell-command ,cmd nil t)))))
          (candidates-in-buffer)
          (type . git-file))))

(defun helm-myutils:git-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-myutils:git-shorten-dir (dir)
  (let ((regexp (format "^%s" (or (getenv "HOME") ""))))
    (replace-regexp-in-string regexp "~" dir)))

(defun helm-myutils:git-project ()
  (interactive)
  (let ((topdir (helm-myutils:git-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let ((default-directory topdir)
          (sources (helm-c-sources-git-project
                    (helm-myutils:git-shorten-dir topdir))))
      (helm-other-buffer sources "*helm git project*"))))

(define-helm-type-attribute 'git-file
  `((action
     ("Find file" . helm-find-many-files)
     ("Git Log" . helm-myutils:action-git-log)
     ("Git Diff" . helm-myutils:action-git-diff)))
  "Type for Files in Git Repos")

(global-set-key  (kbd "C-;") 'helm-myutils:git-project)

;; Dropbox with helm interface
(defvar helm-myutils:dropbox-source
  '((name . "Files in Dropbox")
    (init . (lambda ()
              (let ((cmd (format "find ~/Dropbox/emacs -regex '%s' -type f"
                                 ".*\\(org\\|txt\\)$*")))
                (with-current-buffer (helm-candidate-buffer 'global)
                  (call-process-shell-command cmd nil t)))))
    (candidates-in-buffer)
    (volatile)
    (type . file)))

(defun helm-myutils:dropbox ()
  (interactive)
  (helm :sources '(helm-myutils:dropbox-source)
        :buffer "*helm dropbox*"))

(global-set-key (kbd "<f10>") 'helm-myutils:dropbox)

;; Open files in current directory
(defvar helm-myutils:files-in-curdir-source
  '((name . "Files in Current Directory")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (loop for dir in (directory-files (helm-c-current-directory))
                            when (not (string-match "^\.\.?$" dir))
                            collect dir))))
    (action . (("Open File" . find-file)
               ("Open File Other buffer" . find-file-other-window)
               ("Open File Other buffer to Right" .
                (lambda (c)
                  (let ((split-width-threshold 0))
                    (find-file-other-window c))))))))

(defun helm-myutils:files-in-curdir ()
  (interactive)
  (let ((curbuf (current-buffer))
        (orig-major major-mode))
    (when (helm-other-buffer 'helm-myutils:files-in-curdir-source
                             "*helm-dired*")
      (and (eq orig-major 'dired-mode)
           (eq curbuf (current-buffer))
           (kill-buffer curbuf)))))

(global-set-key (kbd "C-x C-p") 'helm-myutils:files-in-curdir)

(provide 'helm-myutils)

;;; helm-myutils.el ends here
