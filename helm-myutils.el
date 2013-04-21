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

;; List files in git repos
(defun helm-myutils:git-project-source (pwd)
  (loop for (description . option) in
        '(("Modified files" . "--modified")
          ("Untracked files" . "--others --exclude-standard")
          ("All controlled files in this project" . ""))
        for cmd = (format "git ls-files %s" option)
        collect
        `((name . ,(format "%s [%s]" description pwd))
          (init . (lambda ()
                    (with-current-buffer (helm-candidate-buffer 'global)
                      (call-process-shell-command ,cmd nil t))))
          (candidates-in-buffer)
          (action . (("Open File" . find-file)
                     ("Open File other window" . find-file-other-window)
                     ("Insert buffer" . insert-file))))))

(defun helm-myutils:git-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-myutils:git-grep-source ()
  `((name . ,(format "Grep at %s" default-directory))
    (init . helm-myutils:git-grep-init)
    (candidates-in-buffer)
    (type . file-line)))

(defun helm-myutils:git-grep-init ()
  (helm-attrset 'recenter t)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((cmd (read-string "Grep: " "git grep -n ")))
      (call-process-shell-command cmd nil t))))

;;;###autoload
(defun helm-myutils:git-grep ()
  (interactive)
  (let ((default-directory (helm-myutils:git-topdir)))
    (helm :sources (helm-myutils:git-grep-source)
          :buffer (get-buffer-create helm-myutils:git-action-buffer))))

;;;###autoload
(defun helm-myutils:git-project ()
  (interactive)
  (let ((topdir (helm-myutils:git-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let ((default-directory topdir)
          (sources (helm-myutils:git-project-source
                    (file-name-nondirectory
                     (directory-file-name topdir)))))
      (helm-other-buffer sources "*helm git project*"))))

;; Dropbox with helm interface
(defvar helm-myutils:find-command
  (cond ((eq system-type 'darwin)
         (or (executable-find "gfind")
             (error "Please install `gfind' in findutils")))
        (t "find")))

(defvar helm-myutils:dropbox-source
  '((name . "Files in Dropbox")
    (init . (lambda ()
              (let ((cmd (format "%s ~/Dropbox/emacs -regex '%s' -type f"
                                 helm-myutils:find-command
                                 "^.+\.\\(org\\|txt\\)$")))
                (with-current-buffer (helm-candidate-buffer 'global)
                  (call-process-shell-command cmd nil t)))))
    (candidates-in-buffer)
    (volatile)
    (type . file)))

;;;###autoload
(defun helm-myutils:dropbox ()
  (interactive)
  (helm :sources '(helm-myutils:dropbox-source)
        :buffer "*helm dropbox*"))

(provide 'helm-myutils)

;;; helm-myutils.el ends here
