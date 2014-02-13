;;; eshell-util.el --- My eshell utilities

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
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

(require 'cl-lib)

(require 'eshell)
(require 'popwin)

(declare-function eshell/cd "em-dirs")

(defvar eshell-util--pop-buffer "*eshell-pop*")
(defvar eshell-util--prev-buffer nil)

(defun eshell-util--chdir (dir)
  (eshell-kill-input)
  (insert dir)
  (eshell-send-input))

;;;###autoload
(defun eshell-util-pop ()
  (interactive)
  (let ((curdir default-directory))
    (setq eshell-prev-buffer (current-buffer))
    (unless (get-buffer eshell-util--pop-buffer)
      (save-window-excursion
        (pop-to-buffer (get-buffer-create eshell-pop-buffer))
        (eshell-mode)))
    (popwin:popup-buffer (get-buffer eshell-pop-buffer) :height 20 :stick t)
    (unless (string= default-directory curdir)
      (eshell-util--chdir curdir))))

;;
;; eshell command line utilities
;;
(defun eshell/cde ()
  (let* ((file-name (buffer-file-name eshell-prev-buffer))
         (dir (or (and file-name (file-name-directory file-name))
                  (and (eq major-mode 'dired-mode) dired-directory)
                  (with-current-buffer eshell-prev-buffer
                    default-directory))))
    (eshell/cd dir)))

(defun eshell/cdp ()
  (let ((dir (cl-loop with cwd = default-directory
                      for d in '(".git" ".hg" ".svn")
                      when (locate-dominating-file d cwd)
                      return (file-name-directory it))))
    (eshell/cd dir)))

(defun eshell/e (file)
  (let ((curwin (get-buffer-window))
        (filepath (concat default-directory file)))
    (other-window 1)
    (find-file filepath)
    (delete-window curwin)))

(provide 'eshell-util)

;;; eshell-util.el ends here
