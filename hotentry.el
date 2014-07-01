;;; hotentry.el --- Simple hotentry viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-utils
;; Package-Requires: ((cl-lib "0.5"))

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
(require 'xml)
(require 'button)

(defgroup hotentry nil
  "hotentry viewer"
  :group 'url
  :prefix 'hotentry:)

(defface hotentry:entry-face
  '((t (:inherit button)))
  "Face entry"
  :group 'hotentry)

(defvar hotentry:favorites '()
  "Your favorite keys. These can be completioned at inputing key")
(defvar hotentry:default-threshold 3
  "Default threshold.")
(defvar hotentry:buffer "*hotentry*")

(defun hotentry:rss-url (key threshold)
  (format "http://b.hatena.ne.jp/search/tag?q=%s&users=%d&mode=rss" key threshold))

(defun hotentry:download-command (url)
  (cond ((executable-find "curl") (cl-values "curl" (list "-s" url)))
        ((executable-find "wget") (cl-values "wget" (list "-O" "-" url)))
        (t (error "Please install curl or wget"))))

(defun hotentry:parse-rss (url)
  (with-temp-buffer
    (cl-multiple-value-bind (cmd args) (hotentry:download-command url)
      (unless (zerop (apply 'call-process cmd nil t '(t nil) args))
        (error "Download failed: %s" url))
      (hotentry:collect-items (libxml-parse-xml-region (point-min) (point-max))))))

(defun hotentry:collect-items (xml-tree)
  (cl-loop for (tag . item) in (cdr xml-tree)
           when (eq tag 'item)
           collect
           (cl-loop for tag in '(title link description bookmarkcount)
                    append (list tag (cadr (assoc-default tag item))))))

(defun hotentry:short-description (desc limit)
  (cond ((<= (length desc) limit) desc)
        (t (concat (substring desc 0 (1- limit)) "..."))))

(defun hotentry:view-items (title items)
  (let ((buf (get-buffer-create hotentry:buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (concat title "\n"))
      (cl-loop for item in items
               for index = 1 then (+ index 1)
               for label = (plist-get item 'title)
               for link  = (plist-get item 'link)
               for desc  = (purecopy (hotentry:short-description
                                      (plist-get item 'description) 40))
               for count = (plist-get item 'bookmarkcount)
               do
               (progn
                 (insert (format "%2d: [%4s] " index count))
                 (insert-button label
                                'face 'hotentry:entry-face
                                'link link 'help-echo desc
                                'action (lambda (b)
                                          (let ((props (overlay-properties b)))
                                            (browse-url (plist-get props 'link)))))
                 (insert "\n")))
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun hotentry (key threshold)
  (interactive
   (list
    (completing-read "Key: " hotentry:favorites)
    (or (and current-prefix-arg
             (read-number "Bookmarks: " hotentry:default-threshold))
        hotentry:default-threshold)))
  (let* ((url (hotentry:rss-url key threshold))
         (items (hotentry:parse-rss url))
         (title (format "[Query: %s, Threshold: %d]" key threshold)))
    (hotentry:view-items title items)))

(provide 'hotentry)

;;; hotentry.el ends here
