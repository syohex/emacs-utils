;;; editutil.el --- My own Edit Utilities

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

(eval-when-compile
  (require 'cl))

(require 'thingatpt)

(defvar editutil--unwrap-pair
  '(("(" . ")") ("[" . "]") ("{" . "}") ("'" . "'") ("\"" . "\"")
    ("<" . ">") ("|" . "|") ("`" . "`")))

(defun editutil--unwrap-counterpart (sign)
  (let ((pair (assoc-default sign editutil--unwrap-pair)))
    (unless pair
      (error "Not found: pair string of '%s'" sign))
    pair))

;;;###autoload
(defun editutil-unwrap-at-point (arg &optional replaced)
  (interactive "p")
  (save-excursion
    (let ((curpoint (point))
          (count 0))
      (when (re-search-backward "\\([(\[{'\"`|<]\\)" (point-min) t arg)
        (let* ((start (point))
               (matched (match-string-no-properties 1))
               (pair (editutil--unwrap-counterpart matched))
               (replace-pair (and replaced (editutil--unwrap-counterpart replaced))))
          (if (string-match-p "[(\[{]" matched )
              (forward-list 1)
            (save-excursion
              (forward-char 1)
              (while (re-search-forward (regexp-quote matched) curpoint t)
                (incf count)))
            (goto-char curpoint)
            (when (re-search-forward pair nil t (1+ count))
              (when (< (point) curpoint)
                (error "This point is not wrapped!!"))))
          (backward-char)
          (delete-char 1)
          (when replaced
            (insert replace-pair))
          (goto-char start)
          (delete-char 1)
          (when replaced
            (insert replaced)))))))

;;;###autoload
(defun editutil-replace-wrapped-string (arg)
  (interactive "p")
  (let ((replaced (char-to-string (read-char))))
    (editutil-unwrap-at-point arg replaced)))

;;;###autoload
(defun editutil-edit-previous-line (arg)
  (interactive "p")
  (if (< arg 0)
      (editutil-edit-next-line (- arg))
    (dotimes (i arg)
      (if (= (line-number-at-pos) 1)
          (goto-char (line-beginning-position))
        (forward-line -1)
        (end-of-line))
      (newline-and-indent))))

;;;###autoload
(defun editutil-edit-next-line (arg)
  (interactive "p")
  (if (>= arg 0)
      (dotimes (i arg)
        (end-of-line)
        (newline-and-indent))
    (editutil-edit-previous-line (- arg))))

;;;###autoload
(defun editutil-edit-next-line-no-indent (arg)
  (interactive "p")
  (dotimes (i arg)
    (end-of-line)
    (newline)))

;;;###autoload
(defun editutil-edit-next-line-same-column (arg)
  (interactive "p")
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (dotimes (i arg)
      (end-of-line)
      (newline)
      (move-to-column col t))))

;;;###autoload
(defun editutil-zap-to-char (arg char)
  (interactive "p\ncZap to char: ")
  (with-no-warnings
    (when (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char))))
  (delete-region (point)
                 (progn
                   (when (>= arg 0)
                     (forward-char 1))
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (backward-char 1)
                     (forward-char 1))
                   (point))))

;;;###autoload
(defun editutil-next-symbol (arg)
  (interactive "p")
  (let ((symbol (thing-at-point 'symbol))
        (curpoint (point)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (if (>= arg 0)
          (goto-char (cdr bound))
        (goto-char (car bound))))
    (let ((regexp (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (if (re-search-forward regexp nil t arg)
          (goto-char (match-beginning 0))
        (goto-char curpoint)
        (error "No more found('%s')" symbol)))))

;;;###autoload
(defun editutil-previous-symbol (arg)
  (interactive "p")
  (editutil-next-symbol (- arg)))

(defvar editutil--last-search-char nil)

(defsubst editutil--last-command-move-char-p ()
  (memq last-command '(editutil-forward-char editutil-backward-char)))

;;;###autoload
(defun editutil-forward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-char "Forward Char: "))))
  (unless (char-or-string-p char)
    (error "Error: Input Invalid Char %d" char))
  (setq editutil--last-search-char char)
  (when (>= arg 0)
    (forward-char 1))
  (let ((case-fold-search nil))
    (search-forward (char-to-string char) nil t arg))
  (when (>= arg 0)
    (backward-char 1)))

;;;###autoload
(defun editutil-backward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-char "Backward Char: "))))
  (backward-char 1)
  (editutil-forward-char (- arg) char))

;;;###autoload
(defun editutil-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun editutil-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'editutil)

;;; editutil.el ends here
