# My Emacs Utilities


## pomodoro.el

(Pomodoro Technique)[http://www.pomodorotechnique.com/] in Emacs.
This is my own implementation.

### Customize

#### `pomodoro:file`

Open file when a pomodoro is finished.
No file is opened if `pomodoro:file` is `non-nil`.

#### `pomodoro:work-time`(Default: 25)

Minutes of work periodo.

#### `pomodoro:reset-time`(Default: 5)

Minutes of short rest period.

#### `pomodoro:long-rest-time`(Default: 30)

Minutes of long rest period.

#### `pomodoro:iteration-for-long-rest`

Iteration count when switching to long rest

#### Faces and Signed

Default value of all signs are "●".

|              | sign                              | face                    |
|:-------------|:----------------------------------|:------------------------|
|  Work        | pomodoro:mode-line-work-sign      | pomodoro:work-face      |
|  Short Rest  | pomodoro:mode-line-rest-sign      | pomodoro:rest-face      |
|  Long Rest   | pomodoro:mode-line-long-rest-sign | pomodoro:long-rest-face |


#### hooks

`pomodoro.el` has some hooks, they are called after work, short rest,
long rest.

|              | hook                      | Description                    |
|:-------------|:--------------------------|:-------------------------------|
|  Work        | pomodoro:finish-work-hook | Called after work period       |
|  Short Rest  | pomodoro:finish-rest-hook | Called after short rest period |
|  Long Rest   | pomodoro:long-rest-hook   | Called after long rest period  |


### Related Projects

* (https://github.com/konr/tomatinho)[https://github.com/konr/tomatinho]
* (http://ivan.kanis.fr/pomodoro.el)[http://ivan.kanis.fr/pomodoro.el]


## sgit.el

My git utilities


## helm-myutils.el

My own helm sources and them functions


## emacsclient utilities for Linux Desktop
* emacs_serverstart.pl
* emacsclient.sh

### Note

[Xfce4 Desktop](http://www.xfce.org/) does not need these script.


## hotentry.el

Simple hotentry viewer


## editutil.el

My configuration is:

```lisp
(require 'editutil)
(editutil-default-setup)

;; Default Bindings
(global-set-key [(control shift up)] 'editutil-move-line-up)
(global-set-key [(control shift down)] 'editutil-move-line-down)

(global-set-key (kbd "C-M-s") 'editutil-forward-char)
(global-set-key (kbd "C-M-r") 'editutil-backward-char)

(global-set-key (kbd "M-o") 'editutil-edit-next-line)
(global-set-key (kbd "M-O") 'editutil-edit-previous-line)

(global-set-key (kbd "M-s") 'editutil-unwrap-at-point)
(global-set-key (kbd "M-r") 'editutil-replace-wrapped-string)
(global-set-key (kbd "M-z") 'editutil-zap-to-char)

(global-set-key (kbd "M-n") 'editutil-next-symbol)
(global-set-key (kbd "M-p") 'editutil-previous-symbol)

(global-set-key (kbd "M-k") 'editutil-delete-following-spaces)

(global-set-key (kbd "C-y") 'editutil-yank)

(global-set-key (kbd "M-d") 'editutil-delete-word)
(global-set-key [remap backward-kill-word] 'editutil-backward-delete-word)

(global-set-key (kbd "C-x r N") 'editutil-number-rectangle)

(global-set-key (kbd "C-M-SPC") 'editutil-copy-sexp)
(global-set-key (kbd "M-I") 'editutil-indent-same-as-previous-line)

(define-key isearch-mode-map [remap isearch-exit] 'editutil-isearch-exit)

(smartrep-define-key
    global-map "C-x" '(("j" . 'editutil-insert-newline-without-moving)))

(smartrep-define-key
    global-map "M-g" '(("c" . 'editutil-duplicate-thing)))
```

### editutil for view-mode

```lisp
(eval-after-load "view"
  '(progn
     (define-key view-mode-map (kbd "i") 'editutil-view-insert)
     (define-key view-mode-map (kbd "a") 'editutil-view-insert-at-next)
     (define-key view-mode-map (kbd "I") 'editutil-view-insert-at-bol)
     (define-key view-mode-map (kbd "A") 'editutil-view-insert-at-eol)))
```
