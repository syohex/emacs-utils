# My Emacs Utilities


## pomodoro.el

[Pomodoro Technique](http://www.pomodorotechnique.com/) in Emacs.
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

* [https://github.com/konr/tomatinho](https://github.com/konr/tomatinho)
* [http://ivan.kanis.fr/pomodoro.el](http://ivan.kanis.fr/pomodoro.el)


## emacsclient utilities for Linux Desktop
* emacs_serverstart.pl
* emacsclient.sh

### Note

[Xfce4 Desktop](http://www.xfce.org/) does not need these script.


## hotentry.el

Simple hotentry viewer


## editutil.el

[`editutil.el` is moved](https://github.com/syohex/emacs-editutil)

## sgit.el

[`sgit.el` is moved](https://github.com/syohex/emacs-sgit)
