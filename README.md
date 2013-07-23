# vc-git-check-status

This is an Emacs extension that warns you when you are about to quit
Emacs and leaving a git repository that has some file opened in Emacs
in a dirty state.

## Installation and configuration

You just have to put the file `vc-git-check-status.el` in you load path
and require the feature from your `.emacs`.

```lisp
(require 'vc-git-check-status)
```

By default, changes and unpushed commits are checked in all git
repositories. You can change this behavior by tweaking the variables
`vc-git-check-alist' and `vc-git-check'.
