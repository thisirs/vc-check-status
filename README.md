# vc-check-status

This is an Emacs extension that warns you when you are about to quit
Emacs and leaving a git repository that has some file opened in Emacs
in a dirty state.

Currently only git repositories are supported with the file
`vc-git-check-status.el`. Feel free to write corresponding file for
other VCSes.

## Installation and configuration

You just have to put the files `vc-check-status.el` and
`vc-git-check-status.el` in you load path and require the feature from
your `.emacs` and activate it.

```lisp
(require 'vc-check-status)
(vc-check-status-activate 1)
```

By default, changes and unpushed commits are checked in all git
repositories. You can change this behavior by tweaking the variables
`vc-check-alist' and `vc-check'.
