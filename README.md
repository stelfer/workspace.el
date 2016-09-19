# workspace.el

This is part of my attempt to replicate some experiences I had using Mylyn (http://www.eclipse.org/mylyn/), under emacs.  It allows you to create persistent "workspaces" which are tied intricately to git branches. It uses `desktop-mode`, and coordinates with `vc-git` to switch workspaces when you switch branches, and vice-versa.  To get started, just put `workspace.el` in your path and add
```
(require 'workspace)
```
to your emacs config.
