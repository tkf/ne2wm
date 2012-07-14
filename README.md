# NE2WM: Non-Equilibrium Emacs Window Manager

*-- until it reaches to the equilibrium state --*

NE2WM is a set of perspectives, plugins, utility functions and
commands for [Equilibrium Emacs Window Manager][e2wm] (E2WM).

You need to install [e2wm.el][e2wm] (and its dependency
[window-layout.el][wlf]) to try NE2WM.
<!-- If you use el-get, you can install NE2WM as a package `ne2wm` and
let el-get handle the dependencies. -->

[e2wm]: https://github.com/kiwanami/emacs-window-manager
[wlf]: https://github.com/kiwanami/emacs-window-layout

To load all NE2WM functionality, add the following line in your Emacs setting.

```lisp
(require 'ne2wm-setup)
```

You can load specific files if you like.


## Perspectives and plugins

<!--[[[cog
import cog
from glob import glob
files = (sorted(glob('ne2wm-pst-*.el')) +
         sorted(glob('ne2wm-plugin-*.el')) +
         ['ne2wm-utils.el', 'ne2wm-toggle.el'])
line0 = (file(f).readline().strip(';;;').strip() for f in files)

cog.outl('')
cog.outl('\n'.join(map('+ {0}'.format, line0)))
cog.outl('')
]]]-->

+ ne2wm-pst-code+.el --- code+ perspective
+ ne2wm-pst-magit+.el --- magit+ perspective
+ ne2wm-pst-monky+.el --- monky+ perspective
+ ne2wm-pst-one+.el --- one+ perspective
+ ne2wm-pst-three+.el --- three+ perspective
+ ne2wm-pst-two+.el --- two+ perspective
+ ne2wm-pst-vc-annotate.el --- a perspective to use vc-annotate command
+ ne2wm-plugin-history-list+.el --- history-list plugin for many windows
+ ne2wm-plugin-org-clock.el --- org-clock plugin
+ ne2wm-utils.el --- utility functions
+ ne2wm-toggle.el --- buffer-toggling utilities for e2wm

<!--[[[end]]]-->


## Screenshots

### Rotate windows -- `ne2wm:win-ring-rotate`

This is how `ne2wm:win-ring-rotate` command works in the three+ perspective.
Focus stays in the second column window (dark background):

![win-ring-rotate](http://i.imgur.com/lnwYa.gif)


### Move the current window forward -- `ne2wm:win-ring-push`

This is how `ne2wm:win-ring-push` command works in the three+ perspective.
This time, the "1" window is always selected:

![win-ring-push](http://i.imgur.com/Mt1WI.gif)


### org-clock plugin

When you clock-in (`C-c C-x C-i`) an org node, this window will pop-up:

![org-clock](http://i.imgur.com/IZMIj.png)
