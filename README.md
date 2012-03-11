# NE2WM: Non-Equilibrium Emacs Window Manager

*-- until it reaches to the equilibrium state --*

NE2WM is a set of perspectives, plugins, utility functions and commands for [Equilibrium Emacs Window Manager](https://github.com/kiwanami/emacs-window-manager) (E2WM).


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
         sorted(glob('ne2wm-plugin-*.el')))
line0 = (file(f).readline().strip(';;;').strip() for f in files)

cog.outl('')
cog.outl('\n'.join(map('+ {0}'.format, line0)))
cog.outl('')
]]]-->

+ ne2wm-pst-code+.el --- code+ perspective
+ ne2wm-pst-one+.el --- one+ perspective
+ ne2wm-pst-three+.el --- three+ perspective
+ ne2wm-pst-two+.el --- two+ perspective
+ ne2wm-pst-vc-annotate.el --- a perspective to use vc-annotate command
+ ne2wm-plugin-history-list+.el --- history-list plugin for many windows
+ ne2wm-plugin-org-clock.el --- org-clock plugin

<!--[[[end]]]-->
