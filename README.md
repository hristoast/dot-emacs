# /home/hristos/.emacs.d/

## Screenshots

Check out [the screenshots page](https://man.sr.ht/%7Ehristoast/dot-emacs/screenshots.md) on the wiki.

## About

My Emacs configuration.  I use it on Emacs 25.1+, but it should run fine on Emacs 24 (with some warnings.)  It sports the [Material Theme for Emacs](https://github.com/cpaulik/emacs-material-theme) and [Smart Mode Line](https://github.com/Malabarba/smart-mode-line) with the Respectful theme.  I recommend running this configuration in daemon mode - it's great for GUI Emacs or terminal Emacs.

The main file will set you up with a pretty solid editor for several languages (C/C++, Common Lisp, Lua, Python, Racket, Ruby, YAML, and more) and even some frameworks and other things (Magit, Ansible, Django (via `web-mode`), Nginx, `skewer-mode`,  and others).

## Installation

Copy all `.el` files from the root of this repository into your `~/.emacs.d`, start Emacs.  It may be wise to `mv` your current `~/.emacs.d` out of the way and start fresh.

Two install methods are demonstrated below:

```
# Put it right in place:
mv -v ~/.emacs.d ~/$(date +%F)-emacs.d
git clone https://git.sr.ht/~hristoast/dot-emacs ~/.emacs.d

# Make symlinks
mv -v ~/.emacs.d ~/$(date +%F)-emacs.d
mkdir ~/.emacs.d
git clone https://git.sr.ht/~hristoast/dot-emacs ~/src/hristoast-dot-emacs
ln -sv ~/src/toasty-dotfiles/soma/.emacs.d/{init.el,lib} ~/.emacs.d/
```

If you're an Emacs daemon user, your first run should not be in daemon mode so that you can accept the theme (if you choose to use that).  After completing the first successful startup, where all packages and etc are installed, you may start the daemon.

## Configuring

See [the config page](https://man.sr.ht/%7Ehristoast/dot-emacs/config.md) on the wiki.

## Issues

* As noted above, this configuration needs to be ran with non-daemon Emacs first.
* Additionally, the very first run will fail with an error regarding `ensure-system-package`.  Re-running Emacs will continue the setup as intended.
* This configuration works best with GUI Emacs.  It tries to behave well when used in terminal Emacs (and should be fine on a GNU/Linux system), but there may be issues with some keybindings on macOS.
