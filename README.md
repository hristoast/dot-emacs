# /home/hristos/.emacs.d/

## About

My Emacs configuration.  I use it on Emacs 26.3, but it should run fine on Emacs 24 or higher (with some warnings.)  It sports the [Material Theme for Emacs](https://github.com/cpaulik/emacs-material-theme) and [Smart Mode Line](https://github.com/Malabarba/smart-mode-line) with the Dark theme.  I recommend running this configuration in daemon mode - it's great for GUI Emacs or terminal Emacs.

It sets up Emacs with support for a wide variety of languages, as well as other more opinionated things like: color themes, many aspects of Emacs internals, code completion, and much much more.

## Screenshots

Check out [the screenshots page](https://man.sr.ht/%7Ehristoast/dot-emacs/screenshots.md) on the wiki.

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
ln -sv ~/src/hristoast-dot-emacs/{init.el,lib} ~/.emacs.d/
```

If you're an Emacs daemon user, your first run should not be in daemon mode so that you can accept the theme (if you choose to use that).  After completing the first successful startup, where all packages and etc are installed, you may start the daemon.

## Configuring

Large portions of the overall configuration are toggle-able.  See [the config page](https://man.sr.ht/%7Ehristoast/dot-emacs/config.md) on the wiki for more information.

## Issues/Notes

* When installing, this configuration should not be ran in daemon mode for the first run.
* Some of my keybindings might not work well with terminal Emacs.  If that's an issue, don't load them (see [the config wiki page](https://man.sr.ht/%7Ehristoast/dot-emacs/config.md)).
* You can view the startup time in the `*Messages*` buffer, look for a line starting with `[STARTUP] Loading ...`.
