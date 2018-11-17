# /home/hristos/.emacs.d/

![This configuration in action](screenshot.png)

## About

My Emacs configuration.  I use it on Emacs 25.1+, but it should run fine on Emacs 24 (with some warnings.)  It sports the [Material Theme for Emacs](https://github.com/cpaulik/emacs-material-theme) and [Smart Mode Line](https://github.com/Malabarba/smart-mode-line) with the Respectful theme.  I recommend running this configuration in daemon mode - it's great for GUI Emacs or terminal Emacs.

The main file will set you up with a pretty solid editor for several languages (C/C++, Common Lisp, Lua, Python, Racket, Ruby, YAML, and more) and even some frameworks and other things (Magit, Ansible, Django (via `web-mode`), Nginx, `skewer-mode`,  and others).

## Installation requirements

For language-specific support, you need to have some related packages installed:

### C/C++

For the best completion support for C/C++ (via irony), the below packages are needed:

    clang llvm

Then, after running Emacs you will need to interactively run `irony-install-server` (e.g. `M-x irony-install-server RET`.)

At the moment, this config doesn't have equivalent support for GCC.

### Javascript

To ensure tern works as expected, the `tern` binary must be available on your system.

### Lua

For full lua support via Flycheck, ensure you've got [luackeck](https://github.com/mpeterv/luacheck) on your system.

### Python

The below packages are needed for full python3 support:

    python3 python3-flake8 python3-pip python3-jedi python3-virtualenv

Your distribution or operating system may name them differently.  Python 2 support requires equivalent packages for that version.

## Mode-specific bindings

### Flycheck

These apply to all modes with flycheck compatibility.

* `C-c e n`: `flycheck-next-error`
* `C-c e p`: `flycheck-previous-error`

### Magit

* `C-c g d`: `magit-diff`
* `C-x g`: `magit-status`

### Org

* `C-c f`: `org-store-link`
* `C-c a`: `org-agenda`

### Python

* `S-down-mouse-1`: Or, shift and left-mouse click; this places the mark runs `jedi:goto-definition` on it.
* `S-down-mouse-3`: Or, shift and right-mouse click; this places the mark runs `pydoc-at-point` on it.

### Skewer

* `C-c h p`: `httpd-start`
* `C-c h s`: `httpd-stop`

## Usage

Clone this repo to `~/.emacs.d/` and fire up Emacs, it should work fine on modern Linux and Mac OSX setups.

## Extra

To use the Clojure, Golang, or Rust bits, one of the following environment variables are required to be defined (the value can be anything): `EMACS_CLOJURE`, `EMACS_GO`, or `EMACS_RUST` (with any value, it just needs to be set) and that file will get loaded.

## Issues

* This configuration works best with GUI Emacs.  It tries to behave well when used in terminal Emacs (and should be fine on a GNU/Linux system), but there may be issues with some keybindings on macOS.
* More packages could be "split" out as I've done with the `extra` directory.
