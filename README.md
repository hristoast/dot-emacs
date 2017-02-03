# /home/hristos/.emacs.d/
## About

My Emacs configuration.  I use it on Emacs 25.1+, but it should run fine on Emacs 24 (with some warnings.)

## Usage

Clone this repo to `~/.emacs.d/` and fire up Emacs, it should work fine on modern Linux and Mac OSX setups.  The main file will set you up with a pretty solid editor for several languages (C/C++, Lua, Python, Racket, Ruby, YAML, and more) and even some frameworks and other things (Magit, Ansible, Django (via `web-mode`), Nginx, `skewer-mode`,  and others).

## Extra

To use the Clojure, Golang, Javascript, or Rust bits, export an environment variable like `EMACS_RUST=true` (with any value, it just needs to be set) and that file will get loaded.  I don't use these too often so they may or may not be as up to date as they ought to be.

## Issues

* This configuration works best with GUI Emacs.  It tries to behave well when used in terminal Emacs (and should be fine on a GNU/Linux system), but there may be issues with some keybindings on macOS.
* More packages could be "split" out as I've done with the `extra` directory.
