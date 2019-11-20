;;; init.el --- Self-installing, for Emacs 25 and up.
;;; Commentary:
;;
;; My Emacs configuration for writing code and things.  Might work on Emacs 24.
;;
;; A breakdown of keybindings can be found in this repository at lib/bindings.el, or at:
;; https://git.sr.ht/~hristoast/dot-emacs/tree/master/lib/bindings.el
;;
;;; Code:

;; Convenience variables
(defconst emacs-start-time (current-time))
(defconst env-home (getenv "HOME"))
;; user-emacs-directory is provided too
;; late for my purposes so I set this now.
(defconst dot-emacs (concat env-home "/.emacs.d"))

;; Some initial package stuff
(require 'package)
(setq package-enable-at-startup nil)
(setq
 ;; Keep custom stuff out of here!
 custom-file (concat dot-emacs "/my-custom.el")
 package-archives
 ;; GNU over SSL
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
   ("melpa" . "https://melpa.org/packages/")
   ;; MELPA Stable
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ;; Org mode ELPA archive
   ("org" . "https://orgmode.org/elpa/")))

;; TODO: This is necessary to trust sml themes, but can
;; the later, redundant loading of custom-file be stopped?
(load custom-file :noerror :nomessage)

;; Refresh
(package-initialize)

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  ;; Install use-package
  ;; https://github.com/jwiegley/use-package/
  (package-install 'use-package))

;; Configure use-package
(defvar use-package-verbose t)
(defvar use-package-always-ensure t)
(eval-when-compile (require 'use-package))

;; https://github.com/jwiegley/use-package#key-binding
(require 'bind-key)

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(use-package diminish)

;; https://github.com/jwiegley/use-package#use-package-ensure-system-package
;; TODO: This fails on a first run.  Specifically when it si being used in the golang module.
;; (use-package use-package-ensure-system-package :demand)

;; Module definitions
(defvar modules-to-load)
(setq modules-to-load
      #s(hash-table
         size 4
         data
         ("EMACS_NO_EDITING_TWEAKS" "editing"
          "EMACS_NO_EXTRA_FUNCTIONS" "functions"
          "EMACS_NO_INTERNALS_TWEAKS" "internals"
          "EMACS_NO_KEYBINDINGS_TWEAKS" "keybindings"
          "EMACS_NO_UI_TWEAKS" "ui"

          "EMACS_NO_ANSIBLE" "ansible"
          "EMACS_NO_C_CPP" "c-cpp"
          "EMACS_NO_CLOJURE" "clojure"
          "EMACS_NO_CSS" "css"
          "EMACS_NO_DOCKER" "docker"
          "EMACS_NO_EPL" "emacs-package-library"
          "EMACS_NO_FISH" "fish"
          "EMACS_NO_GIT" "git"
          "EMACS_NO_GODOT" "godot"
          "EMACS_NO_GOLANG" "golang"
          "EMACS_NO_GROOVY" "groovy"
          "EMACS_NO_HTML" "html"
          "EMACS_NO_JAVA" "java"
          "EMACS_NO_JAVASCRIPT" "javascript"
          "EMACS_NO_JINJA2" "jinja2"
          "EMACS_NO_JSON" "json"
          "EMACS_NO_LUA" "lua"
          "EMACS_NO_MARKDOWN" "markdown"
          "EMACS_NO_NGINX" "nginx"
          "EMACS_NO_ORGMODE" "orgmode"
          "EMACS_NO_PYTHON" "python"
          "EMACS_NO_RACKET" "racket"
          "EMACS_NO_RUBY" "ruby"
          "EMACS_NO_RUST" "rust"
          "EMACS_NO_SHADERS" "shaders"
          "EMACS_NO_SLIME" "slime"
          "EMACS_NO_SYSTEMD" "systemd"
          "EMACS_NO_TERRAFORM" "terraform"
          "EMACS_NO_TOML" "toml"
          "EMACS_NO_YAML" "yaml")))

;; Load everything
(maphash
 (lambda (env-var filename)
   (unless (getenv env-var)
     ;; TODO: dry up the emacs.d dir location
     (let ((el-file (concat user-emacs-directory "lib/" filename ".el")))
       (if (file-exists-p el-file)
           (load el-file)
         (message (concat "Could not load the file: " el-file))))))
 modules-to-load)

;; How long did we take to load?
(let ((elapsed
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
