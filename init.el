;;; init.el --- Self-installing, for Emacs 25 and up.
;;; Commentary:
;;
;; My Emacs configuration for writing code and things.  Might work on Emacs 24.
;;
;; A breakdown of keybindings can be found in this repository at lib/bindings.el, or at:
;; https://git.sr.ht/~hristoast/dot-emacs/tree/master/lib/bindings.el
;;
;;; Code:

;; Start a timer.
(defconst emacs-start-time (current-time))

(setq
 ;; use-package handles enabling things.
 package-enable-at-startup nil
 ;; Keep custom stuff out of here!
 custom-file (or (getenv "EMACS_CUSTOM_FILE")
                 (concat user-emacs-directory "/my-custom.el"))
 package-archives
 ;; GNU over SSL
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
   ("melpa" . "https://melpa.org/packages/")
   ;; MELPA Stable
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ;; Org mode ELPA archive
   ("org" . "https://orgmode.org/elpa/")))

;; Init package.el
(require 'package)
(package-initialize)

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; https://github.com/jwiegley/use-package/blob/4b58ab78177f636f862a66c7a8fdcf9b070e0925/README.md#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile (require 'use-package))
(require 'bind-key)

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(use-package diminish :ensure t)

;; Module definitions
(defvar h/modules
  #s(hash-table
     size 36
     data
     ;; "Env var that disables loading if present" "file name in lib/ minus the extension"
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
      "EMACS_NO_VISUAL_BASIC" "visual-basic-mode"
      "EMACS_NO_YAML" "yaml")))

;; Load everything
(maphash
 (lambda (env-var filename)
   (unless (getenv env-var)
     (let ((el-file (concat user-emacs-directory "lib/" filename ".el")))
       (if (file-exists-p el-file)
           (load el-file)
         (message (concat "Could not load the file: " el-file))))))
 h/modules)

;; How long did we take to load?
(let ((elapsed
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
