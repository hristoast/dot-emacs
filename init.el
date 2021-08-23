;;; init.el --- Self-installing.
;;; Commentary:
;;
;; My Emacs configuration for writing code and things.
;;
;; A breakdown of keybindings can be found in this repository at lib/bindings.el, or at:
;; https://git.sr.ht/~hristoast/dot-emacs/tree/master/lib/h-bindings.el
;;
;;; Code:

;; Start a timer.
(defconst emacs-start-time (current-time))

(setq
 ;; Keep custom stuff out of here!
 custom-file (or (getenv "EMACS_CUSTOM_FILE")
                 (concat user-emacs-directory "/my-custom.el")))

;; https://github.com/raxod502/straight.el/blob/2d407bccd9378f1d5218f8ba2ae85c6be73fbaf1/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/2d407bccd9378f1d5218f8ba2ae85c6be73fbaf1/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/raxod502/straight.el/blob/2d407bccd9378f1d5218f8ba2ae85c6be73fbaf1/README.md#integration-with-use-package
(straight-use-package 'use-package)

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(use-package diminish :straight t)

;; Module definitions
(defvar hristoast-modules
  #s(hash-table
     size 42
     data
     ;; "Env var that disables loading if present" "file name in lib/ minus the extension"
     ;; Be sure to check `list-load-path-shadows' every so often!
     ("EMACS_NO_EDITING_TWEAKS" "editing"
      "EMACS_NO_EXTRA_FUNCTIONS" "functions"
      "EMACS_NO_INTERNALS_TWEAKS" "internals"
      "EMACS_NO_KEYBINDINGS_TWEAKS" "keybindings"
      "EMACS_NO_UI_TWEAKS" "ui"

      "EMACS_NO_COMPANY" "h-company"
      "EMACS_NO_FLYCHECK" "h-flycheck"
      "EMACS_NO_LSP" "h-lsp"
      "EMACS_NO_ANSIBLE" "ansible"
      "EMACS_NO_C_CPP" "c-cpp"
      "EMACS_NO_CALC" "h-calc"
      "EMACS_NO_CLOJURE" "clojure"
      "EMACS_NO_CSS" "css"
      "EMACS_NO_DOCKER" "docker"
      "EMACS_NO_EXTRAS" "h-extras"
      "EMACS_NO_FENNEL" "fennel"
      "EMACS_NO_FISH" "fish"
      "EMACS_NO_GIT" "git"
      "EMACS_NO_GODOT" "godot"
      "EMACS_NO_GOLANG" "golang"
      "EMACS_NO_GROOVY" "groovy"
      "EMACS_NO_HTML" "html"
      "EMACS_NO_JAVA" "java"
      "EMACS_NO_JAVASCRIPT" "javascript"
      "EMACS_NO_JINJA2" "jinja2"
      "EMACS_NO_JSON" "h-json"
      "EMACS_NO_LUA" "lua"
      "EMACS_NO_MARKDOWN" "markdown"
      "EMACS_NO_NGINX" "nginx"
      "EMACS_NO_OCAML" "h-ocaml"
      "EMACS_NO_ORGMODE" "orgmode"
      "EMACS_NO_PYTHON" "h-python"
      "EMACS_NO_RACKET" "racket"
      "EMACS_NO_RUBY" "ruby"
      "EMACS_NO_SHADERS" "shaders"
      "EMACS_NO_SLIME" "h-slime"
      "EMACS_NO_SYSTEMD" "h-systemd"
      "EMACS_NO_TERRAFORM" "terraform"
      "EMACS_NO_TOML" "toml"
      "EMACS_NO_VISUAL_BASIC" "vbscript"
      "EMACS_NO_YAML" "yaml"
      "EMACS_NO_CUSTOMIZE" "customize")))

;; Maybe load everything. Not loading everything is useful for
;; when you want plain Emacs with straight.el and use-package.
(when (not (getenv "EMACS_VANILLA_SETUP"))
  (maphash
   (lambda (env-var filename)
     (unless (getenv env-var)
       (let ((el-file (concat user-emacs-directory "lib/" filename ".el")))
         (if (file-exists-p el-file)
             (load el-file)
           (message (concat "Could not load the file: " el-file))))))
   hristoast-modules))

;; How long did we take to load?
(let ((elapsed
       (float-time (time-subtract (current-time) emacs-start-time))))
  (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
