;;; h-lsp.el --- eglot and related
;;; Commentary:
;; Packages related to eglot
;;; Code:

;;TODO: also configure Flymake here
(use-package eglot
  :straight nil
  :defer t
  :hook
  ((ansible-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   ;; https://github.com/rcjsuen/dockerfile-language-server-nodejs
   (dockerfile-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   ;; https://github.com/typescript-language-server/typescript-language-server
   (js-mode . eglot-ensure)
   (json-mode . eglot-ensure)
   (lua-mode . eglot-ensure)
   (nginx-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (sh-mode . eglot-ensure)
   ;;TODO: https://joaotavora.github.io/eglot/#Setting-Up-LSP-Servers
   ;; https://github.com/tamasfe/taplo/releases/tag/0.8.1
   (toml-mode . eglot-ensure)
   ;; https://github.com/redhat-developer/yaml-language-server
   (yaml-mode . eglot-ensure)))

;;; h-lsp.el ends here
