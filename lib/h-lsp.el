;;; h-lsp.el --- eglot and related
;;; Commentary:
;; Packages related to eglot
;;; Code:

;;TODO: also configure Flymake here
(use-package eglot
  :straight nil
  :defer t
  ;; :ensure-system-package
  ;; ((ccls)
  ;;  (gopls)
  ;;  (lua-language-server)
  ;;  (python3-lsp-server . pylsp)
  ;;  (bash-language-server)
  ;;  (terraform-ls))
  :bind
  (("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error))
  :config
  (add-to-list 'eglot-server-programs
               '(nginx-mode . ("nginx-language-server")))
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
               '(toml-mode . ("taplo" "lsp" "stdio")))
  :hook
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-ansible/
  ((ansible-mode . eglot-ensure)
   ;; system package: ccls
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   ;; https://github.com/rcjsuen/dockerfile-language-server-nodejs
   (dockerfile-mode . eglot-ensure)
   ;; system package: gopls
   (go-mode . eglot-ensure)
   ;; https://github.com/typescript-language-server/typescript-language-server
   (js-mode . eglot-ensure)
   ;; https://www.npmjs.com/package/vscode-json-languageserver
   (json-mode . eglot-ensure)
   ;; system package: lua-language-server
   (lua-mode . eglot-ensure)
   ;; https://github.com/pappasam/nginx-language-server
   (nginx-mode . eglot-ensure)
   ;; system package: python3-lsp-server
   (python-mode . eglot-ensure)
   ;; https://github.com/Shopify/ruby-lsp
   ;; https://github.com/castwide/solargraph
   (ruby-mode . eglot-ensure)
   ;; system package: bash-language-server
   (sh-mode . eglot-ensure)
   ;; system package: terraform-ls
   (terraform-mode . eglot-ensure)
   ;; https://github.com/tamasfe/taplo/releases/tag/0.8.1
   (toml-mode . eglot-ensure)
   ;; https://github.com/redhat-developer/yaml-language-server
   (yaml-mode . eglot-ensure)))

;;; h-lsp.el ends here
