;;; h-lsp.el --- eglot and related
;;; Commentary:
;; Packages related to eglot
;;; Code:

(use-package eglot
  :straight nil
  :defer t
  :bind
  (("C-c e c a" . eglot-code-actions)
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error))
  :config
  (let ((lsp-lib (expand-file-name "games/openmw/Mods/Tools/LSP/0.51.0/luaLS"
                                   (getenv "HOME"))))
    (when (file-directory-p lsp-lib)
      (setq-default eglot-workspace-configuration
                    `((:Lua . (:runtime (:version "Lua 5.4")
                                        :workspace (:library [,lsp-lib])
                                        :diagnostics (:globals ["require"])))))))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-base-mode python-ts-mode) . ("ty" "server")))
    (add-to-list 'eglot-server-programs
                 '((lua-mode lua-ts-mode) . ("lua-language-server")))
    (add-to-list 'eglot-server-programs
                 '(nginx-mode . ("nginx-language-server")))
    (add-to-list 'eglot-server-programs
                 '(terraform-mode . ("terraform-ls" "serve")))
    (add-to-list 'eglot-server-programs
                 '((toml-mode toml-ts-mode) . ("taplo" "lsp" "stdio"))))
  :hook
  ;; system package: ansible-language-server
  ((ansible-mode . eglot-ensure)
   ;; system package: ccls
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   ;; https://github.com/rcjsuen/dockerfile-language-server-nodejs
   (dockerfile-mode . eglot-ensure)
   (dockerfile-ts-mode . eglot-ensure)
   ;; system package: gopls
   (go-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   ;; https://github.com/typescript-language-server/typescript-language-server
   (js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   ;; https://www.npmjs.com/package/vscode-json-languageserver
   (json-mode . eglot-ensure)
   (json-ts-mode . eglot-ensure)
   ;; system package: lua-language-server
   (lua-mode . eglot-ensure)
   (lua-ts-mode . eglot-ensure)
   ;; https://github.com/pappasam/nginx-language-server
   (nginx-mode . eglot-ensure)
   ;; system package: ty
   (python-base-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   ;; https://github.com/Shopify/ruby-lsp
   ;; https://github.com/castwide/solargraph
   (ruby-mode . eglot-ensure)
   (ruby-ts-mode . eglot-ensure)
   ;; system package: bash-language-server
   (sh-mode . eglot-ensure)
   (bash-ts-mode . eglot-ensure)
   ;; system package: terraform-ls
   (terraform-mode . eglot-ensure)
   ;; system package: taplo
   (toml-mode . eglot-ensure)
   (toml-ts-mode . eglot-ensure)
   ;; system package: yaml-language-server
   (yaml-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)))

;;; h-lsp.el ends here
