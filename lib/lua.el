;;; lua.el --- Load lua packages
;;; Commentary:
;; Packages related to lua.
;;; Code:

;; company-mode completion backend for Lua
;; https://github.com/ptrv/company-lua
(use-package company-lua :defer t :straight t)

;; Emacs major mode for editing Lua
;; http://immerrr.github.io/lua-mode/
;; TODO: require a `luacheck' install
(use-package lua-mode
  :straight t
  :init
  (add-hook 'lua-mode-hook 'lsp-deferred)
  (setq lua-indent-string-contents t))

;; Use lua-mode for PICO-8 source files
;; TODO: document that this is here, or make a separate pico8.el file.
(setq auto-mode-alist (append '(("\\.p8$" . lua-mode))
                              auto-mode-alist))

;; https://github.com/EmmyLua/EmmyLua-LanguageServer/blob/58ac70f40d84cf7ce0e31fd3aba2f021d9d18c72/readme.md#adding-to-emacs
;; https://github.com/phenix3443/lsp-lua-emmy
(use-package lsp-lua-emmy
  :demand
  :straight (lsp-lua-emmy :type git :host github :repo "phenix3443/lsp-lua-emmy")
  :init
  (setq lsp-lua-emmy-jar-path
        (expand-file-name "EmmyLua-LS-all-ac977d4.jar" (concat (getenv "HOME") "/src/EmmyLua-LanguageServer"))))

;;; lua.el ends here
