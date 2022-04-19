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
  :interpreter "lua"
  :hook
  ;; Keep the stock C-l behavior, please!
  (lua-mode . (lambda ()
                (local-unset-key (kbd "C-f"))
                (local-unset-key (kbd "C-l"))))
  :init
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

;; Use lua-mode for PICO-8 source files
(add-to-list 'auto-mode-alist '("\\.p8$" . lua-mode))

;;; lua.el ends here
