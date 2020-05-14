;;; godot.el --- Load godot packages
;;; Commentary:
;; Packages related to godot.
;;; Code:

;; Major mode for editing Godot GDScript files
(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "GDQuest/emacs-gdscript-mode")
  ;; Use toml-mode for .godot and .tscn source files
  :init
  (add-hook 'gdscript-mode-hook 'lsp-deferred)
  (setq auto-mode-alist (append '(("\\.godot$" . toml-mode))
                                '(("\\.tscn$" . toml-mode))
                                '(("\\.gd$" . gdscript-mode))
                                auto-mode-alist)))
;;; godot.el ends here
