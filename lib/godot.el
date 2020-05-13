;;; godot.el --- Load godot packages
;;; Commentary:
;; Packages related to godot.
;;; Code:

;; Major mode for editing Godot GDScript files
(use-package gdscript-mode
  :defer t
  :straight t
  ;; Use toml-mode for .godot and .tscn source files
  :init (setq auto-mode-alist (append '(("\\.godot$" . toml-mode))
                                      '(("\\.tscn$" . toml-mode))
                                      auto-mode-alist)))




;;; godot.el ends here
