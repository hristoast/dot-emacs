;;; godot.el --- Load godot packages
;;; Commentary:
;; Packages related to godot.
;;; Code:

;; Major mode for editing Godot GDScript files
(use-package gdscript-mode :defer t)

;; Use conf-mode for .godot and .tscn source files
(setq auto-mode-alist (append '(("\\.godot$" . conf-mode))
                              '(("\\.tscn$" . conf-mode))
                              auto-mode-alist))

;;; godot.el ends here
