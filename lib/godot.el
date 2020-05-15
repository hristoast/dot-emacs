;;; godot.el --- Load godot packages
;;; Commentary:
;; Packages related to godot.
;;; Code:

;; Major mode for editing Godot GDScript files
;; https://github.com/GDQuest/emacs-gdscript-mode
;; https://github.com/scony/godot-gdscript-toolkit/
;; python3 -m pip install --user gdtoolkit
;; C-c C-r C-s is bound to (gdscript-godot-run-current-scene)!
(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "GDQuest/emacs-gdscript-mode")
  ;; Use toml-mode for .godot and .tscn source files
  :init
  (defun gdscript-format-buffer-save-hook ()
    (interactive)
    (when (eq major-mode 'gdscript-mode)
      (gdscript-format-buffer)))
  (add-hook 'before-save-hook 'gdscript-format-buffer-save-hook)
  (add-hook 'gdscript-mode-hook 'lsp-deferred)
  (setq auto-mode-alist (append '(("\\.godot$" . toml-mode))
                                '(("\\.tscn$" . toml-mode))
                                '(("\\.gd$" . gdscript-mode))
                                auto-mode-alist)))
;;; godot.el ends here
