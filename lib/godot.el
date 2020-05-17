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
  :bind ("C-x r s" . h/godot-run-current-scene-debug)
  :init

  (defun h/godot-run-current-scene-debug ()
    "Run the current scene in debug mode and switch to the output buffer."
    (interactive)
    (progn
      (let ((godot-buffer "*godot*"))
        (with-current-buffer godot-buffer (erase-buffer))
        (gdscript-godot-run-current-scene-debug)
        (switch-to-buffer-other-window godot-buffer))))

  (defun h/gdscript-format-buffer-save-hook ()
    (when (eq major-mode 'gdscript-mode)
      (progn
        (gdscript-format-buffer)
        ;; Gotta run this to get things going again...
        (lsp))))

  (add-hook 'before-save-hook 'h/gdscript-format-buffer-save-hook)
  (add-hook 'gdscript-mode-hook 'lsp-deferred)
  (setq auto-mode-alist (append '(("\\.godot$" . conf-toml-mode))
                                '(("\\.tres$" . conf-toml-mode))
                                '(("\\.tscn$" . conf-toml-mode))
                                '(("\\.gd$" . gdscript-mode))
                                auto-mode-alist)))

;;; godot.el ends here
