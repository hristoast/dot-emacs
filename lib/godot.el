;;; godot.el --- Load godot packages
;;; Commentary:
;; Packages related to godot.
;;; Code:

;; Major mode for editing Godot GDScript files
;; https://github.com/GDQuest/emacs-gdscript-mode
;; https://github.com/scony/godot-gdscript-toolkit/
;; python3 -m pip install gdtoolkit
;; C-c C-r C-s is bound to (gdscript-godot-run-current-scene)!
(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :hook
  (before-save . (lambda ()
                   (when (eq major-mode 'gdscript-mode)
                     (progn
                       (gdscript-format-buffer)))))
  :init
  (setq-default gdscript-docs-local-path "~/.local/godot-engine/docs")
  (bind-key "C-x r s" (lambda ()
                        "Run the current scene in debug mode and switch to the output buffer."
                        (interactive)
                        (progn
                          (let ((godot-buffer "*godot*"))
                            (if (get-buffer godot-buffer)
                                (with-current-buffer godot-buffer (erase-buffer)))
                            (gdscript-godot-run-current-scene-debug)
                            (switch-to-buffer-other-window godot-buffer))))))

;;; godot.el ends here
