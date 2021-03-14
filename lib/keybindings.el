;;; keybindings.el --- Custom keybindings that are not necessarily mode-specific.
;;; Commentary:
;; Custom keybindings that are not necessarily mode-specific.
;;; Code:

;; Insert a newline, then indent according to major mode
(global-set-key (kbd "RET") 'newline-and-indent)
;; Build, compile, that stuff
(global-set-key (kbd "<f5>") 'build-project)
;; Make undo work like other editors
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "M-z") 'undo)
;; Better comment-toggling
(global-set-key (kbd "M-/") 'toggle-comment)
;; Make text bigger
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-=") 'text-scale-increase)
(global-set-key (kbd "<f9>") 'text-scale-increase)
;; Ctrl-mouse scroll up to make text bigger
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
;; Or make it smaller
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "<f8>") 'text-scale-decrease)
;; Ctrl-mouse scroll down to make text smaller
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
;; Toggle whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; Extra keybindings that make life great
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "<f13>") 'rgrep) ;; Nice for macs
(global-set-key (kbd "C-x r b") 'revert-buffer)
(global-set-key (kbd "C-c q q q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c d w") 'downcase-word)
(global-set-key (kbd "C-c u w") 'upcase-word)
(global-set-key (kbd "C-x u") 'upcase-region)
(global-set-key (kbd "C-x t m") 'menu-bar-mode)
(global-set-key (kbd "TAB") 'indent-appropriately)
(global-set-key (kbd "C-x C-v") 'clipboard-yank)
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-S-^") 'enlarge-window)
(global-set-key (kbd "C-%") 'shrink-window)
(global-set-key (kbd "C-c g l") 'goto-line)
(global-set-key (kbd "C-c e d") 'eldoc-doc-buffer)

(defun toggle-fc-and-ws ()
  "Toggle displaying the fill column indicator and `whitespace-mode' in one handy function."
  (interactive)
  (display-fill-column-indicator-mode 'toggle)
  (whitespace-mode 'toggle))

(global-set-key (kbd "<f11>") 'toggle-fc-and-ws)

;; Kill this buffer!
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

;;; keybindings.el ends here
