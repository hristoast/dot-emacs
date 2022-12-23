;;; keybindings.el --- Custom keybindings that are not necessarily mode-specific.
;;; Commentary:
;; Custom keybindings that are not necessarily mode-specific.
;;; Code:

;; Insert a newline, then indent according to major mode
(global-set-key (kbd "RET") 'newline-and-indent)
;; Build, compile, that stuff
(global-set-key (kbd "<f5>") 'hristoast-build-project)
;; Make undo work like other editors
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "M-z") 'undo)
;; Better comment-toggling
(global-set-key (kbd "C-;") 'hristoast-toggle-comment)
;; Make text bigger
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
(global-set-key (kbd "C-x r b") 'revert-buffer)
(global-set-key (kbd "C-c q q q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c d w") 'downcase-word)
(global-set-key (kbd "C-c u w") 'upcase-word)
(global-set-key (kbd "C-x u") 'upcase-region)
(global-set-key (kbd "C-x t m") 'menu-bar-mode)
(global-set-key (kbd "C-x C-v") 'clipboard-yank)
(global-set-key (kbd "C-S-^") 'enlarge-window)
(global-set-key (kbd "C-%") 'shrink-window)
(global-set-key (kbd "C-c g l") 'goto-line)
(global-set-key (kbd "C-c e d") 'eldoc-doc-buffer)
;; Code folding
(global-set-key (kbd "C-x C-a f") 'hs-hide-all)
(global-set-key (kbd "C-x C-a c") 'hs-show-all)
(global-set-key (kbd "<backtab>") 'hs-show-all)
(global-set-key (kbd "C-x f") 'hs-hide-block)
(global-set-key (kbd "C-x c") 'hs-show-block)
(global-set-key (kbd "<f11>") 'hristoast-toggle-fc-and-ws)
;; More movement
(global-set-key (kbd "M-n") #'(lambda () (interactive) (scroll-up 4)))
(global-set-key (kbd "M-p") #'(lambda () (interactive) (scroll-down 4)))

;; Kill this buffer!
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

;;; keybindings.el ends here
