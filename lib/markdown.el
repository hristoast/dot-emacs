;;; markdown.el --- Load markdown packages
;;; Commentary:
;; Packages related to markdown.
;;; Code:

;; Emacs Markdown Mode
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :straight t
  :config
  ;; Special indent for markdown-mode
  ;; TODO: set this the right way
  ;; (add-hook 'markdown-mode-hook
  ;;           (global-set-key (kbd "TAB") 'md-indent))
  (defun md-indent ()
    "Indent for `markdown-mode', to be used to rebind TAB - WIP."
    (interactive)
    (if mark-active
        (do-func-to-marked-region 'markdown-indent-region)
      (markdown-indent-line))))

;; Additional functions for Emacs [markdown-mode].
;; https://github.com/milkypostman/markdown-mode-plus
(use-package markdown-mode+ :straight t :defer t)

;;; markdown.el ends here
