;;; racket.el --- Load racket packages
;;; Commentary:
;; Packages related to racket.
;;; Code:

;; GNU Emacs major modes for Racket: Edit and REPL.
;; https://github.com/greghendershott/racket-mode
;; See the below link for why the REPL doesn't load some functions.
;; http://stackoverflow.com/a/31523545
(use-package racket-mode :defer t)

;; Start racket-mode via a hook so we get rainbow delimiters
(use-package racket-repl-mode
  :defer t
  :ensure nil
  :init
  ;; TODO: Currently lacking a good way to start racket-mode when we launch
  ;; TODO: racket-repl on its own, not from a file with C-c C-c.
  ;; (add-hook 'racket-repl-mode-hook (racket-mode))
  (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode))

;;; racket.el ends here
