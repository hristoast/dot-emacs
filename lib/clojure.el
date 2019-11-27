;;; clojure.el -- for clojure functionality

;;; Commentary:
;; Require this from init.el to enable Clojure functionality.

;;; Code:
;; CIDER is a Clojure Interactive Development Environment that Rocks for Emacs
;; https://github.com/clojure-emacs/cider
;; Depends on clojure-mode:
;; https://github.com/clojure-emacs/clojure-mode
(use-package cider
  :ensure t
  :bind
  ("C-c n c" . delete-nrepl)
  :config
  (defun delete-nrepl ()
    "Close nREPL connection and delete the window."
    (interactive)
    (cider--close-connection-buffer (current-buffer))
    (delete-window)))

;; yasnippet 0.7.0+ snippets for clojure
;; https://github.com/mpenet/clojure-snippets
(use-package clojure-snippets :defer t :ensure t)

(provide 'clojure)
;;; clojure.el ends here
