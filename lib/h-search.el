;;; h-search.el --- Packages related to editing.
;;; Commentary:
;; Packages related to editing.
;;; Code:

;; Ignore "vendor" directories when rgrepping. Useful for some go projects I've worked on.
(use-package grep
  :straight nil
  :bind
  ("C-c r" .  rgrep)
  ("<f13>" .  rgrep)
  :config
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories "vendor"))

;;; h-search.el ends here
