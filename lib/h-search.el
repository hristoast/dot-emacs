;;; h-search.el --- Packages related to editing.
;;; Commentary:
;; Packages related to editing.
;;; Code:

;; An Emacs frontend to The Silver Searcher
;; https://github.com/Wilfred/ag.el
(use-package ag
  :straight t
  ;; :ensure-system-package (ag . the_silver_searcher)
  :bind
  ("C-c s a" . ag)
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers 't
        ag-reuse-window 't))

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
