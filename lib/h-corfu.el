;;; h-corfu.el --- Packages related to corfu.
;;; Commentary:
;; Packages related to corfu.
;;; Code:

;; Corfu enhances completion at point with a small completion popup.
;; https://github.com/emacs-straight/corfu/tree/1d8b6030c9022a5b9ad784b8cba2a284b8093ecb#installation-and-configuration
;; https://github.com/emacs-straight/corfu/tree/1d8b6030c9022a5b9ad784b8cba2a284b8093ecb#auto-completion
;; https://github.com/emacs-straight/corfu/tree/1d8b6030c9022a5b9ad784b8cba2a284b8093ecb#tab-and-go-completion
(use-package corfu
  :straight t
  :after corfu-echo corfu-history corfu-info corfu-popupinfo
  ;; TAB-and-Go customizations
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)

  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  :hook
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode)))

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode)

  ;; (corfu-popupinfo-mode)
  (corfu-echo-mode)
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Corfu Extensions - these have to load before corfu itself.
;; https://github.com/emacs-straight/corfu/tree/1d8b6030c9022a5b9ad784b8cba2a284b8093ecb#extensions
(use-package corfu-echo
  :load-path "straight/repos/corfu/extensions")

(use-package corfu-history
  :load-path "straight/repos/corfu/extensions")

(use-package corfu-info
  :load-path "straight/repos/corfu/extensions")

;; TODO: This was pretty buggy but I love the idea.
;; (use-package corfu-popupinfo
;;   :load-path "straight/repos/corfu/extensions")

;; This emacs package adds configurable icon or text-based completion prefixes based on the :company-kind property that many completion backends (such as lsp-mode and Emacs 28's elisp-mode) provide.
;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; h-corfu.el ends here
