;;; ruby.el --- Load ruby packages
;;; Commentary:
;; Packages related to ruby.
;;; Code:

;; Robe: Code navigation, documentation lookup and completion for Ruby
;; https://github.com/dgutov/robe
;; Requires: `gem install pry` and a Gemfile listing your gems
(use-package robe
  :straight t
  :diminish robe-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

;; Activate Robe and company-robe when we start ruby-mode
(use-package ruby-mode
  :straight t
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (when (derived-mode-p 'ruby-mode)
                (add-to-list 'company-backends 'company-robe))))
  (add-hook 'ruby-mode-hook 'robe-start))

;;; ruby.el ends here
