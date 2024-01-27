;; ⛔ Error (use-package): Cannot load powershell-mode
;; ⛔ Warning (comp): powershell.el:746:6: Warning: the function ‘speedbar-add-supported-extension’ is not known to be defined.
(use-package powershell-mode
  :straight (powershell-mode
             :type git
             :host github
             :repo "jschaf/powershell.el")
  :init
  (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))
