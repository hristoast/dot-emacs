;;; python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken :defer t :straight t)

;; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/latest/
;; Requires: `pip install jedi`
;; Company backend for Python jedi
;; https://github.com/syohex/emacs-company-jedi
(use-package company-jedi
  :straight t
  :defer t
  :init
  (setq-default
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0.2))

;; Navigate Python documentation
;; https://github.com/statmobile/pydoc
(use-package pydoc :defer t :straight t)

;; Built into Emacs
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python-mode
  ;; Void packages: python3-language-server autopep8 python3-pycodestyle pylint python3-yapf
  ;; PyPI packages: pydocstyle==5.0.2 rope==0.17.0
  :bind
  ("<S-down-mouse-1>" . goto-definition-at-point)
  ("<S-down-mouse-3>" . quick-pydoc)
  :init
  (defun goto-definition-at-point (event)
    "Move the point to the clicked position
     and jedi:goto-definition the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (lsp-goto-implementation)))

  (defun quick-pydoc (event)
    "Move the point to the clicked position
     and pydoc the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (pydoc-at-point)))

  (add-hook 'python-mode-hook 'blacken-mode)
  ;; Python configuration for lsp-mode done in lsp.el...
  (add-hook 'python-mode-hook 'lsp-deferred))

;;; python.el ends here
