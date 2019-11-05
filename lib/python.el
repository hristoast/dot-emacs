;;; python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken :defer t)

;; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/latest/
;; Requires: `pip install jedi`
;; Company backend for Python jedi
;; https://github.com/syohex/emacs-company-jedi
(use-package company-jedi
  :defer t
  :init
  (setq-default
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0.2))

;; Navigate Python documentation
;; https://github.com/statmobile/pydoc
(use-package pydoc :defer t)

;; Built into Emacs
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python-mode
  :bind
  ("<S-down-mouse-1>" . goto-definition-at-point)
  ("<S-down-mouse-3>" . quick-pydoc)
  :ensure nil
  :functions jedi:goto-definition jedi:stop-server maybe-stop-jedi-server pydoc-at-point
  :init

  (defun goto-definition-at-point (event)
    "Move the point to the clicked position
     and jedi:goto-definition the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (jedi:goto-definition)))

  (defun maybe-stop-jedi-server ()
    "Stop the Jedi server, if need be."
    (if (boundp 'jedi:stop-server)
        (jedi:stop-server)))

  (defun quick-pydoc (event)
    "Move the point to the clicked position
     and pydoc the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (pydoc-at-point)))

  (defun use-system-python3 ()
    "Use the system python3."
    (interactive)
    (maybe-stop-jedi-server)
    (defvar python-check-command)
    (defvar python-shell-interpreter)
    (setq
       python-check-command "pyflakes"
       python-shell-interpreter "python3"
       flycheck-python-flake8-executable "flake8"
       jedi:environment-virtualenv (list "python3" "-m" "venv")
       jedi:environment-root (concat (getenv "HOME") "/.emacs.d" "/.py/system3")
       jedi:server-args
       '("--sys-path" "/usr/lib/python3.6/site-packages"
         "--sys-path" "~/.local/lib/python3.6/site-packages"))
      (if (not (file-exists-p
                (concat jedi:environment-root
                        "/lib/python3.6/site-packages/jediepcserver.py")))
          (jedi:install-server)))

  (add-hook 'python-mode-hook 'use-system-python3)
  (add-hook 'python-mode-hook 'blacken-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (add-to-list 'company-backends 'company-jedi)))))

;;; python.el ends here
