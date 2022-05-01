;;; fennel.el --- Support for fennel
;;; Commentary:
;; Enable support for fennel https://fennel-lang.org/
;;; Code:

;; https://gitlab.com/technomancy/fennel-mode/-/blob/686e4d28a8abeb1fa05cb21e14c4f0cc12217d63/Readme.md
(use-package fennel-mode
  :straight t
  :hook
  (after-save . (lambda ()
                  (when (eq major-mode 'fennel-mode)
                    (let ((compilation-read-command nil)
                          (compile-command "make"))
                      (call-interactively 'compile)))))
  :init
  ;; THANKS: https://emacs.stackexchange.com/a/9421
  (defun hristoast--bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (if (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (search-forward "warning" nil t))))
        (run-with-timer 0 nil
                        (lambda (buf)
                          (bury-buffer buf)
                          (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                        buffer)))
  (add-hook 'compilation-finish-functions 'hristoast--bury-compile-buffer-if-successful)
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

;;; fennel.el ends here
