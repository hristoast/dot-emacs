;;; html.el --- Load html packages
;;; Commentary:
;; Packages related to html.
;;; Code:

;; Built into Emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html
(use-package html-mode
  :no-require t
  :hook (web-mode . skewer-html-mode))

;; Live web development in Emacs
;; https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :straight t
  :init
  (setq-default httpd-root (concat user-emacs-directory "/httpd"))
  :bind
  ("C-c h p" . httpd-start)
  ("C-c h s" . httpd-stop))

;; web-mode: An autonomous emacs major-mode for editing web templates.
;; http://web-mode.org/
(use-package web-mode
  :straight t
  :defer t
  :init
  (setq-default
   web-mode-code-indent-offset 2
   web-mode-comment-style 2
   web-mode-css-indent-offset 2
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-markup-indent-offset 2)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.tpl\\'" . web-mode))

;;; html.el ends here
