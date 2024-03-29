;;; docker.el --- Load docker packages
;;; Commentary:
;; Packages related to docker.
;;; Code:

;; An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode :defer t :straight t)

;; https://earthly.dev/get-earthly
(use-package earthfile-mode :defer t :straight t)

;;; docker.el ends here
