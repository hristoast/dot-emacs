;;; ui.el --- User interface changes.
;;; Commentary:
;; User interface changes.
;;; Code:

(unless (getenv "EMACS_NO_ALERT")
  ;; https://github.com/jwiegley/alert
  ;; A Growl-like alerts notifier for Emacs
  (use-package alert
    :straight t
    :config
    (let ((hristoast-alert-styles
           #s(hash-table
              size 8
              test equal
              data
              ("growl" growl
               "libnotify" libnotify
               "log" log
               "message" message
               "mode-line" mode-line
               "notifications" notifications
               "osx-notifier" osx-notifier
               "x11" x11)))
          (hristoast-alert-default-style "libnotify"))
      (setq-default alert-default-style (gethash (or (getenv "EMACS_ALERT_STYLE") hristoast-alert-default-style)
                                                 hristoast-alert-styles nil)))))

(unless (getenv "EMACS_NO_DIFF_HL") ;; Don't use diff-hl.
  ;; diff-hl - highlight changes/diffs
  ;; https://github.com/dgutov/diff-hl
  (use-package diff-hl
    :straight t
    :config
    (global-diff-hl-mode)))

(unless (getenv "EMACS_NO_DASHBOARD")
  (use-package page-break-lines :straight t :defer t)

  ;; An extensible emacs startup screen showing you what’s most important.
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  (use-package dashboard
    :straight t
    :init (page-break-lines-mode)
    :config
    (dashboard-setup-startup-hook)
    ;; Allow banner customization through an environment variable
    (let ((hristoast-dashboard-banners
           #s(hash-table
              size 5
              test equal
              data
              ("official" official
               "logo" logo
               "1" 1
               "2" 2
               "3" 3)))
          (hristoast-dashboard-default-banner "logo"))
      (setq-default
       ;; For the page break lines
       dashboard-page-separator "\n\f\n"
       dashboard-banner-logo-title (or (getenv "EMACS_DASHBOARD_TITLE")
                                       "Welcome to Hristos' Emacs")
       dashboard-footer-messages '("hristos.co")
       dashboard-items '((recents  . 20)
                         (bookmarks . 20)
                         (agenda . 20))
       dashboard-set-file-icons t
       dashboard-set-heading-icons t
       dashboard-startup-banner (gethash (or (getenv "EMACS_DASHBOARD_BANNER")
                                             hristoast-dashboard-default-banner)
                                         hristoast-dashboard-banners nil))
      ;; Don't setup the dashboard when Emacs is started with files as command-line arguments.
      ;; https://github.com/emacs-dashboard/emacs-dashboard/issues/115#issuecomment-478167057
      (if (< (length command-line-args) 2)
          (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))))

(unless (getenv "EMACS_NO_ALL_THE_ICONS")
  (progn
    ;; A utility package to collect various Icon Fonts and propertize them within Emacs.
    ;; https://github.com/domtronn/all-the-icons.el
    (use-package all-the-icons :straight t)
    (use-package all-the-icons-dired
      :straight t
      :hook (dired-mode . all-the-icons-dired-mode))))

(unless (getenv "EMACS_NO_FIREPLACE") ;; Don't allow Emacs to be a warm cozy fireplace.
  ;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
  (use-package fireplace
    :straight t
    :init (defvar fireplace-mode-map)
    :bind (:map fireplace-mode-map
                ("d" . fireplace-down)
                ("s" . fireplace-toggle-smoke)
                ("u" . fireplace-up))))

(or (getenv "EMACS_NO_SMART_MODE_LINE")
    (getenv "EMACS_NO_THEME")
    ;; A powerful and beautiful mode-line for Emacs.
    ;; https://github.com/Malabarba/smart-mode-line
    ;; TODO: Possibly add support for extra themes (e.g. powerline and solarized)
    (let ((hristoast-sml-themes
           #s(hash-table
              size 3
              test equal
              data
              ("dark" dark
               "light" light
               "respectful" respectful)))
          (hristoast-sml-default-theme "dark"))

      (use-package smart-mode-line
        :straight t
        :config
        (setq
         sml/shorten-directory t
         sml/theme
         (gethash (or (getenv "EMACS_SML_THEME") hristoast-sml-default-theme)
                  hristoast-sml-themes nil))

        (if (getenv "EMACS_TRUST_SML_THEMES")
            ;; This isn't totally cool but here's an option for it anyways:
            ;; https://github.com/Malabarba/smart-mode-line/blob/f53e380c9aadcba42ed2d4ed6ebc508b5e006519/README.org#installation-issues-faq
            (setq sml-no-confirm-load-theme t))

        (sml/setup))))

(unless (getenv "EMACS_NO_THEME") ;; Don't load a theme.
  ;; Color Theme for emacs based on material design colors
  ;; https://github.com/cpaulik/emacs-material-theme
  ;; But use my fork that always selects grey colors over darkblue.
  (use-package material-theme
    :straight (material-theme
               :type git
               :host nil
               :repo "https://git.sr.ht/~hristoast/emacs-material-theme")
    :config
    (if (getenv "EMACS_TRUST_THEMES")
        (setq hristoast-trust-themes t)
      (setq hristoast-trust-themes nil))

    (let ((hristoast-material-themes #s(hash-table
                                size 8
                                test equal
                                data
                                ("dark" material
                                 "light" material-light)))
          (hristoast-material-default "dark"))
      (setq-default hristoast-material-theme
                    (gethash (or (getenv "EMACS_MATERIAL_THEME") hristoast-material-default)
                             hristoast-material-themes nil)))

    (load-theme hristoast-material-theme hristoast-trust-themes)))

(if (getenv "EMACS_LINUM")
    ;; http://is.gd/Mw5KiS
    (if (fboundp 'global-linum-mode)
        (global-linum-mode t)
      (display-line-numbers-mode)))

(defvar h-font-height)
(if (getenv "EMACS_TALLER_FONT")
    (setq h-font-height 140)
  (setq h-font-height 100))

(if (eq system-type 'windows-nt)
        ;; Use Cascadia Mono on Windows, it looks nice
        (set-face-attribute 'default nil
                            :family "Cascadia Mono"
                            :height h-font-height
                            :weight 'normal)

        (unless (getenv "EMACS_NO_HACK_FONT")
          ;; Hack - http://sourcefoundry.org/hack/
          (if (or (file-exists-p (concat (getenv "HOME") "/.fonts/Hack-Regular.ttf"))
                  (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
              (set-face-attribute 'default nil
                                  :family "Hack"
                                  :height h-font-height
                                  :weight 'normal))))

(unless (getenv "EMACS_NO_NOTO_EMOJI")
  ;; I don't work on macOS much anymore.. but if i do:
  ;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") (selected-frame) 'prepend)
  ;; Emoji font
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") (selected-frame) 'prepend))

;; At resolutions higher than 1920x1080 magit will split horizontally and I
;; really don't love that. The below function and binding for it allow me to
;; somewhat quickly rearrange things so the magit frame is split vertically.
;; THANKS: https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-c f s") 'toggle-frame-split)

;; https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode t)

;; No toolbar, please.
(tool-bar-mode -1)

;; Or menu bar...
(menu-bar-mode -1)

;; Or scroll bar.
(scroll-bar-mode -1)

;; Maximize Emacs when it's opened
(unless (getenv "EMACS_NO_MAXIMIZE")
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;; ui.el ends here
