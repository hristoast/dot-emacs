;;; internals.el --- Tweaks to various Emacs internals.
;;; Commentary:
;; Tweaks to various Emacs internals.
;;; Code:

(defconst default-org-file "~/src/org/home.org")

(defvar whitespace-style
  '(spaces tabs newline space-mark tab-mark newline-mark face))

;; Emacs highlights misspellings only in comments or strings
(flyspell-prog-mode)
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(global-auto-revert-mode t)
;; https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode t)
;; No toolbar, please.
(tool-bar-mode -1)
;; Or menu bar...
(menu-bar-mode -1)
;; Or scroll bar.
(scroll-bar-mode -1)
;; Delete highlighted text when you type
(delete-selection-mode t)

;; As advised by https://www.emacswiki.org/emacs/TrampMode
(setq-default tramp-default-method "ssh")

(setq
 ;; Backup files ...
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 ;; Show column numbers
 column-number-mode t
 ;; Make org-agenda-list open when Emacs launches. This does not
 ;; work with daemon mode, instead use this arg with emacsclient:
 ;; --eval '(org-agenda-list)'
 ;; initial-buffer-choice 'org-agenda-list
 ;; Auto-open symlinks that point to vcs-controlled files
 vc-follow-symlinks t
 ;; No splash screen.
 inhibit-splash-screen t
 ;; No default scratch
 initial-scratch-message nil
 ;; Show five lines from the previous page when paging up or down
 next-screen-context-lines 5
 ;; Jive with the system clipboard
 select-enable-clipboard t
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 shell-file-name (executable-find "bash")
 xterm-mouse-mode t)

;; Load any custom stuff
(if (file-exists-p custom-file)
    (load-file custom-file))

(setq-default
;; Which browser to open links in
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox"
 ;; display-time-mode options
 display-time-24hr-format t
 display-time-format "%T"
 display-time-interval 1
 ;; https://emacs.stackexchange.com/a/16836
 ;; TODO: When Emacs is iconified/minimized the title reverts to the default.
 ;; When Emacs is iconified/minimized and another Emacs frame is in focus, the
 ;; title becomes just the file name (not the full path.)
 frame-title-format '("%f [%m]")
 ;; Don't automatically 'fix' files with DOS line endings
 inhibit-eol-conversion t
 ;; Org mode stuff
 org-log-done t
 org-agenda-files (list default-org-file)
 ;; No tabs
 indent-tabs-mode nil
 ;; "Tabs" are 4 spaces
 tab-width 4)

(display-time-mode)

;; Enable the disabled things
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'upcase-word 'disabled nil)

(if (getenv "EMACS_LINUM")
    ;; http://is.gd/Mw5KiS
    (global-linum-mode t))

(defvar h-font-height)
(if (getenv "RETINA_DISPLAY")
    (setq h-font-height 170)
  (setq h-font-height 100))

;; Hack - http://sourcefoundry.org/hack/
(if (or (file-exists-p (concat (getenv "HOME") "/.fonts/Hack-Regular.ttf"))
        (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
    (set-face-attribute 'default nil
                        :family "Hack"
                        :height h-font-height
                        :weight 'normal))

;; Emoji font
(set-fontset-font t 'symbol (font-spec :family "Noto Emoji") (selected-frame) 'prepend)
;; I don't work on macOS much anymore.. but if i do:
;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") (selected-frame) 'prepend))

;; Maximize Emacs when it's opened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; internals.el ends here
