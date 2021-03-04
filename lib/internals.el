;;; internals.el --- Tweaks to various Emacs internals.
;;; Commentary:
;; Tweaks to various Emacs internals.
;;; Code:

;; This is here because it needs to happen before org mode is loaded
(let ((h/org-file (or (getenv "EMACS_DEFAULT_ORG_FILE")
                      "~/src/org/org_home.org")))
  (when (file-exists-p h/org-file)
      (defconst default-org-file h/org-file)))

(defvar whitespace-style
  '(spaces tabs newline space-mark tab-mark newline-mark face))

;; Emacs highlights misspellings only in comments or strings
(flyspell-prog-mode)
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(global-auto-revert-mode t)
;; Delete highlighted text when you type
(delete-selection-mode t)

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
 ;; No splash screen.  TODO: Is this better suited for ui.el?
 inhibit-splash-screen t
 ;; No default scratch
 initial-scratch-message nil
 ;; https://git.sr.ht/~technomancy/better-defaults/tree/4c5409406ee35c5ba46880c6cfe98df4b14dc631/item/better-defaults.el#L85
 load-prefer-newer t
 ;; https://git.sr.ht/~technomancy/better-defaults/tree/4c5409406ee35c5ba46880c6cfe98df4b14dc631/item/better-defaults.el#L82
 mouse-yank-at-point t
 ;; Show five lines from the previous page when paging up or down
 next-screen-context-lines 5
 ;; https://git.sr.ht/~technomancy/better-defaults/tree/4c5409406ee35c5ba46880c6cfe98df4b14dc631/item/better-defaults.el#L83
 require-final-newline t
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
 ;; https://git.sr.ht/~technomancy/better-defaults/tree/4c5409406ee35c5ba46880c6cfe98df4b14dc631/item/better-defaults.el#L81
 apropos-do-all t
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
 ;; No tabs
 indent-tabs-mode nil
 ;; "Tabs" are 4 spaces
 tab-width 4
 ;; As advised by https://www.emacswiki.org/emacs/TrampMode
 tramp-default-method "ssh")

;; Set the default org file when it's bound
(when (boundp 'default-org-file)
  (setq-default org-agenda-files (list default-org-file)))

(unless (getenv "EMACS_NO_SAVE_PLACE")
  ;; https://git.sr.ht/~technomancy/better-defaults/tree/4c5409406ee35c5ba46880c6cfe98df4b14dc631/item/better-defaults.el#L65-66
  (save-place-mode 1))

;; Enable the disabled things
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'upcase-word 'disabled nil)

(if (string-equal (getenv "EMACS_AUDIO_BEEP") "audio")
    (setq ring-bell-function nil)
  (if (string-equal (getenv "EMACS_AUDIO_BEEP") "visual")
      (setq ring-bell-function t)
    ;; No bell/beeping by default.
    (setq ring-bell-function 'ignore)))

;;; internals.el ends here
