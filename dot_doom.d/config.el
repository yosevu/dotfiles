;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yosevu Kilonzo"
      user-mail-address "yosevu@yosevu.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Font and themes
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Fira Sans" :size 14)
      doom-big-font (font-spec :family "Fira Mono" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'doom-spacegray)
;; (setq doom-theme 'night-owl)
;; (setq doom-theme 'elegant-emacs)
;; (setq doom-theme 'modus-vivendi)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'almost-mono-themes)
;; (setq doom-theme 'paper)
(setq doom-theme 'doom-palenight)

(setq auth-sources '("~/.authinfo"))

(setq ghub-use-workaround-for-emacs-bug 'force)

;; Variables
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
 ;; File and directory paths
 ;; +org-path "~/Dropbox/notes/org/notes/"
 +org-path "~/Dropbox/notes/org/"
 +org-roam-path "~/Dropbox/notes/org/notes/"
 +org-journal-path "~/Dropbox/notes/org/journal/"
 +org-roam-db-file "~/Dropbox/notes/org/org-roam.db"

 ;; org-capture
 +org-capture-inbox-file "~/Dropbox/notes/org/inbox.org"
 +org-capture-task-file "~/Dropbox/notes/org/tasks/tasks.org"
 +org-capture-work-file "~/Dropbox/notes/org/tasks/work.org"
 +org-capture-backlog-file "~/Dropbox/notes/org/tasks/backlog.org"
 +org-capture-project-file "~/Dropbox/notes/org/tasks/projects.org")

(setq org-directory +org-path)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Configuration;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Vertical rule at 80 characters
(add-hook! 'web-mode-hook 'display-fill-column-indicator-mode t)
(add-hook! 'js-mode-hook 'display-fill-column-indicator-mode t)
(add-hook! 'typescript-mode-hook 'display-fill-column-indicator-mode t)
(add-hook! 'css-mode-hook 'display-fill-column-indicator-mode t)
(add-hook! 'scss-mode-hook 'display-fill-column-indicator-mode t)

;; Set initial frame size and position
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width . 0.5))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;; Org-mode
(setq org-log-note-clock-out t)
(setq org-show-notification-handler 'message)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq frame-title-format '("%b"))

;; Editor
(setq evil-multiedit-default-keybinds t)

(setq
 projectile-project-search-path '("~/projects/personal/" "~/projects/work/")
 projectile-enable-caching t
 org-startup-folded t
 visual-line-mode t
 auto-fill-mode t
 ;; web-mode-markup-indent-offset 2
 ;; web-mode-code-indent-offset 2
 ;; web-mode-css-indent-offset 2
 js-indent-level 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 dired-dwim-target t ; http://ergoemacs.org/emacs/emacs_dired_tips.html
 css-indent-offset 2)

(after! org (setq)
            org-ellipsis " ▼ "
            org-log-done 'time ; Insert a timestamp after the headline when a task is marked done.
            org-log-into-drawer t
            org-treat-insert-todo-heading-as-state-change t
            ;; org-babel-clojure-backend 'cider
            ;; org-bullets-bullet-list '("·")
            org-tags-column -80
            org-log-done 'time
            org-refile-targets (quote (("external-links.org" :maxlevel . 1)))
            ;; org-refile-use-outline-path t
            ;; org-refile-use-outline-path 'buffer-name
            ;; org-refile-use-outline-path 'file
            ;; org-refile-targets (quote ((nil :maxlevel . 1)))
            ;; org-refile-use-outline-path 'file
            ;; org-outline-path-complete-in-steps nil
            org-capture-templates
            '(
              ("n" "note" entry
               (file +org-capture-inbox-file)
               "* %? %^g" :prepend t :kill-buffer t :empty-lines-before 1)
              ("t" "task" entry
               (file +org-capture-task-file)
               "* TODO %? %^g" :prepend t :kill-buffer t :empty-lines-before 1)
              ("w" "work" entry
               (file +org-capture-work-file)
               "* TODO %? %^g" :prepend t :kill-buffer t :empty-lines-before 1)
              ("b" "backlog" entry
               (file +org-capture-backlog-file)
               "* TODO %? %^g" :prepend t :kill-buffer t :empty-lines-before 1))

            org-todo-keywords '((sequence "TODO(t)" "TODAY(a)" "NEXT(n)" "|" "DONE(d)" "NONE(x)")
                                (sequence "WAIT(w@/!)" "HOLD (h@/!)" "|" "CANC(c@/!)" "MISS(m)" "SKIP(s)")))

(after! org
 (defun yosevu/org-archive-done-tasks ()
   "Archive all done tasks."
   (interactive)
   (org-map-entries 'org-archive-subtree "/DONE" 'file))
 (require 'find-lisp)
 (setq
  org-agenda-files (directory-files +org-path t "\\.org$" t)))

;; org-journal
(use-package! org-journal
  :after org
  :init
  (map! :leader
        (:prefix ("j" . "journal") ;; org-journal bindings
         :desc "Create new journal entry" "j" #'org-journal-new-entry
         :desc "Create new date entry" "d" #'org-journal-new-date-entry
         :desc "Open previous entry" "p" #'org-journal-open-previous-entry
         :desc "Open next entry" "n" #'org-journal-open-next-entry
         :desc "Search journal" "s" #'org-journal-search-forever))
  :custom
  (org-journal-dir +org-journal-path)
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-type 'yearly)
  (org-journal-file-header "#+title: %Y Journal\n#+created: %Y-%m-%d\n#+filetags:\n\n\n")
  (org-journal-date-format "[%Y-%m-%d %a %R] - Week %V")
  (org-journal-date-prefix "* ")
  (org-journal-time-format "")
  (org-journal-time-prefix ""))

;; timestamp on save - requires :head "#+TITLE: ${title}\nTime-stamp: <>\n"

(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
;; (add-hook 'write-file-functions 'time-stamp) ; update when saving

;; org-roam-v2

(use-package! org-roam
  :after org
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-refile" "r" #'org-roam-refile
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory +org-roam-path)
  (setq org-roam-db-location +org-roam-db-file)
  (add-to-list 'display-buffer-alist
                 '(("\\*org-roam\\*"
                    (display-buffer-in-direction)
                    (direction . right)
                    (window-width . 0.33)
                    (window-height . fit-window-to-buffer))))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))
  ;; disable open buffer on find file - doom-emacs var
  ;; (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-mode-sections-functions
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))

  (setq org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :target (file+head "${slug}.org"
                           "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+updated: Time-stamp: \" \"\n\n")
        :immediate-finish t
        :unnarrowed t)
       ("p" "public" plain
        "%?"
        :target (file+head "${slug}.org"
                           "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+filetags: :drafts:\n\n")
        :unnarrowed t)
       ("w" "work" plain
        "%?"
        :target (file+head "${slug}.org"
                           "#+title: ${title}\n#+created: %<%Y-%m-%d>\n\n")
        :unnarrowed t))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! flycheck
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(after! lsp-rust
  (setq lsp-rust-server 'rust-analyzer))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; anki-editor
(use-package! anki-editor
  :commands (anki-editor-mode))

;; poke-line
(use-package! poke-line
  :config
  (poke-line-global-mode 1)
  ;; (setq-default poke-line-pokemon "pikachu"))
  (poke-line-set-random-pokemon))

;;; Miscellaneous
(after! org-pomodoro
  (setq org-pomodoro-start-sound-p  t
        org-pomodoro-start-sound    "~/Dropbox/org/sounds/bell.mp3"
        org-pomodoro-finished-sound "~/Dropbox/org/sounds/bell.mp3"))

(require 'dired+)
(require 'bookmark+)

;; Disable rainbow-mode to prevent bug where CSS custom property names are highlighted
(remove-hook 'css-mode-hook #'rainbow-mode)

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions));;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
