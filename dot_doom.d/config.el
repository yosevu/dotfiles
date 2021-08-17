;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name ""
;; user-mail-address "")

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
(setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'doom-palenight)

(setq auth-sources '("~/.authinfo"))

(setq ghub-use-workaround-for-emacs-bug 'force)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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
;; To get information about any of these functions/macros, move the cursor over
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


;; Variables
(setq
 ;; File and directory paths
 +org-path "~/Dropbox/org/"
 +org-roam-path "~/Dropbox/org/notes/"
 +org-roam-db-path "~/.emacs.d/org-roam.db"
 +org-roam-private-path "~/Dropbox/org/notes/private/"
 +org-journal-path "~/Dropbox/org/notes/private/journal/"

 ;; org-capture
 +org-capture-inbox-file "~/Dropbox/org/inbox.org")

;; UI

;; Dashboard
(use-package! dashboard
  :init ; add config
  (progn
    (setq dashboard-items '(;(agenda . 5)
                            (bookmarks .5)
                            (projects . 5)
                            (recents . 5)))
                            ;(registers . 5)))
    (setq dashboard-banner-logo-title nil)
    (setq dashboard-startup-banner nil)
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-init-info t)
    (setq dashboard-footer-messages '(
                                          "We like to say that we don't get to choose our parents, that they were given by chance--yet we can truly choose whose children we'd like to be. - Seneca"
                                          "Man lives on one quarter of what he eats. On the other three quarters live his doctors. - Unknown"
                                          "If you want everything to be familiar, you will never learn anything new because it can't be significantly different from what you already know - Rich Hickey"
                                          "The best thing a human being can do is to help another human being know more. - Charlie Munger"
                                          "In my whole life, I have known no wise people (over a broad subject matter area) who didn't read all the time — none, zero. - Charlie Munger"
                                          "To be everywhere is to be nowhere. - Seneca"
                                          "If you don't know where you're going, you might not get there - Yogi Berra"
                                          "Substitute nuance for novelty - Angela Duckworth"
                                          "If you want to test your memory, try to remember what you were worrying about one year ago today. - E. Joseph Cossman"
                                          "Don't ask yourself what the world needs. Ask yourself what makes you come alive and then go do that. Because what the world needs is people who have come alive. - Howard Thurman"))
    :config
    (dashboard-setup-startup-hook)))

(map! :leader
      :desc "Go to dashboard."
      "d" #'dashboard-refresh-buffer)

;; Vertical rule at 80 characters
(add-hook! 'web-mode-hook  'display-fill-column-indicator-mode t)
(add-hook! 'css-mode-hook  'display-fill-column-indicator-mode t)
(add-hook! 'js-mode-hook   'display-fill-column-indicator-mode t)
(add-hook! 'typescript-mode-hook   'display-fill-column-indicator-mode t)
(add-hook! 'scss-mode-hook 'display-fill-column-indicator-mode t)

;; Set initial frame size and position
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width . 0.5))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;; Org-mode
;; (require 'ob-clojure)
;; (require 'ob-js)
;; (require 'cider)
;; (require 'htmlize)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq frame-title-format '("%b"))

(setq
 projectile-project-search-path '("~/projects/personal/" "~/projects/work/")
 org-startup-folded t
 visual-line-mode t
 auto-fill-mode t
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 js-indent-level 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 dired-dwim-target t ; http://ergoemacs.org/emacs/emacs_dired_tips.html
 css-indent-offset 2)

(after! org (setq
             org-ellipsis " ▼ "
             org-log-done 'time ; Insert a timestamp after the headline when a task is marked done.
             org-log-into-drawer t
             org-treat-insert-todo-heading-as-state-change t
             ;; org-babel-clojure-backend 'cider
             org-bullets-bullet-list '("·")
             org-tags-column -80
             org-log-done 'time
             org-refile-targets (quote (("external-links.org" :maxlevel . 1)))
             ;; org-refile-use-outline-path t
             ;; org-refile-use-outline-path 'buffer-name
             ;; org-refile-use-outline-path 'file
             ;; org-refile-targets (quote ((nil :maxlevel . 1)))
             ;; org-refile-use-outline-path 'file
             ;; org-outline-path-complete-in-steps nil
             org-capture-templates '(
                                     ("t" "task" entry
                                      (file +org-capture-inbox-file)
                                      "* TODO %? %^g" :prepend t :kill-buffer t :empty-lines-before 1)
                                     ("n" "note" entry
                                      (file +org-capture-inbox-file)
                                      "* %? %^g" :prepend t :kill-buffer t :empty-lines-before 1))
             org-todo-keywords '((sequence "TODO(t)" "TODAY(a)" "NEXT(n)" "|" "DONE(d)" "NONE(x)")
                                 (sequence "WAIT(w@/!)" "HOLD (h@/!)" "|" "CANC(c@/!)" "MISS(m)" "SKIP(s)"))))

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
  (org-journal-file-header "#+title: %Y Journal\n#+created: %Y-%m-%d\n#+filetags: \"journal\"\n\n\n")
  (org-journal-date-format "[%Y-%m-%d %a %R] - Week %V")
  (org-journal-date-prefix "* ")
  (org-journal-time-format "")
  (org-journal-time-prefix ""))

;; org-agenda-skip-scheduled-if-done t)

;; org-super-agenda
;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq org-super-agenda-groups
;;         '((:name "Habits"
;;            :habit t)
;;           (:name "Priority"
;;            :tag "priority"
;;            :order 1)
;;           (:name "Projects"
;;            :tag "Projects"
;;            :order 2)
;;           (:name "Learning"
;;            :tag ("learning" "reading"))
;;           (:name "Programming"
;;            :tag "programming")
;;           (:name "PKM"
;;            :tag "pkm")
;;           (:name "Tools"
;;            :tag "tools")
;;           (:name "Integral"
;;            :tag "integral")
;;           (:name "Work"
;;            :tag "work")
;;           (:name "Chores"
;;            :tag "chores"
;;            :order 8)))
;;   :config
;;   (org-super-agenda-mode))

;; timestamp on save - requires :head "#+TITLE: ${title}\nTime-stamp: <>\n"

(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
;; (add-hook 'write-file-functions 'time-stamp) ; update when saving

;; org-roam-v2

(setq org-roam-v2-ack t)

(use-package! org-roam
  :after org
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory +org-roam-path)
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
 :config
  ;; (setq org-roam-mode-sections
  ;;       (list #'org-roam-backlinks-insert-section
  ;;             #'org-roam-reflinks-insert-section
  ;;             #'org-roam-unlinked-references-insert-section))
 (org-roam-setup)
 (setq org-roam-capture-templates
  '(("d" "default" plain
     "%?"
     ;; :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"private\" \"personal\"\n\n* Links\n** "
     :if-new (file+head "private/${slug}.org"
                        "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+updated: Time-stamp: \" \"")
     :immediate-finish t
     :unnarrowed t)
    ("p" "public" plain
     "%?"
     ;; :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"public\"\n\n"
     :if-new (file+head "${slug}.org"
                        "#+title: ${title}\n#+created: %<%Y-%m-%d>\n")
     :unnarrowed t)
    ("w" "work" plain
     "%?"
     ;; :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"private\" \"work\"\n\n"
     :if-new (file+head "work/${slug}.org"
                        "#+title: ${title}\n#+created: %<%Y-%m-%d>\n")
     :unnarrowed t))))

;; Common Lisp
(after! sly
  (setq sly-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl" "-L" "sbcl" "-Q" "run") :coding-system utf-8-unix))))

;; anki-editor
(use-package! anki-editor
  :commands (anki-editor-mode))

;; deft
(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory +org-roam-path))

;; poke-line
(use-package! poke-line
  :config
  (poke-line-global-mode 1)
  ;; (setq-default poke-line-pokemon "pikachu"))
  (poke-line-set-random-pokemon))

;;; Miscellaneous

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions));;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Temporary
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.theme\\'" . php-mode))
