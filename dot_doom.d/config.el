;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
 +org-roam-path "~/Dropbox/org/roam/"
 +org-roam-db-path "~/.emacs.d/org-roam.db"
 +org-roam-private-path "~/Dropbox/org/roam/private/"
 +org-journal-path "~/Dropbox/org/roam/private/journal/"

 ;; org-capture
 +org-capture-notes-file "~/Dropbox/org/roam/private/notes.org"
 +org-capture-tasks-file "~/Dropbox/org/tasks.org")

;; Font and themes
(setq doom-font (font-spec :family "Fira Code" :size 12))
;; (setq doom-theme 'doom-solarized-light)
(setq doom-theme 'doom-one)

;; UI

;; Vertical rule at 80 characters
(add-hook! 'web-mode-hook  'display-fill-column-indicator-mode t)
(add-hook! 'css-mode-hook  'display-fill-column-indicator-mode t)
(add-hook! 'js-mode-hook   'display-fill-column-indicator-mode t)
(add-hook! 'scss-mode-hook 'display-fill-column-indicator-mode t)

;; Set initial frame size and position
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width . 0.5))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;; Org-mode
;; (require 'org)
;; (require 'ob-clojure)
;; (require 'ob-js)
(require 'cider)
;; (require 'htmlize)

;; (add-to-list 'org-modules 'org-habit t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq
 projectile-project-search-path '("~/projects/personal/" "~/projects/work/")
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

;; org-journal config
;; (use-package org-journal
;;   :after org
;;   :init
;;   (map! :leader
;;         (:prefix ("j" . "journal") ;; org-journal bindings
;;          :desc "Create new journal entry" "j" #'org-journal-new-entry
;;          :desc "Create new date entry" "d" #'org-journal-new-date-entry
;;          :desc "Open previous entry" "p" #'org-journal-open-previous-entry
;;          :desc "Open next entry" "n" #'org-journal-open-next-entry
;;          :desc "Search journal" "s" #'org-journal-search-forever))
;;   :custom
;;   (org-journal-dir +org-journal-path)
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-file-type 'weekly)
;;   (org-journal-file-header "#+title: Week %V, %Y\n#+created: %Y-%m-%d\n#+roam_alias:\n#+roam_tags: \"private\" \"personal\"\n\n[[file:../journal.org][Journal]]\n\n")
;;   (org-journal-date-format "%Y-%m-%d (%A)")
;;   (org-journal-time-prefix "")
;;   (org-journal-time-format ""))

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
                                      (file +org-capture-tasks-file)
                                      "* TODO %? %^g" :prepend t :kill-buffer t :empty-lines-before 1)
                                     ("n" "note" entry
                                      (file +org-capture-notes-file)
                                      "* %? %^g" :prepend t :kill-buffer t :empty-lines-before 1))
             org-todo-keywords '((sequence "TODO(t)" "TODAY(a)" "NEXT(n)" "|" "DONE(d)" "NONE(x)")
                                 (sequence "WAIT(w@/!)" "HOLD (h@/!)" "|" "CANC(c@/!)" "MISS(m)" "SKIP(s)"))))
(setq
 org-agenda-files (directory-files +org-path t "\\.org$" t))
;; org-agenda-skip-scheduled-if-done t)

;; Agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        '((:name "Habits"
           :habit t)
          (:name "Priority"
           :tag "priority"
           :order 2)
          (:name "Projects"
           :tag "Projects"
           :order 3)
          (:name "Programming"
           :tag "programming")
          (:name "PKM"
           :tag "pkm")
          (:name "Tools"
           :tag "tools")
          (:name "Integral"
           :tag "integral")
          (:name "Work"
           :tag "work")
          (:name "Chores"
           :tag "chores"
           :order 8)))
  :config
  (org-super-agenda-mode))

;; Org-roam
;; (require 'org-roam)

(require 'time-stamp)
(add-hook 'write-file-functions 'time-stamp)

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam"                        "r" #'org-roam
        :desc "org-roam-insert"                 "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer"       "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file"              "f" #'org-roam-find-file
        :desc "org-roam-show-graph"             "g" #'org-roam-show-graph
        :desc "org-roam-insert"                 "i" #'org-roam-insert
        :desc "org-roam-capture"                "c" #'org-roam-capture)
  ;; (:prefix ("R" . "roam")
  ;;  (:prefix  ("d" . "by date")
  ;;   :desc "org-roam-dailies-date"           "d" #'org-roam-dailies-date
  ;;   :desc "org-roam-dailies-today"          "t" #'org-roam-dailies-today
  ;;   :desc "org-roam-dailies-tomorrow"       "m" #'org-roam-dailies-tomorrow
  ;;   :desc "org-roam-dailies-yesterday"      "y" #'org-roam-dailies-yesterday))
  :custom
  (org-roam-directory +org-roam-path)
  (org-roam-db-location +org-roam-db-path)
  (org-roam-index-file "index.org")
  (org-roam-file-extensions '("org" "txt"))
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
   '(("d" "personal (default)" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "private/${slug}"
      :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"private\" \"personal\"\n\n* Links\n** "
      :unnarrowed t)
     ("w" "work" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "private/${slug}"
      :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"private\" \"work\"\n\n"
      :unnarrowed t)
     ("t" "draft" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"drafts\"\n\n"
      :unnarrowed t)
     ("p" "public" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+title: ${title}\n#+created: %<%Y-%m-%d>\n#+roam_alias:\n#+roam_tags: \"public\"\n\n"
      :unnarrowed t)))
  (org-roam-link-title-format "%s")
  :config
  (defun org-roam--title-to-slug (title)
    "Convert title to a filename-suitable slug. Uses hyphens rather than underscores."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("--*" . "-")  ;; remove sequential underscores
                      ("^-" . "")  ;; remove starting underscore
                      ("-$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))

  ;; hide org-roam-buffer by default
  ;; (setq +org-roam-open-buffer-on-find-file nil)

  ;; org-roam org-export hook to add backlinks
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) +org-roam-path)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
      ""))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Links\n") links))))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;; TODO refactor
(after! org
  (defun yosevu/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp))

;; (use-package org-download
;;   :after org
;;   :bind
;;   (:map org-mode-map
;;    (("s-Y" . org-download-screenshot)
;;     ("s-y" . org-download-yank))))

;; (use-package deft
;;   :after org
;;   :bind
;;   ("C-c n d" . deft)
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory +org-roam-path))

;;; Custom Package Configuration

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
