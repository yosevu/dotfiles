;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Custom vars
(setq
 ;; File and directory paths
 +org-path "~/Dropbox/org/"
 +org-roam-path "~/Dropbox/org/roam/"
 +org-roam-db-path "~/.emacs.d/org-roam.db"
 +org-roam-private-path "~/Dropbox/org/roam/private/"
 +org-journal-path "~/Dropbox/org/roam/private/journal/"

 ;; Lists
 +org-capture-notes-file "~/Dropbox/org/roam/private/notes.org"
 +org-capture-links-file "~/Dropbox/org/roam/private/links.txt"
 +org-capture-books-file "~/Dropbox/org/roam/private/books.txt"
 +org-capture-resources-file "~/Dropbox/org/roam/private/resources.txt"
 +org-capture-ideas-file "~/Dropbox/org/roam/private/ideas.txt"
 +org-capture-tools-file "~/Dropbox/org/roam/private/tools.txt"

 ;; Tasks
 +org-capture-personal-file "~/Dropbox/org/personal.txt"
 +org-capture-work-file "~/Dropbox/org/work.txt")

;; Font and themes

(setq doom-font (font-spec :family "Fira Code" :size 12))
;; (setq doom-theme 'solo-jazz) ; custom theme in .doom.d/themes
;; (setq doom-theme 'elegant-light) ; FIX: custom theme in .doom.d/themes
(load-theme 'doom-solarized-light t)
;; (load-theme 'doom-nord t)
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-opera-light t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-gruvbox-light t)
;; (load-theme 'dracula t)
;; (load-theme 'palenight t)

(doom-themes-org-config) ; Correct and improve org-mode's native fonts
;; (mac-auto-operator-composition-mode t) ; Ligature support for fonts like Fira Code. Works with emacs-mac.

(global-visual-line-mode t)
;; Entries older than 1 month are marked as read
(after! elfeed
  (setq elfeed-search-filter "@12-months-ago +unread"))

(use-package! poke-line
  :ensure t
  :config
  (poke-line-global-mode 1)
  (setq-default poke-line-pokemon "mewtwo"))
;; Emacs Dashboard
(use-package dashboard
  :ensure t
  :init ; add config
  (progn
    (setq dashboard-items '((agenda . 5)
                            (bookmarks .5)
                            (projects . 5)
                            (recents . 5)
                            (registers . 5)))
    (setq dashboard-startup-banner 'official)
    (setq dashboard-footer-messages '(
                                      "We like to say that we don't get to choose our parents, that they were given by chance--yet we can truly choose whose children we'd like to be. - Seneca"
                                      "Man lives on one quarter of what he eats. On the other three quarters live his doctors. - Unknown"
                                      "If you want everything to be familiar, you will never learn anything new because it can't be significantly different from what you already know - Rich Hickey"
                                      "The best thing a human being can do is to help another human being know more. - Charlie Munger"
                                      "In my whole life, I have known no wise people (over a broad subject matter area) who didn't read all the time — none, zero. - Charlie Munger"
                                      "To be everywhere is to be nowhere. - Seneca"
                                      "If you don't know where you're going, you might not get there - Yogi Berra"
                                      "Substitute nuance for novelty - Angela Duckworth"
                                      "If you want to test your memory, try to remember what you were worrying about one year ago today. - E. Joseph Cossman")))
  "Don't ask yourself what the world needs. Ask yourself what makes you come alive and then go do that. Because what the world needs is people who have come alive. - Howard Thurman"
  :config
  (dashboard-setup-startup-hook))

(use-package forge
  :after magit)

;; Set initial frame size and position
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width . 0.5))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Org-mode
(require 'org)
(require 'ob-clojure)
(require 'ob-js)
(require 'cider)

(add-to-list 'org-modules 'org-habit t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq
 org-ellipsis " ▼ "
 org-log-done 'time ; Insert a timestamp after the headline when a task is marked done.
 org-log-into-drawer t
 org-treat-insert-todo-heading-as-state-change t
 org-babel-clojure-backend 'cider
 projectile-project-search-path '("~/Documents/projects/personal/" "~/Documents/projects/work/")
 visual-line-mode t)


;; org-journal config
(use-package org-journal
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
  (org-journal-file-type 'weekly)
  (org-journal-file-header "#+title: Week %V, %Y\n#+created: %Y-%m-%d\n#+roam_alias:\n#+roam_tags: \"private\" \"personal\"\n\n[[file:../journal.org][Journal]]\n\n")
  (org-journal-date-format "%Y-%m-%d (%A)")
  (org-journal-time-prefix "")
  (org-journal-time-format ""))

(setq
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 js-indent-level 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 dired-dwim-target t ; http://ergoemacs.org/emacs/emacs_dired_tips.html
 org-ellipsis " ▾ "
 org-bullets-bullet-list '("·")
 org-tags-column -80
 org-log-done 'time
 css-indent-offset 2
 ;; org-refile-targets (quote ((nil :maxlevel . 1)))
 org-capture-templates '(
                         ("p" "personal task" entry
                          (file +org-capture-personal-file)
                          "* TODO %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("w" "work task" entry
                          (file +org-capture-work-file)
                          "* TODO %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("n" "note (fleeting)" entry
                          (file +org-capture-notes-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("l" "link" entry
                          (file +org-capture-links-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("b" "book" entry
                          (file +org-capture-books-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("r" "resource" entry
                          (file +org-capture-resources-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("i" "idea" entry
                          (file +org-capture-ideas-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ("t" "tool" entry
                          (file +org-capture-tools-file)
                          "* %?" :prepend t :kill-buffer t :empty-lines-before 1)
                         ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "NONE(x)")
        ;; '((sequence "TODO(t)" "DONE(d)")))
        (sequence "WAIT(w@/!)" "HOLD (h@/!)" "|" "CANC(c@/!)" "MISS(m)" "SKIP(s)")))

(use-package org-super-agenda
  :ensure t
  :config (org-super-agenda-mode))

(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name "Habits"
         :tag "pesonal"
         :habit t)
        (:name "Today"  ; Optionally specify section name
         :time-grid t  ; Items that appear on the time grid
         :date today
         :order 1)
        (:name "Due Today"
         :deadline today
         :order 2)
        (:name "Next"
         :todo "NEXT"
         :order 3)
        (:name "Important"
         :priority "A"
         :order 4)
        (:name "Soon"
         :scheduled future
         :order 5)
        (:name "Overdue"
         :deadline past
         :order 6)))

(require 'htmlize)
(require 'org-roam)


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
          (insert (concat "\n* Backlinks\n") links))))))

;; hide org-roam-buffer by default
(setq +org-roam-open-buffer-on-find-file nil)

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;; org-publish config
(require 'ox-publish)
(setq org-publish-project-alist
      '(("org-notes"
      :auto-sitemap t
      :sitemap-filename "index.org"
      :sitemap-title "Index"
      :base-directory "~/Dropbox/org/roam/"
      :base-extension "org"
      :exclude "private"
      :publishing-directory "~/Dropbox/notes.yosevu.com/public/"
      :recursive t
      :publishing-function org-html-publish-to-html
      :section-numbers nil
      :html-head-extra "<link rel='stylesheet' href='css/main.css' type='text/css'/>"
      :headline-levels 4
      :auto-preamble t)
        ("org-static"
         :base-directory "~/Dropbox/notes.yosevu.com/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/notes.yosevu.com/public/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("org-notes" "org-static"))))

(after! org
  (defun yosevu/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp)
  (setq
   org-agenda-files (directory-files +org-path t "\\.org$" t))
  org-agenda-skip-scheduled-if-done t)

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory +org-roam-path))

;; tide config
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package anki-editor
  :ensure t)

(use-package annotate
  :ensure t)

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer))
                          (setq pomidor-play-sound-file
                                (lambda (file)
                                  (start-process "my-pomidor-play-sound"
                                                 nil
                                                 "mplayer"
                                                 file))))))

(add-hook 'cider-mode-hook
          '(lambda () (add-hook 'after-save-hook
                                '(lambda ()
                                   (if (and (boundp 'cider-mode) cider-mode)
                                       (cider-namespace-refresh)
                                     )))))

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   (cider-ns-refresh)))

(map! :after clojure-mode
      :map clojure-mode-map
      :localleader
      (:prefix ("e" . "eval")
       "s" #'cider-eval-sexp-at-point))

(setq cider-stacktrace-default-filters '(tooling dup))
(setq cider-stacktrace-default-filters '(project))
(defun show-in-finder ()
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))
