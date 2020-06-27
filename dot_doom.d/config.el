;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;

;; Font and Theme
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-nord t)
(load-theme 'doom-solarized-light t)
;; (load-theme 'doom-palenight t)

(doom-themes-org-config) ; Correct and improve org-mode's native fonts
;; (mac-auto-operator-composition-mode t) ; Ligature support for fonts like Fira Code. Works with emacs-mac.

;; Emacs Dashboard
(use-package dashboard
  :ensure t
  :init ; add config
  (progn
    (setq dashboard-items '(
                            ;; (agenda . 5)
                            (projects . 5)
                            (recents . 5)
                            (bookmarks .5)
                            (registers . 5)
                            ))
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
  :config
  (dashboard-setup-startup-hook))

;;(load! "lisp/alfred-org-capture")

;; Org-mode
;; Set initial frame size and position
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width . 0.5))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(require 'org)
(require 'ob-clojure)
(require 'cider)

(setq
 org-ellipsis " ▼ "
 org-log-done 'time ; Insert a timestamp after the headline when a task is marked done.
 org-babel-clojure-backend 'cider
 projectile-project-search-path '("~/Documents/projects/personal/" "~/Documents/projects/work/")
 visual-line-mode t)

;; tide config
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; org-journal config
(use-package org-journal
  :after org
  :init
  (map! :leader
        (:prefix ("j" . "journal") ;; org-journal bindings
         :desc "Create new journal entry" "j" #'org-journal-new-entry
         :desc "Open previous entry" "p" #'org-journal-open-previous-entry
         :desc "Open next entry" "n" #'org-journal-open-next-entry
         :desc "Search journal" "s" #'org-journal-search-forever))
  :custom
  (org-journal-dir "~/Google Drive/org/private/journal")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-type 'weekly)
  (org-journal-file-header "#+TITLE: Week %V, %Y \n#+ROAM_TAGS: journal \n\n [[file:journal.org][Journal]] \n\n")
  (org-journal-date-format "%Y-%m-%d (%A)")
  (org-journal-time-prefix "")
  (org-journal-time-format ""))

(setq org-agenda-files (directory-files-recursively "~/Google Drive/org/" "\\.org$"))

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
  (org-roam-directory "~/Google Drive/org/")
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
   '(("d" "personal (default)" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "private/${slug}"
      :head "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: \"private\" \"personal\"\n\n"
      :unnarrowed t)
     ("w" "work" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "private/${slug}"
      :head "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: \"private\" \"work\"\n\n"
      :unnarrowed t)
     ("f" "draft" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: \"drafts\"\n\n"
      :unnarrowed t)
     ("p" "public" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: \"public\"\n\n"
      :unnarrowed t)))
  (org-roam-link-title-format "%s")
  :config
  (defun org-roam--title-to-slug (title)
    "Convert TITLE to a filename-suitable slug. Uses hyphens rather than underscores."
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
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
        ""))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links))))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;; org-roam org-export hook to add backlinks with content
;; (defun my/org-roam--backlinks-list-with-content (file)
;;   (with-temp-buffer
;;     (if-let* ((backlinks (org-roam--get-backlinks file))
;;               (grouped-backlinks (--group-by (nth 0 it) backlinks)))
;;         (progn
;;           (insert (format "\n\n* %d Backlinks\n"
;;                           (length backlinks)))
;;           (dolist (group grouped-backlinks)
;;             (let ((file-from (car group))
;;                   (bls (cdr group)))
;;               (insert (format "** [[file:%s][%s]]\n"
;;                               file-from
;;                               (org-roam--get-title-or-slug file-from)))
;;               (dolist (backlink bls)
;;                 (pcase-let ((`(,file-from _ ,props) backlink))
;;                   (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
;;                   (insert "\n\n")))))))
;;     (buffer-string)))

;;   (defun my/org-export-preprocessor (backend)
;;     (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
;;       (unless (string= links "")
;;         (save-excursion
;;           (goto-char (point-max))
;;           (insert (concat "\n* Backlinks\n") links)))))

;;   (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;; Publish org files and assets to public HTML

;; (setq org/head-extra "<link rel='stylesheet' type='text/css' href='css/main.css'/>")

;; org-publish config
(require 'ox-publish)
(setq org-publish-project-alist
      '(("org-notes"
      :auto-sitemap t
      :sitemap-filename "index.org"
      :sitemap-title "Index"
      :base-directory "~/Google Drive/org/"
      :base-extension "org"
      :exclude "private"
      :publishing-directory "~/Google Drive/org/public/"
      :recursive t
      :publishing-function org-html-publish-to-html
      :section-numbers nil
      :html-head-extra "<link rel='stylesheet' href='css/main.css' type='text/css'/>"
      :headline-levels 4
      :auto-preamble t)
        ("org-static"
         :base-directory "~/Google Drive/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Google Drive/org/public/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("org-notes" "org-static"))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Google Drive/org/"))