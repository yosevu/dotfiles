;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;

;; Font and Theme
(load-theme 'doom-nord-light t) ; Load theme
(doom-themes-org-config) ; Correct and improve org-mode's native fonts
(mac-auto-operator-composition-mode t)

;;(load! "lisp/alfred-org-capture")

(after! org
  (setq org-capture-templates
        '(("n" "Inbox" entry
           (file "inbox.org")
           "* TODO %?" :prepend t :kill-buffer t)
          ("d" "Idea" entry
           (file "ideas.org")
           "* 💡 %?" :prepend t :kill-buffer t))))

(setq org-directory (expand-file-name "~/Google Drive/org/")
      org-ellipsis " ▼ "
      org-journal-dir "~/Google Drive/org/journal/"
      org-journal-file-type 'yearly
      org-log-done 'time
      projectile-project-search-path '("~/Documents/repos/")
      visual-line-mode t)
