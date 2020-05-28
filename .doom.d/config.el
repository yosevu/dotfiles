;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;

;; Font and Theme
;; (load-theme 'doom-nord-light t) ; Load theme
(load-theme 'doom-nord t) ; Load theme
(doom-themes-org-config) ; Correct and improve org-mode's native fonts
(mac-auto-operator-composition-mode t)

;;(load! "lisp/alfred-org-capture")

;; Org-mode

;; (after! org
;;   (setq org-capture-templates
;;         '(("n" "Inbox" entry
;;            (file "inbox.org")
;;            "* TODO %?" :prepend t :kill-buffer t)
;;           ("d" "Idea" entry
;;            (file "ideas.org")
;;            "* 💡 %?" :prepend t :kill-buffer t))))

;; (setq org-directory (expand-file-name "~/Google Drive/org/")
      ;; org-ellipsis " ▼ "
      ;org-journal-dir "~/Google Drive/org/journal/"
      ;org-journal-file-type 'yearly
      ;; org-log-done 'time
      ;; projectile-project-search-path '("~/Documents/repos/")
      ;; visual-line-mode t)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(use-package org-journal
  :after org
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Google Drive/org/roam/journal")
  (org-journal-date-format "%A, %d %B %Y"))

(require 'ox-publish)
(require 'htmlize)
(require 'org-roam)

(use-package! org-roam
  :custom
  (org-roam-directory "~/Google Drive/org/")
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+TITLE: ${title}\n#+CREATED: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n\n* ${title}\n"
      :unnarrowed t)))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
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
          (insert (concat "\n* Backlinks\n") links)))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))


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

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Google Drive/org/"
         :base-extension "org"
         :publishing-directory "~/Google Drive/org/public/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-head-extra "<link rel='stylesheet' type='text/css' href='css/main.css'/>"
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
 )

("org-static"
 :base-directory "~/Google Drive/org/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/Google Drive/org/public/"
 :recursive t
 :publishing-function org-publish-attachment
 )

("org" :components ("org-notes" "org-static"))

))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Google Drive/org/"))
