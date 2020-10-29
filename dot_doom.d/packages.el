;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The ~unpin!~ macro allows you to unpin single packages.
(unpin! org-roam)

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-roam :recipe (:host github :repo "org-roam/org-roam" :branch "master"))
(package! org-super-agenda)
(package! org-web-tools)
(package! vterm-extra :recipe (:host github :repo "Sbozzolo/vterm-extra" :branch "master"))
;; Not currently using Vue.js
;; (package! vue-mode)

(package! org-cv
  :recipe (:host gitlab :repo "Titan-C/org-cv"))
