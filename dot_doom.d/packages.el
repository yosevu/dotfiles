;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The ~unpin!~ macro allows you to unpin single packages.
(unpin! org-roam)

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! org-journal)
(package! org-super-agenda)
(package! vue-mode)
;;(package! alfred-org-capture :recipe (
;;                                      :fetcher github
;;                                      :repo "jjasghar/alfred-org-capture"
;;                                      :files ("el/alfred-org-capture.el")))
