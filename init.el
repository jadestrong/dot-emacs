;;; init.el --- Where it all begins

(require 'cl)

(defconst vendor-dir
  (expand-file-name "vendor" user-emacs-directory)
  "Path to vendor directory")

(load (expand-file-name "kzkn-paths" user-emacs-directory))

(package-initialize)

(require 'kzkn-ido)
(require 'kzkn-migemo)
(require 'kzkn-helm)
(require 'kzkn-defuns)
(require 'kzkn-misc)
(require 'kzkn-programming)
(optional-require 'kzkn-local)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(server-start)
