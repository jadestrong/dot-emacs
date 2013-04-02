;;; init.el --- Where it all begins

(require 'cl)

(defconst vendor-dir
  (expand-file-name "vendor" user-emacs-directory)
  "Path to vendor directory")

(load (expand-file-name "kzkn-paths.el" user-emacs-directory))

(package-initialize)

(require 'kzkn-ido)
(require 'kzkn-helm)
(require 'kzkn-defuns)
(require 'kzkn-misc)
(require 'kzkn-programming)

(require 'carton)
(carton-setup user-emacs-directory)

(server-start)
