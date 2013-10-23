;;; kzkn-lisp.el --- All things Lisp

;; highlight-parentheses
(require 'highlight-parentheses)
(add-hook 'find-file-hook 'highlight-parentheses-mode)

;; hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(require 'paredit)
(dolist (mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            'enable-paredit-mode))

;; key-bindings
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; special files
(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Carton" . emacs-lisp-mode))

(provide 'kzkn-lisp)
