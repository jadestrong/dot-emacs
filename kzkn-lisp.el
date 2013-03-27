;;; kzkn-lisp.el --- All things Lisp

(require 'highlight-parentheses)
(add-hook 'find-file-hook 'highlight-parentheses-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "M-&") 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("Carton" . emacs-lisp-mode))

(put 'ert-deftest 'lisp-indent-function 'defun)

(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(provide 'kzkn-lisp)
