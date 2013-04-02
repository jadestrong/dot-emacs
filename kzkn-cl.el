;;; kzkn-cl.el --- Common Lisp Programming stuff

(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asdf$" . lisp-mode))

;; use quicklisp-slime-helper
;; installation: (ql:quickload "quicklisp-slime-helper")
(when (optional-load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(provide 'kzkn-cl)
