;;; kzkn-programming.el --- Programming stuff

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-n") nil)
            (define-key markdown-mode-map (kbd "M-p") nil)))

(subword-mode 1)

(require 'markdown-mode)

(require 'kzkn-c)
(require 'kzkn-lisp)
(require 'kzkn-sh)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq markdown-css-path "http://kevinburke.bitbucket.org/markdowncss/markdown.css")

(provide 'kzkn-programming)
