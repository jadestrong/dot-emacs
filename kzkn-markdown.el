;;; kzkn-markdown.el --- Markdown editing

(require 'markdown-mode)

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-n") nil)
            (define-key markdown-mode-map (kbd "M-p") nil)
            (unless window-system
              ;; `C-t' confilict to tmux's escape key, so avoid it
              (define-key markdown-mode-map (kbd "C-c t 0") 'markdown-remove-header)
              (define-key markdown-mode-map (kbd "C-c t 1") 'markdown-insert-header-atx-1)
              (define-key markdown-mode-map (kbd "C-c t 2") 'markdown-insert-header-atx-2)
              (define-key markdown-mode-map (kbd "C-c t 3") 'markdown-insert-header-atx-3)
              (define-key markdown-mode-map (kbd "C-c t 4") 'markdown-insert-header-atx-4)
              (define-key markdown-mode-map (kbd "C-c t 5") 'markdown-insert-header-atx-5)
              (define-key markdown-mode-map (kbd "C-c t 6") 'markdown-insert-header-atx-6)
              (define-key markdown-mode-map (kbd "C-c t h") 'markdown-insert-header-dwim)
              (define-key markdown-mode-map (kbd "C-c t s") 'markdown-insert-header-setext-2)
              (define-key markdown-mode-map (kbd "C-c t t") 'markdown-insert-header-setext-1))))

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq markdown-css-path "http://kevinburke.bitbucket.org/markdowncss/markdown.css")

(provide 'kzkn-markdown)
