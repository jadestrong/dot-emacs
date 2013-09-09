;;; kzkn-helm.el --- helm settings

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)

(require 'helm)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)

(require 'helm-ag)
(defalias 'ag 'helm-ag)
(defalias 'ag-file 'helm-ag-this-file)

(require 'helm-gtags)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
(add-hook 'helm-gtags-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
            (local-set-key (kbd "M-,") 'helm-gtags-find-rtag)
            (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
            (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
            (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)
            (local-set-key (kbd "C-c o") 'helm-gtags-parse-file)))



(provide 'kzkn-helm)
