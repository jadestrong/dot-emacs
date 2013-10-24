(eval-when-compile
  (require 'cl))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

(defun load-x (file)
  (load (expand-file-name file user-emacs-directory)))

(load-x "misc")
(load-x "defuns")

(server-start)

;;;; packages

(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-case-fold t)))

(use-package migemo
  :if (executable-find "cmigemo")
  :init
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (migemo-init)))

(use-package helm
  :init
  (progn
    (use-package helm-ag
      :config
      (progn
        (defalias 'ag 'helm-ag)
        (defalias 'ag-file 'helm-ag-this-file)))
    (use-package helm-gtags
      :config
      (progn
        (add-hook 'c-mode-common-hook 'helm-gtags-mode)
        (add-hook 'helm-gtags-mode-hook
                  (lambda ()
                    (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
                    (local-set-key (kbd "M-,") 'helm-gtags-find-rtag)
                    (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
                    (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
                    (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)
                    (local-set-key (kbd "C-c o") 'helm-gtags-parse-file))))))
  :config
  (progn
    (bind-key "C-c h" 'helm-mini)
    (bind-key "M-y" 'helm-show-kill-ring)
    (bind-key "C-M-n" 'helm-next-source helm-map)
    (bind-key "C-M-p" 'helm-previous-source helm-map)))

(use-package markdown-mode
  :config
  (progn
    (setq markdown-css-path "http://kevinburke.bitbucket.org/markdowncss/markdown.css")
    (bind-key "M-n" nil markdown-mode-map)
    (bind-key "M-p" nil markdown-mode-map)
    (unless window-system
      ;; `C-t' confilict to tmux's escape key, so avoid it
      (bind-key "C-c t 0" 'markdown-remove-header markdown-mode-map)
      (bind-key "C-c t 1" 'markdown-insert-header-atx-1 markdown-mode-map)
      (bind-key "C-c t 2" 'markdown-insert-header-atx-2 markdown-mode-map)
      (bind-key "C-c t 3" 'markdown-insert-header-atx-3 markdown-mode-map)
      (bind-key "C-c t 4" 'markdown-insert-header-atx-4 markdown-mode-map)
      (bind-key "C-c t 5" 'markdown-insert-header-atx-5 markdown-mode-map)
      (bind-key "C-c t 6" 'markdown-insert-header-atx-6 markdown-mode-map)
      (bind-key "C-c t h" 'markdown-insert-header-dwim markdown-mode-map)
      (bind-key "C-c t s" 'markdown-insert-header-setext-2 markdown-mode-map)
      (bind-key "C-c t t" 'markdown-insert-header-setext-1 markdown-mode-map)))
  :mode
  (("\\.markdown$" . markdown-mode)
   ("\\.md$" . markdown-mode)))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-common-hook (lambda () (c-set-style "K&R")))
    (setq tab-width 2)
    (setq c-basic-offset 2)))

(use-package highlight-parenthese
  :init (add-hook 'find-file-hook 'highlight-parentheses-mode))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package auto-async-byte-compile
      :init (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)))
  :config (bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode (("Cask" . emacs-lisp-mode)))

(use-package paredit
  :init
  (dolist (mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              'enable-paredit-mode)))

(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asdf$" . lisp-mode))
(when (optional-load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(use-package lua-mode
  :mode (("\\.lua$" . lua-mode)))

(use-package sh-script
  :config (setq sh-basic-offset 2))
