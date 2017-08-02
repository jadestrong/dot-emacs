(require 'cl-lib)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(let ((vendor-dir-path (expand-file-name "vendor" user-emacs-directory)))
  (dolist (dir (cl-remove-if (lambda (s)
                               (or (member s '("." ".."))
                                   (not (file-directory-p
                                         (expand-file-name s vendor-dir-path)))))
                             (directory-files vendor-dir-path)))
    (add-to-list 'load-path (expand-file-name dir vendor-dir-path))))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

(defun el-exists-p (f)
  (or (file-exists-p f)
      (file-exists-p (concat f ".el"))
      (file-exists-p (concat f ".elc"))))

(defun load-x (file)
  (let ((f (expand-file-name file user-emacs-directory)))
    (when (el-exists-p f)
      (load f))))

(load-x "misc")
(load-x "defuns")
(load-x "site")

(server-start)

;;;; Packages

(use-package ido
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 10)
  (setq ido-case-fold t))

(defun update-gtags ()
  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
         (dir (directory-file-name (file-name-directory file))))
    (when (executable-find "global")
      (start-process "gtags-update" nil "global" "-u"))))

(use-package helm
  :config
  (bind-key "M-y" 'helm-show-kill-ring)
  (bind-key "C-M-o" 'helm-occur)
  (bind-key "C-M-g" 'helm-ag))

;; (use-package helm-gtags
;;   :config
;;   (add-hook 'c-mode-common-hook 'helm-gtags-mode)
;;   (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;   (add-hook 'helm-gtags-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
;;               (local-set-key (kbd "M-,") 'helm-gtags-find-rtag)
;;               (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;               (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
;;               (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)
;;               (local-set-key (kbd "C-c o") 'helm-gtags-parse-file)
;;               (add-hook 'after-save-hook 'update-gtags nil 'local))))

(use-package markdown-mode
  :commands (markdown-mode)
  :config
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
    (bind-key "C-c t t" 'markdown-insert-header-setext-1 markdown-mode-map))
  :mode
  (("\\.markdown$" . markdown-mode)
   ("\\.md$" . markdown-mode)))

(defun compile-immediate ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(use-package cc-mode
  :commands (cc-mode)
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'inextern-lang 0)
              (setq-local c-default-style "K&R")
              (setq-local indent-tabs-mode nil)
              (setq-local tab-width 2)
              (setq-local c-basic-offset 2)))
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local c-basic-offset 4)))
  (mapc (lambda (map)
          (bind-key "C-c c" 'compile-immediate map)
          (bind-key "C-c n" 'next-error map)
          (bind-key "C-c p" 'previous-error map))
        (list c-mode-map
              c++-mode-map)))

(use-package highlight-parentheses
  :config
  (add-hook 'find-file-hook 'highlight-parentheses-mode))

(use-package elisp-mode
  :init
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
  (use-package auto-async-byte-compile
    :config
    (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
    (setq auto-async-byte-compile-suppress-warnings t))
  :config
  (bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
  :interpreter
  (("emacs" . emacs-lisp-mode))
  :mode
  (("Cask" . emacs-lisp-mode)))

(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asdf$" . lisp-mode))
(when (load (expand-file-name "~/quicklisp/slime-helper.el") t)
  (setq inferior-lisp-program "sbcl"))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package inkpot-theme
  :if window-system)

;; (use-package esup)

(use-package web-mode
  :commands (web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset nil
        web-mode-engines-alist '(("php" . "\\.ctp$")))
  :mode
  (("\\.ctp$" . web-mode)
   ("\\.html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.json$" . web-mode)))

(use-package php-mode
  :commands (php-mode)
  :config
  (setq php-manual-path "/opt/phpdoc")
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local c-basic-offset 4)))
  (bind-key "C-c C-m" 'php-search-documentation php-mode-map)
  :mode
  (("\\.php$" . php-mode)))

(use-package ws-butler
  :config
  (mapc (lambda (hook)
          (add-hook hook 'ws-butler-mode))
        '(c-mode-common-hook
          enh-ruby-mode-hook
          python-mode-hook
          haml-mode-hook
          yaml-mode-hook
          lisp-mode-hook
          emacs-lisp-mode-hook)))

(use-package flycheck)
(use-package flycheck-pyflakes)

(use-package yaml-mode
  :commands (yaml-mode))

(use-package editorconfig
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package adoc-mode
  :commands (adoc-mode)
  :mode
  (("\\.adoc$" . adoc-mode)))

(defun set-enh-ruby-mode-face ()
  (set-face-attribute 'enh-ruby-op-face nil :foreground nil :inherit 'default))

(use-package enh-ruby-mode
  :commands (enh-ruby-mode)
  :config
  (setq enh-ruby-deep-indent-paren nil
        enh-ruby-add-encoding-comment-on-save nil)
  (add-hook 'enh-ruby-mode-hook 'set-enh-ruby-mode-face t)
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package ruby-electric
  :commands (ruby-electric-mode)
  :init (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

(use-package inf-ruby
  :commands (inf-ruby-minor-mode))

(use-package rspec-mode
  :commands (rspec-mode)
  :config
;  (setq rspec-use-bundler-when-possible t
;        rspec-use-spring-when-possible nil)
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (when (string-match "_spec\\.rb\\'" (buffer-file-name))
                (rspec-mode)))))

(use-package haml-mode
  :commands (haml-mode))

(use-package magit
  :commands (magit-status)
  :init (bind-key "C-x g" 'magit-status))

(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "gray20")
  (set-face-background 'highlight-indentation-current-column-face "gray35")

  (defadvice highlight-indentation-guess-offset (around my-highlight-indentation-guess-offset)
    (cond ((and (eq major-mode 'enh-ruby-mode) (boundp 'enh-ruby-indent-level))
           (setq ad-return-value enh-ruby-indent-level))
          ((and (eq major-mode 'haml-mode) (boundp 'haml-indent-offset))
           (setq ad-return-value haml-indent-offset))
          (t
           ad-do-it)))

  (ad-activate 'highlight-indentation-guess-offset)

  (dolist (hook '(enh-ruby-mode-hook python-mode-hook haml-mode-hook))
    (add-hook hook 'highlight-indentation-mode)
    (add-hook hook 'highlight-indentation-current-column-mode)))


;;;; Global Bindings

(bind-key "M-g" 'goto-line)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)

(bind-key "C-c s" 'swap-windows)
(bind-key "C-c w" 'whitespace-mode)
