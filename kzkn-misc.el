;;; kzkn-misc.el --- Miscellaneous settings

;; display time on mode line
(display-time-mode t)

;; mojibake
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; urusai
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; region hilighting
(setq-default trasient-mark-mode t)
(set-face-background 'region "SkyBlue")
(set-face-foreground 'region "black")

;; show line number/column number on mode line
(require 'linum)
(global-linum-mode t)
(column-number-mode t)

;; auto save
(auto-save-mode t)

;; create backup
(setq backup-inhibited t)

;; backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; use space for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; beep
(setq visible-bell t)

;; show matching parentheses
(show-paren-mode 1)

;; font
(condition-case nil
    (set-frame-font "Consolas 10")
  (error (ignore-errors (set-frame-font "Ricty 10"))))

;; delete region
(delete-selection-mode t)

;; hide tool bar
(when window-system
  (tool-bar-mode -1))

;; hide menu bar when -nw
(unless window-system
  (menu-bar-mode -1))

;; move lines as logical line (like Emacs 22)
(setq line-move-visual nil)

;; use clipboard
(when window-system
  (setq x-select-enable-clipboard t))

;; ignore cases on find-file completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; mozc
(require 'mozc)
(setq default-input-method "japanese-mozc")

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; inkpot-theme
(when window-system
  (require 'inkpot-theme))

;;; delete trailing whitespace (like Vim) on before save
(defvar do-delete-trailing-whitespace-on-save t)

(defun delete-trailing-whitespace-on-save ()
  (interactive)
  (when do-delete-trailing-whitespace-on-save
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace-on-save)

;; save with not delete trailing whitespace
(defun save-buffer-with-no-delete-trailing-whitespace ()
  (interactive)
  (let ((do-delete-trailing-whitespace-on-save nil))
    (save-buffer)))

;; auto backup/restore scratch buffer
(defun save-scratch-data ()
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max)
                  (expand-file-name "~/.emacs.d/scratch"))))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.emacs.d/scratch"))
    (when (file-exists-p file)
      (set-buffer "*scratch*")
      (erase-buffer)
      (insert-file-contents file))))

(read-scratch-data)

(provide 'kzkn-misc)
