;; GC
(setq gc-cons-threshold (* 128 1024 1024))

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

;; show column number on mode line
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
(defun set-font (fonts)
  (if fonts
      (condition-case nil
          (set-frame-font (car fonts))
        (error (set-font (cdr fonts))))
    nil))

(set-font '("Consolas 10" "Ricty 10"))

;; delete region
(delete-selection-mode t)

;; move lines as logical line (like Emacs 22)
(setq line-move-visual nil)

;; use clipboard
(when window-system
  (setq x-select-enable-clipboard t))

;; ignore cases on find-file completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; current line
(defvar hl-line-face) ;; Quiet the byte-compiler
(defface hlline-face
  '((t (:background "dark slate gray")))
  "*Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

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
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max)
                    (expand-file-name "~/.emacs.d/scratch")))))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.emacs.d/scratch"))
    (when (file-exists-p file)
      (set-buffer "*scratch*")
      (erase-buffer)
      (insert-file-contents file))))

(add-hook 'emacs-startup-hook 'read-scratch-data)
