;;; kzkn-evil.el --- evil settings

(require 'evil)
(evil-mode 1)

(defun define-normal-mode-keymap (kbd fn)
  (define-key evil-normal-state-map kbd fn))

(defun define-insert-mode-keymap (kbd fn)
  (define-key evil-insert-state-map kbd fn))

(defun define-ex-mode-keymap (cmd fn)
  (define-key evil-ex-map cmd fn))

;; ido-mode integration
(define-ex-mode-keymap "e" 'ido-find-file)
(define-ex-mode-keymap "b" 'ido-switch-buffer)

;; paredit-mode integration
(when (locate-library "paredit")
  (define-normal-mode-keymap ")" 'paredit-forward-up)
  (define-normal-mode-keymap "(" 'paredit-backward-up)
  (define-normal-mode-keymap (kbd "C-0") 'paredit-backward-down)
  (define-normal-mode-keymap (kbd "C-9") 'paredit-forward-down))

;; slime integration
(when (locate-library "slime")
  (define-normal-mode-keymap (kbd "M-.") 'slime-edit-definition)
  (define-normal-mode-keymap (kbd "M-,") 'slime-pop-find-definition-stack))

(provide 'kzkn-evil)
