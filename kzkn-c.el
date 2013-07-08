;;; kzkn-c.el --- C programming stuff

(defun kzkn-c-mode-hook ()
  (c-set-style "K&R")
  (setq tab-width 2)
  (setq c-basic-offset 2))

(add-hook 'c-mode-hook 'kzkn-c-mode-hook)

(provide 'kzkn-c)
