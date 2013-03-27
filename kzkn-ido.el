;;; kzkn-ido.el --- Interactive do

(ido-mode t)

(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-max-prospects 10)
(setq ido-case-fold t)

(provide 'kzkn-ido)
