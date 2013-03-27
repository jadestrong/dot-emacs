;;; kzkn-paths.el --- All sorts of paths

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path vendor-dir)

(mapc (lambda (dir) (add-to-list 'load-path dir))
      (remove-if (lambda (file)
                   (or (string-match-p "\\(?:\\.\\|\\.\\.\\)$" file)
                       (not (file-directory-p file))))
                 (directory-files vendor-dir t)))

(provide 'kzkn-paths)
