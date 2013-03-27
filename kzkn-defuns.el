;;; kzkn-defuns.el --- Custom functions

(defun google ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(defun toggle-samba-path ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (equal "smb:" (buffer-substring (point) (+ (point) 4)))
        (thunar-to-win)
      (win-to-thunar))))

(defun thunar-to-win ()
  (%toggle-smb-path "/" "\\\\" (lambda () (delete-char 4))))

(defun win-to-thunar ()
  (%toggle-smb-path "\\" "/" (lambda () (insert "smb:"))))

(defun %toggle-smb-path (before-path-separator
                         after-path-separator
                         on-beginning-line)
  (let ((end (progn (end-of-line) (point))))
    (beginning-of-line)
    (funcall on-beginning-line)
    (while (and (search-forward before-path-separator nil t)
                (<= (point) end))
      (replace-match after-path-separator))))

(defun resize-window ()
  "Resize the current window."
  (interactive)
  (when (one-window-p)
    (error "Cannot resize sole window"))
  (catch 'done
    (while t
      (message "size[%dx%d]" (window-width) (window-height))
      (let ((c (read-char)))
        (condition-case nil
            (cond ((= c ?f) (enlarge-window-horizontally 2))
                  ((= c ?b) (shrink-window-horizontally 2))
                  ((= c ?n) (enlarge-window 2))
                  ((= c ?p) (shrink-window 2))
                  ((= c ?q)
                   (message "quit") (throw 'done t))
                  (t nil))
          (error nil))))))

(provide 'kzkn-defuns)
