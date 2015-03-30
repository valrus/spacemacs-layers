; (setq display-buffer-alist ())
(add-to-list 'display-buffer-alist
             '("^[^\\*].*[^\\*]$" .
               (display-buffer-pop-up-frame . ((reusable-frames . nil)))))

; (rassq-delete-all 'pop-up-frame display-buffer-alist)
(setq pop-up-frame-alist
      '((top . (+ 0))))

(defun my-helm-new-frame (buf)
  (switch-to-buffer-other-frame buf)
  (with-selected-window (helm-window)
    (delete-other-windows)))

(defun my-helm-display (buf)
  (display-buffer buf '((display-buffer-pop-up-frame) (reusable-frames . nil)))
  (pop-to-buffer buf))

(defun my-popup-frame-cleanup (&optional frame force)
    (delete-frame))

(setq helm-display-function 'my-helm-new-frame)
(setq helm-cleanup-hook 'my-popup-frame-cleanup)

(setq-default pop-up-frames "graphic-only")
(setq frame-auto-hide-function 'my-popup-frame-cleanup)

; Make magit open in a frame

(add-to-list 'display-buffer-alist
             '(".*COMMIT_EDITMSG" .
               ((display-buffer-pop-up-frame) . ())))

(add-to-list 'display-buffer-alist
             '("\*MAGIT.*\*" .
               ((display-buffer-pop-up-frame) . ())))
