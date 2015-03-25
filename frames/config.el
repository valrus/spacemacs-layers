(setq display-buffer-alist
      '(("^[^\\*].*[^\\*]$" . (display-buffer-pop-up-frame (('top . (- 0))))))
      )

; (rassq-delete-all 'pop-up-frame display-buffer-alist)
(setq pop-up-frame-alist
      '((top . (+ 0))))

(defun my-helm-new-frame (buf)
   (switch-to-buffer-other-frame buf))

(defun my-helm-kill-frame ()
  (delete-frame))
;; this wipes out the active frame for some reason?
  ;; (display-buffer-pop-up-frame buf
  ;;                              '(
  ;;                               (pop-up-frame-parameters .
  ;;                                                         ((top . (- 0)))
  ;;                                                         ))))

(setq helm-display-function 'my-helm-new-frame)
(setq helm-cleanup-hook 'my-helm-kill-frame)
(setq-default pop-up-frames "graphic-only")
(setq frame-auto-hide-function 'delete-frame)
