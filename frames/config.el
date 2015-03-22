(setq display-buffer-alist
      '(("^[^\\*].*[^\\*]$" . (display-buffer-pop-up-frame)))
      '(("^[^\\*].*[^\\*]$" . (display-buffer-pop-up-frame)))
      )

; (rassq-delete-all 'pop-up-frame display-buffer-alist)

(defun my-helm-new-frame (buf)
  (switch-to-buffer-other-frame buf))

(setq helm-display-function 'my-helm-new-frame)
(setq helm-cleanup-hook 'delete-frame)
(setq-default pop-up-frames "graphic-only")
