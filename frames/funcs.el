(defun my-new-maximized-frame ()
  (interactive)
  (switch-to-buffer-other-frame "*scratch*")
  (toggle-frame-maximized))

(defun my-new-frame ()
  (interactive)
  (switch-to-buffer-other-frame "*scratch*"))
