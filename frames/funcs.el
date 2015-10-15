(defun my-new-maximized-frame ()
  (interactive)
  (select-frame (make-frame))
  (toggle-frame-maximized))
