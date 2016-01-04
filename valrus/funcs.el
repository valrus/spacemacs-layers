;;; Easy open common useful files
(defun my-open-todos ()
    "Open todo.org file in a new frame."
    (interactive)
    (find-file-other-frame (concat (getenv "HOME") "/.emacs.d/my-org/todo.org")))

(defun my-open-config ()
  "Open this config layer's main config in a new frame."
  (interactive)
  (find-file-other-frame (concat (getenv "HOME")
                                 "/.emacs.d/private/valrus/config.el"))
  (persp-switch 'conf))

(defun my-open-init ()
  "Open the main spacemacs config in a new frame."
  (interactive)
  (find-file-other-frame (concat (getenv "HOME") "/.spacemacs")))
