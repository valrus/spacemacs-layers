(spacemacs/set-leader-keys
  "Vc" 'vc-next-action
  "Vu" 'vc-update)

(spacemacs/set-leader-keys-for-major-mode 'log-edit-mode
  "cc" 'log-edit-done)

(defun cvs/set-log-edit-keys ()
  (local-set-key "\C-k" 'log-edit-previous-comment)
  (local-set-key "\C-j" 'log-edit-next-comment))

(add-hook 'log-edit-mode-hook 'cvs/set-log-edit-keys)
