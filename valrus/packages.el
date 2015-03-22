(defvar valrus-excluded-packages
  '(
    flyspell
    ispell
    )
  "List of packages to exclude. ")

(defvar valrus-packages
  '(
    persp-mode
    fill-column-indicator
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun valrus/init-persp-mode ()
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda ()
                                 (setq persp-auto-save-opt 2)
                                 (setq persp-auto-save-num-of-backups 1)
                                 (setq persp-save-dir (concat (getenv "HOME") "/.emacs.d/private/persp-confs/"))
                                 (setq persp-auto-save-fname "default")
                                 (setq persp-auto-resume-time 0)
                                 (setq persp-set-last-persp-for-new-frames nil)
                                 (persp-mode 1)
                                 (persp-load-state-from-file "default")
                                 ))
  (evil-leader/set-key
    "Ps" 'persp-switch
    "Pr" 'persp-rename
    "Pt" 'persp-temporarily-display-buffer
    "Pa" 'persp-add-buffer
    "Pk" 'persp-remove-buffer
    "Pp" 'persp-prev
    "Pn" 'persp-next
    "Pw" 'persp-save-state-to-file
    "Pl" 'persp-load-state-from-file))

(defun valrus/init-fill-column-indicator ()
  (turn-on-fci-mode))
