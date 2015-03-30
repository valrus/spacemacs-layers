(defvar valrus-excluded-packages
  '(
    ; Languages I don't use
    coffee-mode
    csharp-mode
    ensime
    less-css-mode
    powershell
    powershell-mode
    sbt-mode
    scala-mode2
    scss-mode

    ; Features I hate
    ac-ispell
    flyspell
    ispell

    ; UI things I hate
    vi-tilde-fringe

    ; solarized 4 lyfe
    monokai-theme
    zenburn-theme
    )
  "List of packages to exclude. ")

(defvar valrus-packages
  '(
    helm-ag
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
                                 (setq persp-auto-save-fname "work")
                                 (setq persp-auto-resume-time 0)
                                 (setq persp-set-last-persp-for-new-frames nil)
                                 (persp-mode 1)
                                 (persp-load-state-from-file "work")
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
