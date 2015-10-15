;;; packages.el --- persp-mode Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar persp-mode-packages
  '(
    persp-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar persp-mode-excluded-packages '()
  "List of packages to exclude.")

(defun persp-mode/init-persp-mode ()
  (use-package persp-mode
    :commands (
               persp-switch
               persp-rename
               persp-temporarily-display-buffer
               persp-add-buffer
               persp-remove-buffer
               persp-prev
               persp-next
               persp-save-state-to-file
               persp-load-state-from-file
               )
    :init
    (progn
      (setq wg-morph-on nil)
      (setq persp-auto-save-opt 2)
      (setq persp-auto-save-num-of-backups 1)
      (setq persp-save-dir (concat (getenv "HOME") "/.emacs.d/private/persp-confs/"))
      (setq persp-auto-save-fname "work")
      (setq persp-auto-resume-time 0)
      (setq persp-set-last-persp-for-new-frames nil)
      (add-hook 'kill-emacs-hook 'persp-save-state-to-file)

      (spacemacs/declare-prefix "L" "layouts")
      (evil-leader/set-key
        "La" 'persp-add-buffer
        "Lk" 'persp-remove-buffer
        "Ls" 'persp-switch
        "Lr" 'persp-rename
        "Lt" 'persp-temporarily-display-buffer
        "Lp" 'persp-prev
        "Ln" 'persp-next
        "Lw" 'persp-save-state-to-file
        "Ll" 'persp-load-state-from-file
        "Lc" 'persp-kill))
    :config
    (progn
      (persp-mode 1)
      (persp-load-state-from-file "work"))))

