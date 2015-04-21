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
                                 )))
