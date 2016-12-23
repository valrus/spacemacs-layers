;;; packages.el --- kivy Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq kivy-packages
    '(
      glsl-mode
      kivy-mode
      ))

;; List of packages to exclude.
(setq kivy-excluded-packages '())

;; For each package, define a function kivy/init-<package-name>
;;
(defun kivy/init-kivy-mode ()
  "Initialize kivy-mode."
  (use-package kivy-mode
    :mode "\\.kv$"

    :config (when kivy-indent-on-enter
              (add-hook 'kivy-mode-hook
                        '(lambda ()
                           (define-key kivy-mode-map "\C-m" 'newline-and-indent))))))

(defun kivy/init-glsl-mode ()
  "Initialize glsl-mode."
  (use-package glsl-mode
    :init
    (autoload 'glsl-mode "glsl-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
