(defvar frames-packages
  '(
    autofit-frame
    frame-cmds
    window+
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar frames-excluded-packages
  '(
    )
  "List of packages to exclude.")

(defun frames/init-autofit-frame ()
  (setq fit-frame-min-width 120)
  (setq fit-frame-min-height 40)
  (add-hook 'after-make-frame-functions 'fit-frame)
  (add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window 'append))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
