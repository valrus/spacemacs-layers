(defvar frames-pre-extensions
  '(
    )
  "List of all extensions to load before the packages.")

(defvar frames-post-extensions
  '(
    ;; post extension framess go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function frames/init-<extension-frames>
;;
(defun frames/init-popframe ()
  "Initialize popframe"
  ; (popframe-mode 1)
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
