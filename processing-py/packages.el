;;; packages.el --- processing-py layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Ian McCowan <valrus@iMac.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `processing-py-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `processing-py/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `processing-py/pre-init-PACKAGE' and/or
;;   `processing-py/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst processing-py-packages
  '(
    (processing-py-mode :location local)
    flycheck
    )
  "The list of Lisp packages required by the processing-py layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun processing-py/init-processing-py-mode ()
  (use-package processing-py-mode
    :mode "\\.pyde"))

(defun processing-py/processing-py-mode-settings ()
  (setq flycheck-pylintrc (concat (configuration-layer/get-layer-path 'processing-py) "pylintrc")))

(defun processing-py/post-init-processing-py-mode ()
  (add-hook 'processing-py-mode-hook 'processing-py/processing-py-mode-settings))

(defun processing-py/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'processing-py-mode-hook))


;;; packages.el ends here
