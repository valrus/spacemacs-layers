;;; packages.el --- tablature layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `tablature-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `tablature/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `tablature/pre-init-PACKAGE' and/or
;;   `tablature/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst tablature-packages
  '(
    (tablature-mode :location local)
    )
  "The list of Lisp packages required by the tablature layer.

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

(defun tablature/init-tablature-mode ()
  (use-package tablature-mode
    :config
    (progn
      (tab-make-mode-map)
      (evilified-state-evilify-map tab-mode-map
        :mode tab-mode
        :bindings
        ;; add hjkl navigation
        ; won't work with tab-mode because "h" is the 6th string
        ; Movement keys on uppercase instead
        "h" 'tab-e2
        "H" 'tab-backward-char
        "L" 'tab-forward-char
        "J" 'tab-down-staff
        "K" 'tab-up-staff
        ;; add scrolling feature on C-f, C-b, C-d and C-u
        ;; G and gg to go to the end and beginning of the buffer
        ;; add incremental search with /, n and N
        ; all of these already have meanings in tab-mode
        ; and also incremental searching has dubious usefulness in tablature
        "n"	'tab-e3
        "N"	'tab-e4
        "/" 'tab-slide-up
        ;; enabling evil-ex on :
        ;; add visual state and visual line state on v and V
        ; also used by tab-mode, and visual mode has dubious usefulness in tab
        "v"	'tab-G3
        "V"	'tab-G4
        ;; add yank on y in visual state only
        ;; activate evil-leader key on SPC
        ))))


;;; packages.el ends here
