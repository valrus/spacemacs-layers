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
    spaceline
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


(defun tablature/post-init-spaceline ()
  (spaceline-define-segment base-fret-segment
    (format "Fret: %s" tab-position-as-string)
    :when (equal major-mode 'tab-mode))
  (add-to-list 'spaceline-right 'base-fret-segment))


(defun tablature/init-tablature-mode ()
  (use-package tablature-mode
    :config
    (progn
      ; don't wait for tab-mode activation to create the mode-map
      (tab-make-mode-map)

      ; import tablature-mode maps suitable for normal mode
      (cl-loop for (key . action) in tab-normal-mode-map-alist
            do (evil-define-key 'normal tab-mode-map key action))

      (evil-define-key 'normal tab-mode-map "h" 'tab-backward-char)
      (evil-define-key 'normal tab-mode-map "l" 'tab-forward-char)
      (evil-define-key 'normal tab-mode-map "j" 'tab-down-staff)
      (evil-define-key 'normal tab-mode-map "k" 'tab-up-staff)

      (evil-define-key 'normal tab-mode-map "H" 'evil-backward-char)
      (evil-define-key 'normal tab-mode-map "L" 'evil-forward-char)
      (evil-define-key 'normal tab-mode-map "J" 'evil-next-line)
      (evil-define-key 'normal tab-mode-map "K" 'evil-previous-line)

      ;; TODO: C-h	delete previous (lead-mode) or current (chord-mode) note
      ;; TODO: C-?	delete previous note/chord
      )))


(defun tablature/setup-normal-mode-line ()
  (setq mode-line-format (list ""
  			     'mode-line-modified
  			     'mode-line-buffer-identification
  			     "   "
  			     'global-mode-string
  			     "   %[("
  			     'mode-name
  			     'minor-mode-alist
  			     "--"
  			     'tab-position-as-string
  			     'tab-pending-embellishment
  			     "%n"
  			     'mode-line-process
  			     ")%]----"
  			     '(line-number-mode "L%l--")
  			     '(-3 . "%p")
  			     "-%-")))


(defun tablature/setup-spaceline ()
  ; not very useful for tablature mode
  (spaceline-toggle-line-column-off))


(defun tablature/tab-mode-line ()
  (if (not (configuration-layer/layer-usedp 'spaceline))
    (tablature/setup-normal-mode-line)))


(defun tablature/tab-mode-settings ()
  (tablature/tab-mode-line)
  (chord-mode)
  (setq evil-insert-state-cursor '("chartreuse3" box)))


(defun tablature/post-init-tablature-mode ()
  (add-hook 'tab-mode-hook 'tablature/tab-mode-settings))


;;; packages.el ends here
