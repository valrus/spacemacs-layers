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


(defun tablature/init-tablature-mode ()
  (use-package tablature-mode
    :mode "\\.tab"

    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'tab-mode
        "ca" 'tab-analyze-chord
        "cp" 'tab-label-chord
        )

      (dolist (x '(
                   ("mc" . "chord")
                   ))
        (spacemacs/declare-prefix-for-mode 'tab-mode (car x) (cdr x)))
      )

    :config
    (progn
      ; don't wait for tab-mode activation to create the mode-map
      (tab-make-mode-map)

      ; import tablature-mode maps suitable for normal mode
      (cl-loop for (key . action) in tab-normal-mode-map-alist
            do (evil-define-key 'normal tab-mode-map key action))

      ; Insert mode bindings
      (evil-define-key 'insert tab-mode-map (kbd "<left>") 'tab-backward-char)
      (evil-define-key 'insert tab-mode-map (kbd "<right>") 'tab-forward-char)
      (evil-define-key 'insert tab-mode-map (kbd "<down>") 'evil-next-line)
      (evil-define-key 'insert tab-mode-map (kbd "<up>") 'evil-previous-line)

      (evil-define-key 'insert tab-mode-map " " 'tab-forward)

      ; Normal mode bindings
      (evil-define-key 'normal tab-mode-map "h" 'tab-backward-char)
      (evil-define-key 'normal tab-mode-map "l" 'tab-forward-char)
      (evil-define-key 'normal tab-mode-map "j" 'tab-down-string)
      (evil-define-key 'normal tab-mode-map "k" 'tab-up-string)

      (evil-define-key 'normal tab-mode-map "H" 'tab-backward-char)
      (evil-define-key 'normal tab-mode-map "L" 'tab-forward-char)
      (evil-define-key 'normal tab-mode-map "J" 'tab-down-staff)
      (evil-define-key 'normal tab-mode-map "K" 'tab-up-staff)

      (evil-define-key 'normal tab-mode-map "o" 'tab-make-staff)

      (evil-define-key 'normal tab-mode-map "w" 'tab-forward-barline)
      (evil-define-key 'normal tab-mode-map "b" 'tab-backward-barline)

      (evil-define-key 'normal tab-mode-map "{" 'tab-up-staff)
      (evil-define-key 'normal tab-mode-map "}" 'tab-down-staff)

      (evil-define-key 'normal tab-mode-map (kbd "<S-left>") 'tab-backward-char)
      (evil-define-key 'normal tab-mode-map (kbd "<S-right>") 'tab-forward-char)
      (evil-define-key 'normal tab-mode-map (kbd "<S-down>") 'tab-down-staff)
      (evil-define-key 'normal tab-mode-map (kbd "<S-up>") 'tab-up-staff)

      (evil-define-key 'normal tab-mode-map "x" 'tab-delete-current-note)
      (evil-define-key 'normal tab-mode-map "X" 'tab-delete-chord-forward)

      ; Visual mode bindings
      (evil-define-key 'visual tab-mode-map "+" 'tab-transpose)

      ;; TODO: C-h	delete previous (lead-mode) or current (chord-mode) note
      ;; TODO: C-?	delete previous note/chord
      )
    ))


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


(defun tablature/post-init-spaceline ()
  ; not very useful for tablature mode
  (spaceline-toggle-line-column-off)
  (spaceline-define-segment tablature
    (when (equal major-mode 'tab-mode)
      (list
       (if lead-mode "Lead" "Chord")
       tab-position-as-string
       ))
    :separator ":")
  (spaceline-toggle-base-fret-segment-on)
  (spaceline-spacemacs-theme 'tablature))


(defun tablature/tab-mode-line ()
  (when (not (configuration-layer/package-usedp 'spaceline))
    (tablature/setup-normal-mode-line)))


(defun tablature/tab-mode-settings ()
  (tablature/tab-mode-line)
  (chord-mode)
  (setq-local evil-insert-state-cursor '("chartreuse3" box))
  (setq-local evil-move-cursor-back nil))


(defun tablature/post-init-tablature-mode ()
  (add-hook 'tab-mode-hook 'tablature/tab-mode-settings))


;;; packages.el ends here
