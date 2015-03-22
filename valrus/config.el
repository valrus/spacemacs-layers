;;; UI stuff
;; Line numbers are real useful for Vim
(setq linum-format "%4d\u2502 ")
(global-linum-mode t)

; Turn off minor mode symbols
; (spacemacs/mode-line-minor-modes-toggle)

;; Load changes from disk automatically
(global-auto-revert-mode 1)

;;; Mode-specific settings
;; GNU makefile
(defun my-gnumakefile-settings ()
  (whitespace-mode 1))

(add-hook 'makefile-gmake-mode-hook 'my-gnumakefile-settings)

;; org-mode
(defun my-org-settings ()
  (linum-mode 0))

(add-hook 'org-mode-hook 'my-org-settings)

;; markdown
(defun my-markdown-config ()
  (visual-line-mode t)
  (auto-complete-mode 0)
  (variable-pitch-mode t))

(add-hook 'markdown-mode-hook 'my-markdown-config)

;; python
(defun flycheck-python-set-executables ()
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq flycheck-python-pylint-executable (executable-find "pylint")
          flycheck-python-flake8-executable (executable-find "flake8")))
  ;; Force Flycheck mode on
  (flycheck-mode))

(defun flycheck-python-setup ()
  (add-hook 'hack-local-variables-hook #'flycheck-python-set-executables
            nil 'local))

(defun my-python-config ()
  (flycheck-python-setup))

(add-hook 'python-mode-hook 'my-python-config)
