;;; UI stuff
;; Line numbers are real useful for Vim
(global-linum-mode t)

; Don't allow cursor to leave minibuffer when helm is up, it breaks everything
(setq-default helm-prevent-escaping-from-minibuffer t)

;; Load changes from disk automatically
(global-auto-revert-mode 1)

;; Center after searching
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-jumper/forward (after advice-for-evil-jumper/forward activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-jumper/backward (after advice-for-evil-jumper/backward activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;;; Mode-specific settings
;; GNU makefile
(defun my-gnumakefile-settings ()
  (whitespace-mode 1))

(add-hook 'makefile-gmake-mode-hook 'my-gnumakefile-settings t)

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
  (flycheck-python-setup)
  (smartparens-mode 0))

(add-hook 'python-mode-hook 'my-python-config)
