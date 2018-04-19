; List of all packages to install and/or initialized. Built-in packages
; which require an initialization must be listed explicitly in the list.
(setq valrus-packages
  '(
    elm-mode
    evil-escape
    fill-column-indicator
    flycheck
    linum-relative
    ;; helm
    markdown-mode
    ;; neotree
    org-mode
    persp-mode
    ;; rainbow-delimiters
    theming
    yaml-mode

    ;;; Exclusions
    ;; Languages I don't use
    (coffee-mode :excluded t)
    (csharp-mode :excluded t)
    (ensime :excluded t)  ; (scala)
    (less-css-mode :excluded t)
    (powershell :excluded t)
    (powershell-mode :excluded t)
    (sbt-mode :excluded t)  ; (also scala)
    (scala-mode2 :excluded t)
    (scss-mode :excluded t)
    ;; Features I hate
    (ac-ispell :excluded t)
    (flyspell :excluded t)
    (ispell :excluded t)
    (smartparens :excluded t)
    ;; UI things I hate
    (vi-tilde-fringe :excluded t)
    ;; I use either solarized or spacemacs theme
    (monokai-theme :excluded t)
    (zenburn-theme :excluded t)
    ))

(defun valrus/post-init-auctex ()
  (cond
   ((string-equal system-type "darwin")
    (progn (setq TeX-view-program-selection '((output-pdf "Preview")))))
   ((string-equal system-type "gnu/linux")
    (progn
      (add-to-list 'TeX-view-program-list '("mupdf" "/usr/bin/mupdf %o"))
      (setq TeX-view-program-selection '((output-pdf "mupdf")))))))

(defun valrus/post-init-fill-column-indicator ()
  (turn-on-fci-mode))

(defun valrus/pre-init-linum-relative ()
  (setq linum-relative-backend 'display-line-numbers-mode))

(defun valrus/post-init-linum-relative ()
  (linum-relative-global-mode))

(defun valrus/post-init-yasnippet ()
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/private/snippets"))

(defun valrus/post-init-evil-escape ()
  (global-set-key [escape] 'evil-escape))

(defun valrus/post-init-persp-mode ()
  ;; Don't kill foreign buffers to avoid a bunch of warnings on clean-buffer-list
  (setq persp-kill-foreign-buffer-behaviour nil)
  (spacemacs|define-custom-layout "@conf"
    :binding "c"
    :body
    (find-file (concat (getenv "HOME") "/.spacemacs"))
    (split-window-right)
    (find-file (concat (getenv "HOME") "/.emacs.d/private/valrus/config.el"))))

(defun valrus/org-fonts ())

(defun valrus/pre-init-org-mode ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
    (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))
    (valrus/org-fonts)))

(defun valrus/org-settings ()
  (visual-line-mode t))

(defun valrus/post-init-org-mode ()
  (add-hook 'org-mode-hook 'valrus/org-settings))

(defun valrus/post-init-flycheck ()
  (setq-default flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list))

(defun valrus/rainbow-delimiters-fonts (orig-fun &rest args)
  ;; Turn off overlines; they mess up line spacing
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :overline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :overline nil)
  (apply orig-fun args))

(defun valrus/post-init-theming ()
  (when (configuration-layer/package-usedp 'rainbow-delimiters)
    (advice-add 'load-theme :after #'valrus/rainbow-delimiters-fonts))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :overline nil)
     (valrus/fix-face-box face))
   (face-list)))

(defun valrus/fix-face-box (face)
  (let ((box-attr (face-attribute face :box nil 'default)))
    (when (and (consp box-attr) (plist-member box-attr :line-width))
      (set-face-attribute face nil :box
                          (plist-put box-attr :line-width (- (abs (plist-get box-attr :line-width))))))))

(defun valrus/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook 'spacemacs/toggle-auto-completion-off))

;; (defun valrus/post-init-neotree ()
;;   (setq neo-theme 'nerd))

(defun valrus/post-init-elm-mode ()
  (setq elm-indent-offset 4))

(defun valrus/post-init-yaml-mode ()
  (setq yaml-indent-offset 4))
