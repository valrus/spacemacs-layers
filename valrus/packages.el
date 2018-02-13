; List of all packages to install and/or initialized. Built-in packages
; which require an initialization must be listed explicitly in the list.
(setq valrus-packages
  '(
    atomic-chrome
    elm-mode
    evil-escape
    fill-column-indicator
    flycheck
    ;; helm
    markdown-mode
    ;; neotree
    org-mode
    persp-mode
    spaceline
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

(defun valrus/init-mediawiki ()
  "Initialize mediawiki mode."
  (use-package mediawiki))

(defun valrus/init-atomic-chrome ()
  "Initialize atomic-chrome."
  (use-package atomic-chrome
    :config
    (atomic-chrome-start-server)
    (setq atomic-chrome-url-major-mode-alist
          '(("jira.com" . mediawiki-mode)))))

(defun valrus/post-init-fill-column-indicator ()
  (turn-on-fci-mode))

(defun valrus/post-init-yasnippet ()
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/private/snippets"))

(defun valrus/post-init-evil-escape ()
  (global-set-key [escape] 'evil-escape))

(defun valrus/post-init-persp-mode ()
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

(defun org-settings ()
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
     (set-face-attribute face nil :overline nil))
   (face-list)))

(defun valrus/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook 'spacemacs/toggle-auto-completion-off))

;; (defun valrus/post-init-neotree ()
;;   (setq neo-theme 'nerd))

(defun valrus/post-init-elm-mode ()
  (setq elm-indent-offset 4))

(defun valrus/post-init-spaceline ()
  (let ((modeline-font "Iosevka Slab")
        (modeline-height 120))
                                        ; File name and navigation percentage
    (set-face-attribute 'mode-line nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
    (set-face-attribute 'mode-line-inactive nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
                                        ; Other modeline faces
    (set-face-attribute 'powerline-active1 nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
    (set-face-attribute 'powerline-active2 nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
    (set-face-attribute 'powerline-inactive1 nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
    (set-face-attribute 'powerline-inactive2 nil
                        :font modeline-font
                        :height modeline-height
                        :weight 'ultra-light)
    )
  (setq powerline-default-separator nil)
  (spaceline-compile))

(defun valrus/post-init-yaml-mode ()
  (setq yaml-indent-offset 4))
