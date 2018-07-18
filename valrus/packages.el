; List of all packages to install and/or initialized. Built-in packages
; which require an initialization must be listed explicitly in the list.
(setq valrus-packages
  '(
    company
    ;; deadgrep
    elm-mode
    enh-ruby-mode
    evil-escape
    fill-column-indicator
    flycheck
    linum-relative
    ;; helm
    linum-relative
    markdown-mode
    ;; neotree
    org-mode
    persp-mode
    ;; rainbow-delimiters
    spaceline
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
    ;; Disable in favor of linum-relative
    (linum :excluded t)
    ))

(defun valrus/post-init-company ()
  (define-key evil-insert-state-map (kbd "<S-tab>") 'company-complete)
  (setq company-idle-delay nil))

;; Deadgrep doesn't play nice with... something
;; (defun valrus/init-deadgrep ()
;;   (use-package deadgrep
;;     :config
;;     (progn
;;       (spacemacs/set-leader-keys "/" #'deadgrep))))

(defun valrus/pre-init-enh-ruby-mode ()
  (spacemacs|use-package-add-hook enh-ruby-mode
    :post-config
    (progn
      (modify-syntax-entry ?_ "w" enh-ruby-mode-syntax-table)
      (modify-syntax-entry ?: "_" enh-ruby-mode-syntax-table))))

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
  (setq clean-buffer-list-delay-general 1)
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

(defun valrus/post-init-spaceline ()
  (setq powerline-default-separator nil)
  (spaceline-compile))

(defun valrus/post-init-org-mode ()
  (add-hook 'org-mode-hook 'valrus/org-settings))

(defun valrus/post-init-flycheck ()
  (setq-default flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list))

(defun valrus/disable-rainbow-delimiter-overline ()
  ;; Turn off overlines; they mess up line spacing
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :overline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :overline nil))

(defun valrus/rainbow-delimiters-fonts (orig-fun &rest args)
  (valrus/disable-rainbow-delimiter-overline)
  (apply orig-fun args))

(defun valrus/post-init-rainbow-delimiters ()
  (when (configuration-layer/layer-usedp 'theming)
    (advice-add 'load-theme :after #'valrus/rainbow-delimiters-fonts))
  (valrus/disable-rainbow-delimiter-overline))

(defun valrus/post-init-markdown-mode ()
  ;; why the fuck aren't these face names defined here??
  ;; (set-face-attribute 'markdown-inline-code-face nil
  ;;                     :font "Iosevka"
  ;;                     :weight 'regular
  ;;                     :height 100)
  ;; (set-face-attribute 'markdown-pre-face nil
  ;;                     :font "Iosevka"
  ;;                     :weight 'regular
  ;;                     :height 100)
  (add-hook 'markdown-mode-hook 'spacemacs/toggle-auto-completion-off))

(defun valrus/post-init-spaceline ()
  (setq spaceline-window-numbers-unicode nil))

;; (defun valrus/post-init-neotree ()
;;   (setq neo-theme 'nerd))

(defun valrus/post-init-elm-mode ()
  (setq elm-indent-offset 4))

(defun valrus/post-init-spaceline ()
  (let ((modeline-font "Iosevka Term Slab"))
                                        ; File name and navigation percentage
    (set-face-attribute 'mode-line nil
                        :font modeline-font
                        :weight 'light)
    (set-face-attribute 'mode-line-inactive nil
                        :font modeline-font
                        :weight 'light)
                                        ; Other modeline faces
    (set-face-attribute 'powerline-active1 nil
                        :font modeline-font
                        :weight 'ultra-light)
    (set-face-attribute 'powerline-active2 nil
                        :font modeline-font
                        :weight 'ultra-light)
    (set-face-attribute 'powerline-inactive1 nil
                        :font modeline-font
                        :weight 'light)
    (set-face-attribute 'powerline-inactive2 nil
                        :font modeline-font
                        :weight 'light)
    )
  (setq powerline-default-separator nil)
  (spaceline-compile))

(defun valrus/post-init-yaml-mode ()
  (setq yaml-indent-offset 4))
