; List of all packages to install and/or initialized. Built-in packages
; which require an initialization must be listed explicitly in the list.
(setq valrus-packages
  '(
    elm-mode
    fill-column-indicator
    flycheck
    helm
    markdown-mode
    neotree
    persp-mode
    ; rainbow-delimiters
    ; theming
    yasnippet

    ;; Exclusions
    ; Languages I don't use
    (coffee-mode :excluded t)
    (csharp-mode :excluded t)
    (ensime :excluded t)  ; (scala)
    (less-css-mode :excluded t)
    (powershell :excluded t)
    (powershell-mode :excluded t)
    (sbt-mode :excluded t)  ; (also scala)
    (scala-mode2 :excluded t)
    (scss-mode :excluded t)
                                        ; Features I hate
    (ac-ispell :excluded t)
    (flyspell :excluded t)
    (ispell :excluded t)
    (smartparens :excluded t)
                                        ; UI things I hate
    (vi-tilde-fringe :excluded t)
                                        ; I use either solarized or spacemacs theme
    (monokai-theme :excluded t)
    (zenburn-theme :excluded t)
    ))

(defun valrus/post-init-fill-column-indicator ()
  (turn-on-fci-mode))

(defun valrus/post-init-yasnippet ()
  (setq yas-snippet-dirs "~/.emacs.d/private/snippets" yas-installed-snippets-dir))

(defun valrus/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@conf"
    :binding "c"
    :body
    (find-file (concat (getenv "HOME") "/.spacemacs"))
    (split-window-right)
    (find-file (concat (getenv "HOME") "/.emacs.d/private/valrus/config.el"))
    ))

(defun valrus/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
    (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))
    (my-org-fonts)))

(defun valrus/post-init-flycheck ()
  (setq-default flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list))

(defun valrus/rainbow-delimiters-fonts (orig-fun &rest args)
  ;; Turn off overlines; they mess up line spacing
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :overline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :overline nil)
  (apply orig-fun args))

(advice-add 'load-theme :after #'valrus/rainbow-delimiters-fonts)

(defun valrus/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook 'spacemacs/toggle-auto-completion-off))

(defun valrus/post-init-neotree ()
  (setq neo-theme 'nerd))

(defun valrus/post-init-elm-mode ()
  (setq elm-indent-offset 4))

(defun valrus/post-init-helm ()
  ; CVS backup files start with .#
  (add-to-list 'helm-boring-file-regexp-list "\\.\\#")
  (delete "CVS$" helm-boring-file-regexp-list)
  (setq-default helm-ff-skip-boring-files t))
