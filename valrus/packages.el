(defvar valrus-excluded-packages
  '(
    ; Languages I don't use
    coffee-mode
    csharp-mode
    ensime  ; (scala)
    less-css-mode
    powershell
    powershell-mode
    sbt-mode  ; (also scala)
    scala-mode2
    scss-mode

    ; Features I hate
    ac-ispell
    flyspell
    ispell
    smartparens

    ; UI things I hate
    vi-tilde-fringe

    ; I use either solarized or spacemacs theme
    monokai-theme
    zenburn-theme
    )
  "List of packages to exclude.")

; List of all packages to install and/or initialized. Built-in packages
; which require an initialization must be listed explicitly in the list.
(setq valrus-packages
  '(
    fill-column-indicator
    flycheck
    persp-mode
    rainbow-delimiters
    yasnippet

    (coffee-mode :excluded t)
    (csharp-mode :excluded t)
    (ensime :excluded t)
    (less-css-mode :excluded t)
    (powershell :excluded t)
    (powershell-mode :excluded t)
    (sbt-mode :excluded t)
    (scala-mode2 :excluded t)
    (scss-mode :excluded t)
    (ac-ispell :excluded t)
    (flyspell :excluded t)
    (ispell :excluded t)
    (smartparens :excluded t)
    (vi-tilde-fringe :excluded t)
    (monokai-theme :excluded t)
    (zenburn-theme :excluded t)
    )
  )

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

(defun valrus/rainbow-delimiters-fonts ()
  ;; Turn off overlines; they mess up line spacing
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :overline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :overline nil))

(advice-add 'load-theme :after 'valrus/rainbow-delimiters-fonts)
