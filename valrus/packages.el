(defvar valrus-excluded-packages
  '(
    ; Languages I don't use
    coffee-mode
    csharp-mode
    ; (scala)
    ensime
    less-css-mode
    powershell
    powershell-mode
    ; (also scala)
    sbt-mode
    scala-mode2
    scss-mode

    ; Features I hate
    ac-ispell
    flyspell
    ispell

    ; UI things I hate
    vi-tilde-fringe

    ; I use either solarized or spacemacs theme
    monokai-theme
    zenburn-theme
    )
  "List of packages to exclude. ")

(defvar valrus-packages
  '(
    fill-column-indicator
    org
    rainbow-delimiters
    yasnippet
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun valrus/post-init-fill-column-indicator ()
  (turn-on-fci-mode))

(defun valrus/post-init-yasnippet ()
  (setq yas-snippet-dirs "~/.emacs.d/private/snippets" yas-installed-snippets-dir))

(defun my-org-fonts ()
  "Better fonts for Org mode."
  (set-face-attribute 'org-level-1 nil
                      :weight 'normal
                      :height 1.0
                      :inherit 'header-line)
  (set-face-attribute 'org-level-2 nil
                      :weight 'normal
                      :height 1.0
                      :inherit 'header-line)
  (set-face-attribute 'org-level-3 nil
                      :weight 'normal
                      :height 1.0
                      :inherit 'header-line)
  (set-face-attribute 'org-tag nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :font "Pic0"
                      :inherit 'header-line
                      :height 120
                      :weight 'normal)
  )

(defun valrus/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
    (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))
    (my-org-fonts)))

(defun valrus/rainbow-delimiters-fonts ()
  ;; Turn off overlines; they mess up line spacing
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :overline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :overline nil))

(with-eval-after-load 'rainbow-delimiters 'valrus/rainbow-delimiters-fonts)
