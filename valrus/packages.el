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

    ; solarized 4 lyfe
    monokai-theme
    zenburn-theme
    )
  "List of packages to exclude. ")

(defvar valrus-packages
  '(
    elm-mode
    fill-column-indicator
    yasnippet
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun valrus/init-fill-column-indicator ()
  (turn-on-fci-mode))

(defun valrus/init-elm-mode ()
  (setq exec-path (append exec-path '("c:/Program Files (x86)/Elm Platform/0.14.1/bin)"))))

(defun valrus/init-yasnippet ()
  (setq yas-snippet-dirs "~/.emacs.d/private/snippets" yas-installed-snippets-dir))
