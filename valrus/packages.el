(defvar valrus-excluded-packages
  '(
    ; Languages I don't use
    coffee-mode
    csharp-mode
    ensime
    less-css-mode
    powershell
    powershell-mode
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
    fill-column-indicator
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun valrus/init-fill-column-indicator ()
  (turn-on-fci-mode))


