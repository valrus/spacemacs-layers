(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4)
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(flycheck-display-errors-function (quote flycheck-display-error-messages))
 '(flycheck-python-pylint-executable "/Users/valrus/.pyenv/versions/kivy/bin/pylint")
 '(flycheck-standard-error-navigation nil)
 '(package-selected-packages
   (quote
    (yaml-mode intero hlint-refactor hindent parent-mode haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode wgrep smex ivy-hydra counsel-projectile counsel swiper ivy winum fuzzy anaconda-mode jedi jedi-core csv-mode auctex-latexmk company-auctex auctex web-mode tagedit slim-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data hide-comnt yapfify ws-butler window-numbering window+ which-key web-beautify volatile-highlights uuidgen use-package toc-org spacemacs-theme spaceline smeargle restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pytest pyenv-mode py-isort pungi popwin pip-requirements persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint kivy-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot glsl-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md frame-cmds flycheck-pos-tip flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elm-mode elisp-slime-nav dumb-jump define-word cython-mode company-tern company-statistics company-quickhelp company-anaconda column-enforce-mode color-identifiers-mode clean-aindent-mode autofit-frame auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "/Users/valrus/Code/tone_poem/bin/py /Users/valrus/Code/tone_poem/src/tonepoem.py -m inspector" projectile-compilation-cmd-map)
           (pyenv-mode-set "kivy"))
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "/Users/valrus/Code/tone_poem/bin/py /Users/valrus/Code/tone_poem/src/tonepoem.py -m inspector" projectile-compilation-cmd-map)
           (pyenv-mode-set
            (quote kivy)))
     (python-shell-virtualenv-path "/Users/valrus/.pyenv/kivy/bin")
     (python-shell-virtualenv-path "/Users/valrus/Code/tone_poem")
     (flycheck-mode . 1)
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "/Users/valrus/Code/tone_poem/bin/py /Users/valrus/Code/tone_poem/src/tonepoem.py -m inspector" projectile-compilation-cmd-map))
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "/Users/valrus/Code/tone_poem/bin/py /Users/valrus/Code/tone_poem/src/tonepoem.py" projectile-compilation-cmd-map))
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "/Users/valrus/.pyenv/versions/kivy2/bin/python src/tonepoem.py" projectile-compilation-cmd-map)))))
 '(solarized-high-contrast-mode-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
