(defun my-default-fonts ()
  (set-face-attribute 'default nil
                      :font "Iosevka"
                      :weight 'light
                      :height 100)
  (set-face-attribute 'font-lock-comment-face nil
                      :font "Iosevka"
                      :weight 'ultra-light
                      :height 100)
  (set-face-attribute 'variable-pitch nil
                      :font "Inconsolata Nerd Font"
                      :weight 'light
                      :height 100))

(my-default-fonts)

;; kill ugly line wrap things
(spacemacs/toggle-visual-line-navigation-off)
(spacemacs/toggle-mode-line-minor-modes-off)
