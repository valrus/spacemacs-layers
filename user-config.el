(defun my-local-fonts ()
  (set-face-attribute 'default nil
                      :font "Iosevka"
                      :weight 'light
                      :height 120)
  (set-face-attribute 'font-lock-comment-face nil
                      :font "Iosevka"
                      :weight 'ultra-light
                      :height 120)
  (set-face-attribute 'line-number nil
                      :font "Inconsolata"
                      :weight 'light
                      :height 100)
  (set-face-attribute 'line-number-current-line nil
                      :font "Inconsolata"
                      :weight 'bold
                      :height 100)
  ;; Add a little extra line spacing so over/underlines don't mess things up
  (setq-default line-spacing 0))

(my-local-fonts)
