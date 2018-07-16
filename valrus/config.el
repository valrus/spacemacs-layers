;;; UI stuff
; Don't allow cursor to leave minibuffer when helm is up, it breaks everything
(setq-default helm-prevent-escaping-from-minibuffer t)

;; Load changes from disk automatically
(global-auto-revert-mode 1)

;; wtf mac os x pasting
(fset 'evil-visual-update-x-selection 'ignore)

;; Center after searching
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-jumper/forward (after advice-for-evil-jumper/forward activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-jumper/backward (after advice-for-evil-jumper/backward activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; Don't let boxes mess with line heights
(defun valrus/fix-face-box (face)
  "Make sure all faces with a box have negative :line-width so they don't shift text around"
  (let ((box-attr (face-attribute face :box nil 'default)))
    (cond
     ((consp box-attr)
      (let ((result (copy-sequence box-attr)))
        (plist-put result :line-width (- (abs (or (plist-get box-attr :line-width) 1))))
        (set-face-attribute face nil :box result)))
     ((stringp box-attr)
      (set-face-attribute face nil :box '(:color box-attr :line-width -1)))
     (box-attr
      (set-face-attribute face nil :box '(:line-width -1))))))

(when (configuration-layer/layer-usedp 'theming)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :overline nil)
     (valrus/fix-face-box face))
   (face-list)))


;;; Relocate backup files that get strewn everywhere
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;;; Security https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; (let ((bad-hosts
;;        (loop for bad
;;              in `("https://wrong.host.badssl.com/"
;;                   "https://self-signed.badssl.com/")
;;              if (condition-case e
;;                     (url-retrieve
;;                      bad (lambda (retrieved) t))
;;                   (error nil))
;;              collect bad)))
;;   (if bad-hosts
;;       (error (format "tls misconfigured; retrieved %s ok"
;;                      bad-hosts))
;;     (url-retrieve "https://badssl.com"
;;                   (lambda (retrieved) t))))

;;; Mode-specific settings
;; GNU makefile
(defun my-gnumakefile-settings ()
  (whitespace-mode 1))

(add-hook 'makefile-gmake-mode-hook 'my-gnumakefile-settings t)

;; org-mode
(defun my-org-settings ())

(add-hook 'org-mode-hook 'my-org-settings)

;; markdown and ReST
(defun my-text-config ()
  (visual-line-mode t)
  (auto-complete-mode 0)
  (variable-pitch-mode t))

(add-hook 'markdown-mode-hook 'my-text-config)
(add-hook 'rst-mode-hook 'my-text-config)

;; python
(defun flycheck-python-set-executables ()
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq flycheck-python-pylint-executable (executable-find "pylint")
          flycheck-python-flake8-executable (executable-find "flake8")))
  ;; Force Flycheck mode on
  (flycheck-mode))

(defun flycheck-python-setup ()
  (add-hook 'hack-local-variables-hook #'flycheck-python-set-executables
            nil 'local))

(defun my-python-config ()
  (flycheck-python-setup)
  (smartparens-mode 0))

(add-hook 'python-mode-hook 'my-python-config)
;; include underscores in word motions
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; C
(defun my-c-settings ()
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'my-c-settings)

;; give me my em-dashes back
(setq mac-right-option-modifier nil)
