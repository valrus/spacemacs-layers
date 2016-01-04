;;; Commands with the leader (<SPC> for Spacemacs)
;; Quick-open useful files with <SPC> o
(spacemacs/set-leader-keys
  "ot" 'my-open-todos
  "oc" 'my-open-config
  "oi" 'my-open-init)

;;; Misc things I'm just used to
(define-key evil-normal-state-map "U" 'undo-tree-redo)

;;; Mode-specific bindings

;;; http://metasandwich.com/2013/01/19/having-my-vim-and-m-x-emacs-ing-it-too/
;; lw for "little words" that stop at underscores or capital letters
(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))

(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

;; X to delete without clipboarding
(evil-define-operator evil-destroy (beg end type register yank-handler)
  (evil-delete beg end type ?_ yank-handler))

(define-key evil-normal-state-map "X" 'evil-destroy)

;; R to replace without squashing the clipboard with replaced text
(evil-define-operator evil-destroy-replace (beg end type register yank-handler)
  (evil-destroy beg end type register yank-handler)
  (evil-paste-before 1 register))

(define-key evil-normal-state-map "R" 'evil-destroy-replace)

;; Don't put all-whitespace regions into the clipboard
;; This causes a max recursion error :(
;; (defun whitespace-only-p (string)
;;   (equal "" (replace-regexp-in-string "[ \t\n]" "" string)))

;; (defadvice evil-delete (around evil-delete-yank activate)
;;   (if (whitespace-only-p (buffer-substring beg end))
;;       (evil-destroy beg end type register yank-handler)
;;     ad-do-it))
