;;; Commands with the leader (<SPC> for Spacemacs)
;; Quick-open useful files with <SPC> o
(evil-leader/set-key
  "ot" 'my-open-todos
  "oc" 'my-open-config
  "oi" 'my-open-init)

;;; Misc things I'm just used to
(define-key evil-normal-state-map "U" 'undo-tree-redo)

;;; Mode-specific bindings
;; Spacemacs-ify eval-last-sexp
(evil-leader/set-key-for-mode 'emacs-lisp-mode "xe" 'eval-last-sexp)
