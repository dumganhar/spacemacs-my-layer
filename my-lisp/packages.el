
(setq my-lisp-packages
      '(
        ;; package my-lisps go here
        geiser
        lispy))

;; For each package, define a function my-lisp/init-<package-my-lisp>
(defun my-lisp/init-geiser ()
  "init geiser, in Mac, you should use `brew install guile`"
  (use-package geiser
    :defer t
    :config
    (setq geiser-active-implementations '(guile))
    (evil-leader/set-key-for-mode 'scheme-mode
      "mhd" 'geiser-doc-symbol-at-point
      "mgg" 'geiser-edit-symbol-at-point)
    ))

;;
(defun my-lisp/init-lispy()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn 
      (add-hook 'emacs-lisp-mode-hook (lambda ()(lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
