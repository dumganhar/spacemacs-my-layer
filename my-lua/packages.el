(setq my-lua-packages
      '(
        ;; package my-luas go here
        lua-mode
        company))

(defun my-lua/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (push 'company-dabbrev company-backends-lua-mode)
    (push 'company-etags company-backends-lua-mode)))


(defun my-lua/post-init-company ()
  (use-package company
    :defer t
    :config
    (spacemacs|add-company-hook lua-mode)
    ))
