(setq my-web-packages
      '(
        ;; package my-webs go here
        impatient-mode
        moz-controller))

;; For each package, define a function my-web/init-<package-my-web>
;;
(defun my-web/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

    (defun my-web-mode-hook()
      "my web mode hook for HTML REPL"
      (interactive)
      (impatient-mode)
      (httpd-start))

    (add-hook 'web-mode-hook 'my-web-mode-hook)
    ))
  )

(defun my-web/init-moz-controller ()
  (use-package moz-controller
    :init
    (moz-controller-global-mode t)
    :diminish moz-controller-mode))

