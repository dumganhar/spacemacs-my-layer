;;; packages.el --- my-web Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar my-web-packages
  '(
    ;; package my-webs go here
    impatient-mode
    moz-controller
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-web-excluded-packages '()
  "List of packages to exclude.")

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

