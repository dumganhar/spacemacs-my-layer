;;; packages.el --- my-lisp Layer packages File for Spacemacs
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

(defvar my-lisp-packages
  '(
    ;; package my-lisps go here
    lispy
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-lisp-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function my-lisp/init-<package-my-lisp>
;;
(defun my-lisp/init-lispy()
  "Initialize lispy"
  (use-package lispy
    :diminish (lispy-mode)
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda ()(lispy-mode 1)))
    (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
    ))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
