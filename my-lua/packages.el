;;; packages.el --- my-lua Layer packages File for Spacemacs
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

(defvar my-lua-packages
  '(
    ;; package my-luas go here
    lua-mode
    company
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-lua-excluded-packages '()
  "List of packages to exclude.")

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
