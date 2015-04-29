;;; packages.el --- my-org Layer packages File for Spacemacs
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

(defvar my-org-packages
  '(
    ;; package my-orgs go here
    org-pomodoro
    ox-reveal
    worf
    org-download
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-org-excluded-packages '()
  "List of packages to exclude.")

(defun my-org/post-init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (setq pomodoro-break-time 2)
      (setq pomodoro-long-break-time 5)
      (setq pomodoro-work-time 15)
      ;; (setq-default mode-line-format
      ;;               (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
      ;;                     mode-line-format))
      )))

(defun my-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js")
      )))

(defun my-org/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

(defun my-org/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))
