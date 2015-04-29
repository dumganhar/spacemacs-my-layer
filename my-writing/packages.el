;;; packages.el --- my-writing Layer packages File for Spacemacs
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

(defvar my-writing-packages
  '(
    ;; package my-writings go here
    markdown-mode
    org-octopress
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-writing-excluded-packages '()
  "List of packages to exclude.")

(defun my-writing/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :mode ("\\.md\\'" . gfm-mode)
    :config
    (progn
      (defun zilongshanren/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name)

                       )
        (browse-url (format  "http://localhost:5000/%s.md" (file-name-base))))

      (define-key gfm-mode-map (kbd "s-h") 'zilongshanren/markdown-to-html) 
      )))

(defun my-writing/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (setq org-octopress-directory-top       "~/myblog/octopress/source")
    (setq org-octopress-directory-posts     "~/myblog/octopress/source/_posts")
    (setq org-octopress-directory-org-top   "~/myblog/octopress/source")
    (setq org-octopress-directory-org-posts "~/myblog/octopress/source/blog")
    (setq org-octopress-setup-file          "~/myblog/octopress/setupfile.org")
      )))
