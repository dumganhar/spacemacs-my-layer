;;; packages.el --- my-c-c++ Layer packages File for Spacemacs
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

(defvar my-c-c++-packages
  '(
    ;; package my-c-c++s go here
    company-c-headers
    company
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-c-c++-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function my-c-c++/init-<package-my-c-c++>
;;
;; (defun my-c-c++/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun my-c-c++/post-init-company-c-headers()
  (use-package company-c-headers
    :defer t
    :init(progn
           (setq company-c-headers-path-system
    (quote
     ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
  (setq company-c-headers-path-user
    (quote
     ("/Users/guanghui/cocos2d-x/cocos/platform" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/")))
  )))

(defun my-c-c++/post-init-company()
  (use-package company
    :defer t
    :config
    (global-set-key (kbd "C-.") 'company-complete)
    (setq company-idle-delay 0.08)
    (setq company-minimum-prefix-length 1)
    ))
