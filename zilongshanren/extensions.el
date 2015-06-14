;;; extensions.el --- zilongshanren Layer extensions File for Spacemacs
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

(setq zilongshanren-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq zilongshanren-post-extensions
      '(
        ;; post extension names go here
        doxymacs
        eim
        ))

;; For each extension, define a function zilongshanren/init-<extension-name>
;;
(defun zilongshanren/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (progn
      ;; (defun my-doxymacs-font-lock-hook ()
      ;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      ;;       (doxymacs-font-lock)))
      ;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (add-hook 'c-mode-common-hook 'doxymacs-mode)
      ))
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun zilongshanren/init-eim ()
    "Initialize eim"
  (use-package eim
    :init
    (progn
      (autoload 'eim-use-package "eim" "Another emacs input method")
      ;; Tooltip 暂时还不好用
      (setq eim-use-tooltip nil)

      (register-input-method
       "eim-wb" "euc-cn" 'eim-use-package
       "五笔" "汉字五笔输入法" "wb.txt")
      (register-input-method
       "eim-py" "euc-cn" 'eim-use-package
       "拼音" "汉字拼音输入法" "py.txt")
      ;; 用 ; 暂时输入英文
      (require 'eim-extra)
      (global-set-key ";" 'eim-insert-ascii)
      )))
