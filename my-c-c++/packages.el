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
    ws-butler
    rtags
    cmake-font-lock
    google-c-style
    cmake-mode
    irony
    company-irony
    flycheck-irony
    flycheck
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
    (setq company-backends (delete 'company-semantic company-backends))
    (setq company-backends (delete 'company-clang company-backends))
    ))

(defun my-c-c++/init-ws-butler ()
    (use-package ws-butler
      :init
      (progn
       (add-hook 'c-mode-common-hook 'ws-butler-mode)
       (add-hook 'python-mode-hook 'ws-butler-mode)
       (add-hook 'cython-mode-hook 'ws-butler-mode) 
        )))

(defun my-c-c++/init-rtags ()
  (use-package rtags
    :defer t
    :init (require 'company-rtags)))

(defun my-c-c++/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun my-c-c++/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun my-c-c++/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :config
    (progn
      (defun cmake-rename-buffer ()
        "Renames a CMakeLists.txt buffer to cmake-<directory name>."
        (interactive)
        (when (and (buffer-file-name)
                   (string-match "CMakeLists.txt" (buffer-name)))
          (setq parent-dir (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory (buffer-file-name)))))
          (setq new-buffer-name (concat "cmake-" parent-dir))
          (rename-buffer new-buffer-name t)))

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer))
      )))

(defun my-c-c++/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)

      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async)
        (add-to-list 'company-backends 'company-irony))

      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      ;; it is not fast and accurate
      ;; (add-hook 'irony-mode-hook 'irony-eldoc)

      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

      )))

(defun my-c-c++/init-company-irony ()
  (use-package company-irony
    :defer t))

(defun my-c-c++/init-flycheck-irony ()
  (use-package flycheck-irony
    :defer t))

(defun my-c-c++/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))
