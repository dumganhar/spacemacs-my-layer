;;; packages.el --- zilongshanren Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq zilongshanren-packages
    '(
      ;; package names go here
      lispy
      lua-mode
      company
      discover-my-major
      ws-butler
      rtags
      cmake-font-lock
      ;; google-c-style
      cmake-mode
      company-c-headers
      flycheck
      helm-make
      helm-gtags
      ggtags
      ycmd
      markdown-mode
;;      org-octopress
      impatient-mode
      ;; moz-controller
      youdao-dictionary
      helm-github-stars
      swiper
      magit
      git-messenger
      helm-flyspell
      helm
      ace-window
      avy
      helm-ls-git
      mwe-log-commands
      keyfreq
      ))

;; List of packages to exclude.
(setq zilongshanren-excluded-packages '())

(defun zilongshanren/post-init-company-c-headers()
  (use-package company-c-headers
    :defer t
    :init(progn
           (setq company-c-headers-path-system
                 (quote
                  ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
           (setq company-c-headers-path-user
                 (quote
                  ("/Users/guanghui/cocos2d-x/cocos/platform" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/")))
           )
    ))

(defun zilongshanren/init-lispy()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn 
      (add-hook 'emacs-lisp-mode-hook (lambda ()(lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))

(defun zilongshanren/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (push 'company-dabbrev company-backends-lua-mode)
    (push 'company-etags company-backends-lua-mode)))


(defun zilongshanren/post-init-company ()
  (use-package company
    :defer t
    :config
    (setq company-minimum-prefix-length 1)
    (global-set-key (kbd "C-.") 'company-complete)
    (spacemacs|add-company-hook lua-mode)
    ))


(defun zilongshanren/init-ws-butler ()
    (use-package ws-butler
      :diminish ws-butler-mode
      :init
      (progn
       (add-hook 'c-mode-common-hook 'ws-butler-mode)
       (add-hook 'python-mode-hook 'ws-butler-mode)
       (add-hook 'cython-mode-hook 'ws-butler-mode) 
        )))

(defun zilongshanren/init-rtags ()
  (use-package rtags
    :init (require 'company-rtags)
    :config
    ))

(defun zilongshanren/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun zilongshanren/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun zilongshanren/post-init-cmake-mode ()
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


(defun zilongshanren/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (setq flycheck-display-errors-delay 0.2)))

(defun zilongshanren/init-helm-make ()
  (use-package helm-make
    :defer t))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t))

(defun zilongshanren/post-init-ycmd ()
  (setq ycmd-tag-files 'atuo))

;; configs for writing
(defun zilongshanren/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (defun zilongshanren/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name)

                       )
        (browse-url (format  "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "mp" 'zilongshanren/markdown-to-html
        )
      (evil-leader/set-key-for-mode 'markdown-mode
        "mp" 'zilongshanren/markdown-to-html
        )
      )))


(defun zilongshanren/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun zilongshanren-mode-hook()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (httpd-start))

      (add-hook 'web-mode-hook 'zilongshanren-mode-hook)
      ))
  )

;; (defun zilongshanren/init-moz-controller ()
;;   (use-package moz-controller
;;     :init
;;     (moz-controller-global-mode t)
;;     :diminish moz-controller-mode))

(defun zilongshanren/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "dumganhar")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache")
      )))

(defun zilongshanren/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t)

      ;; Set file path for saving search history
      (setq youdao-dictionary-search-history-file "~/.emacs.d/.cache/.youdao")

      ;; Enable Chinese word segmentation support (支持中文分词)
      (setq youdao-dictionary-use-chinese-word-segmentation t)
      )))


(defun zilongshanren/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun zilongshanren/post-init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init
    (global-set-key (kbd "C-c s") 'helm-flyspell-correct)
    ))

(defun zilongshanren/post-init-helm ()
  (use-package helm
    :init
      (setq helm-completing-read-handlers-alist
            '((describe-function . ido)
              (describe-variable . ido)
              (debug-on-entry . helm-completing-read-symbols)
              (find-function . helm-completing-read-symbols)
              (find-tag . helm-completing-read-with-cands-in-buffer)
              (ffap-alternate-file . nil)
              (tmm-menubar . nil)
              (dired-do-copy . nil)
              (dired-do-rename . nil)
              (dired-create-directory . nil)
              (find-file . ido)
              (copy-file-and-rename-buffer . nil)
              (rename-file-and-buffer . nil)
              (w3m-goto-url . nil)
              (ido-find-file . nil)
              (ido-edit-input . nil)
              (mml-attach-file . ido)
              (read-file-name . nil)
              (yas/compile-directory . ido)
              (execute-extended-command . ido)
              (minibuffer-completion-help . nil)
              (minibuffer-complete . nil)
              (c-set-offset . nil)
              (wg-load . ido)
              (rgrep . nil)
              (read-directory-name . ido)
              ))
      ))

(defun zilongshanren/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :config
    (progn
      (setq helm-ls-git-show-abs-or-relative 'relative)
      )))


