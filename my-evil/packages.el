;;; packages.el --- my-evil Layer packages File for Spacemacs
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

(defvar my-evil-packages
  '(
    ;; package my-evils go here
    evil
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-evil-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function my-evil/init-<package-my-evil>
;;
(defun my-evil/post-init-evil ()
  "Initialize evil"
  (use-package evil
    :defer t
    :config
    (progn
      ;;mimic "nzz" behaviou in vim
      (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
        (evil-scroll-line-to-center (line-number-at-pos)))


      (define-key evil-insert-state-map "\C-e" 'end-of-line)
      (define-key evil-insert-state-map "\C-n" 'next-line)
      (define-key evil-insert-state-map "\C-p" 'previous-line)
      (define-key evil-insert-state-map "\C-k" 'kill-line)
      (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
      (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

      (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
      (define-key evil-ex-completion-map "\C-b" 'backward-char)
      (define-key evil-ex-completion-map "\C-k" 'kill-line)
      (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

      
      (evil-add-hjkl-bindings ibuffer-mode-map 'emacs)
      (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)
      (evil-add-hjkl-bindings dired-mode-map 'emacs)
      (evil-add-hjkl-bindings help-mode-map 'emacs)
      (evil-add-hjkl-bindings elfeed-search-mode-map 'emacs)
      (evil-add-hjkl-bindings elfeed-show-mode-map 'emacs)


      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
      (evil-add-hjkl-bindings Info-mode-map 'emacs)

      ;; Evil Normal state map
;; key map for magit mode
;;
;; Magit from avsej
;;
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings occur-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

      ))
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
