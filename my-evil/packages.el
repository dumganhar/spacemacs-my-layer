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

(defun prelude-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun prelude-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun zilongshanren/open-line-above()
  "open an empty line above the current line"
  (interactive)
  (save-excursion
    (evil-open-above 1)
    (evil-normal-state)
    ))

(defun zilongshanren/open-line-below()
  "open an empty line below the current line"
  (interactive)
  (save-excursion
    (evil-open-below 1)
    (evil-normal-state))
  )

(defun zilongshanren/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
      (evil-ex command-string))
      )))

;; For each package, define a function my-evil/init-<package-my-evil>
;;
(defun my-evil/post-init-evil ()
  "Initialize evil"
  (use-package evil
    :init
    (

     )
    :defer t
    :config
    (progn
      ;;mimic "nzz" behaviou in vim
      (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      ;; (setq evil-search-module 'evil-search)
      ;; (define-key evil-normal-state-map "n" 'evil-ex-search-next)
      ;; (define-key evil-normal-state-map "N" 'evil-ex-search-previous)
      ;; (define-key evil-normal-state-map "*" 'evil-ex-search-word-forward)
      ;; (define-key evil-normal-state-map "#" 'evil-ex-search-word-backward)
      ;; (define-key evil-normal-state-map "/" 'evil-ex-search-forward)
      (define-key evil-normal-state-map "[b" 'previous-buffer)
      (define-key evil-normal-state-map "]b" 'next-buffer)
      (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-normal-state-map (kbd ",ii") 'iimage-mode)
      (define-key evil-normal-state-map (kbd "s-g") 'helm-ag-project-root)
      (define-key evil-normal-state-map (kbd "s-f") 'ido-find-file)
      (define-key evil-normal-state-map (kbd ",f") 'ff-find-other-file)

      (define-key evil-normal-state-map (kbd "[ SPC") 'zilongshanren/open-line-above)
      (define-key evil-normal-state-map (kbd "] SPC") 'zilongshanren/open-line-below)

      (define-key evil-normal-state-map
        (kbd "Y") 'zilongshanren/yank-to-end-of-line)

      ;; rebind g,k to gj and gk
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

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

      (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
      (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
      (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
      (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)

      (evil-add-hjkl-bindings ibuffer-mode-map 'emacs)
      (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)
      (evil-add-hjkl-bindings dired-mode-map 'emacs)
      (evil-add-hjkl-bindings help-mode-map 'emacs)
      (evil-add-hjkl-bindings elfeed-search-mode-map 'emacs)
      (evil-add-hjkl-bindings elfeed-show-mode-map 'emacs)


      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
      (evil-add-hjkl-bindings Info-mode-map 'emacs)

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

      ;; Evil Emacs state map
      (define-key evil-emacs-state-map "[b" 'previous-buffer)
      (define-key evil-emacs-state-map "]b" 'next-buffer)
      ;; define c-w h,j,k,l to window movement in evil-eamcs mode
      (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
      (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
      (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
      (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

      ;; for emacs shell mode
      (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
      (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
      (evil-define-key 'emacs term-raw-map (kbd "C-w")
        'evil-delete-backward-word)
      (define-key evil-emacs-state-map (kbd "s-p") 'projectile-switch-project)

      )))
