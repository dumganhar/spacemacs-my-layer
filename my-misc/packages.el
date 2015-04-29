;;; packages.el --- my-misc Layer packages File for Spacemacs
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

(defvar my-misc-packages
  '(
    ;; package my-miscs go here
    swiper
    magit
    git-messenger
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-misc-excluded-packages '()
  "List of packages to exclude.")

;; (require 'ivy)
;; For each package, define a function my-misc/init-<package-my-misc>
;;
(defun my-misc/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :defer t
    :config
    (progn
      ;; http://oremacs.com/2015/04/16/ivy-mode/
      ;; (ivy-mode -1)
      ;; (setq magit-completing-read-function 'ivy-completing-read)

      ;; http://oremacs.com/2015/04/19/git-grep-ivy/
      (defun counsel-git-grep-function (string &optional _pred &rest _u)
        "Grep in the current git repository for STRING."
        (split-string
         (shell-command-to-string
          (format
           "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
           string))
         "\n"
         t))

      (defun counsel-git-grep ()
        "Grep for a string in the current git repository."
        (interactive)
        (let ((default-directory (locate-dominating-file
                                  default-directory ".git"))
              (val (ivy-read "pattern: " 'counsel-git-grep-function))
              lst)
          (when val
            (setq lst (split-string val ":"))
            (find-file (car lst))
            (goto-char (point-min))
            (forward-line (1- (string-to-number (cadr lst)))))))

      (define-key global-map (kbd "C-s") 'swiper)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))


(defun my-misc/post-init-magit ()
  (use-package magit 
    :defer t
    :config
    (progn
     ;; Githu PR settings
     ;;http://endlessparentheses.com/easily-create-github-prs-from-magit.html
     (defun endless/visit-pull-request-url ()
       "Visit the current branch's PR on Github."
       (interactive)
       (browse-url
        (format "https://github.com/%s/compare/%s"
                (replace-regexp-in-string
                 "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                 (magit-get "remote"
                            (magit-get-current-remote)
                            "url"))
                (magit-get-current-branch))))

     (define-key magit-mode-map "V"
       'endless/visit-pull-request-url)

     (defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
       "when entering magit blame mode, change evil normal state to emacs state"
       (if (evil-normal-state-p)
           (evil-emacs-state)
         (evil-normal-state))
       )

     (ad-activate 'magit-blame-mode)

     (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
       "when entering git-timemachine mode, change evil normal state to emacs state"
       (if (evil-normal-state-p)
           (evil-emacs-state)
         (evil-normal-state)))

     (ad-activate 'git-timemachine-mode)

     (define-key magit-log-mode-map (kbd "W") 'magit-copy-item-as-kill)
     ))
  )

(defun my-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
     (defun my-vc-visit-file-revision (file rev)
       "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
       ;; based on `vc-revision-other-window'.
       (interactive
        (let ((file (expand-file-name
                     (read-file-name
                      (if (buffer-file-name)
                          (format "File (%s): " (file-name-nondirectory
                                                 (buffer-file-name)))
                        "File: ")))))
          (require 'vc)
          (unless (vc-backend file)
            (error "File %s is not under version control" file))
          (list file (vc-read-revision
                      "Revision to visit (default is working revision): "
                      (list file)))))
       (require 'vc)
       (unless (vc-backend file)
         (error "File %s is not under version control" file))
       (let ((revision (if (string-equal rev "")
                           (vc-working-revision file)
                         rev))
             (visit (if current-prefix-arg
                        'switch-to-buffer
                      'switch-to-buffer-other-window)))
         (funcall visit (vc-find-revision file revision))))

     (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision)
     )))
