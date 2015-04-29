;;add abbrev mode
(require 'use-package)

(use-package abbrev
  :defer t
  :config
  (progn

    (define-abbrev-table 'global-abbrev-table '(

                                                ;; math/unicode symbols
                                                ("8in" "∈")
                                                ("8nin" "∉")
                                                ("8inf" "∞")
                                                ("8luv" "♥")
                                                ("8smly" "☺")

                                                ;; email
                                                ("8me" "guanghui.qu@cocos2d-x.org")

                                                ;; computing tech
                                                ("8wp" "Wikipedia")
                                                ("8ms" "Microsoft")
                                                ("8g" "Google")
                                                ("8it" "IntelliType")
                                                ("8msw" "Microsoft Windows")
                                                ("8win" "Windows")
                                                ("8ie" "Internet Explorer")
                                                ("8ahk" "AutoHotkey")
                                                ("82dx" "Cocos2D-X")

                                                ;; signature
                                                ("zl" "zilongshanren")
                                                ;; emacs regex
                                                ("8d" "\\([0-9]+?\\)")
                                                ("8str" "\\([^\"]+?\\)\"")


                                                ))

    ;; stop asking whether to save newly added abbrev when quitting emacs
    (setq save-abbrevs nil)

    ;; turn on abbrev mode globally
    (setq-default abbrev-mode t)
    )
  :diminish abbrev-mode
  )

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

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))

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

(require 'git-messenger)
(define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision)

(require 'magit)
(define-key magit-log-mode-map (kbd "W") 'magit-copy-item-as-kill)
