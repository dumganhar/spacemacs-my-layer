(setq my-org-packages
      '(
        ;; package my-orgs go here
        org-pomodoro
        ox-reveal
        worf
        org-download))

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
