(setq my-better-defaults-packages
      '(
        discover-my-major))

(defun my-better-defaults/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (evil-leader/set-key (kbd "mhm") 'discover-my-major)))

