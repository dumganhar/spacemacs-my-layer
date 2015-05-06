(defvar my-better-defaults-packages
  '(
    discover-my-major
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-better-defaults-excluded-packages
  '(
    )
  "List of packages to exclude.")

(defun my-better-defaults/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (evil-leader/set-key (kbd "mhm") 'discover-my-major)
    ))

