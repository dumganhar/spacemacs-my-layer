(setq my-writing-packages
      '(
        ;; package my-writings go here
        markdown-mode
        org-octopress))

(defun my-writing/post-init-markdown-mode ()
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

(defun my-writing/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (setq org-octopress-directory-top       "~/myblog/octopress/source")
    (setq org-octopress-directory-posts     "~/myblog/octopress/source/_posts")
    (setq org-octopress-directory-org-top   "~/myblog/octopress/source")
    (setq org-octopress-directory-org-posts "~/myblog/octopress/source/blog")
    (setq org-octopress-setup-file          "~/myblog/octopress/setupfile.org")
      )))
