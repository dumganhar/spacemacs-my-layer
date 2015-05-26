(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c r") 'org-capture)
(evil-leader/set-key-for-mode 'org-mode  "mt" 'org-set-tags)
