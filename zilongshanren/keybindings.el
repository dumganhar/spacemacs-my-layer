(global-set-key (kbd "C-c l") 'zilongshanren/insert-chrome-current-tab-url)
(global-set-key (kbd "C-c o") 'spacemacs/open-in-external-app)

(require 'dired)
(define-key dired-mode-map (kbd "<mouse-2>") 'my-dired-find-file)

(define-key dired-mode-map "r" 'dired-start-process)
(define-key dired-mode-map "E" 'dired-toggle-read-only)

(define-key dired-mode-map (kbd "`") 'dired-open-term)


(eval-after-load 'dired-mode
  (progn
    (define-key dired-mode-map (kbd "C-k") 'zilongshanren/dired-up-directory)))

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

(eval-after-load 'term '(define-key term-raw-map (kbd "C-:") 'dired-jump))

(define-key dired-mode-map (kbd "z") 'dired-get-size)
(define-key dired-mode-map (kbd "C-c C-e") 'dired-toggle-read-only)

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'smart-open-line)

(define-key global-map (kbd "<f1>") 'hotspots)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c r") 'org-capture)
(define-key global-map (kbd "<f9>") 'org-capture)

