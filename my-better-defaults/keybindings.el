(global-set-key (kbd "C-c l") 'zilongshanren/insert-chrome-current-tab-url)
(global-set-key (kbd "C-c o") 'prelude-open-with)

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

(eval-after-load 'term'
      (define-key term-raw-map (kbd "C-:") 'dired-jump)
      )

(define-key dired-mode-map (kbd "z") 'dired-get-size)
(define-key dired-mode-map (kbd "C-c C-e") 'dired-toggle-read-only)

(global-set-key (kbd "s-/") 'hippie-expand)
