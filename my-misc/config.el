;;add abbrev mode
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
                                            ("8str" "\\([^\"]+?\\)\"")))

;; stop aking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

    ;; turn on abbrev mode globally
(setq-default abbrev-mode t)


(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))


;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))
