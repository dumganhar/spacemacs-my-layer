(spacemacs|defvar-company-backends lua-mode)

(require 'dired-x)
(require 'dired-aux)

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "open")
        ("\\.docx\\'" "open")
        ("\\.\\(?:djvu\\|eps\\)\\'" "open")
        ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
        ("\\.\\(?:xcf\\)\\'" "open")
        ("\\.csv\\'" "open")
        ("\\.tex\\'" "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
         "open")
        ("\\.\\(?:mp3\\|flac\\)\\'" "open")
        ("\\.html?\\'" "open")
        ("\\.md\\'" "open")))



(defvar dired-filelist-cmd
  '(("vlc" "-L")))


;; find Chinese filename by pingyin
;; (require 'find-by-pinyin-dired)
;; (define-key dired-mode-map "p" 'find-by-pinyin-dired)

(add-hook 'term-mode-hook 'ash-term-hooks)


(global-prettify-symbols-mode 1)
(setq-default fill-column 110)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a keypress
(delete-selection-mode t)


;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " James - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                   "%b"))))

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)

;; when press c-d, the result is wrong.
;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
(require 'whitespace)
(setq whitespace-line-column fill-column) ;; limit line length
(setq whitespace-style '(face lines-tail))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
(global-whitespace-mode +1)

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-mode))
       auto-mode-alist))

(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/
;;need to install coreutils at first


;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")


(require 'cc-mode)

;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior


(setq c-default-style "linux") ;; set style to "linux"
(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)
;; (setq cc-other-file-alist
;;       '(("\\.cpp"   (".h"))
;;         ("\\.h"   (".c"".cpp"))))


;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode t)

(require 'semantic/bovine/c)
(require 'semantic/ia)
(defvar cocos2dx-dir "~/Project/cocos2d-x")
(semantic-add-system-include cocos2dx-dir 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/cocos") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/platform") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/audio/include") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/platform/mac") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/extensions") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/luajit") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/freetype") 'c++-mode)
(semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/zlib") 'c++-mode)
;; include path for OpenGL
(semantic-add-system-include "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" 'c++-mode)
(add-to-list 'auto-mode-alist (cons cocos2dx-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DLL" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("EXPORT_DLL" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_TARGET_OS_MAC" . "1"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_GUI_DLL" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_KEYBOARD_SUPPORT" . "1"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DEPRECATED_ATTRIBUTE" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("USE_FILE32API" . "1"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_ENABLE_CHIPMUNK_INTEGRATION" . "1"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("COCOS2D_DEBUG" . "1"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/mac/CCPlatformDefine-mac.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/CCPlatformMacros.h"))

(set-default 'semantic-case-fold t)


(require 'font-lock)

(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


(add-hook 'c++-mode-hook
          '(lambda()
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                    ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                    ;; user-types (customize!)
                    ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)

;;add abbrev mode
(define-abbrev-table 'global-abbrev-table '(

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")

                                            ;; email
                                            ("8me" "jianhua.chen@cocos2d-x.org")

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

(require 'org-compat)
(require 'org)
(require 'org-install)
;; (add-to-list 'org-modules "org-habit")
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)

(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
(setq org-agenda-window-setup 'current-window)
(setq org-log-done t)


(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))


;; (define-key global-map "\C-cl" 'org-insert-link)

(setq org-mobile-directory "~/org-notes/org")


;; {{ export org-mode in Chinese into PDF
;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;; and you need install texlive-xetex on different platforms
;; To install texlive-xetex:
;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
	"xelatex -interaction nonstopmode -output-directory %o %f"
	"xelatex -interaction nonstopmode -output-directory %o %f"))
;; }}
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
	      (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t);; Show the clocked-in task - if any - in the header line

(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(setq org-default-notes-file "~/org-notes/gtd.org")

;; the %i would copy the selected text into the template
;;http://www.howardism.org/Technical/Emacs/journaling-org.html
;;add multi-file journal
(setq org-capture-templates
      '(("n" "notes" entry (file+headline "~/org-notes/notes.org" "Quick notes")
         "* %?\n  %i\n %U"
         :empty-lines 1)
        ("e" "emacs" entry (file+headline "~/org-notes/emacs.org" "Quick notes")
         "* %? \n %(zilongshanren/retrieve-chrome-current-tab-url)\n %U\n "
         :empty-lines 1)
        ("t" "Todo" entry (file+headline "~/org-notes/gtd.org" "Daily Tasks")
	 "* TODO %?\n  %i\n"
         :empty-lines 1)
        ("w" "Todo" entry (file+headline "~/org-notes/gtd.org" "Weekly Tasks")
	 "* TODO %?\n  %i\n"
         :empty-lines 1)
        ("m" "Todo" entry (file+headline "~/org-notes/gtd.org" "Monthly Tasks")
	 "* TODO %?\n  %i\n"
         :empty-lines 1)
        ("j" "Journal Entry"
         entry (file+datetree "~/org-notes/journal.org")
         "* %?"
         :empty-lines 1)
	))

