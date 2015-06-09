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
(setq cc-other-file-alist
      '(("\\.cpp"   (".h"))
        ("\\.h"   (".c"".cpp"))))


;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode t)

(require 'semantic/bovine/c)
(require 'semantic/ia)
(defvar cocos2dx-dir "~/cocos2d-x")
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

;;add doxyemacs 

(add-to-list 'load-path "~/.emacs.d/spacemacs-private/my-c-c++/extensions/doxyemacs")
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook'doxymacs-mode)

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

;;settings for  chinese font
(add-to-list 'load-path "~/.emacs.d/spacemacs-private/my-c-c++/extensions/eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")
;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

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
