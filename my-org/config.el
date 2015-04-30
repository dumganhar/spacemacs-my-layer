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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands nil)
 '(org-agenda-files (quote ("~/org-notes/gtd.org"
                            "~/org-notes/emacs.org"
                            "~/org-notes/cocos2d-x.org"
                            "~/org-notes/notes.org"
                            "~/org-notes/learning.org"
                            "~/org-notes/vim.org"
                            "~/org-notes/journal.org"
                            )))
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t))
