(defun hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :sources `(((name . "Mail and News")
                    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                                   ;; ("Gmail" . (lambda() (mu4e)))
                                   ("RSS" . elfeed)
                                   ("Github" . (lambda() (helm-github-stars)))
                                   ;; ("Writing" . (lambda()(olivetti-mode)))
                                   ;; ("weibo" . (lambda()(weibo-timeline)))
                                   ("Agenda" . (lambda () (org-agenda "" "a")))
                                   ("sicp" . (lambda() (w3m-browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))
                                   ))

                    (action . (("Open" . (lambda (x) (funcall x))))))
                   ((name . "My Locations")
                    (candidates . ((".emacs.d" . "~/.emacs.d/init.el" )
                                   ("blog" . "~/4gamers.cn/")
                                   ("notes" . "~/org-notes/notes.org")
                                   ))
                    (action . (("Open" . (lambda (x) (find-file x))))))

                   helm-source-recentf
                   helm-source-bookmarks
                   helm-source-bookmark-set)))
