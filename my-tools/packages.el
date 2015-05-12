(setq my-tools-packages
      '(
        ;; package my-toolss go here
        youdao-dictionary
        helm-github-stars
        elfeed))


(defun my-tools/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "andyque")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache")
      )))

(defun my-tools/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t)

      ;; Set file path for saving search history
      (setq youdao-dictionary-search-history-file "~/.emacs.d/.cache/.youdao")

      ;; Enable Chinese word segmentation support (支持中文分词)
      (setq youdao-dictionary-use-chinese-word-segmentation t)
      )))

(defun my-tools/init-elfeed ()
  (use-package elfeed
    :defer t
    :config
    (progn
      (global-set-key (kbd "C-x w") 'elfeed)

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://sachachua.com/blog/feed/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://blog.codingnow.com/atom.xml"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://zh.lucida.me/atom.xml"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsnyc.org/atom.xml"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"
              ))

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank)
      )))
