;;; -*- lexical-binding: t -*-

(use-package elfeed
  :custom
  (elfeed-db-directory (expand-file-name "var/elfeed/db/" user-emacs-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "var/elfeed/enclosures/" user-emacs-directory))
  (elfeed-search-filter "@1-month-ago +unread")
  (elfeed-feeds
   '(("https://elixir-lang.org/atom.xml" elixir official must-read)

     ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news must-read)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://www.masteringemacs.org/feed" emacs)
     ("https://irreal.org/blog/?feed=rss2" emacs)
     ("https://karthinks.com/software/index.xml" emacs)
     ("https://protesilaos.com/codelog.xml" emacs)

     ("https://feeds.feedburner.com/martinkl" systems must-read)
     ("https://aphyr.com/posts.atom" systems)
     ("https://brooker.co.za/blog/rss.xml" systems must-read)
     ("https://www.allthingsdistributed.com/atom.xml" systems)
     ("https://blog.cloudflare.com/tag/engineering/rss/" systems)
     ("https://engineering.fb.com/feed/" systems)
     ("https://planet.postgresql.org/rss20.xml" systems)

     ("https://lobste.rs/rss" tech news)
     ("https://hnrss.org/frontpage" tech news)
     ("http://feeds.bbci.co.uk/news/world/rss.xml" news)
     ("https://www.aljazeera.com/xml/rss/all.xml" news)
     ("https://www.npr.org/rss/rss.php?id=1001" news)))
  :bind
  (:map elfeed-show-mode-map
        ("M-i" . #'ace-link))
  (:map +leader-map
        ("ar" . #'elfeed)))
