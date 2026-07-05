;;; -*- lexical-binding: t -*-

(use-package elfeed
  :custom
  (elfeed-db-directory (expand-file-name "var/elfeed/db/" user-emacs-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "var/elfeed/enclosures/" user-emacs-directory))
  (elfeed-search-filter "@1-month-ago +unread")
  (elfeed-feeds
   '(("https://elixir-lang.org/atom.xml" elixir official must-read)
     ("https://www.phoenixframework.org/blog/rss.xml" elixir official)
     ("https://dashbit.co/blog/rss.xml" elixir)
     ("https://fly.io/blog/feed.xml" elixir systems)

     ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news must-read)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://www.masteringemacs.org/feed" emacs)
     ("https://irreal.org/blog/?feed=rss2" emacs)
     ("https://karthinks.com/software/index.xml" emacs)
     ("https://protesilaos.com/codelog.xml" emacs)
     ("https://emacsair.me/feed.xml" emacs)

     ("https://feeds.feedburner.com/martinkl" systems must-read)
     ("https://aphyr.com/posts.atom" systems)
     ("https://brooker.co.za/blog/rss.xml" systems must-read)
     ("https://www.allthingsdistributed.com/atom.xml" systems)
     ("https://blog.cloudflare.com/tag/engineering/rss/" systems)
     ("https://engineering.fb.com/feed/" systems)
     ("https://discord.com/blog/rss.xml" systems company)
     ("https://planet.postgresql.org/rss20.xml" systems)
     ("https://www.postgresql.org/about/newsarchive/rss/" systems databases official)
     ("https://www.postgresql.org/about/press/rss/" systems databases official)
     ("https://duckdb.org/feed.xml" systems databases official)
     ("https://www.sqlite.org/news.rss" systems databases)
     ("https://tigerbeetle.com/blog/index.xml" systems databases must-read)
     ("https://ffmpeg.org/index.xml" media systems official)
     ("https://www.tailscale.com/blog/index.xml" networking systems company)

     ("https://developer.nvidia.com/blog/feed/" gpu company)
     ("https://developer.nvidia.com/blog/tag/cuda/feed/" gpu cuda company)

     ("https://projectf.io/posts/index.xml" systems fpga)
     ("https://blog.yosyshq.com/feed.xml" systems fpga open-source)

     ("https://blog.rust-lang.org/feed.xml" rust official)
     ("https://blog.rust-lang.org/inside-rust/feed.xml" rust official)
     ("https://smallcultfollowing.com/babysteps/atom.xml" rust)
     ("https://ziglang.org/news/index.xml" systems languages)

     ("https://herbsutter.com/feed/" cpp)
     ("https://quuxplusone.github.io/blog/feed.xml" cpp)

     ("https://hnrss.org/frontpage" tech news)
     ("http://feeds.bbci.co.uk/news/world/rss.xml" news)
     ("https://www.aljazeera.com/xml/rss/all.xml" news)
     ("https://www.npr.org/rss/rss.php?id=1001" news)))
  :bind
  (:map elfeed-show-mode-map
        ("M-i" . #'ace-link))
  (:map +leader-map
        ("nr" . #'elfeed)))
