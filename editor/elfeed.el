;;; -*- lexical-binding: t -*-

;; view name, filters, and excluded words
(defvar +elfeed-views
  '(("Default" . "@4-day-ago +unread -news")
    ("News" . "@1-month-ago +unread +news !\\b\\(Bollywood\\|Hollywood\\|celebrity\\|film\\|movie\\|singer\\|actor\\|actress\\|concert\\|album\\|Halloween\\|Arsenal\\|football\\|soccer\\)\\b")
    ("Emacs" . "@1-month-ago +unread +emacs"))
  "Named Elfeed search filters.")

;;;###autoload
(defun +elfeed-view ()
  "Choose a named Elfeed search filter."
  (interactive)
  (elfeed-search-set-filter
   (cdr (assoc (completing-read "Elfeed view: " +elfeed-views nil t)
               +elfeed-views))))

;;;###autoload
(defun +elfeed-default ()
  "Show the default Elfeed search view."
  (interactive)
  (elfeed-search (default-value 'elfeed-search-filter)))

;;;###autoload
(defun +elfeed-news ()
  "Show unread news, excluding common entertainment and sports terms."
  (interactive)
  (elfeed-search (cdr (assoc "News" +elfeed-views))))

;;;###autoload
(defun +elfeed-sort-by-priority (a b)
  "Sort entries A and B by feed priority (1 first), then newest first."
  (let ((priority-a (elfeed-meta (elfeed-entry-feed a) :user/priority 7))
        (priority-b (elfeed-meta (elfeed-entry-feed b) :user/priority 7)))
    (if (= priority-a priority-b)
        (> (elfeed-entry-date a) (elfeed-entry-date b))
      (< priority-a priority-b))))

(use-package elfeed
  :hook
  ;; (elfeed-search-mode . scroll-lock-mode)
  (elfeed-show-mode . scroll-lock-mode)
  :custom
  (elfeed-db-directory (expand-file-name "var/elfeed/db/" user-emacs-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "var/elfeed/enclosures/" user-emacs-directory))
  (elfeed-entry-point #'+elfeed-default)
  (elfeed-search-filter (cdr (assoc "Default" +elfeed-views)))
  (elfeed-search-sort-function #'+elfeed-sort-by-priority)
  (elfeed-feeds
   '(("https://elixir-lang.org/atom.xml" :user/priority 4 elixir official)
     ("https://www.phoenixframework.org/blog/rss.xml" :user/priority 6 elixir official)
     ("https://dashbit.co/blog/rss.xml" :user/priority 4 elixir)
     ("https://fly.io/blog/feed.xml" :user/priority 4 elixir systems)

     ("https://go.dev/blog/feed.atom" :user/priority 1 go official)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" :user/priority 1 go systems)
     ("https://dave.cheney.net/feed" :user/priority 3 go)

     ("https://sachachua.com/blog/category/emacs-news/feed/" :user/priority 7 emacs)
     ("https://planet.emacslife.com/atom.xml" :user/priority 6 emacs)
     ("https://www.masteringemacs.org/feed" :user/priority 2 emacs)
     ("https://irreal.org/blog/?feed=rss2" :user/priority 7 emacs)
     ("https://emacsair.me/feed.xml" :user/priority 6 emacs)
     ("https://www.matem.unam.mx/~omar/apropos-emacs.xml" :user/priority 6 emacs)

     ("https://feeds.feedburner.com/martinkl" :user/priority 1 systems)
     ("https://aphyr.com/posts.atom" :user/priority 2 systems)
     ("https://brooker.co.za/blog/rss.xml" :user/priority 1 systems)
     ("https://www.allthingsdistributed.com/atom.xml" :user/priority 2 systems)
     ("https://blog.cloudflare.com/tag/engineering/rss/" :user/priority 1 systems)
     ("https://engineering.fb.com/feed/" :user/priority 1 systems)
     ("https://discord.com/blog/rss.xml" :user/priority 1 systems company)
     ("https://www.postgresql.org/about/newsarchive/rss/" :user/priority 1 systems databases official)
     ("https://www.postgresql.org/about/press/rss/" :user/priority 1 systems databases official)
     ("https://duckdb.org/feed.xml" :user/priority 2 systems databases official)
     ("https://www.sqlite.org/news.rss" :user/priority 2 systems databases)
     ("https://tigerbeetle.com/blog/index.xml" :user/priority 1 systems databases)
     ("https://ffmpeg.org/index.xml" :user/priority 3 media systems official)
     ("https://www.tailscale.com/blog/index.xml" :user/priority 1 networking systems company)

     ("https://defensetechnewsletter.curated.co/issues.rss" :user/priority 1 defense technology)
     ("https://www.anduril.com/feed.xml" :user/priority 1 defense technology anduril official)
     ("https://www.pentlandedge.com/feed/" :user/priority 2 defense technology radar)
     ("https://hforsten.com/feeds/all.atom.xml" :user/priority 2 defense engineering)
     ("https://spacenews.com/feed/" :user/priority 2 space)
     ("https://www.navalnews.com/feed/" :user/priority 2 defense maritime)
     ("https://breakingdefense.com/feed/" :user/priority 1 defense technology)

     ("https://feeds.aps.org/rss/recent/physics.xml" :user/priority 4 physics official)
     ("https://www.miniphysics.com/rss-blog.xml" :user/priority 4 physics learning)
     ("https://ciechanow.ski/atom.xml" :user/priority 2 learning)

     ("https://www.johndcook.com/blog/feed/" :user/priority 3 math)
     ("https://www.math3ma.com/blog/rss.xml" :user/priority 3 math learning)

     ("https://www.electronics-tutorials.ws/feed" :user/priority 5 electronics learning beginner)
     ("https://www.baldengineer.com/feed" :user/priority 4 electronics practical)

     ("https://developer.nvidia.com/blog/feed/" :user/priority 3 gpu company)
     ("https://developer.nvidia.com/blog/tag/cuda/feed/" :user/priority 3 gpu cuda company)

     ("https://projectf.io/posts/index.xml" :user/priority 3 systems fpga)
     ("https://blog.yosyshq.com/feed.xml" :user/priority 3 systems fpga open-source)
     ("https://mitchellh.com/feed.xml" :user/priority 1 open-source)

     ("https://blog.rust-lang.org/feed.xml" :user/priority 7 rust official)
     ("https://blog.rust-lang.org/inside-rust/feed.xml" :user/priority 7 rust official)
     ("https://smallcultfollowing.com/babysteps/atom.xml" :user/priority 7 rust)
     ("https://ziglang.org/news/index.xml" :user/priority 7 systems languages)

     ("https://herbsutter.com/feed/" :user/priority 7 cpp)
     ("https://quuxplusone.github.io/blog/feed.xml" :user/priority 7 cpp)

     ("https://levels.io/rss" :user/priority 7 entrepreneur)
     ("https://world.hey.com/dhh/feed.atom" :user/priority 7 entrepreneur)
     ("https://world.hey.com/jason/feed.atom" :user/priority 7 entrepreneur)
     ("https://nav.al/feed" :user/priority 4 entrepreneur)
     ("https://www.reidhoffman.org/feed/" :user/priority 5 entrepreneur)
     ("https://pmarca.substack.com/feed" :user/priority 5 entrepreneur)

     ("http://feeds.bbci.co.uk/news/world/rss.xml" :user/priority 6 news)
     ("https://feeds.bbci.co.uk/news/technology/rss.xml" :user/priority 4 news)
     ("https://feeds.bbci.co.uk/news/science_and_environment/rss.xml" :user/priority 4 news)
     ("https://www.aljazeera.com/xml/rss/all.xml" :user/priority 7 news)))
  :config
  ;; suppress warnings from using ace-link in elfeed
  (cl-pushnew '(org-element org-element-parser) warning-suppress-types :test #'equal)
  :bind
  (:map elfeed-show-mode-map
        ("M-i" . #'+ace-link)
        ("n" . #'next-line)
        ("p" . #'previous-line)
        ("N" . #'elfeed-show-next)
        ("M-i" . #'+ace-link-eww)
        ("P" . #'elfeed-show-prev))
  (:map elfeed-search-mode-map
        ("l" . #'recenter-top-bottom)
        ("v" . #'+elfeed-view))
  (:map +leader-map
        ("nr" . #'+elfeed-default)
        ("nn" . #'+elfeed-news)))
