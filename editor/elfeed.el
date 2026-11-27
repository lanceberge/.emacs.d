;;; -*- lexical-binding: t -*-

;; view name, filters, and excluded words
(defvar +elfeed-views
  '(("Default" . "@4-day-ago +unread -news")
    ("News" . "@1-month-ago +unread +news !\\b\\(Bollywood\\|Hollywood\\|celebrity\\|film\\|movie\\|singer\\|actor\\|actress\\|concert\\|album\\|Halloween\\|Arsenal\\|football\\|soccer\\)\\b")
    ("Emacs" . "@1-month-ago +unread +emacs")
    ("Tech Blogs" . "@30-days-ago +unread +tech-blog"))
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
(defun +elfeed-sort-by-day-and-priority (a b)
  "Sort entries A and B by local day, feed priority, then publication time."
  (let* ((date-a (elfeed-entry-date a))
         (date-b (elfeed-entry-date b))
         (day-a (time-to-days (seconds-to-time date-a)))
         (day-b (time-to-days (seconds-to-time date-b)))
         (priority-a (elfeed-meta (elfeed-entry-feed a) :user/priority 7))
         (priority-b (elfeed-meta (elfeed-entry-feed b) :user/priority 7)))
    (cond ((/= day-a day-b) (> day-a day-b))
          ((/= priority-a priority-b) (< priority-a priority-b))
          (t (> date-a date-b)))))

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
  (elfeed-search-sort-function #'+elfeed-sort-by-day-and-priority)
  (elfeed-search-separator-date-format "%a %b %d, %Y")
  (elfeed-feeds
   '(("https://elixir-lang.org/atom.xml" :user/priority 4 elixir official tech-blog)
     ("https://www.phoenixframework.org/blog/rss.xml" :user/priority 6 elixir official tech-blog)
     ("https://dashbit.co/blog/rss.xml" :user/priority 4 elixir tech-blog)
     ("https://fly.io/blog/feed.xml" :user/priority 4 elixir systems tech-blog)

     ("https://go.dev/blog/feed.atom" :user/priority 1 go official tech-blog)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" :user/priority 1 go systems tech-blog)
     ("https://dave.cheney.net/feed" :user/priority 3 go tech-blog)

     ("https://planet.emacslife.com/atom.xml" :user/priority 6 emacs)
     ("https://www.masteringemacs.org/feed" :user/priority 2 emacs tech-blog)
     ("https://www.matem.unam.mx/~omar/apropos-emacs.xml" :user/priority 2 emacs tech-blog)

     ("https://feeds.feedburner.com/martinkl" :user/priority 1 systems tech-blog)
     ("https://aphyr.com/posts.atom" :user/priority 2 systems tech-blog)
     ("https://brooker.co.za/blog/rss.xml" :user/priority 1 systems tech-blog)
     ("https://www.allthingsdistributed.com/atom.xml" :user/priority 2 systems tech-blog)
     ("https://blog.cloudflare.com/tag/engineering/rss/" :user/priority 1 systems tech-blog)
     ("https://engineering.fb.com/feed/" :user/priority 1 systems tech-blog)
     ("https://discord.com/blog/rss.xml" :user/priority 1 systems company)
     ("https://netflixtechblog.com/feed" :user/priority 1 systems company tech-blog)
     ("https://rssbridge.sciunto.org/?action=display&bridge=UberEngineering&format=Atom" :user/priority 1 systems company tech-blog)
     ("https://www.postgresql.org/about/newsarchive/rss/" :user/priority 1 systems databases official)
     ("https://www.postgresql.org/about/press/rss/" :user/priority 1 systems databases official)
     ("https://duckdb.org/feed.xml" :user/priority 2 systems databases official tech-blog)
     ("https://www.sqlite.org/news.rss" :user/priority 2 systems databases)
     ("https://tigerbeetle.com/blog/index.xml" :user/priority 1 systems databases tech-blog)
     ("https://ffmpeg.org/index.xml" :user/priority 3 media systems official)
     ("https://www.tailscale.com/blog/index.xml" :user/priority 1 networking systems company tech-blog)

     ("https://defensetechnewsletter.curated.co/issues.rss" :user/priority 1 defense technology)
     ("https://www.anduril.com/feed.xml" :user/priority 1 defense technology anduril official)
     ("https://www.pentlandedge.com/feed/" :user/priority 2 defense technology radar tech-blog)
     ("https://hforsten.com/feeds/all.atom.xml" :user/priority 2 defense engineering tech-blog)
     ("https://spacenews.com/feed/" :user/priority 2 space)
     ("https://www.navalnews.com/feed/" :user/priority 2 defense maritime)
     ("https://breakingdefense.com/feed/" :user/priority 1 defense technology)

     ("https://feeds.aps.org/rss/recent/physics.xml" :user/priority 4 physics official)
     ("https://www.miniphysics.com/rss-blog.xml" :user/priority 4 physics learning)
     ("https://ciechanow.ski/atom.xml" :user/priority 2 learning tech-blog)

     ("https://www.johndcook.com/blog/feed/" :user/priority 3 math)
     ("https://www.math3ma.com/blog/rss.xml" :user/priority 3 math learning)

     ("https://www.electronics-tutorials.ws/feed" :user/priority 5 electronics learning beginner tech-blog)
     ("https://www.baldengineer.com/feed" :user/priority 4 electronics practical tech-blog)

     ("https://developer.nvidia.com/blog/feed/" :user/priority 3 gpu company tech-blog)
     ("https://developer.nvidia.com/blog/tag/cuda/feed/" :user/priority 3 gpu cuda company tech-blog)

     ("https://projectf.io/posts/index.xml" :user/priority 3 systems fpga tech-blog)
     ("https://blog.yosyshq.com/feed.xml" :user/priority 3 systems fpga open-source tech-blog)
     ("https://mitchellh.com/feed.xml" :user/priority 1 open-source tech-blog)

     ("https://blog.rust-lang.org/feed.xml" :user/priority 7 rust official tech-blog)
     ("https://blog.rust-lang.org/inside-rust/feed.xml" :user/priority 7 rust official tech-blog)
     ("https://smallcultfollowing.com/babysteps/atom.xml" :user/priority 7 rust tech-blog)
     ("https://ziglang.org/news/index.xml" :user/priority 7 systems languages)

     ("https://herbsutter.com/feed/" :user/priority 7 cpp tech-blog)
     ("https://quuxplusone.github.io/blog/feed.xml" :user/priority 7 cpp tech-blog)

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
        ("l" . #'recenter-top-bottom)
        ("M-i" . #'+ace-link-eww)
        ("P" . #'elfeed-show-prev))
  (:map elfeed-search-mode-map
        ("l" . #'recenter-top-bottom)
        ("v" . #'+elfeed-view))
  (:map +leader-map
        ("nr" . #'+elfeed-default)
        ("nn" . #'+elfeed-news)))
