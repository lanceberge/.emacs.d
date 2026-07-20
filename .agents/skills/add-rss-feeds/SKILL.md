---
name: add-rss-feeds
description: Find and add high-quality RSS feeds
disable-model-invocation: true
---

# Add RSS Feeds

1. Search the web for feeds matching the requested topics.
2. Prefer simple, concise, educational writing over entertainment.
3. Fetch every candidate with `curl`; reject broken or invalid feeds.
4. Review recent posts; reject low-quality, stale, promotional, or misleading feeds.
5. Report these feeds to the user. Await further input. *DO NOT* make any code changes.
6. Only when authorized by the user, add the requested feeds to `editor/elfeed.el` with appropriate tags.
