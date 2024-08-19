(use-package lsp-java
  :defer t
  :defer-incrementally (request)
  :custom
  (lsp-java-java-path "/usr/bin/java")
  :config
  (setenv "JAVA_HOME"  "/usr/lib/jvm/java-8-openjdk-amd64")
  )
