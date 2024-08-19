(use-package lsp-java
  :defer t
  :defer-incrementally (request)
  :custom
  (lsp-prefer-flymake nil)
  :config
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java")

  (setenv "JAVA_HOME"  "/usr/lib/jvm/java-17-openjdk-amd64")
  (setq lsp-java-vmargs
        ;; Lombok jar path

        '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"
          "-javaagent:/home/labergeron/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"
          "-Xbootclasspath/a:/home/labergeron/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
  )
