(use-package lsp-java
  :defer t
  :defer-incrementally (request)
  :custom
  (lsp-java-java-path "/usr/bin/java")
  :config
  (setenv "JAVA_HOME"  "/usr/lib/jvm/java-8-openjdk-amd64")

  (setq lsp-java-vmargs
        ;; Lombok classpath
        '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"
          "-javaagent:/home/labergeron/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"
          "-Xbootclasspath/a:/home/labergeron/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))


  )
