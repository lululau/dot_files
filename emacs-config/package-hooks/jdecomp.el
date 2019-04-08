(with-eval-after-load 'jdecomp

  (defun jdecomp--fernflower-decompile-file (file &optional extracted-p)
    "Decompile FILE with Fernflower and return result as string.

FILE must be a Java classfile.

Optional parameter EXTRACTED-P, when non-nil, indicates that FILE
was extracted from a JAR with `jdecomp--extract-to-file'."
    (jdecomp--ensure-decompiler 'fernflower)
    (with-temp-buffer
      (let* ((classpath (or (file-name-directory file) default-directory))
             (destination (if extracted-p
                              (file-name-directory file)
                            (jdecomp--make-temp-file (concat "jdecomp" (file-name-sans-extension file)) t))))
        ;; The java-decompiler.jar is not executable
        ;; See: http://stackoverflow.com/a/39868281/864684
        (apply #'call-process "java" nil nil nil
               `("-cp" ,(expand-file-name (jdecomp--decompiler-path 'fernflower))
                 "org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler"
                 ,@(jdecomp--decompiler-options 'fernflower)
                 ,file
                 ,destination))
        (insert-file-contents (cl-first (jdecomp--java-files destination)))
        (buffer-string)))))
