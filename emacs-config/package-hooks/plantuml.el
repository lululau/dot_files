(spacemacs|use-package-add-hook org
  :post-config
  (progn
    (defun org-babel-execute:puml (body params)
      "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
      (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
             (out-file (or (cdr (assoc :file params))
                           (error "PlantUML requires a \":file\" header argument")))
             (cmdline (cdr (assoc :cmdline params)))
             (in-file (org-babel-temp-file "plantuml-"))
             (java (or (cdr (assoc :java params)) ""))
             (cmd (if (string= "" org-plantuml-jar-path)
                      (error "`org-plantuml-jar-path' is not set")
                    (concat "java " java " -jar "
                            (shell-quote-argument
                             (expand-file-name org-plantuml-jar-path))
                            (if (string= (file-name-extension out-file) "svg")
                                " -tsvg" "")
                            (if (string= (file-name-extension out-file) "eps")
                                " -teps" "")
                            " -p " cmdline " < "
                            (org-babel-process-file-name in-file)
                            " > "
                            (org-babel-process-file-name out-file)))))
        (unless (file-exists-p org-plantuml-jar-path)
          (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
        (with-temp-file in-file (insert (concat "@startuml\n" body "\n@enduml")))
        (message "%s" cmd) (org-babel-eval cmd "")
        nil)) ;; signal that output has already been written to file

    (defun org-babel-prep-session:puml (session params)
      "Return an error because plantuml does not support sessions."
      (error "Plantuml does not support sessions"))))
