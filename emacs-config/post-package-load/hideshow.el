(spacemacs|use-package-add-hook hideshow
  :post-config
  (add-to-list 'hs-special-modes-alist
                `(enh-ruby-mode
                  ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                       ; Block end
                  ,(rx (or "#" "=begin"))                        ; Comment start
                  enh-ruby-forward-sexp nil)))
