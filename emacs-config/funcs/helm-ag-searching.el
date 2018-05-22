;;;###autoload
(defun lx/helm-ag-search-prefilled-pattern (keyword)
  (require 'helm-ag)
  (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;; make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing) keyword)))
    (helm-do-ag (projectile-project-root))))

;;;###autoload
(defun lx/helm-ag-search-pry-breakpoints ()
  (interactive)
  (lx/helm-ag-search-prefilled-pattern "binding\.pry"))
