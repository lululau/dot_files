(with-eval-after-load 'org
  (define-key org-mode-map [M-tab] 'spacemacs/alternate-buffer)
  (define-key org-mode-map (kbd "C-M-i") nil)
  (define-key org-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'code-archive-goto-src)
  (unless (display-graphic-p)
    (define-key org-mode-map (kbd "C-RET") 'org-insert-heading-respect-content)
    (define-key org-mode-map (kbd "M-S-RET") 'org-insert-todo-heading))
  (evil-define-key 'normal org-mode-map (kbd "RET") #'(lambda () (interactive) (condition-case nil (call-interactively 'org-open-at-point) (user-error (evil-insert-newline-below)))))
  (define-key org-mode-map (kbd "<tab>") #'copilot-accept-or-org-cycel)

  (defun copilot-accept-or-org-cycel ()
    (interactive)
    (if (copilot--overlay-visible)
        (copilot-accept-completion)
      (org-cycle)))

  (defun org-display-inline-images (&optional include-linked refresh beg end)
    "Display inline images.

  An inline image is a link which follows either of these
  conventions:

    1. Its path is a file with an extension matching return value
      from `image-file-name-regexp' and it has no contents.

    2. Its description consists in a single link of the previous
      type.  In this case, that link must be a well-formed plain
      or angle link, i.e., it must have an explicit \"file\" type.

  When optional argument INCLUDE-LINKED is non-nil, also links with
  a text description part will be inlined.  This can be nice for
  a quick look at those images, but it does not reflect what
  exported files will look like.

  When optional argument REFRESH is non-nil, refresh existing
  images between BEG and END.  This will create new image displays
  only if necessary.  BEG and END default to the buffer
  boundaries."
    (interactive "P")
    (when (display-graphic-p)
      (unless refresh
        (org-remove-inline-images)
        (when (fboundp 'clear-image-cache) (clear-image-cache)))
      (org-with-point-at (or beg 1)
        (let* ((case-fold-search t)
        (file-extension-re (image-file-name-regexp))
        (link-abbrevs (mapcar #'car
            (append org-link-abbrev-alist-local
              org-link-abbrev-alist)))
        ;; Check absolute, relative file names and explicit
        ;; "file:" links.  Also check link abbreviations since
        ;; some might expand to "file" links.
        (file-types-re
          (format "\\[\\[\\(?:file%s:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
            (if (not link-abbrevs) ""
        (concat "\\|" (regexp-opt link-abbrevs))))))
    (while (re-search-forward file-types-re end t)
      (let* ((link (org-element-lineage
        (save-match-data (org-element-context))
        '(link) t))
      (inner-start (match-beginning 1))
      (path
        (cond
        ;; No link at point; no inline image.
        ((not link) nil)
        ;; File link without a description.  Also handle
        ;; INCLUDE-LINKED here since it should have
        ;; precedence over the next case.  I.e., if link
        ;; contains filenames in both the path and the
        ;; description, prioritize the path only when
        ;; INCLUDE-LINKED is non-nil.
        ((or (not (org-element-property :contents-begin link))
        include-linked)
          (and (equal "file" (org-element-property :type link))
        (org-element-property :path link)))
        ;; Link with a description.  Check if description
        ;; is a filename.  Even if Org doesn't have syntax
        ;; for those -- clickable image -- constructs, fake
        ;; them, as in `org-export-insert-image-links'.
        ((not inner-start) nil)
        (t
          (org-with-point-at inner-start
            (and (looking-at
            (if (char-equal ?< (char-after inner-start))
          org-angle-link-re
              org-plain-link-re))
          ;; File name must fill the whole
          ;; description.
          (= (org-element-property :contents-end link)
              (match-end 0))
          (match-string 2)))))))
        (when (and path (string-match-p file-extension-re path))
          (let ((file (expand-file-name path)))
      (when (file-exists-p file)
        (let ((width
        ;; Apply `org-image-actual-width' specifications.
        (cond
          ((not (image-type-available-p 'imagemagick)) nil)
          ((eq org-image-actual-width t) nil)
          ((listp org-image-actual-width)
          (or
            ;; First try to find a width among
            ;; attributes associated to the paragraph
            ;; containing link.
            (let ((paragraph
            (let ((e link))
              (while (and (setq e (org-element-property
                  :parent e))
              (not (eq (org-element-type e)
                  'paragraph))))
              e)))
              (when paragraph
          (save-excursion
            (goto-char (org-element-property :begin paragraph))
            (when
                (re-search-forward
                "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                (org-element-property
            :post-affiliated paragraph)
                t)
              (let ((w (match-string 1)))
                (if (string-match-p "%" w)
                    (/ (* (string-to-number (shell-command-to-string (format "convert %s -print %%w /dev/null 2>&-" (shell-quote-argument file)))) (string-to-number w)) 100)
                  (string-to-number w)))))))
            ;; Otherwise, fall-back to provided number.
            (car org-image-actual-width)))
          ((numberp org-image-actual-width)
          org-image-actual-width)))
        (old (get-char-property-and-overlay
              (org-element-property :begin link)
              'org-image-overlay)))
          (if (and (car-safe old) refresh)
        (image-refresh (overlay-get (cdr old) 'display))
            (let ((image (create-image file
              (and width 'imagemagick)
              nil
              :width width)))
        (when image
          (let ((ov (make-overlay
              (org-element-property :begin link)
              (progn
                (goto-char
            (org-element-property :end link))
                (skip-chars-backward " \t")
                (point)))))
            (overlay-put ov 'display image)
            (overlay-put ov 'face 'default)
            (overlay-put ov 'org-image-overlay t)
            (overlay-put
            ov 'modification-hooks
            (list 'org-display-inline-remove-overlay))
            (push ov org-inline-image-overlays)))))))))))))))

  (defun lx/get-babel-src ()
    (let* ((info (org-babel-get-src-block-info nil (org-element-context))))
      (if (not info)
          (org-babel-read-element (org-element-context))
        (let* ((lang (nth 0 info))
               (params (nth 2 info))
               (body
                (let ((coderef (nth 6 info))
                      (expand
                       (if (org-babel-noweb-p params :eval)
                           (org-babel-expand-noweb-references info)
                         (nth 1 info))))
                  (if (not coderef) expand
                    (replace-regexp-in-string
                     (org-src-coderef-regexp coderef) "" expand nil nil 1)))))
          (if (symbol-function (intern (format "org-babel-expand-body:%s" lang)))
              (funcall (intern (format "org-babel-expand-body:%s" lang)) body params)
            (org-babel-expand-body:generic
             body params (funcall (intern (format "org-babel-variable-assignments:%s" lang)) params)))))))

  (defun lx/yank-babel-src ()
    (interactive)
    (kill-new (lx/get-babel-src)))

  (defun lx/org-table-recalculate-multi-formulas ()
    (interactive)
    (save-excursion
      (goto-char (org-table-end))
      (while (string-match "^[[:blank:]]*#\\+TBLFM:" (thing-at-point 'line t))
        (org-table-calc-current-TBLFM)
        (forward-line))))

  (defun org-refresh-inline-images (&optional include-linked)
    (interactive "P")
    (org-remove-inline-images)
    (org-display-inline-images include-linked))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "DI" #'lx/download-org-images)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "tR" #'lx/org-table-recalculate-multi-formulas)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "oy" #'lx/yank-babel-src)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "os" #'org-babel-execute-subtree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ob" #'org-babel-execute-buffer)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "sy" #'org-copy-subtree)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "bk" #'org-babel-remove-result-one-or-many)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ttt" #'org-table-transpose-table-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "TS" #'org-tree-slide-mode)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode ">" #'org-tree-slide-move-next-tree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "." #'org-tree-slide-move-next-tree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "<" #'org-tree-slide-move-previous-tree)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "TI" #'org-refresh-inline-images)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ic" 'org-cycle-list-bullet)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ik" 'org-move-item-up)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ij" 'org-move-item-down)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "gt" 'grip-mode)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "gs" 'grip-start-preview)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "gr" 'grip-restart-preview)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "go" 'grip-stop-preview)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "gb" 'grip-browse-preview)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ed" 'org-excalidraw-create-drawing)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ei" 'org-excalidraw-initialize)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "di" 'org-redisplay-inline-images)

  ;; (add-hook 'org-mode-hook #'turn-company-english-helper-on 100)


  (add-to-list 'org-babel-load-languages '(elasticsearch . t))
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (add-to-list 'org-babel-load-languages '(sql . t))
  (add-to-list 'org-babel-load-languages '(jq . t))

  (autoload 'org-babel-execute:jq "ob-jq")

  (global-org-modern-mode 1)

  (face-spec-set 'org-modern-done
                 '((t :inherit (org-done org-modern-label)
                      :weight semibold :foreground "lime green" :inverse-video t)))

  (org-link-set-parameters "excalidraw"
                           :follow 'org-excalidraw--open-file-from-svg
                           :image-data-fun (lambda (_protocol link _desc)
                                             (with-temp-buffer (insert-file-contents-literally link)
                                                               (buffer-substring-no-properties
                                                                (point-min)
                                                                (point-max)))))

  (require 'org-mouse))

;; (eval-after-load "org"
;;   '(orgit-link-set-parameters "orgit"
;;                               :store    'orgit-status-store
;;                               :follow   'orgit-status-open
;;                               :export   'orgit-status-export
;;                               :complete 'orgit-status-complete-link))

(with-eval-after-load 'org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("capture-html"
                 :protocol "capture-html"
                 :function org-protocol-capture-html--with-pandoc
                 :kill-client t)))

(with-eval-after-load 'evil-org
  (evil-define-key 'motion 'evil-org-mode (kbd "C-i") 'org-cycle))
