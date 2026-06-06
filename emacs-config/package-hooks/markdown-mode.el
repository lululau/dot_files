;; -*- lexical-binding: t; -*-

(with-eval-after-load 'markdown-mode
  ;; Extend code block background to window right edge
  (set-face-attribute 'markdown-code-face nil :extend t)

  ;; org-modern style box-drawn tables for markdown
  (add-hook 'markdown-mode-hook #'markdown-modern-mode)

  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "'" 'markdown-edit-code-block)
  (evil-define-key 'motion markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (evil-define-key 'normal markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "M-h") nil)
  (define-key markdown-mode-map (kbd "M-j") nil)
  (define-key markdown-mode-map (kbd "M-k") nil)
  (define-key markdown-mode-map (kbd "M-l") nil)
  (define-key markdown-mode-map (kbd "M-H") nil)
  (define-key markdown-mode-map (kbd "M-J") nil)
  (define-key markdown-mode-map (kbd "M-K") nil)
  (define-key markdown-mode-map (kbd "M-L") nil)
  (define-key markdown-mode-map (kbd "S-<tab>") 'markdown-shifttab)
  ;; (add-hook 'markdown-mode-hook #'turn-company-english-helper-on 100)

  (defun markdown-before-first-heading-p ()
    "Return non-nil if point is before the first heading."
    (save-excursion
      (let ((pos (point)))
        (goto-char (point-min))
        (let ((first-heading (markdown-next-heading)))
          (or (not first-heading) (< pos first-heading))))))

  (defun markdown-show-branches-buffer ()
    "Show all branches in the buffer."
    (outline-hide-sublevels 1)
    (save-excursion
      (goto-char (point-min))
      (when (markdown-next-heading)
        (outline-show-branches)
        (while (outline-get-next-sibling)
          (outline-show-branches))))
    (goto-char (point-min))
    (markdown-outline-fix-visibility))

  (defun markdown-show-branches ()
    "Hide subtree body but show child headings, like `org-kill-note-or-show-branches'.
When before the first heading, show all branches in the buffer."
    (interactive)
    (cond
     ((markdown-before-first-heading-p)
      (markdown-show-branches-buffer)
      (message "BRANCHES"))
     (t
      (markdown-back-to-heading)
      (outline-hide-subtree)
      (outline-show-children 1000)
      (setq markdown-cycle-subtree-status 'children)
      (message "CHILDREN"))))

  (define-key markdown-mode-map (kbd "C-c C-k") #'markdown-show-branches)
  (define-key markdown-mode-map [remap outline-show-branches] #'markdown-show-branches)

  (defun markdown-cycle (&optional arg)
    "Visibility cycling for Markdown mode.
  This function is called with a `\\[universal-argument]' or if ARG is t, perform
  global visibility cycling.  If the point is at an atx-style header, cycle
  visibility of the corresponding subtree.  Otherwise, indent the current line
  or insert a tab, as appropriate, by calling `indent-for-tab-command'."
    (interactive "P")
    (cond

    ;; Global cycling
    (arg
      (cond
      ;; Move from overview to contents
      ((and (eq last-command this-command)
            (eq markdown-cycle-global-status 2))
        (outline-hide-sublevels 1)
        (message "CONTENTS")
        (setq markdown-cycle-global-status 3)
        (markdown-outline-fix-visibility))
      ;; Move from contents to all
      ((and (eq last-command this-command)
            (eq markdown-cycle-global-status 3))
        (outline-show-all)
        (message "SHOW ALL")
        (setq markdown-cycle-global-status 1))
      ;; Defaults to overview
      (t
        (outline-hide-body)
        (message "OVERVIEW")
        (setq markdown-cycle-global-status 2)
        (markdown-outline-fix-visibility))))

    ;; At a heading: rotate between three different views
    ((save-excursion (beginning-of-line 1) (markdown-on-heading-p))
      (markdown-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (markdown-back-to-heading)
          (save-excursion
            (beginning-of-line 2)
            (while (and (not (eobp)) ;; this is like `next-line'
                        (get-char-property (1- (point)) 'invisible))
              (beginning-of-line 2)) (setq eol (point)))
          (markdown-end-of-heading)   (setq eoh (point))
          (markdown-end-of-subtree t)
          (skip-chars-forward " \t\n")
          (beginning-of-line 1) ; in case this is an item
          (setq eos (1- (point))))
        ;; Find out what to do next and set `this-command'
        (cond
        ;; Nothing is hidden behind this heading
        ((= eos eoh)
          (message "EMPTY ENTRY")
          (setq markdown-cycle-subtree-status nil))
        ;; Entire subtree is hidden in one line: open it
        ((>= eol eos)
          (markdown-show-entry)
          (outline-show-children)
          (message "CHILDREN")
          (setq markdown-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        ((and (eq last-command this-command)
              (eq markdown-cycle-subtree-status 'children))
          (outline-show-subtree)
          (message "SUBTREE")
          (setq markdown-cycle-subtree-status 'subtree))
        ;; Default action: hide the subtree.
        (t
          (outline-hide-subtree)
          (message "FOLDED")
          (setq markdown-cycle-subtree-status 'folded)))))

    ;; In a table, move forward by one cell
    ((markdown-table-at-point-p)
      (call-interactively #'markdown-table-forward-cell))

    ;; Otherwise, indent as appropriate
    (t
      (call-interactively #'lx/tab))))

  )
