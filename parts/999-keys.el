;; Mac-specific
(setq ns-function-modifier 'hyper)

;; Home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)

;; Page Up/Down half screen scroll
(global-set-key (kbd "<prior>") 'scroll-down-command)
(global-set-key (kbd "<next>") 'scroll-up-command)

;; F keys
(eval-after-load "c-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(eval-after-load "cc-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(global-set-key [(f9)] 'projectile-compile-project)
(global-set-key [(f10)] 'previous-error)
(global-set-key [(f11)] 'next-error)

;; deleting trailing whitespaces
;; (global-set-key [(f12)] 'delete-trailing-whitespace)

;; Open shell with C-z
;; (global-set-key (kbd "C-z") 'shell)

;; Browse URLs with C-x /
(global-set-key (kbd "C-x /") 'browse-url)

;; Override news with man
(global-set-key (kbd "C-h n") 'man)

;; Scroll without moving the cursor
(global-set-key "\C-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\C-p"  (lambda () (interactive) (scroll-down 4)) )
(global-set-key "\M-n"  (lambda () (interactive) (scroll-other-window   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-other-window-down 4)) )

;; Window moving
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-left>") 'windmove-left)

;; zooming
;(global-set-key (kbd "C-}") 'text-scale-increase)
;(global-set-key (kbd "C-{") 'text-scale-decrease)

;; commenting/uncommenting
(defun my/comment-or-uncomment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-x ;") 'my/comment-or-uncomment-current-line)

;; Error jumping
(global-set-key (kbd "C-x <C-down>") 'next-error)
(global-set-key (kbd "C-x <C-up>") 'previous-error)

;; Line joining
(add-hook 'prog-mode-hook '(lambda () (global-set-key (kbd "M-j")
                                           (lambda ()
                                             (interactive)
                                             (join-line -1)))))

;; override weird keys on js2-mode
(define-key js2-mode-map (kbd "M-j") nil)
(define-key js2-mode-map (kbd "C-c C-e") nil)




;; Kill/save the active region or the current line
(defun kill-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-w") 'kill-line-or-region)

(defun save-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-w") 'save-line-or-region)

;; Goto function definitions
(global-set-key (kbd "C-h C-f") 'find-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dan's additions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-}") (lambda () (interactive) (enlarge-window-horizontally 3)))
(global-set-key (kbd "C-{") (lambda () (interactive) (shrink-window-horizontally 3)))
(global-set-key (kbd "C-\"") (lambda () (interactive) (enlarge-window 3)))
(global-set-key (kbd "C-|") (lambda () (interactive) (shrink-window 3)))
(global-set-key (kbd "C-;") (lambda () (interactive) (my/set-default-face-height 150)))
(global-set-key (kbd "C-:") (lambda () (interactive) (my/set-default-face-height 105)))

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x <up>") 'buf-move-up)
(global-set-key (kbd "C-x <down>") 'buf-move-down)
(global-set-key (kbd "C-x <right>") 'buf-move-right)
(global-set-key (kbd "C-x <left>") 'buf-move-left)

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key [(control b)] 'switch-to-buffer)
(global-set-key [(control c) (m)] 'mc/edit-lines)
(global-set-key [(control l)] 'find-file)
(global-set-key [(control meta g)] 'my/kill-current-buffer)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(control z)] 'undo)

(global-set-key [(f7)] 'magit-status)
(global-set-key [(control x) (f7)] 'magit-branch-manager)

(global-set-key [(control f1)] 'ibuffer)
(global-set-key [(shift control f1)] 'recentf-open-files)

(global-set-key [(control f2)] 'dired-jump)

(global-set-key [(control f3)] 'my/spawn-dup-in-current-dir)
(global-set-key [(control shift f3)] 'vc-print-root-log)

(global-set-key [f4] 'next-error)
(global-set-key [(meta f4)] 'flymake-goto-next-error)
(global-set-key [(ctrl f4)] 'flymake-display-err-menu-for-current-line)

(global-set-key [(control f5)] 'ff-find-other-file)
(global-set-key [f5] 'switch-to-prev-buffer)

(global-set-key [(control f6)] 'my/switch-prev-buffer)
(global-set-key [f6] 'switch-to-next-buffer)

(global-set-key [(f9)] 'compile)
(global-set-key [(meta f9)] 'recompile)
(global-set-key [(control f9)] 'grep)

(global-set-key [f11] 'delete-window)
(global-set-key [f12] 'call-last-kbd-macro)
