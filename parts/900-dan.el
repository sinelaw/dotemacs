; diff-hl
(add-to-list 'load-path (in-modes-d "diff-hl"))
(require 'diff-hl)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

; Dired-autoload
(autoload 'dired-jump "dired-x"
    "Jump to dired buffer corresponding to current buffer."
    'interactive)
(autoload 'dired-jump-other-window "dired-x"
    "Like \\[dired-jump] (`dired-jump') but in other window."
    'interactive)

; Color theme
(cond
 ((>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
  (load-theme 'tomorrow-night-bright t))
 ((< emacs-major-version 24)
  (add-to-list 'load-path (in-emacs-d "legacy/themes/"))
  (load-library "color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-dark-laptop)))

(require 'flycheck)

(flycheck-define-checker my/flymake-compatible-checker
    "A syntax checker using make."
    :command ("flymake-compat" source-inplace)
    :error-patterns
    ((error line-start
           (message "In file included from") " " (file-name) ":" line ":"
           line-end)
    (info line-start (file-name) ":" line ":" column
          ": note: " (message) line-end)
    (warning line-start (file-name) ":" line ":" column
          ": warning: " (message) line-end)
    (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
   :error-filter
   (lambda (errors)
     (flycheck-fold-include-errors
      (flycheck-sanitize-errors errors) "In file included from"))
   :modes (c-mode c++-mode))

(global-flycheck-mode)

(set-face-attribute 'flycheck-error nil :background "#990000")
(set-face-attribute 'flycheck-warning nil :background "#505000")

; Emacs 24 bugfix for face value after new-frame
(defun my-after-make-frame-hook (&rest frame)
  (if window-system
      (let ((f (if (car frame)
                   (car frame)
                 (selected-frame))))
        (progn
          (set-face-foreground 'mode-line "#dedede" f)))))
(add-hook 'after-make-frame-functions 'my-after-make-frame-hook t)

(defun my/spawn-dup-in-current-dir ()
  "---"
  (interactive)
  (shell-command-to-string "setsid dup >/dev/null 2>/dev/null &")
)

(defun my/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))
  )
)

(defun my/set-default-face-height (h)
  (set-face-attribute 'default nil :height h)
)

; Editing utility functions

(defvar my/tag-browsing-list '())
(setq my/tag-browsing-list '())

(defun my/find-tag ()
  "Find the next definition of the tag already specified, but in
   another window only if we have started browsing tags"
  (interactive)
  (let ((this (current-buffer)))
    (if (memql this my/tag-browsing-list)
      (progn
          (find-tag (current-word))
          (if (not (eq this (current-buffer)))
              (add-to-list 'my/tag-browsing-list (current-buffer)))
        )
      (save-selected-window
        (save-excursion
          (find-tag-other-window (current-word))
          (setq my/tag-browsing-list '())
          (if (not (eq this (current-buffer)))
              (add-to-list 'my/tag-browsing-list (current-buffer)))
        ))
     )
  )
)

(defun my/auto-find-replace-regex ()
  "---"
  (interactive)
  (let (pt end sp)
    (beginning-of-buffer)
    (search-forward " EMACS REFACTOR: ")
    (setq pt (point))
    (end-of-line)
    (setq end (point))
    (goto-char pt)
    (setq sp (split-string (buffer-substring pt end) " --> "))
    (beginning-of-line)
    (delete-region (point) (+ end 1))
    (save-excursion
       (replace-regexp (nth 0 sp) (nth 1 sp)))
  )
)

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(control prior)] 'gcm-scroll-up)
(global-set-key [(control next)]  'gcm-scroll-down)

(defcustom git-grep-switches "--extended-regexp -I --no-color -n"
  "Switches to pass to `git grep'."
  :type 'string)

(defun git-grep (command-args)
  ;; Read command-args
  (interactive
   (let ((root (vc-git-root default-directory)))
     (if root
       (list
          (read-shell-command
           "Run git-grep (like this): "
           (format (concat
                    "cd %s && "
                    "git --no-pager grep %s -e '\\b%s\\b'")
                   root
                   git-grep-switches
                   (let ((thing (and
                                 buffer-file-name
                                 (thing-at-point 'symbol))))
                     (or (and thing (progn
                                      (set-text-properties 0 (length thing) nil thing)
                                      (shell-quote-argument (regexp-quote thing))))
                         "")))
           'git-grep-history))
       (list))))

   ;; Do the actual work
   (if command-args
     (let ((grep-use-null-device nil))
       (grep command-args))
     (message "Not a git tree"))
  )
