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
