;; Load paths
(setq emacs-dir "~/.emacs.d/")
(defun in-emacs-d (path)
  (concat emacs-dir path))
(setq mode-dir (in-emacs-d "modes/"))
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path mode-dir)

(let ((default-directory mode-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Custom file
(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

(add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
(load-theme 'tomorrow-night-bright t)

;; Misc
(setq global-auto-revert-non-file-buffers t)
(setq inhibit-splash-screen t)

;; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)

;; Window settings
(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(tool-bar-mode 0)
(scroll-bar-mode t)
(modify-all-frames-parameters '((scroll-bar-width . 10)))

; winner mode - Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Don't warn on some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Show-paren
(setq show-paren-delay 0)
(show-paren-mode)

(global-linum-mode)

;; Usability
(fset 'yes-or-no-p 'y-or-n-p) ; yes/no turns to y/n

;; starting a daemon process
(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; Always-on modes
(ido-mode)
(require 'ido-recentf-open)
(recentf-mode 1)
(put 'ido-exit-minibuffer 'disabled nil)
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

(require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Flycheck
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

;; Haskell
(require 'haskell-mode-autoloads)

;; GHC stuff mode
;;
;; ghc-mod disabled until memory consumption issues are sorted out!
;;
;; (require 'ghc)
;; (setq ghc-module-command "~/.cabal/bin/ghc-mod")
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

(defun my-structured-haskell-mode-hook ()
  (ghc-init)
  (structured-haskell-mode)
  (local-set-key [return] 'shm/newline-indent)
  (local-set-key [delete] 'delete-char) ;; Instead of shm/delete
  (local-set-key [tab] 'shm/backtab)
  (local-unset-key (kbd "("))
  (local-unset-key (kbd ")"))
  (local-set-key [(ctrl f4)] 'ghc-display-errors)
)

(defun my/ghc-goto-next-error ()
  (interactive)
  (beginning-of-buffer)
  (ghc-goto-next-error)
  )

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-x <up>") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-x <down>") 'haskell-navigate-imports-return)
  (define-key haskell-mode-map (kbd "C-,") 
    (lambda () (interactive) (haskell-move-nested-left 2)))
  (define-key haskell-mode-map (kbd "C-.") 
    (lambda () (interactive) (haskell-move-nested-right 2)))  
  (define-key haskell-mode-map (kbd "C-x C-g .")
    'isearch-forward-symbol-at-point)
  (define-key haskell-mode-map (kbd "C-x C-g _")
    'isearch-forward-symbol)
  (define-key haskell-mode-map (kbd "C-x C-g w")
    'isearch-forward-word)
  ;; ghc-mod disabled
  ;;
  ;; (define-key haskell-mode-map [f4] 'ghc-goto-next-error)
  ;; (define-key haskell-mode-map [(ctrl f4)] 'my/ghc-goto-next-error)
  ;; (define-key haskell-mode-map [(shift ctrl f4)] 'ghc-display-errors)
  ;; (define-key haskell-mode-map [(ctrl c) f4] 'ghc-check-insert-from-warning)
  ;; (define-key haskell-mode-map [(ctrl c) f6] 'ghc-extract-type)
  (define-key haskell-mode-map [(ctrl c) f5] 'haskell-mode-stylish-buffer)
 ))

(defun my-original-haskell-mode-hook ()
;;   (ghc-init)
  (setq ghc-display-error 'minibuffer)  
  (turn-on-haskell-indentation)
)

(defun my-haskell-mode-hook ()
  (my-original-haskell-mode-hook)
)

;; (setq shm-program-name "/home/dan/src/haskell/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")
;; (add-to-list 'load-path "/home/dan/src/haskell/structured-haskell-mode/elisp")
;; (require 'shm)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defvar dax-tag-browsing-list '())

(defun dax-find-tag ()
  "Find the next definition of the tag already specified, but in
   another window only if we have started browsing tags"
  (interactive)
  (let ((this (current-buffer)))
    (if (memql this dax-tag-browsing-list)
      (progn
          (find-tag (current-word))
          (if (not (eq this (current-buffer)))
              (add-to-list 'dax-tag-browsing-list (current-buffer)))
        )
      (save-selected-window
        (save-excursion
          (find-tag-other-window (current-word))
          (setq dax-tag-browsing-list '())
          (if (not (eq this (current-buffer)))
              (add-to-list 'dax-tag-browsing-list (current-buffer)))
        ))
     )
  )
)

(require 'align)
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(defun my/haskell-cabal-mode-hook ()
  (setq indent-tabs-mode nil)
)
(add-hook 'haskell-cabal-mode-hook 'my/haskell-cabal-mode-hook)

;; Shell

(defun my/shell-script-mode-hook ()
  (setq indent-tabs-mode nil)
)
(add-hook 'shell-script-mode-hook 'my/shell-script-mode-hook)

;; Utility functions
(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

; Emacs 24 bugfix for face value after new-frame
(defun my-after-make-frame-hook (&rest frame)
  (if window-system
      (let ((f (if (car frame)
                   (car frame)
                 (selected-frame))))
        (progn
	  (set-face-background 'cursor "#00ff00" f)
          (set-face-foreground 'mode-line "#dedede" f)))))
(add-hook 'after-make-frame-functions 'my-after-make-frame-hook t)

(defun emacsclient-post-frame-fixups ()
  (my-after-make-frame-hook (selected-frame))
  ;; (load-file custom-file)
  ;; (my/reset-default-face-font-height)
  )

;; C++/C

(defun my-c-mode-hook ()
;;  (whitespace-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 8)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode t)
  (linum-mode)
  (local-set-key [return] 'newline-and-indent)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (linum-mode)
  (local-set-key [return] 'newline-and-indent)
  (setq tab-width 4))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun c-instantiate-vtable ()
  "---"
  (interactive)
  (let (orig pt tmp start end struct-name functions prefix var-name)
    (setq orig (point))
    (search-backward "{")
    (setq pt (point))
    (beginning-of-line)
    (setq tmp (buffer-substring-no-properties pt (point)))
    (setq functions '())

    (if (string-match "struct \\([a-zA-Z0-9_]+\\) " tmp)
        (let ()
          (setq struct-name (match-string 1 tmp))
          (setq start (point))
          (search-forward "}")
          (setq end (point))
          (goto-char start)
          (while (search-forward "(*" end t)
            (let (funcname funcproto start-proto)
              (setq tmp (point))
              (if (search-forward ")(" end t)
                  (let (a)
                    (setq funcname (buffer-substring-no-properties tmp (- (point) 2)))
                    (save-excursion
                      (beginning-of-line)
                      (setq start-proto (point))
                      (search-forward ";" end)
                      (setq funcproto (buffer-substring-no-properties start-proto (- (point) 1)))
                      )
                    (push (list funcname funcproto) functions)
                    ))
            )
          )
          (setq var-name (read-string "Variable: "))
          (setq prefix (read-string "Prefix: "))

          (delete-region start end)
          (mapc (lambda (f)
             (let ((funcname (nth 0 f))
                   (funcproto (nth 1 f)))
               (setq funcproto (replace-regexp-in-string "([*]" (concat prefix "_") funcproto))
               (setq funcproto (replace-regexp-in-string ")(" "(" funcproto))
               (setq funcproto
                     (if (string-match "\\`[ \t\n\r]+" funcproto)
                         (replace-match "" t t funcproto) funcproto))
               (insert funcproto)
               (newline)
               (insert "{")
               (newline)
               (insert "}")
               (newline)
               (newline)
             ))
             functions)
          (insert "struct " struct-name " " var-name " = {")
          (mapc (lambda (f)
             (let ((funcname (nth 0 f))
                   (funcproto (nth 1 f)))
               (newline-and-indent)
               (insert "." funcname " = " prefix "_" funcname ",")
             ))
             functions)
          (newline)
          (insert "}")
        ))
  )
)

(defun c-prototype-comment ()
  "---"
  (interactive)
  (let (orig pt funcname end params i)
    (setq orig (point))
    (setq params '())
    (search-forward "(")
    (setq pt (point))
    (save-excursion
      (search-backward " ")
      (setq funcname (buffer-substring-no-properties (+ (point) 1) (- pt 1))))
    (save-excursion
      (search-forward ")")
      (setq end (point)))
    (beginning-of-line)
    (save-excursion
      (catch 'foo
        (while (< (point) end)
          (if (not (search-forward "," end t))
              (if (not (search-forward ")" end t))
                  (throw 'foo t)))
          (save-excursion
            (backward-char)
            (let ((start (point)))
              (search-backward-regexp "[^a-zA-Z0-9_]")
              (forward-char)
              (push (buffer-substring-no-properties (point) start) params)
              )
            ))))
    (insert "\n")
    (insert "/**\n")
    (insert " * " funcname "()\n")
    (insert " *\n")
    (dolist (i (reverse params))
      (insert " * @param " i "\n"))
    (insert " *\n")
    (insert " * \n")
    (insert " *\n")
    (insert " * @returns\n")
    (insert " */\n")
  )
)

;; Use 'C-c v' to review the commit for the currently edited commit message

(defun my/magit-show-diff-current-head ()
  (interactive)
  (magit-diff "HEAD~1" "HEAD")
)

(defun my/git-commit-mode-hook ()
  (local-set-key [(control c) (v)] 'my/magit-show-diff-current-head)
)

(add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)

(require 'magit)

;; Git grep
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
                    "'\\b%s\\b'")
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
     (let ((grep-use-null-device nil)
	   (root (vc-git-root default-directory)))
       (grep (format (concat "cd %s && git --no-pager grep %s -e %s") root git-grep-switches command-args)))
     (message "Not a git tree"))
  )

(defun my/spawn-dup-in-current-dir ()
  "---"
  (interactive)
  (shell-command-to-string "setsid dup >/dev/null 2>/dev/null &")
)

(defun my/set-default-face-height (h)
  (set-face-attribute 'default nil :height h)
)


(setq my/toggle-default-face-font-height-large nil)

(defun my/reset-default-face-font-height ()
  (interactive)
  (if (eq my/toggle-default-face-font-height-large nil)
      (my/set-default-face-height 105)
      (my/set-default-face-height 150)
    )
  )

(defun my/toggle-default-face-font-height ()
  (interactive)
  (setq my/toggle-default-face-font-height-large (not my/toggle-default-face-font-height-large))
  (my/reset-default-face-font-height)
  )

;; Global bindings
(global-set-key (kbd "C-;") (lambda () (interactive) (my/toggle-default-face-font-height)))
(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "C-x <up>") 'buf-move-up)
(global-set-key (kbd "C-x <down>") 'buf-move-down)
(global-set-key (kbd "C-x <right>") 'buf-move-right)
(global-set-key (kbd "C-x <left>") 'buf-move-left)

(global-set-key (kbd "M-^") 'highlight-symbol-query-replace)

(global-set-key [(control b)] 'switch-to-buffer)
(global-set-key [(control l)] 'find-file)

(global-set-key [(control meta g)] 'my/kill-current-buffer)
(global-set-key [(meta g)] 'goto-line)

(global-set-key [(control f1)] 'ibuffer)
(global-set-key [(shift control f1)] 'recentf-open-files)

(global-set-key [(control f2)] 'dired-jump)

(global-set-key [(control f3)] 'my/spawn-dup-in-current-dir)
(global-set-key [(control shift f3)] 'vc-print-root-log)

(global-set-key [f4] 'next-error)
(global-set-key [(ctrl f4)] 'flycheck-first-error)
(global-set-key [(shift ctrl f4)] 'flycheck-list-errors)

(global-set-key [(control f5)] 'ff-find-other-file)
(global-set-key [f5] 'switch-to-prev-buffer)

(global-set-key [(control tab)] 'other-window)
(global-set-key [(control z)] 'undo)
(global-set-key [(f9)] 'compile)

(global-set-key [(meta f9)] 'recompile)
(global-set-key [(control f9)] 'git-grep)
(global-set-key [(shift control f9)] 'grep)

(global-set-key [f11] 'delete-window)
(global-set-key [f12] 'call-last-kbd-macro)
