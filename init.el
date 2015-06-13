;;; Commentary;

;; Load paths
;;; Code:

(setq emacs-dir "~/.emacs.d/")
(defun in-emacs-d (path)
  (concat emacs-dir path))

(setq mode-dir (in-emacs-d "modes/"))
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
(setq inhibit-splash-screen t)

;; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)
(column-number-mode)

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

;; Auto-revert
(setq auto-revert-verbose nil)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

;; Show-paren
(setq show-paren-delay 0)
(show-paren-mode)

;; Smart-parens
(require 'smartparens)
; (require 'smartparens-config)
; (smartparens-global-strict-mode)

;; Global Linum
(global-linum-mode)
(setq-default indicate-empty-lines t)

;; Usability
(fset 'yes-or-no-p 'y-or-n-p) ; yes/no turns to y/n

;; starting a daemon process
(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; C comment edit mode
(require 'c-comment-edit)

; drag stuff
(require 'drag-stuff)
(setq drag-stuff-modifier '(super control))
(drag-stuff-global-mode t)

(defun my/compilation-hook ()
  (make-local-variable 'show-trailing-whitespace)
  (setq show-trailing-whitespace nil))
(add-hook 'compilation-mode-hook 'my/compilation-hook)

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

;; Always-on modes
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'ido-recentf-open)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)

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
    :command ("~/.emacs.d/bin/flymake-compat" source-inplace
	      source-original)
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

;; Company mode
(require 'company)

; (custom-set-variables '(company-ghc-show-info t))


;; Haskell
(require 'haskell-mode-autoloads)

;; GHC stuff mode
;;
(require 'ghc)
(setq ghc-module-command "~/.cabal/bin/ghc-mod")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun my/ghc-goto-first-error ()
  (interactive)
  (let ((orig (point)))
    (beginning-of-buffer)
    (let ((beg (point)))
      (ghc-goto-next-error)
      (if (eq (point) beg)
	  (goto-char orig))))
  )

(defun my/run-ghc-compilation ()
  (interactive)
  (save-some-buffers 1)
  (compile (concat "runhaskell -Wall " buffer-file-name))
  )

(defun my/original-haskell-mode-hook ()
  (ghc-init)
  (flycheck-mode)
  (setq ghc-display-error 'minibuffer)
  (turn-on-haskell-indentation)
  (company-mode)
  (define-key haskell-mode-map (kbd "C-x <up>") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-x <down>") 'haskell-navigate-imports-return)
  (define-key haskell-mode-map (kbd "C-,")
    (lambda () (interactive) (haskell-move-nested-left 2)))
  (define-key haskell-mode-map (kbd "C-.")
    (lambda () (interactive) (haskell-move-nested-right 2)))
  (define-key haskell-mode-map (kbd "C-c C-c")
    'my/run-ghc-compilation)
  (define-key haskell-mode-map (kbd "C-c r")
    'ghc-toggle-check-command)
  (define-key haskell-mode-map (kbd "C-x C-g .")
    'isearch-forward-symbol-at-point)
  (define-key haskell-mode-map (kbd "C-x C-g _")
    'isearch-forward-symbol)
  (define-key haskell-mode-map (kbd "C-x C-g w")
    'isearch-forward-word)
  (define-key haskell-mode-map (kbd "C-c C-d")
    'inferior-haskell-load-file)

  (define-key haskell-mode-map [f4] 'ghc-goto-next-error)
  (define-key haskell-mode-map [(ctrl f4)] 'my/ghc-goto-first-error)
  (define-key haskell-mode-map [(shift ctrl f4)] 'ghc-display-errors)
  (define-key haskell-mode-map [(ctrl c) f4] 'ghc-check-insert-from-warning)
  (define-key haskell-mode-map [(ctrl c) f5] 'haskell-mode-stylish-buffer)
)

(defun my/haskell-mode-hook ()
  (my/original-haskell-mode-hook)
)

(defun my/inferior-haskell-mode-hook()
  (make-local-variable 'show-trailing-whitespace)
  (setq show-trailing-whitespace nil))
(add-hook 'inferior-haskell-mode-hook 'my/inferior-haskell-mode-hook)


(require 'company-ghc)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;; (setq shm-program-name "/home/dan/src/haskell/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")
;; (add-to-list 'load-path "/home/dan/src/haskell/structured-haskell-mode/elisp")
;; (require 'shm)
(add-hook 'haskell-mode-hook 'my/haskell-mode-hook)

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
(defun my/after-make-frame-hook (&rest frame)
  (interactive)
  (if window-system
      (let ((f (if (car frame)
		   (car frame)
		 (selected-frame))))
	(progn
	  (set-face-background 'cursor "#00ff00" f)
	  (set-face-foreground 'mode-line "#dedede" f)))))
(add-hook 'after-make-frame-functions 'my/after-make-frame-hook t)

(defun my/emacsclient-post-frame-fixups ()
  (my/after-make-frame-hook (selected-frame))
  ;; (load-file custom-file)
  ;; (my/reset-default-face-font-height)
  )

;; Overlay highlighting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redisplay

(defface lawlist-active-region-face
  '((t (:background "#3c3c3c")))
  "Face for `lawlist-active-region-face`."
  :group 'init)

(defvar lawlist-redisplay-unhighlight-region-function
  (lambda (rol) (when (overlayp rol) (delete-overlay rol))))

(defvar lawlist-redisplay-highlight-region-function
  (lambda (start end window rol)
    (if (not (overlayp rol))
        (let ((nrol (make-overlay start end)))
          (funcall lawlist-redisplay-unhighlight-region-function rol)
          (overlay-put nrol 'window window)
          (overlay-put nrol 'face 'lawlist-active-region-face)
          (overlay-put nrol 'priority '(10000 . 100))
          nrol)
      (unless (and (eq (overlay-buffer rol) (current-buffer))
                   (eq (overlay-start rol) start)
                   (eq (overlay-end rol) end))
        (move-overlay rol start end (current-buffer)))
      rol)))   

(defun lawlist-redisplay--update-region-highlight (window)
  (with-current-buffer (window-buffer window)
    (let ((rol (window-parameter window 'internal-region-overlay)))
      (if (not (region-active-p))
          (funcall lawlist-redisplay-unhighlight-region-function rol)
        (let* ((pt (window-point window))
               (mark (mark))
               (start (min pt mark))
               (end   (max pt mark))
               (new
                (funcall lawlist-redisplay-highlight-region-function
                         start end window rol)))
          (unless (equal new rol)
            (set-window-parameter window 'internal-region-overlay
                                  new)))))))

(defun lawlist-redisplay--update-region-highlights (windows)
  (with-demoted-errors "lawlist-redisplay--update-region-highlights: %S"
    (if (null windows)
        (lawlist-redisplay--update-region-highlight (selected-window))
      (unless (listp windows) (setq windows (window-list-1 nil nil t)))
      (if highlight-nonselected-windows
          (mapc #'lawlist-redisplay--update-region-highlight windows)
        (let ((msw (and (window-minibuffer-p) (minibuffer-selected-window))))
          (dolist (w windows)
            (if (or (eq w (selected-window)) (eq w msw))
                (lawlist-redisplay--update-region-highlight w)
              (funcall lawlist-redisplay-unhighlight-region-function
                       (window-parameter w 'internal-region-overlay)))))))))

;; simple.el -- lines 4683 to 4684
(remove-function pre-redisplay-function #'redisplay--update-region-highlights)

(add-function :before pre-redisplay-function #'lawlist-redisplay--update-region-highlights)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; C++/C
(defun my/c-mode-hook ()
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
  (my/c-cc-mode-hook-set-keys)
  (setq tab-width 8))

(add-hook 'c-mode-hook 'my/c-mode-hook)

(defun my/c++-mode-hook ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (linum-mode)
  (my/c-cc-mode-hook-set-keys)
  (setq tab-width 4))
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

(defun my/c-instantiate-vtable ()
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

(defun my/c-prototype-comment ()
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

(defun my/align-c-function-parameters ()
  (interactive)
  (unless (region-active-p) (error "No region selected"))
  (align-regexp (region-beginning) (region-end) "[,(]\\(\\s-*\\)" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\()[ \t]*;\\)" 1 1 t)
  )

;; Use 'C-c v' to review the commit for the currently edited commit message

(defun my/magit-show-diff-current-head ()
  (interactive)
  (magit-diff "HEAD~1" "HEAD")
)

(defun my/magit-show-diff-current-head-working-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD")
)

(add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)
(add-hook 'magit-branch-manager-mode-hook 'my/git-magit-branches-mode-hook)

(require 'magit)
(require 'diff-hl)
(global-diff-hl-mode)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

(require 'buffer-move)

;; Rust

(require 'rust-mode)

;; D lang

(require 'd-mode)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(defun my/d-mode-hook ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (linum-mode)
  (my/d-mode-hook-set-keys)
  (setq tab-width 4))

(add-hook 'd-mode-hook 'my/d-mode-hook)

;; Markdown

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Repository root

(require 'repository-root)
(add-to-list 'repository-root-matchers repository-root-matcher/git)

(require 'sticky-windows)

(defun my/append-to-kill-ring (&optional arg)
  "Append to clipboard"
  (interactive "P")
  (append-next-kill)
  (kill-ring-save (mark) (point))
)

(defun my/open-repository-root-dir ()
  (interactive)
  (find-file (repository-root))
)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(defun my/generalized-shell-command (command arg) ;; From StackOverflow
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
selected, run a shell command just like M-x shell-command (M-!).  If
no region is selected and an argument is a passed, run a shell command
and place its output after the mark as in C-u M-x `shell-command' (C-u
M-!).  If a region is selected pass the text of that region to the
shell and replace the text in that region with the output of the shell
command as in C-u M-x `shell-command-on-region' (C-u M-|). If a region
is selected AND an argument is passed (via C-u) send output to another
buffer instead of replacing the text in region."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (if (eq arg nil)
            (shell-command command)
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))

(defun my/auto-spell ()
  (interactive)
  (let ((x (point)))
    (mark-whole-buffer)
    (my/generalized-shell-command "auto-spell - -" nil)
    (goto-char x)
    )
)

(require 'unbound)
(require 'highlight-symbol)

(setq highlight-symbol-on-navigation-p t)


;; Git grep

(defcustom git-grep-switches "--extended-regexp -I --no-color -n"
  "Switches to pass to `git grep'."
  :type 'string)

(require 'grep-a-lot)
(grep-a-lot-advise git-grep)

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

(defun my/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(defun my/git-comment-amend-no-questions ()
  (interactive)
  (shell-command "git commit --amend --no-edit --date=\"`date -R`\" -a")
  (magit-refresh-all)
  (diff-hl-update)
  )

(defun my/magit-log-new-frame ()
  (interactive)
  (make-frame-command)
  (magit-log)
  (delete-other-windows)
  )

(defun my/magit-file-log-y ()
  (interactive)
  (magit-file-log (buffer-file-name))
  )

(defun my/vc-visit-file-revision (file rev)
  "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  ;; based on `vc-revision-other-window'.
  (interactive
   (let ((file (expand-file-name
                (read-file-name
                 (if (buffer-file-name)
                     (format "File (%s): " (file-name-nondirectory
                                            (buffer-file-name)))
                   "File: ")))))
     (require 'vc)
     (unless (vc-backend file)
       (error "File %s is not under version control" file))
     (list file (vc-read-revision
                 "Revision to visit (default is working revision): "
                 (list file)))))
  (require 'vc)
  (unless (vc-backend file)
    (error "File %s is not under version control" file))
  (let ((revision (if (string-equal rev "")
                      (vc-working-revision file)
                    rev))
        (visit (if current-prefix-arg
                   'switch-to-buffer
                 'switch-to-buffer-other-window)))
    (funcall visit (vc-find-revision file revision))))

;; Workspaces
(require 'workspaces)

;; Global bindings
(global-set-key (kbd "C-;") (lambda () (interactive) (my/toggle-default-face-font-height)))
(global-set-key (kbd "C-v") 'yank)

;; Swap M-y and C-y
(global-set-key (kbd "M-y") 'yank)
(global-set-key (kbd "C-y") 'yank-pop)

(global-set-key (kbd "C-s-1") '(lambda () (interactive) (workspace-goto ?1)))
(global-set-key (kbd "C-s-2") '(lambda () (interactive) (workspace-goto ?2)))
(global-set-key (kbd "C-s-3") '(lambda () (interactive) (workspace-goto ?3)))
(global-set-key (kbd "C-s-4") '(lambda () (interactive) (workspace-goto ?4)))

(global-set-key [(control d)] 'highlight-symbol-prev)
(global-set-key [(control f)] 'highlight-symbol-next)
(global-set-key (kbd "M-^") 'highlight-symbol-query-replace)

(global-set-key (kbd "C-x <left>") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-x <delete>") 'diff-hl-revert-hunk)
(global-set-key (kbd "C-x <right>") 'diff-hl-next-hunk)

(global-set-key [(control b)] 'switch-to-buffer)
(global-set-key [(control l)] 'find-file)

(global-set-key (kbd "C-s-a") 'sp-beginning-of-sexp)
(global-set-key (kbd "C-s-d") 'sp-end-of-sexp)

(global-set-key (kbd "C-M-a") 'sp-backward-sexp)
(global-set-key (kbd "C-M-d") 'sp-forward-sexp)

(global-set-key (kbd "C-M-f") 'sp-down-sexp)
(global-set-key (kbd "C-M-b") 'sp-up-sexp)

(global-set-key (kbd "C-M-e") 'sp-backward-down-sexp)
(global-set-key (kbd "C-M-u") 'sp-backward-up-sexp)

(global-set-key (kbd "C-M-n") 'sp-next-sexp)
(global-set-key (kbd "C-M-p") 'sp-previous-sexp)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)
(global-set-key (kbd "C-M-w") 'sp-copy-sexp)

(global-set-key (kbd "M-<delete>") 'sp-splice-sexp)
(global-set-key (kbd "C-]") 'sp-select-next-thing-exchange)

(global-set-key [(control meta g)] 'my/kill-current-buffer)
(global-set-key [(meta g)] 'goto-line)

(global-set-key [(control f1)] 'ibuffer)
(global-set-key [(shift control f1)] 'recentf-open-files)

(global-set-key [(control f2)] 'dired-jump)

(global-set-key [(control prior)] 'backward-paragraph)
(global-set-key [(control next)] 'forward-paragraph)

(global-set-key [f3] 'my/spawn-dup-in-current-dir)
(global-set-key [(control f3)] 'magit-log)
(global-set-key [(control x) (control f3)] 'my/magit-log-new-frame)
(global-set-key [(control x) (v) (e)] 'my/magit-show-diff-current-head-working-tree)
(global-set-key [(control x) (v) (S)] 'magit-status)
(global-set-key (kbd "C-x v <return>") 'my/git-comment-amend-no-questions)

(global-set-key (kbd "C-x v <up>") 'buf-move-up)
(global-set-key (kbd "C-x v <down>") 'buf-move-down)
(global-set-key (kbd "C-x v <right>") 'buf-move-right)
(global-set-key (kbd "C-x v <left>") 'buf-move-left)
(global-set-key (kbd "C-x v <insert>") 'magit-commit)
(global-set-key (kbd "C-x v l") 'magit-log)

(global-unset-key [(control meta r)])  ;; isearch-backward-regexp
(global-set-key [(control meta r)] 'my/open-repository-root-dir)

(global-set-key (kbd "C-!") 'delete-other-windows)

(global-unset-key [(control n)]) ;; next-line
(global-unset-key [(control p)]) ;; previous-line
(global-unset-key [(control q)]) ;; quoted-insert
(global-set-key [(control q)] 'magit-file-log)
(global-unset-key (kbd "C--")) ;; negative-arugment
(global-unset-key (kbd "C-/")) ;; undo
(global-unset-key (kbd "C-\\")) ;; toggle-input-mode
(global-set-key (kbd "C-\\") 'my/append-to-kill-ring)
(global-unset-key (kbd "C-@")) ;; set-mark-command
(global-unset-key (kbd "C-_")) ;; undo
(global-unset-key (kbd "M-=")) ;; count-words-region
(global-set-key (kbd "M-=") 'magit-branch-manager)
(global-unset-key (kbd "M-a")) ;; backward-sentence
(global-set-key (kbd "M-a") 'my/magit-show-diff-current-head-working-tree)
(global-set-key (kbd "M-n") 'magit-status)
(global-unset-key (kbd "M-c")) ;; capitalize-word
(global-set-key (kbd "M-c") 'copy-to-register)
(global-unset-key (kbd "M-e")) ;; forward-sentence
(global-set-key (kbd "M-e") 'magit-log)
(global-unset-key (kbd "M-f")) ;; forward-word
(global-set-key (kbd "M-f") 'my/magit-file-log-y)
(global-unset-key (kbd "M-i")) ;; tab-to-tab-stop
(global-set-key (kbd "M-i") 'insert-register)
(global-unset-key (kbd "M-k")) ;; kill-sentence
(global-set-key (kbd "M-k") 'highlight-symbol-query-replace)
(global-unset-key (kbd "M-t")) ;; transport-words
(global-set-key (kbd "M-t") 'query-replace)
(global-unset-key (kbd "M-v")) ;; scroll-down-command
(global-set-key (kbd "M-v") 'winner-undo)
(global-unset-key (kbd "M-b")) ;; backward-word
(global-set-key (kbd "M-b") 'winner-redo)
(global-unset-key (kbd "M-{")) ;; backward-paragraph
(global-set-key (kbd "M-{") 'sp-beginning-of-sexp)
(global-unset-key (kbd "M-}")) ;; forward-paragraph
(global-set-key (kbd "M-}") 'sp-end-of-sexp)
(global-unset-key (kbd "M-~")) ;; not-modified
(global-unset-key (kbd "M-SPC")) ;; just-one-space

(defun my/git-commit-mode-hook ()
  (local-set-key [(control c) (v)] 'my/magit-show-diff-current-head)
)

(defun my/git-magit-branches-mode-hook ()
  (local-set-key (kbd "M-n") 'magit-status)
)

(eval-after-load "magit"
  '(define-key magit-diff-mode-map (kbd "k") 'magit-revert-item))

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

(defun my/c-cc-mode-hook-set-keys ()
  (local-set-key [return] 'newline-and-indent)
  (local-unset-key [(control d)])
  (local-unset-key [(meta e)])
  (local-unset-key [(meta a)])
)

(defun my/d-mode-hook-set-keys ()
  (local-set-key [return] 'newline-and-indent))

(defun my/grep-a-lot-setup-keys()
  "Define some key bindings for navigating multiple
grep search results buffers."
  (interactive)
  (global-set-key [(control h) left] 'grep-a-lot-goto-prev)
  (global-set-key [(control h) right] 'grep-a-lot-goto-next)
  (global-set-key [(control h) up] 'grep-a-lot-pop-stack)
  (global-set-key [(control h) down] 'grep-a-lot-clear-stack)
  (global-set-key [(control h) home] 'grep-a-lot-restart-context)
  )

(my/grep-a-lot-setup-keys)

(global-set-key (kbd "M-p") 'shrink-window-horizontally)
(global-set-key (kbd "M-[") 'enlarge-window-horizontally)
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)

;; Mouse

(global-set-key [(meta mouse-4)] 'drag-stuff-up)
(global-set-key [(meta mouse-5)] 'drag-stuff-down)

;;; init.el ends here
