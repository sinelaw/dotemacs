(require 'haskell-mode)

;; GHC stuff mode
(add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))
(require 'ghc)
(setq ghc-module-command "~/.cabal/bin/ghc-mod")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;; Haskell mode

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

(defun my-original-haskell-mode-hook ()
  (ghc-init)
  (turn-on-haskell-indentation)
)

(defun my-haskell-mode-hook ()
  (my-original-haskell-mode-hook)
)

(setq shm-program-name "/home/dan/src/haskell/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")
(add-to-list 'load-path "/home/dan/src/haskell/structured-haskell-mode/elisp")
(require 'shm)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(define-key haskell-mode-map (kbd "C-,") 
  (lambda () (interactive) (haskell-move-nested-left 2)))
(define-key haskell-mode-map (kbd "C-.") 
  (lambda () (interactive) (haskell-move-nested-right 2)))
