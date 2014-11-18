(require 'haskell-mode)

;; GHC stuff mode
(add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))
(require 'ghc)
(setq ghc-module-command "~/.cabal/bin/ghc-mod")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;; Haskell mode
(defun my-haskell-mode-hook ()
  (ghc-init)
;;
;; The original haskell mode
;;  (turn-on-haskell-simple-indent)
;;  (local-set-key [return] 'newline) ;; need this because newline-and-indent is annoying me
;;
;; Instead:
;;
  (structured-haskell-mode)
  (local-set-key [return] 'shm/newline-indent)
  (local-set-key [delete] 'delete-char) ;; Instead of shm/delete
  (local-set-key [tab] 'shm/backtab)
  (local-set-key [backtab] 'shm/tab)
  (local-set-key [(ctrl f4)] 'ghc-display-errors)
)

(setq shm-program-name "/home/dan/src/haskell/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")
(add-to-list 'load-path "/home/dan/src/haskell/structured-haskell-mode/elisp")
(require 'shm)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
