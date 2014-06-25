(require 'haskell-mode)


; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
; (setq haskell-process-suggest-hoogle-imports nil)

(add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))

(require 'ghc)
(setq ghc-module-command "~/.cabal/bin/ghc-mod")

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun my-haskell-mode-hook ()
  (ghc-init)
  (local-set-key [(ctrl f4)] 'ghc-display-errors)
)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
