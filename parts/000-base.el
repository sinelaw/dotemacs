(require 'dash)
(require 's)

;; Make the super key work in Windows
(setq
 w32-pass-lwindow-to-system nil
 w32-pass-rwindow-to-system nil
 w32-pass-apps-to-system nil
 w32-lwindow-modifier 'super ; Left Windows key
 w32-rwindow-modifier 'super ; Right Windows key
 w32-apps-modifier 'hyper) ; Menu key

(setq init-parts-blacklist
  '(
    "-ibuffer.el"
    "-dired.el"
    "-helm.el"
    ))

;; Based on stuff from github.com/Fuuzetsu/.emacs.d

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))
