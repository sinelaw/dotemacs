; crosshairs

;; (require 'crosshairs)

;; (defadvice switch-to-buffer (after switch-to-buffer-flash-crosshairs activate)
;;   "Call `flash-crosshairs' after `switch-to-buffer'"
;;   (flash-crosshairs))

;; (defadvice select-window (around select-window-flash-crosshairs activate)
;;   "Call `flash-crosshairs' after `select-window', if switching to another buffer.
;; The check is necessary to prevent issues with mini-buffer switching."
;;   (let (cons (cur-buffer-name (buffer-name (current-buffer)))
;;              ad-arg-bindings)
;;     ad-do-it
;;     (unless (string= (buffer-name (window-buffer window))
;;                      cur-buffer-name)
;;       (flash-crosshairs))))

;; (ad-remove-advice 'select-window 'around 'select-window-flash-crosshairs)
;; (ad-remove-advice 'switch-to-buffer 'after 'switch-to-buffer-flash-crosshairs)
