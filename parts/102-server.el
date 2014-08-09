;; Always use emacs as daemon, and emacsclient to open new frames. Here
;; is some code to help to in that mode.

(defun remove-last-file-name-history ()
  "Add to server-switch-hook to omit emacsclient's temporary filenames from the history."
  (setq file-name-history (cdr file-name-history)))
(add-hook 'server-switch-hook 'remove-last-file-name-history)

(defun server-edit-or-close ()
  "Saves and calls `server-edit', if opened by server, or kills buffer."
  (interactive)
  (save-buffer)
  (if server-buffer-clients
      (server-edit)
    (kill-this-buffer)))

(defun emacsclient-post-frame-fixups ()
  (my-after-make-frame-hook (selected-frame))
  (load-file custom-file)
  (my/reset-default-face-font-height)
)
