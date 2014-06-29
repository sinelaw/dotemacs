(require 'haxe-mode)

(defvar *haxe-compiler* "haxe-lint")

(defun haxe-flymake-install ()
  (add-to-list
   'compilation-error-regexp-alist
   '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
     1 2 3))
  (message "haxe flymake installed")
  (let* ((key "\\.hx\\'")
         (haxeentry (assoc key flymake-allowed-file-name-masks)))
    (if haxeentry
        (setcdr haxeentry '(haxe-flymake-init haxe-flymake-cleanup))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'haxe-flymake-init 'haxe-flymake-cleanup)))))

(defun haxe-flymake-init ()
  (let ((create-temp-f 'flymake-create-temp-inplace)
        (use-relative-base-dir nil)
        (use-relative-source nil)
        (get-cmdline-f 'haxe-flymake-get-cmdline)
        args
        temp-source-file-name)
    (setq temp-source-file-name
	  (flymake-init-create-temp-buffer-copy create-temp-f)
          args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))

(defun haxe-flymake-get-cmdline (source base-dir)
  (list *haxe-compiler* (list source)))

(defun haxe-flymake-cleanup ()
  (flymake-simple-cleanup))

(defun my-haxe-mode-hook ()
  (haxe-flymake-install)
  (flymake-mode)
  (local-set-key [return] 'newline-and-indent))

(add-hook 'haxe-mode-hook 'my-haxe-mode-hook)
