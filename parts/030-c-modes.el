(defun my/c-mode-new-block (prefix) (interactive "P")
  "Opens a new C-style (curly-brace) block after the current line.
With prefix arg, forces opening brace to be in a new line"
  (beginning-of-line)
  (back-to-indentation)
  (let ((current-statement (current-word)))
    (end-of-line)
    (if (and (not prefix) (-contains? '("class" "do" "else" "for" "if" "struct" "switch" "while") current-statement))
        (insert " {")
      (newline)
      (insert "{")
      (c-indent-line-or-region))
    (newline)
    (insert "}")
    (if (-contains? '("class" "struct") current-statement)
        (insert ";"))
    (c-indent-line-or-region)
    (previous-line)
    (end-of-line)
    (newline-and-indent)))

(setq gdb-show-main t)
(add-hook 'c-mode-common-hook '(lambda ()
                                (define-key c-mode-base-map [(meta return)] 'my/c-mode-new-block)
				(local-set-key (kbd "C-c g") 'gdb-many-windows)))

(setq c-default-style "bsd"
      c-basic-offset 4)

(add-hook 'prog-mode-hook '(lambda ()
                             (unless (derived-mode-p 'makefile-mode) (setq indent-tabs-mode nil))))

(defun my-c-mode-hook ()
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 8)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode t)
  (local-set-key [return] 'newline-and-indent)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
;  (whitespace-mode)
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
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

(require 'c-comment-edit)
