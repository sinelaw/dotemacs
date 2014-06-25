(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "pythonpath-epylint" (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
(add-hook 'python-mode-hook (lambda () (flymake-mode)))

; Javascript
(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

(push '("\\.hpp\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
(push '("\\.hh\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
(push '("\\.h\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)

; flymake for Tex live
; (defun flymake-get-tex-args (file-name)
;    (list "env-pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

