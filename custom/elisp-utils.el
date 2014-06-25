(defun elisp-utils/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
	 (string-equal (substring s 0 (length arg)) arg))
	(t nil)))

(defun elisp-utils/ends-with (s arg)
  "returns non-nil if string S ends with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
	(string= (substring s (- 0 (length arg))) arg))
        (t nil)))

(defun elisp-utils/one-ends-with (s endings)
  ""
  (let ((arg nil) (res nil))
    (while (and endings (not res))
      (if (elisp-utils/ends-with s (car endings))
          (setq res t))
      (setq endings (cdr endings)))
    res))

(provide 'elisp-utils)
