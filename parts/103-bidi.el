(defun zc-hebrew (&optional arg)
  "Switch to Hebrew input.
Without arg does RTL/LTR determination automatically (e.g. - via the rules
of the Unicode Bidirectional Algorithm). With arg forces RTL. Calling
again toggles off bidi and Hebrew input."
  (interactive "P")
  (if bidi-display-reordering
	  (progn
                (setq bidi-display-reordering nil)
                (inactivate-input-method))
        (setq bidi-display-reordering t)
        (if arg
                (setq bidi-paragraph-direction 'right-to-left)
          (setq bidi-paragraph-direction nil))
        (set-input-method 'hebrew)))
