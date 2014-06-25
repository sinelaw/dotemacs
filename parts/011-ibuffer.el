(require 'ibuffer)

(setq ibuffer-saved-filter-groups (quote (("default"))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(require 'ibuf-ext)
