(require-from-modes-d "helm" 'helm-config)
(helm-mode t)
(helm-dired-mode t)
(setq helm-input-idle-delay 0)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)