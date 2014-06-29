; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; recentf - save history of recently visited files
(autoload 'recentf-mode "recentf.el" nil t)
(autoload 'recentf-save-list "recentf.el" nil t)

; recentf - save history of recently visited files
(require 'recentf)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)

; saveplace - save position in files (http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (in-emacs-d ".places"))

(defun my/base-rename-current-buffer-file (vc)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (if vc
              (vc-rename-file filename new-name)
              (vc-rename-file filename new-name 1))
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

; rename buffer and file
(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (my/base-rename-current-buffer-file nil))

(defun my/vc-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (my/base-rename-current-buffer-file t))

(global-set-key (kbd "C-x C-r") 'my/vc-rename-current-buffer-file)

(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

; find-files-in-project
(require 'find-file-in-project)

; Recognize zsh files
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
