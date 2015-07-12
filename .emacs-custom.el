(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 5)
 '(company-ghc-show-info t)
 '(doc-view-resolution 200)
 '(flycheck-checkers
   (quote
    (asciidoc my/project-aware-checker cfengine chef-foodcritic coffee coffee-coffeelint css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis go-gofmt go-golint go-vet go-build go-test haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint racket rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-disabled-checkers
   (quote
    (c/c++-cppcheck c/c++-gcc c/c++-clang haskell-hlint haskell-ghc)))
 '(flycheck-display-errors-delay 0)
 '(flycheck-highlighting-mode (quote sexps))
 '(git-grep-switches "--extended-regexp -I --no-color -n")
 '(haskell-process-log t)
 '(ibuffer-default-sorting-mode (quote filename/process))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(linum-format "%4d")
 '(safe-local-variable-values
   (quote
    ((c-continued-statement-offset . 8)
     (c-label-offset . -8)
     (c-argdecl-indent . 8)
     (c-brace-offset . -8)
     (c-brace-imaginary-offset . 0)
     (include-header-prefix . "ELFS__")
     (eval setq dir-local-curdir
	   (file-name-directory
	    (let
		((d
		  (dir-locals-find-file ".")))
	      (if
		  (stringp d)
		  d
		(car d)))))
     (ff-search-directories "../include" ".")
     (ff-search-directories "../../include")
     (ff-search-directories "include"))))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(switch-to-buffer-preserve-window-point (quote already-displayed))
 '(whitespace-display-mappings nil)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :foundry "xos4" :family "Terminus"))))
 '(col-highlight ((t (:background "#444466"))) t)
 '(diff-hl-insert ((t (:background "#77bb77" :foreground "#33bb33"))))
 '(egoge-display-time ((t (:foreground "#ccffff"))) t)
 '(flycheck-error-face ((t (:background "#990000"))) t)
 '(flycheck-warning-face ((t (:background "#505000"))) t)
 '(flymake-errline ((t (:background "#990000"))))
 '(flymake-warnline ((t (:background "#505000"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "#ffffb0"))))
 '(font-lock-constant-face ((t (:foreground "#ffbbbb" :weight bold))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))) t)
 '(font-lock-function-name-face ((nil (:foreground "#FFCC00"))))
 '(font-lock-keyword-face ((nil (:foreground "yellow"))))
 '(font-lock-preprocessor-face ((nil (:foreground "yellow"))))
 '(font-lock-reference-face ((nil (:foreground "#FFE0A0"))) t)
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((nil (:foreground "#50DD50"))))
 '(font-lock-variable-name-face ((nil (:foreground "#A0FFA0"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(ghc-face-error ((t (:background "#990000"))))
 '(ghc-face-warn ((t (:background "#505000"))))
 '(hl-line ((t (:inherit col-highlight :background "green4"))) t)
 '(lawlist-active-region-face ((t (:background "#3c3c3c"))))
 '(linum ((t (:inherit (shadow default) :background "#2a2a2a" :foreground "#778888"))))
 '(scroll-bar ((t (:background "#111111" :foreground "#999999"))))
 '(show-paren-match ((((class color) (background dark)) (:background "#262e4e"))))
 '(whitespace-indentation ((t (:background "#202020"))))
 '(whitespace-space ((t (:background "#181818"))))
 '(whitespace-space-after-tab ((t (:background "#333333" :foreground "firebrick")))))
