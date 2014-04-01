;;
;; PACKAGES
;;

;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages)
  (package-initialize))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed 'auto-compile ;; Basic
						  'flx-ido
						  'key-chord
						  'undo-tree
						  'multiple-cursors
						  'ace-jump-mode
						  'expand-region
						  'window-number
						  'smex
						  'recentf ;; Projects and files
						  'projectile
						  'project-explorer
						  'helm ;; HELM
						  'helm-anything
						  'helm-dash
						  'helm-flycheck
						  'helm-flymake
						  'helm-git
						  'helm-projectile
						  'helm-pydoc
						  'helm-themes
						  'helm-c-yasnippet
						  'helm-helm-commands
						  'visual-regexp ;; Text manipulation/search
						  'move-text
						  'volatile-highlights
						  'powerline ;; Apperance
						  'git-gutter-fringe
						  'doremi
						  'rainbow-delimiters
;						  'color-theme
;						  'distinguished-theme
						  'yasnippet ;; Programming
						  'markdown-mode
						  'pkgbuild-mode
						  'php-mode
						  'php-extras
						  'php-eldoc
						  'python-mode
						  'virtualenvwrapper
						  'python-pylint
						  'jedi
						  'js2-mode
						  'zencoding-mode
						  'web-mode
						  'multi-web-mode
						  'ruby-mode
						  'inf-ruby
						  'rsense
						  'auto-complete ;; Autocomplete
						  'auto-complete-auctex
						  'ac-js2
						  'ac-helm
						  'ac-c-headers
						  'ac-octave
						  'flyspell-lazy ;; Syntax checking
						  'flymake
						  'flymake-shell
						  'flymake-php
						  'flymake-python-pyflakes
						  'flymake-ruby
						  'flymake-css
						  'magit ;; Applications
						  'auctex 'latex-extra 'latex-preview-pane
						  'notifications
						  'org-ac
						  'howdoi
						  'httprepl
						  'prodigy
						  'multi-term)

(provide 'cnfg-packages)
