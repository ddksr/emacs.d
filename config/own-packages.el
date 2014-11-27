(defun own/ensure-package-installed (&rest packages)
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

(own/ensure-package-installed
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Basic
 'auto-compile ; auto compile elisp files
 'flx-ido ; improve IDO minibuffer mode
 'key-chord ; enable two-key commands
 'undo-tree ; enable undo-tree
 'multiple-cursors ; enable multiple cursors
 'ace-jump-mode ; enable jumping around in buffer
 'ace-jump-buffer ; enable jumping over buffers
 'ace-window ; enable jumping in windows
 'ibuffer-vc ; organize ibuffer using version control
 'smex ; ido for elisp interactive functions
 'expand-region ; expand region (s-e)
 'visual-regexp ; for better regexes
 ;'doremi ; change things incrementally 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; File and project management
 'recentf ; recent projects and files
 'projectile ; simple project management
 'grizzl ; file completion system for projectile

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; ORG-MODE (latest)
 'org
 'org2blog
 'outorg
 'outshine
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; HELM - search anything
 'helm ; default helm
 'helm-anything ; for compatibility for older packages
 ;; TODO: check and use this packages
 ;'helm-dash ; Helm search trough different docummentations
 ;'helm-flycheck
 ;'helm-flymake
 ;'helm-git
 'helm-projectile ; search all files in the current project
 'helm-pydoc ; search python documentation
 'helm-c-yasnippet ; yasnippet with helm
 'helm-helm-commands ; list all helm commands
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Text manipulation and search
 'move-text ; move text up and down on buffer TODO: check if keybindings
 'volatile-highlights ; highlight pastes etc.

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Appearance
 'powerline ; make powerline more beautiful
 'git-gutter-fringe ; show git additions in deletions in buffer
 'rainbow-delimiters ; colorize delimiters (like brackets) for easier debugging
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Programming languages utilities
 'git-timemachine ; move trough file git history
 'git-messenger ; see line commit
 'gist ; integration with GIST
 'yasnippet ; provide snippets for faster programming
 'markdown-mode ; MARKDOWN mode
 'es-mode ; Elasticsearch
 ;'pkgbuild-mode ; for creating packages with emacs
 'php-mode ; PHP mode
 'php-extras ; PHP mode utilities
 'php-eldoc ; PHP documentation TODO: use docs
 'python-mode ; PYTHON mode
 'virtualenvwrapper ; for python virtualenv manipulation
 'pyenv-mode
 'anaconda-mode ; python code navigation
 ;'python-pylint ; check python syntax TODO: check if using this
 'jedi ; python autocomplete
 'js2-mode ; JAVASCRIPT mode
 'tern ; javascript environment
 'tern-auto-complete ; TERN javascript autocomplete
 'zencoding-mode ; code with ZEN (for creating html using selectors)
 'web-mode ; for editing web files (html, css)
 'ruby-mode ; RUBY mode
 'inf-ruby ; ruby console
 'ruby-electric ; faster ruby programming (autocompletion and closing)
; 'rsense ; ruby development tools
 'auto-complete ; autocomplete mode for various programming languages
; 'company ; alternative for autocomplete - use for ac-company
 'auto-complete-auctex ; latex autocomplete
 'ac-js2 ; autocomplete javascript
 'org-ac ; autocomplete for ORG mode TODO: set up
 'ac-helm
 'ac-c-headers
 'ac-octave ; autocomplete for octave
 'ac-anaconda ; autocomplete for anaconda
 'flyspell-lazy ; check spelling
 'flymake ; check syntax
 'flymake-shell ; syntax checking for SH
 'flymake-php ; syntax checking for PHP
 'flymake-python-pyflakes ; use pyflakes via flymake for python
 'flymake-ruby ; ruby syntax checking
 'flymake-css ; check css syntax

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Themes
 ;'color-theme ; color theme helper package
 'afternoon-theme
 'pastels-on-dark-theme
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Applications and their additions
 'magit ; git source control
 'auctex ; latex editor
 'latex-extra ; extra latex commands
 'notifications ; for notifications via emacs
 'howdoi ; get stack overflow answers trough emacs
 'prodigy ; manage services trouhg emacs
 'multi-term ; terminal emulator via emacs
 'restclient ; RESTful emacs client
)

;; Packages I used but didn't serve my needs:
;; - window-number
;; - multi-web-mode

(provide 'own-packages)
;;; own-packages ends here
