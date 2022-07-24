(defun own/install-package (package)
  (if (listp package)
      (if (car package)
          (mapcar 'own/install-package (cdr package)))
    (unless (package-installed-p package) (package-install package))))

(defun own/ensure-packages-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar 'own/install-package packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(own/ensure-packages-installed
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Libraries
 'async
 'dash
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Basic
 'auto-compile ; auto compile elisp files
 'selectrum ; replacing smex and ido
 'prescient ; improving selectrum
 'selectrum-prescient
; 'key-chord ; enable two-key commands
 'undo-tree ; enable undo-tree
 'multiple-cursors ; enable multiple cursors
 'ace-jump-mode ; enable jumping around in buffer
 'ace-jump-buffer ; enable jumping over buffers
 'ace-jump-zap ; enable jumping in windows
 'ace-window ; enable jumping in windows
 'ace-isearch ; enable jumping in windows
 'ibuffer-vc ; organize ibuffer using version control
 'expand-region ; expand region (s-e)
 'paradox ; improved package management
 'hydra ; keybinding management
 'rg ; ripgrep

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; File and project management
 'recentf
 'projectile ; simple project management
 'grizzl ; file completion system for projectile
 'flyspell-lazy ; check spelling
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; ORG-MODE (latest)
 'org
 'orglink
 'outorg
 'outshine
 'org-roam
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; HELM - search anything
 'helm ; default helm

 'helm-projectile ; search all files in the current project
 'helm-pydoc ; search python documentation
 'helm-c-yasnippet ; yasnippet with helm

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Text manipulation and search

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Programming
 'yasnippet
 'flymake

 ;; Python
 (list own/enable-python 
       'python-mode
       'py-autopep8 ; peeeeeep
       'flymake-python-pyflakes
       'anaconda-mode ; semantic autocomplete
       'company-anaconda ; semantic autocomplete
       'django-snippets
       'blacken)

 ;; PHP
 (list own/enable-php
       'php-mode
       'web-mode
       'flymake-php
       'company-php)

 ;; GO
 (list own/enable-go
       'go-mode
       'golint
       'go-playground
       'go-snippets
       'company-go
       'golint
       'flymake-go
       'go-complete)
 
 ;; JavaScript
 (list own/enable-js
       'js2-mode
       'tern
       'company-tern)

 ;; ELISP
 'paredit
 
 ;; Other
 'json-mode
 'less-css-mode
 'yaml-mode

 ;; Rest
 'restclient
 'company-restclient

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Appearance
 'powerline ; make powerline more beautiful
 'git-gutter-fringe ; show git additions in deletions in buffer
 'rainbow-delimiters ; colorize delimiters (like brackets) for easier debugging
 'volatile-highlights ; highlight changes
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Themes
 'zenburn-theme
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Applications and their additions
 'magit ; git source control
)

;; Packages I used but didn't serve my needs:
;; - window-number
;; - multi-web-mode

