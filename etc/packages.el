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
 ;;;;;;;;; Libraries
 'async
 'dash
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Basic
 'auto-compile ; auto compile elisp files
 'flx-ido ; improve IDO minibuffer mode
; 'key-chord ; enable two-key commands
 'undo-tree ; enable undo-tree
 'multiple-cursors ; enable multiple cursors
 'ace-jump-mode ; enable jumping around in buffer
 'ace-jump-buffer ; enable jumping over buffers
 'ace-jump-zap ; enable jumping in windows
 'ace-window ; enable jumping in windows
 'ace-isearch ; enable jumping in windows
 'ibuffer-vc ; organize ibuffer using version control
 'smex ; ido for elisp interactive functions
 'expand-region ; expand region (s-e)
; 'visual-regexp ; for better regexes
 ;'doremi ; change things incrementally 
 'paradox ; improved package management
 'hydra ; keybinding management
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; File and project management
 'recentf ; recent projects and files
 'projectile ; simple project management
 'grizzl ; file completion system for projectile
 'flyspell-lazy ; check spelling
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; ORG-MODE (latest)
 'org
 'orglink
 'outorg
 'outshine
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; HELM - search anything
 'helm ; default helm
 'helm-anything ; for compatibility for older packages
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
 'python-mode
 'py-autopep8 ; peeeeeep
 'flymake-python-pyflakes
 'anaconda-mode ; semantic autocomplete
 'company-anaconda ; semantic autocomplete

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Appearance
 'powerline ; make powerline more beautiful
 'git-gutter-fringe ; show git additions in deletions in buffer
 'rainbow-delimiters ; colorize delimiters (like brackets) for easier debugging
 'volatile-highlights ; highlight changes
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Themes
 ;'color-theme ; color theme helper package
 'afternoon-theme
 'pastels-on-dark-theme
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;; Applications and their additions
 'magit ; git source control
)

;; Packages I used but didn't serve my needs:
;; - window-number
;; - multi-web-mode
