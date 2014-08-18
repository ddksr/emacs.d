;;
;; BASIC
;; 

(add-hook 'prog-mode-hook 'hook-mark-todo)

;; OTHER CUSTOM KEYBINDINGS
(define-key global-map (kbd "s-t") 'comment-or-uncomment-region)

;;
;; SNIPPETS
;;

(require 'yasnippet)
(yas/global-mode 1) ;; load global mode ;; TODO: check if necessary

(setq yas-snippet-dirs (append yas-snippet-dirs
							   '("~/.emacs.d/snippets")))


;(add-to-list 'yas/root-directory "~/.snippets/")
;(yas/initialize)

;; OVERWRITE TAB WITH 
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "s-SPC") 'yas-expand)



;;
;; SHELL
;;

(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")

;; SHELL AUTOCOMPLETE: TODO: check if working
(add-hook 'shell-dynamic-complete-functions
		  'bash-completion-dynamic-complete)

(add-hook 'shell-command-complete-functions
		  'bash-completion-dynamic-complete)
; TODO: add my hooks

;;
;; MARKDOWN
;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; PKGBUILD (ArchLinux)
;;

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;;
;; C and C++
;;

(setq c-default-style "linux"
	  c-basic-offset 4
	  tab-width 4
	  ident-tabs-mode t)
;TODO: add my hooks

;;
;; PHP
;; 

;; SYNTAX
(require 'php-extras)
(require 'php-completion) ; this sun of a bitch was missing
(require 'flymake)
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

;; OTHER

(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  (hook-mark-todo)

  ;; Autocomplete
  (when (require 'auto-complete nil t)
	(make-variable-buffer-local 'ac-sources)
	(add-to-list 'ac-sources 'ac-source-php-completion)
	;; if you like patial match,
	;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
	;; (add-to-list 'ac-sources 'ac-source-php-completion-patial)
	(auto-complete-mode t))

  ;; Tabs and indent  
  (setq indent-tabs-mode t)
  (setq-default indent-tabs-mode t)
  
  ;; Set the tab width
  (setq default-tab-width 4)
  (setq tab-width 4)
  
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))

;; TODO: check
;(require 'mmm-mode)
;(setq mmm-global-mode 'maybe)
;(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)

;;
;; R
;;
(require 'ess-site)
(require 'ess-R-object-popup)
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

;;
;; OCTAVE
;;

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
(cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
		  (lambda ()
			(hook-mark-todo)
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
				(font-lock-mode 1))))

(require 'ac-octave)
(defun ac-octave-mode-setup ()
  (setq ac-sources '(ac-source-octave)))
(add-hook 'octave-mode-hook
		  '(lambda () ;; TODO: CHECK IF IT WORKS (DIDN't SEE AC IN OCTAVE YET)
			 (hook-mark-todo)
			 (ac-octave-mode-setup)))

;;
;; PYTHON
;;
(setq py-spaces-projects '("bobergame" "intro-data-science-naloge" "cliist"))

(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
(require 'projectile)

(defun python-mode-config ()
  (hook-mark-todo)
  (if (member (projectile-project-name) py-spaces-projects)
	  (spaces-py-settings)
	(tabs-py-settings)))

(add-hook 'python-mode-hook 'python-mode-config)

(defun tabs-py-settings ()
  (interactive)
  (message "python: tabs")
  (setq indent-tabs-mode t)
  (setq python-indent 4)
  (setq tab-width 4))
(defun spaces-py-settings ()
  (interactive)
  (message "python: spaces")
  (setq indent-tabs-mode nil)
  (setq python-indent 4))

;; virualenvs
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs")

;; SYNTAX
(require 'flymake-python-pyflakes)

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

; this code first checks if there is a related virtual environment
; and uses the executable from there
; TODO: do the same for jedi maybe?
(defun virtualenv-flymake ()
  (defvar virtualenv-exec (concat "~/.virtualenvs/" (projectile-project-name) "/bin/pyflakes"))
  (if (file-exists-p virtualenv-exec)
	  (setq flymake-python-pyflakes-executable virtualenv-exec)
	(setq flymake-python-pyflakes-executable "pyflakes"))
  (flymake-python-pyflakes-load))
(add-hook 'python-mode-hook 'virtualenv-flymake)

;; JEDI EVALVATION
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optiona

;; IPYTHON
(setq-default py-shell-name "ipython2")
(setq-default py-which-bufname "IPython")

;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
	  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation nil)

;(setq elpy-rpc-backend "native")

;; DJANGO
;(require 'python-django)
;(require 'django-html-mode)
;(require 'django-mode)
;(yas/load-directory "/usr/share/emacs/site-lisp/django-mode/snippets")
;(add-to-list 'auto-mode-alist '("\.djhtml$" . django-html-mode))
;(global-set-key (kbd "C-x j") 'pyton-django-open-project)

;;
;; JAVASCRIPT
;;

; FLYMAKE SYNTAX CHECKING WITH JSLINT
;(require 'flymake-jslint)
;(require 'flymake-easy)
;(add-hook 'js-mode-hook 'flymake-jslint-load)
;(add-hook 'js-mode-hook (lambda() 
;		  (setq js-indent-level 4)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 4)
(setq js2-use-font-lock-faces t)
(setq js2-mode-hook
	  '(lambda ()
		 (progn
		   (set-variable 'indent-tabs-mode t))
		 (hook-mark-todo)
		 (tern-mode t) ; install tern via npm
		 (auto-complete-mode nil)
		 (ac-js2-mode t)))

;; TERN
; Installation on Arch Linux:
; sudo npm install tern
; cd /bin
; ln -s PATHTOTERN /bin/tern
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
;;
;; HTML
;;
(add-hook 'html-mode-hook
          (lambda()
			(hook-mark-todo)
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

;; ZEN
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(global-set-key (kbd "s-z") 'zencoding-expand-line)

;;
;; WEB MODE
;;
;;TODO: make this work better
;;or just don't bother with it
;(setq mweb-default-major-mode 'html-mode)
;(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;(multi-web-global-mode 1)


;;
;; RUBY
;;

;;
;; RUBY
;;

(setq-default indent-tabs-mode t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook (lambda () 
							(setq indent-tabs-mode t)
							(setq ruby-indent-level 4)
							(setq tab-width 4)
							(hook-mark-todo)
							(local-set-key "\n" 'newline-and-indent)))
(require 'ruby-electric)
(eval-after-load "ruby-mode" 
  '(add-hook 'ruby-mode-hook 'ruby-electric-mode)) ; must have if you want brackets to work

; fix for void symbol
(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(defun own/ruby-init ()
  (require 'flymake-ruby)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)

  (require 'inf-ruby)
  (setq rsense-home (expand-file-name "/opt/rsense-0.3"))
  (setq rsense-home "/opt/rsense-0.3")
  (add-to-list 'load-path (concat rsense-home "/etc"))
  (require 'rsense)

  ;; AUTOCOMPLETE (with rsense)
  (add-hook 'ruby-mode-hook 'auto-complete-mode)
  (add-hook 'ruby-mode-hook
			(lambda ()
			  (add-to-list 'ac-sources 'ac-source-rsense-method)
			  (add-to-list 'ac-sources 'ac-source-rsense-constant)))
  )
(if (eq system-type 'gnu/linux) (own/ruby-init))

;; SKEWER BROWSER TODO: check if remove
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;
;; CSS
;;

;; SYNTAX CHECK CSS
(require 'flymake-css)
(add-hook 'css-mode-hook 'flymake-css-load)

;;
;; APACHE CONFIGURATION FILES
;;
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; SKEWER
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'cnfg-prog)
