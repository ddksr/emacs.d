(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq debug-on-error nil)

(setf epa-pinentry-mode 'loopback)
(setq byte-compile-warnings '(not free-vars mapcar suspicious obsolete))

(setq custom-file (concat user-emacs-directory "etc/custom.el"))
(load custom-file)

; is server running?
; is init server?

(setq own/nick ""
      own/full-name ""
      own/email "")

(setq own/initial-buffer "~/.emacs.d/welcome"
      own/is-nox nil ; if it's not run in tty
      own/emacs-d-dirs '("lib/")
      own/yas-dirs '("~/.emacs.d/snippets" "~/.emacs.d/snippets-private")
      own/project-dirs nil
      own/org-dir nil
      own/org-agenda-files nil
      own/org-agenda-custom-commands nil
      own/org-tags nil
      own/roam-dir "~/notes"
      own/py-venvs "~/.virtualenvs"
      own/go-path "~/env/go"
      own/npm-path "~/.nvm/versions/node/v8.9.4/bin/")

(setq own/enable-themes nil
      own/enable-php nil
      own/enable-go nil
      own/enable-js nil
      own/enable-python nil)

(defun own/etc-load (file)
  (let ((etc-path (concat user-emacs-directory "etc/" file)))
        (message etc-path)
        (if (file-exists-p etc-path)
                (progn
                  (load-file etc-path)
                  t)
          nil)))

(defun own/shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(mapcar #'(lambda (dir)
            (add-to-list 'load-path (concat user-emacs-directory dir)))
        own/emacs-d-dirs)

(unless (own/etc-load "private.el.gpg")
  (own/etc-load "private.el"))

(require 'package)
(setq use-package-verbose t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (use-package hydra
    :ensure t)
  (use-package diminish
    :ensure t)
  (use-package use-package-hydra
    :after hydra
    :ensure t)
  (require 'diminish)
  (require 'bind-key))

(setq make-backup-files nil ; Disable backup files
      next-line-add-newlines t) ; Make newline at the bottom

(global-unset-key [(up)])
(global-unset-key [(down)])
(global-unset-key [(left)])
(global-unset-key [(right)])
(global-unset-key [(prior)])
(global-unset-key [(next)])
(global-unset-key [(home)])
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

(define-key global-map (kbd "RET") 'newline-and-indent) ; indent after RETURN

(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t    ; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(setq whitespace-display-mappings
       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4
          tab-width 4
      c-basic-offset 4) ;; use only tabs and no spaces

(setq truncate-lines nil)
(setq fill-column 80)
(add-hook 'text-mode-hook '(lambda ()
    (setq truncate-lines nil
          word-wrap t)))
(add-hook 'prog-mode-hook '(lambda ()
                             (setq truncate-lines nil
                                   word-wrap nil)))

(use-package doom-themes
  :if own/enable-themes

  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :ensure t)

(defun own/delete-current-buffer-file ()
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
(defun own/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(defun own/show-filename ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun own/google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))
(defun own/goto-url ()
  "Open browser"
  (interactive)
  (browse-url 
         (concat "http://" (read-string "URL: ") )))

(defun own/flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (own/flatten (car mylist)) (own/flatten (cdr mylist))))))

(defun own/hook-mark-todo () 
  "A hook that sets bold reserved words FIXME, SIGITODO, TODO and BUG"
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|SIGITODO\\|TODO\\|BUG\\):"
                             1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'own/hook-mark-todo)

(use-package auto-compile
  :init
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :ensure t)

(use-package selectrum
  :init
  (selectrum-mode +1)
  :ensure t)
(use-package prescient
  :ensure t)
(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1) ; use prescient for minibuffer completion
  (prescient-persist-mode +1) ; remember favorite selections
  :after (selectrum prescient)
  :ensure t)

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode)
  :ensure t)

(use-package rg
  :config
  (grep-apply-setting 'grep-template "rg --no-heading -H -uu -g <F> <R> <D>")
  :ensure t)

(use-package expand-region
  :bind ("C-<" . er/expand-region)
  :ensure t)

(use-package git-gutter
;    :if own/is-nox
    :init
    (global-git-gutter-mode +1)
    :ensure t)
;  (use-package git-gutter-fringe
;    :unless own/is-nox
;    :init
;    (global-git-gutter-mode +1)
;    :ensure t)

(use-package yasnippet
  :config
  ;(define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;(define-key yas-minor-mode-map (kbd "TAB") nil)
  (setq yas-snippet-dirs (append (yas-snippet-dirs)
                                 own/yas-dirs))
  (yas/global-mode 1)
  :ensure t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

(use-package volatile-highlights
  :init
  (volatile-highlights-mode t)
  :ensure t)

(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)
(use-package doom-modeline
  :config
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-env-version t)
  (doom-modeline-mode 1)
  :ensure t)

(use-package flyspell-lazy
  :config
  (flyspell-lazy-mode 1)
  (flyspell-mode 1) 
  :ensure t)

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-s" . mc/mark-next-like-this)
         ("C-S-r" . mc/mark-previous-like-this)
         ("C-S-a" . mc/mark-all-like-this))
  :ensure t)

(use-package string-inflection
  :ensure t
  :bind (:map prog-mode-map
              ("C-:" . string-inflection-all-cycle)))

(use-package dired
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :ensure nil)

(use-package projectile
  :config
  (setq projectile-project-search-path own/project-dirs
        projectile-mode-line-function '(lambda () (format " p[%s]" (projectile-project-name)))
        projectile-sort-order 'modification-time)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :ensure t)

(use-package magit
  :bind ("C-x C-g" . magit-status)
  :ensure t)

(use-package org
  :bind (("s-q" . org-agenda))
  :config
  ;; (require 'org-indent)
  (setq org-log-done t
        org-agenda-files (own/flatten (mapcar 'file-expand-wildcards (own/flatten own/org-agenda-files)))
        org-directory own/org-dir
        org-src-fontify-natively t
        org-tag-alist own/org-tags
        org-agenda-custom-commands own/org-agenda-custom-commands
        org-ellipsis " ▾")
  :ensure nil)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :ensure t)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename own/roam-dir))
  :bind (("s-x n" . hydra-roam/body)

         ("C-đ C-đ" . org-roam-dailies-goto-today)
         ("C-đ C-š" . org-roam-dailies-goto-tomorrow)

         ("M-đ M-đ" . org-roam-buffer-toggle)

         ("C-š C-š" . org-roam-node-insert)
         ("C-š C-đ" . org-roam-capture)

         ("M-š M-š" . org-roam-node-find)
         ("M-š M-đ" . org-roam-buffer-display-dedicated))
  :hydra (hydra-roam (:color blue :hint nil :exit t)
                     ("b" org-roam-buffer-toggle "Toggle")
                     ("g" org-roam-graph "Graph")
                     ("i" org-roam-node-insert "Insert")
                     ("c" org-roam-capture "Capture")

                     ("f" org-roam-node-find "Find")
                     ("x" org-roam-buffer-display-dedicated "Find connections for node")

                     ("t" org-roam-dailies-capture-today "Capture today")
                     ("T" org-roam-dailies-goto-today "Today notes")
                     ("j" org-roam-dailies-capture-tomorrow "Capture tomorrow")
                     ("J" org-roam-dailies-goto-tomorrow "Tomorrow notes")
                     ("y" org-roam-dailies-capture-yesterday "Capture yesterday")
                     ("Y" org-roam-dailies-goto-yesterday "Yesterday notes")
                     ("d" org-roam-dailies-capture-date "Capture date")
                     ("D" org-roam-dailies-goto-date "Daily notes"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; If using org-roam-protocol
                                        ;(require 'org-roam-protocol)
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  :ensure t)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t)

(use-package which-key
  :if nil ; still testing
  :config
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  :ensure t)

(use-package helm
  :ensure t)
(use-package helm-c-yasnippet
  :ensure t)
(use-package helm-projectile
  :ensure t)

(use-package restclient
  :ensure t)

(use-package esup
  :config
  (setq esup-depth 0)
  :pin melpa
  :ensure t)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :ensure t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (vue-mode . lsp-deferred)
         (cc-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind ("s-x l" . hydra-lsp/body)
  :hydra (hydra-lsp (:color blue :hint nil :exit t)
                    ("f" lsp-format-buffer "Format buffer")
                    ("d" lsp-ui-peek-find-definitions "Find definitions")
                    ("r" lsp-ui-peek-find-references "Find references")
                    ("s" lsp-ui-peek-find-workspace-symbol "Find workspace symbol")
                    ("h" lsp-document-highlight "Document Highlight")
                    ("p" lsp-describe-thing-at-point "Describe @ p"))
  :config
  (setq lsp-disabled-clients '(vls))
  ;;    :custom
  ;;    (lsp-enable-indentation nil)
  ;;    (lsp-html-format-enable nil)
  :commands lsp lsp-deferred
  :ensure t)

;; UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  :ensure t)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol :ensure t)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-pyright
  :if own/enable-python
  :config
  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/.emacs.d/.cache/python-stubs")) ;; example
  :ensure t)
(use-package python-mode
  :mode "\\.py\\'"
  :if own/enable-python
  :config
  (setq py-autopep8-options '("--max-line-length=120"))
  :ensure t)

(use-package go-mode
  :mode "\\.go\\'"
  :if own/enable-go
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)
                      (setq tab-width 4))))
  :config
  (setq exec-path (cons "/usr/local/go/bin" exec-path))
  (add-to-list 'exec-path (concat own/go-path "/bin"))
  :ensure t)

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :ensure t)

(use-package js2-mode
  :if own/enable-js
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 4)
  :ensure t)

(use-package vue-mode
  :if own/enable-js
  :mode "\\.vue\\'"
  :custom
  (vue-html-extra-indent 4)
  (vue-html-tab-width 4)
  :config
  (setq sgml-basic-offset 4)
  :ensure t)

(use-package php-mode
  :mode "\\.php\\'"
  :hook ((php-mode . php-enable-symfony2-coding-style))
  :if own/enable-php
  :ensure t)

(use-package web-mode
  :mode (("\\.blade\\." . web-mode)
         ("\\.html\\'" . web-mode))
  :config
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)   
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  (setq web-mode-comment-style 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  :ensure t)

(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)
(use-package less-css-mode
  :mode "\\.less\\'"
  :ensure t)
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :custom
  (yaml-indent-offset 4)
  :ensure t)

(bind-key "C-x C-k" 'own/delete-current-buffer-file)
(bind-key "C-x C-r" 'own/rename-current-buffer-file)
(bind-key "<f5>" 'rgrep)
(bind-key "C-x C-b" 'ibuffer)

(bind-key "s-u" 'undo)

(bind-key "<f5>" 'rgrep)

(with-eval-after-load 'hydra
  (defhydra hydra-actions (:exit t)
    "Common actions"
    ("a" helm-mini "mini")
    ("p" helm-projectile "projectile"))
  (bind-key "s-a" 'hydra-actions/body)
  (defhydra hydra-buffer (:exit t)
    "Common actions"
    ("i" helm-imenu "imenu")
    ("o" helm-occur "occur")
    ("y" helm-yas-complete "yas")
    ("k" helm-show-kill-ring "kill ring"))
  (bind-key "s-y" 'hydra-buffer/body))

(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and own/initial-buffer (file-exists-p own/initial-buffer))
  (setq initial-buffer-choice own/initial-buffer))
(message "Initialization finished sucessfully")
