;;
;; OPTIMIZATIONS
;;

;; KOMPAJLANJE
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;
;; EMACS TWEAKS
;; 

;; FULLSCREEn
(require 'fullscreen)

;; KEY CHORD
(require 'key-chord)
(key-chord-mode 1)
										

;; UNDO TREE AND KILL RING
(require 'undo-tree)
(global-undo-tree-mode)

(key-chord-define-global "uu" 'undo)
(key-chord-define-global ".-" 'undo-tree-visualize)

;; MULTIPLE CURSOR
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-s") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-r") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; JUMP MODE
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
										;(key-chord-define-global "" 'ace-jump-mode)
(key-chord-define-global "qw" 'ace-jump-char-mode)
(key-chord-define-global "yx" 'ace-jump-word-mode)
(key-chord-define-global "<y" 'ace-jump-line-mode)
(key-chord-define-global "yy" 'ace-jump-buffer)

;; AUTOCOMPLETE
										;(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(auto-complete 1) ;; da se samo nalozi vsakic
(global-auto-complete-mode t)

;; IDO
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; COMBINE LINES
(global-set-key (kbd "M-j")
				(lambda ()
                  (interactive)
                  (join-line -1)))

;; DISABLE ARROW KEYS
(global-unset-key [(up)])
(global-unset-key [(down)])
(global-unset-key [(left)])
(global-unset-key [(right)])
(global-unset-key [(prior)])
(global-unset-key [(next)])
(global-unset-key [(home)])
(global-unset-key [(next)])

;; RECENT FILES
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-y") 'recentf-open-files)

;; DISABLE BACKUPS
(setq make-backup-files nil)
(setq make-backup-files nil)

;; NEW LINE AT THE BOTTOM
(setq next-line-add-newlines t)

;; ALWAYS INDENT AFTER RETURN
(define-key global-map (kbd "RET") 'newline-and-indent)


;;
;; PROJEKTI
;;

;; PROJECTILE
(projectile-global-mode)

;; PROJECT EXPLORER
(require 'project-explorer)

(global-set-key (kbd "s-p" ) 'project-explorer-open)

;;
;; SHELL
;;

;; HIDE PASSWORD IN SHELL
(add-hook 'comint-output-filter-functions
		  'comint-watch-for-password-prompt)

;; TODO: what?
(defadvice shell (around always-new-shell)
  "Always start a new shell."
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))

(ad-activate 'shell)

;; ENABLE MULTITERM WITH ZSH
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook ;; make yank work
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))
(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("s-<" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("s->" . multi-term-next))))
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))


;; IDO LIKE M-x

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; WINDOW NUMBERS
(require 'window-number)
(global-set-key (kbd "s-y s-y") 'window-number-switch)
(window-number-mode 1)
(window-number-define-keys window-number-mode-map "s-")

;; WINDOOW CONFIGURATIONS
(winner-mode 1)
;(require 'elscreen)
;(setq elscreen-prefix-key (kbd "C-."))
;(elscreen-start)

;; RESIZING WINDOWS
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; MOVE TEXT
(require 'move-text)
(move-text-default-bindings)

;; VOLATILE
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; SMART EXPANDING
(require 'expand-region)
(global-set-key (kbd "s-e") 'er/expand-region)

;; REGEX
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-S-c C-S-r") 'vr/mc-mark)

(provide 'cnfg-basic)
