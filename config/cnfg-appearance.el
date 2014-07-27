;;
;; INTERFACE
;; 

;; POWERLINE
(require 'powerline)
(powerline-center-theme)


;; PROMPTS AND TOOLTIPS
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; LINE NUMBERS
(global-linum-mode 1)
(defun nolinum ()
  (global-linum-mode 0)
)
(require 'magit)
(add-hook 'magit-mode-hook 'nolinum)

;; GIT GUTTER
(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

;;
;; BUFFERS
;;

(setq default-tab-width 4) ;; tab size
(setq tab-width 4) ;; tab size
(setq-default indent-tabs-mode 1) ;; use only tabs and no spaces

(require 'doremi) ;; TODO: why and where

;; RAINBOW

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; BUFFER NAMES
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; WHITESPACE
(setq whitespace-display-mappings
       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; THEMES
;(require 'color-theme)
;(require 'moe-theme)
;(color-theme-initialize)
;(setq moe-theme-mode-line-color 'w/b)
;(powerline-moe-theme)
;(moe-dark)

(add-to-list 'custom-theme-load-path (concat my-emacs-d "themes/"))
(if (display-graphic-p) 
	(load-theme 'twilight-anti-bright t))
	
;;
;; SPELLING
;;
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;
;; Mode line
;; 

(require 'projectile)
(defun own/projectile-mode-line ()
  "Report project in mode-line."
  (let* ((project-name (projectile-project-name)))
    (format " π[%s]" project-name)))


;; Hide some minor modes
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . "")
	(yas-minor-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
	(flymake-mode . " Φ")
	(flyspell-mode . " φ")
	(git-gutter-mode . "")
	(projectile-mode . own/projectile-mode-line)
	(volatile-highlights-mode . "")
	(tern-mode . " τ")
	(skewer-mode . "")
	(undo-tree-mode . "")
	(multi-web-mode . "mwm")
	(skewer-html-mode . "")
	(zencoding-mode . " ζ")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (python-mode . "py")
	(html-mode . "w")
	(js2-mode . "js")
    (emacs-lisp-mode . "el"))
  "Alist for `clean-mode-line'.
 
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
(defun own/clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-def (cdr cleaner))
				 (mode-str (if (symbolp  mode-def)
								 (funcall mode-def)
							 mode-def))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'own/clean-mode-line)
 
;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
;; TODO: need?
(defalias 'flymake-report-status 'own/flymake-report-status-slim)
(defun own/flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

;; projectile update

(provide 'cnfg-appearance)
