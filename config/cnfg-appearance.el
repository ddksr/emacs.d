
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
;(color-theme-initialize)
;(color-theme-arjen)
(load-theme 'distinguished t)
 
;;
;; SPELLING
;;
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'cnfg-appearance)
