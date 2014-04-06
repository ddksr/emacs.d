;; -*- no-byte-compile: t -*-

;;
;; BEFORE MAIN INIT
;; 

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; EMACS VSTAVKI
;; 
(setq my-emacs-d "~/.emacs.d/")

;;
;; LOAD CONFIGURATION
;;

(add-to-list 'load-path my-emacs-d)
(add-to-list 'load-path (concat my-emacs-d "config/"))
(add-to-list 'load-path (concat my-emacs-d "nonelpa/"))
(require 'cnfg-private)
(require 'cnfg-packages)
(require 'cnfg-basic)
(require 'cnfg-funcs)
(require 'cnfg-appearance)
(require 'cnfg-specific)
(require 'cnfg-helm)
(require 'cnfg-prog)
(require 'cnfg-terms)
(require 'cnfg-custom)

;;
;; AFTER INIT
;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode t))

;(maximize)

;;
;; AUTOMATIC!!!!!!
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "bf21256aec163a65b30bf803c8b7d26aac71616a2b4af3e754d4a2250629e02c" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" default)))
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "/home/fgallina/Code/Projects/anue-site/anue/manage.py shell") (python-shell-interpreter . "python"))))
 '(tool-bar-mode nil)
 '(tool-bar-position (quote right)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foundry "unknown" :slant normal :weight normal :height 95 :width normal)))))
(put 'downcase-region 'disabled nil)
