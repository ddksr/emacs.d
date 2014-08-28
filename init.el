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
(add-to-list 'load-path (concat my-emacs-d "themes/"))
(message "loading private")
(require 'cnfg-private)
(message "loading packages")
(require 'cnfg-packages)
(message "loading basic")
(require 'cnfg-basic)
(message "loading funcs")
(require 'cnfg-funcs)
(message "loading apperance")
(require 'cnfg-appearance) 
(message "loading specific")
(require 'cnfg-specific)
(message "loading helm")
;; rewrite done to this line
(require 'cnfg-helm)
(message "loading prog")
(require 'cnfg-prog)
(message "loading custom")
(require 'cnfg-custom)
(message "loading done!")
;;
;; AFTER INIT
;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode t))

;(maximize)

;;
;; INIT FROM ORG
;; 

;(require 'ob-tangle)
;; (setq debug-on-error t)
;(org-babel-load-file
; (expand-file-name "emacs-init.org"
;                   user-emacs-directory))

;;
;; AUTOMATIC!!!!!!
;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))
(put 'downcase-region 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(safe-local-variable-values (quote ((python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "/home/fgallina/Code/Projects/anue-site/anue/manage.py shell") (python-shell-interpreter . "python")))))
