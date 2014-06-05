
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
;; AUTOMATIC!!!!!!
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(auth-source-save-behavior nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("758da0cfc4ecb8447acb866fb3988f4a41cf2b8f9ca28de9b21d9a68ae61b181" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "b1f685c871220e3008b5e1dec076bc2e24dc4b6c65e64a79bfb2ea5ff7e17978" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "8dd5991bf912b39dc4ae77e2d6aa4882949f4441570222eaf25e07ec38c44d50" "ee6081af57dd389d9c94be45d49cf75d7d737c4a78970325165c7d8cb6eb9e34" "e405d0299272a1c7b7bfc1e8931dac96691ce99781a2414d158a6b6e0e70ee09" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "bf21256aec163a65b30bf803c8b7d26aac71616a2b4af3e754d4a2250629e02c" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" default)))
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/docs/org/emacs.org" "~/docs/org/faks-eizobrazevanje.org" "~/docs/org/git.org" "~/docs/org/ukazi.org")))
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))
(put 'downcase-region 'disabled nil)


