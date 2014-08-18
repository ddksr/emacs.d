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
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auth-source-save-behavior nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "0744f61189c62ed6d1f8fa69f6883d5772fe8577310b09e623c62c040f208cd4" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "a55119926805b4bb3d190b45f8f620d5f972488cc3bc37d849b77b73185aa4e2" "758da0cfc4ecb8447acb866fb3988f4a41cf2b8f9ca28de9b21d9a68ae61b181" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "ca229a0a89717c8a6fe5cd580ee2a85536fbafce6acb107d33cf38d52e2f492c" "b1f685c871220e3008b5e1dec076bc2e24dc4b6c65e64a79bfb2ea5ff7e17978" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "8dd5991bf912b39dc4ae77e2d6aa4882949f4441570222eaf25e07ec38c44d50" "ee6081af57dd389d9c94be45d49cf75d7d737c4a78970325165c7d8cb6eb9e34" "e405d0299272a1c7b7bfc1e8931dac96691ce99781a2414d158a6b6e0e70ee09" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "bf21256aec163a65b30bf803c8b7d26aac71616a2b4af3e754d4a2250629e02c" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" default)))
 '(ecb-options-version "2.40")
 '(fringe-mode 10 nil (fringe))
 '(inhibit-startup-screen t)
 '(linum-format " %6d ")
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(org-agenda-files (quote ("~/docs/org/emacs.org" "~/docs/org/faks-eizobrazevanje.org" "~/docs/org/git.org" "~/docs/org/ukazi.org")))
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F")
 '(safe-local-variable-values (quote ((require-final-newline) (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
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


