;; HELM settings

(require 'ac-helm) ;; Not necessary if using ELPA package
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; HELM COMMANDS

;; HELM OCCOUR

;; HELM do-grep C-u M-x do-grep

;; HELM velocity
(require 'helm-grep)
(setq helm-velocity-dir (list my-org-docs-dir)
	  helm-velocity-ext '("*.txt" "*.org"))
(defun own/helm-velocity ()
  (interactive)
  (helm-do-grep-1 helm-velocity-dir t nil helm-velocity-ext))

(global-set-key (kbd "s-a i") 'helm-imenu)
(global-set-key (kbd "s-a o") 'helm-occur)
(global-set-key (kbd "s-a s") 'own/helm-velocity)
(global-set-key (kbd "s-a y") 'helm-yas-complete)
(global-set-key (kbd "s-a k") 'helm-show-kill-ring)
(global-set-key (kbd "s-a g") 'helm-do-grep)

(global-set-key (kbd "s-x p") 'helm-projectile)
(global-set-key (kbd "s-x s-f") 'helm-browse-project)
(global-set-key (kbd "s-x r") 'helm-recentf)
(global-set-key (kbd "s-x b") 'helm-buffers-list)

(global-set-key (kbd "s-x h h") 'helm-helm-commands)
(global-set-key (kbd "s-x h p") 'helm-pydoc)
(global-set-key (kbd "s-x h t") 'helm-top)

(provide 'cnfg-helm)
