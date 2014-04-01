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
(setq helm-velocity-dir '("~/docs/org")
	  helm-velocity-ext '("*.txt" "*.org"))
(defun helm-velocity ()
  (interactive)
  (helm-do-grep-1 helm-velocity-dir t nil helm-velocity-ext))

(global-set-key (kbd "s-x v") 'helm-velocity)
(global-set-key (kbd "s-x o") 'helm-occur)
(global-set-key (kbd "s-x g") 'helm-do-grep)
(global-set-key (kbd "s-x p") 'helm-pydoc)
(global-set-key (kbd "s-x t") 'helm-top)
(global-set-key (kbd "s-x y") 'helm-yas-complete)
(global-set-key (kbd "s-x f") 'helm-projectile)
(global-set-key (kbd "s-x s-f") 'helm-browse-project)
(global-set-key (kbd "s-x b") 'helm-buffers-list)
(global-set-key (kbd "s-x k") 'helm-show-kill-ring)
(global-set-key (kbd "s-x s-x") 'helm-helm-commands)
(global-set-key (kbd "s-x f") 'helm-recentf)

; s-p: project explorer (browser)


(provide 'cnfg-helm)
