(defun own/no-private () 
  (setq my-nick ""
		my-full-name "")
  (setq my-org-docs-dir '("~"))
  (setq helm-delicious-user ""
		helm-delicious-password ""))

(if (file-exists-p (concat my-emacs-d "config/private.el.gpg"))
	(load-file (concat my-emacs-d "config/private.el.gpg"))
  (own/no-private))


(provide 'cnfg-private)
