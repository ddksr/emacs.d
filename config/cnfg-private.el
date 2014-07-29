;; private vars:
;; - my-nick
;; - my-full-name
(setq my-nick ""
      my-full-name "")
(if (eq system-type 'gnu/linux) (load-file (concat my-emacs-d "config/private.el.gpg")))
(provide 'cnfg-private)
