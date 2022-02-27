(require 'ob-tangle)
(setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))
(put 'downcase-region 'disabled nil)
