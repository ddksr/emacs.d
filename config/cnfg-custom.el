;; Zasedeni: ž, đ, š
;; č namenjen ostalim znakom

(global-set-key (kbd "M-ž") 'backward-paragraph)
(global-set-key (kbd "C-ž") 'forward-paragraph)
(global-set-key (kbd "M-đ") 'scroll-other-window)
(global-set-key (kbd "C-đ") 'scroll-other-window-down)

(global-set-key (kbd "s-l b") 'delete-blank-lines)
(global-set-key (kbd "s-w") 'delete-region)
(global-set-key (kbd "s-i") 'indent-region)
(global-set-key (kbd "s-u") 'undo)

;;(global-set-key (kbd "C-č y") ')

;; Razni drudi

(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "C-s-u") 'repeat)
(global-set-key (kbd "<f5>") 'rgrep)
(global-set-key (kbd "C-x C-b") 'ibuffer)



(provide 'cnfg-custom)
