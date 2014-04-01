;;
;; OTHER
;;

;; WEB BROWSER ( NOT OK YET )
;;(require 'webkit "~/.emacs.d/webkit/webkit.el")

;; HOW DOI

(define-key global-map (kbd "s-h s") 'howdoi-query-line-at-point-replace-by-code-snippet)
(global-set-key (kbd "s-h q") 'howdoi-query)

;;
;; IRC
;; 
; M-x start-irc
(defun start-irc ()
   "Connect to IRC."
   (interactive)
   (erc :server "irc.freenode.net" :port 6667
        :nick my-nick :full-name my-full-name)
   (setq erc-autojoin-channels-alist '(("freenode.net" "#coderdojo.si" "#emacs"))))

; Notifikacije:
(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

;;
;; LATEX
;;

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; visual-line-mode ALI auto-fill-mode (NE OBOJE)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode) 
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

; Compile and preview with C-c C-c


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)
;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1") )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Okular as the pdf viewer. Build okular 
;; command, so that Okular jumps to the current line 
;; in the viewer.
(setq TeX-view-program-selection
 '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
 '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'latex '(latex/setup-keybinds))

;;
;; ORG MODE
;;

(require 'org-install)
(require 'org-habit)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)

(setq org-agenda-files 
      (file-expand-wildcards "~/docs/org/*.org"))
(setq org-directory "~/docs/org/")

;;
;; ORG TRELLO
;;
;(require 'org-trello)
;(add-hook 'org-mode-hook 'org-trello-mode)

;;
;; HTTPREPEL
;;

(require 'httprepl)

;;
;; PRODIGY
;;
(require 'prodigy)
(prodigy-define-service
  :name "Zemanta RTWTD"
  :command "/home/sigi/.virtualenvs/rtwtd/bin/python"
  :args '("manage.py" "runserver" "localhost:9095")
  :cwd "/home/sigi/projekti/zemanta/repo/rtwtd"
  :tags '(work zemanta django)
  :port 9095
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Zemanta REPS"
  :command "/home/sigi/.virtualenvs/reps/bin/python"
  :args '("manage.py" "runserver" "localhost:9094")
  :cwd "/home/sigi/projekti/zemanta/repo/reps/reps"
  :tags '(work zemanta django)
  :port 9090
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Zemanta EYEPATCH"
  :command "/home/sigi/.virtualenvs/eyepatch/bin/python"
  :args '("manage.py" "runserver" "localhost:9093")
  :cwd "/home/sigi/projekti/zemanta/repo/eyepatch"
  :tags '(work zemanta django)
  :port 9091
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)


(provide 'cnfg-specific)
