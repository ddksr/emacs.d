;;
;; FUNCTIONS
;;

(defun own/new-line-after (times)
  (interactive "p")
  (save-excursion
	(move-end-of-line 1)
	(newline times)))
(global-set-key (kbd "s-l s-l") 'own/new-line-after)

(defun own/new-line-before (times)
  (interactive "p")
  (save-excursion
	(move-beginning-of-line 1)
	(newline times)
  ))
(global-set-key (kbd "s-M-l s-M-l") 'own/new-line-before)

(defun own/reopen-file-with-sudo ()
  "Open the currently visited file as root via sudo."
  (interactive)
  (if (buffer-file-name)
    (let ((file-name (buffer-file-name)))
      (kill-buffer (current-buffer))
      (find-file (concat "/sudo::" file-name))
      (message "now editing %s as root" file-name))))

(defun own/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun own/set-pyflakes (bin-path)
  "Set the pyflakes executive"
  (interactive "FPyflakes find file: ")
  (setq flymake-python-pyflakes-executable bin-path))

;; SHOW CURRENT FILE FILENAME
(defun own/show-filename ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))


;; DUPLICATE CURRENT LINE (s-d)
(defun own/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "s-l d") 'own/duplicate-line)

;; COMBINE LINES
(defun own/combine-lines ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "s-l c") 'own/combine-lines)

;; SPLIT WINDOW MULTIPLE WAYS
(defun own/split-window-multiple-ways (x y)
    "Split the current frame into a grid of X columns and Y rows."
	  (interactive "nColumns: \nnRows: ")
	    ;; one window
	    (delete-other-windows)
		  (dotimes (i (1- x))
			      (split-window-horizontally)
				        (dotimes (j (1- y))
						  (split-window-vertically))
						      (other-window y))
		    (dotimes (j (1- y))
			      (split-window-vertically))
			  (balance-windows))

;; SHOW WINDOWS WITH MAJOR MODE
(defun own/show-buffers-with-major-mode (mode)
    "Fill all windows of the current frame with buffers using major-mode MODE."
	  (interactive
	      (let* ((modes (loop for buf being the buffers
							         collect (symbol-name (with-current-buffer buf
															      major-mode)))))
			     (list (intern (completing-read "Mode: " modes)))))
	    (let ((buffers (loop for buf being the buffers
							        when (eq mode (with-current-buffer buf
													       major-mode))
									       collect buf)))
		      (dolist (win (window-list))
				      (when buffers
						(show-buffer win (car buffers))
						(setq buffers (cdr buffers))))))

;; GOOGLE SEARCH
(defun own/google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

;; SWITCH DICTIONARY (<f8>)
(defun own/fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "slovenian") "english" "slovenian")))
	(ispell-change-dictionary change)
	(message "Dictionary switched from %s to %s" dic change)
	))
(global-set-key (kbd "<f8>")   'own/fd-switch-dictionary)


;; FIND USER INIT FILE
(defun own/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))


;; FIND SHELL INIT FILE
(defun own/find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))


;; GO-TO URL
(defun own/goto-url ()
  "Open browser"
  (interactive)
  (browse-url 
	 (concat "http://" (read-string "URL: ") )))


;; DELETE CURRENT BUFFER FILE (C-x C-k)
(defun own/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'own/delete-current-buffer-file)


;; RENAME CURRENT BUFFER FILE (C-x C-r)
(defun own/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'own/rename-current-buffer-file)


;;
;; HOOKS
;; 

;; MARK TODOS, FIXME AND SIMILAR 
(defun hook-mark-todo () 
  (font-lock-add-keywords nil
						  '(("\\<\\(FIXME\\|SIGITODO\\|TODO\\|BUG\\):"
							 1 font-lock-warning-face t))))




(provide 'cnfg-funcs)

