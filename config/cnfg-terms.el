(setq terminals '(
				  ("zgit" . "cd ~/zgit;")
				  ("ipy" . "ipython")))
(setq virtualenvs-loc "/home/sigi/.virtualenvs/")
(setq twstations '(
				   ("eyepatch" . (lambda ()
								   (open-terminal "zgit")
								   (command-terminal "cd eyepatch; workon eyepatch;")
								   (split-window-right)
								   (other-window 1)
								   (open-terminal "zgit")
								   (command-terminal "cd eyepatch;")
								   (run-virtualenv-command-terminal "eyepatch" "manage.py runserver localhost:9090")
								   
								   (split-window-below)
								   (open-terminal "zgit")
								   (command-terminal "ssh eyepatch@portal2")
								   ))))

(defun run-virtualenv-command-terminal (name command)
  (command-terminal (concat virtualenvs-loc name "/bin/python " command)))

(defun command-terminal (command)
  (term-send-raw-string command)
  (term-send-raw-string "\n"))

(defun new-terminal (command)
  (multi-term)
  (command-terminal command))

(defun open-terminal (&optional tconf)
  (interactive)
  (let ((config (if tconf tconf (read-string "Configuration: " (car (car terminals)) nil terminals (car (car terminals))))))
	(let ((command (cdr (assoc config terminals))))
	  (if command
		  (progn
			(new-terminal command))
		(message "Configuration not found")))))

(defun open-station (&optional tstat)
  (interactive)
  (let ((config (if tstat tstat (read-string "Station: " (car (car twstations)) nil twstations (car (car twstations))))))
	(let ((command (cdr (assoc config twstations))))
	  (if command
		  (progn
			(funcall command))
		(message "Station not found")))))

;(mapcar (lambda (x) (car x))

(provide 'cnfg-terms)
