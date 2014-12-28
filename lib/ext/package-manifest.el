;;; package-manifest.el --- Sync your packages via a manifest file

;; Copyright (C) 2013 Mattias Bengtsson

;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; URL: 
;; Version: 20131103.43
;; Keywords: packages
;; Package-Requires: ((emacs "24.1"))


(defgroup package-manifest nil
  "Extends package.el with a manifest. *UPDATE*"
  :group 'applications)

;; TODO: Investigate 
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Definitions.html
;; Specifically :initialize to see if it is possible to initialize this to the list of
;; currently installed packages.
(defcustom package-manifest nil
  "A list of packages that the user expects to have installed. 
The package-manifest-sync function will try to install any package on
this list and also uninstall any external package not on this list."
  :type 'list
  :group 'package-manifest)



;;;###autoload
(defadvice package-install (after package-manifest-install-advice activate)
  (package-manifest--add-to-manifest (symbol-name (ad-get-arg 0)))
)
;;;###autoload
(defadvice package-delete (after package-manifest-uninstall-advice activate)
  (package-manifest--remove-from-manifest (ad-get-arg 0))
)



;;;###autoload
(defun package-manifest--add-to-manifest (pkg-name)
  (customize-save-variable 'package-manifest (cons pkg-name package-manifest))
  (message (concat "Added " pkg-name " to manifest"))
)

;;;###autoload
(defun package-manifest--remove-from-manifest (pkg-name)
  (customize-save-variable 'package-manifest (delete pkg-name package-manifest))
  (message (concat "Removed " pkg-name " from manifest"))
)

;;;###autoload
(defun package-manifest-sync ()
  (interactive)
  (message "package-manifest-sync")
  (package-manifest-install-missing)
  (package-manifest-uninstall-extras)
)

;;;###autoload
(defun package-manifest-install-missing ()
  (interactive)
  (message "package-manifest-install-missing")
)

;;;###autoload
(defun package-manifest-uninstall-extras ()
  (interactive)
  (message "package-manifest-uninstall-extras")
)

(provide 'package-manifest)

;;; package-manifest.el ends here
