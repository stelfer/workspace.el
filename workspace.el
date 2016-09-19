
(require 'vc-git)
(require 'projectile)
(require 'dash)

;;;###autoload
(define-minor-mode workspace-mode
  "A workspace mode"
  :lighter workspace-mode-line
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c w c") 'workspace-create)
	    (define-key map (kbd "C-c w l") 'workspace-load)
	    map)
  ())

(defvar workspace-current-tag nil)
(defvar workspace-current-project nil)

(defvar workspace-sync-timer nil)
(defvar workspace-sync-ival 0.5)

;;;###autoload
(defface workspace-modeline-face '((t (:foreground "orange" :weight normal)))  "A face for the workspace modeline")

(defun workspace-matchy-matchy()
  (if (equal workspace-current-project (projectile-project-name))
      ""
    "* "))

;;;###autoload
(defvar workspace-mode-line
  '(:propertize
    (:eval
     (concat " [" (workspace-matchy-matchy) workspace-current-tag "/" workspace-current-project "]"))
    face workspace-modeline-face))

(put 'workspace-mode-line 'risky-local-variable t)

;;;###autoload
(add-hook 'projectile-mode-hook 'workspace-mode)

;;;###autoload
(add-hook 'after-init-hook (lambda ()
		       (workspace-start-on-init)))

;; (defvar workspace-stash-sha nil)
(defvar workspace-helpful-hello "HI")

(defvar workspace-extra-vars '(workspace-helpful-hello workspace-current-project workspace-current-tag compile-history))

;;;###autoload
(with-eval-after-load "desktop"
  (mapcar (lambda (x) (add-to-list 'desktop-globals-to-save x)) workspace-extra-vars))

(defun workspace-local-branches() (vc-git-branches))

(defun workspace-local-branch() (car (workspace-local-branches)))

(defun workspace-prepare-desktop-vars (tag)
  "Set all of the global variables used by desktop"
  (setq desktop-dirname  (concat (file-name-as-directory (projectile-project-root)) ".desktops"))
  (setq desktop-base-file-name tag)
  (setq desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))

(defun workspace-tags()
  "All of the valid tags for this project"
  (let* ((desktop-dirname (concat (file-name-as-directory (projectile-project-root)) ".desktops"))
	 (files (and (file-exists-p desktop-dirname) (directory-files desktop-dirname )))
	 (files-exclude-regexp "^\\.\\|\\..*$"))
    (-intersection
     (-remove (lambda (x) (string-match files-exclude-regexp x)) files)
     (workspace-local-branches))))

(defun workspace-invalid-tags()
  "Find tag for which there is no branch"
  (let* ((desktop-dirname (concat (file-name-as-directory (projectile-project-root)) ".desktops"))
	 (files (and (file-exists-p desktop-dirname) (directory-files desktop-dirname )))
	 (files-exclude-regexp "^\\.\\|\\..*$"))
    (-difference
     (-remove (lambda (x) (string-match files-exclude-regexp x)) files)
     (workspace-local-branches))))

(defun workspace-cleanup-tags()
  "Cleanup orphaned and invalid tags"
  (-map (lambda (x)
	  (let* ((file (concat (file-name-as-directory (projectile-project-root)) ".desktops/" x )))
	    (if (file-exists-p file)
		(delete-file file)
	      (error "File %s doesn't exist" file))))
	(workspace-invalid-tags)))

(defun workspace-create (tag)
  "Create a workspace"
  (unless (desktop-save-mode)
    (desktop-save-mode t)) 

  (unless (projectile-project-p)
    (error "Not a project"))

  (unless (-contains? (workspace-local-branches) tag)
    (vc-git-create-tag default-directory tag t))
  
  (workspace-prepare-desktop-vars tag)
  
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname))
  
  (let ((desktop-restore-eager t))
    (desktop-save desktop-dirname 'RELEASE 'AUTOSAVE)))

(defun workspace-load (&optional tag)
  "Load a workspace"
  (interactive
   (list (completing-read "tag: " (workspace-tags) nil nil)))

  (unless (desktop-save-mode)
    (desktop-save-mode t))
  
  (unless (projectile-project-p)
    (error "Not a project"))
  
  (unless (-contains? (workspace-tags) tag)
    (workspace-create tag))

  (condition-case err
      (vc-git-retrieve-tag default-directory tag t)
    (error (error "Can't checkout branch: %s" err)))
  
  (desktop-release-lock)

  (workspace-prepare-desktop-vars tag)
  (desktop-clear)
  (desktop-read desktop-dirname)

  (setq workspace-current-tag tag)
  (setq workspace-current-project (projectile-project-name))

  (workspace-sync-update-timer workspace-sync-ival))

(defun workspace-switch-to-tag(branch)
  "Switch to a tag"
  (let ((current-tag (and branch (-contains? (workspace-tags) branch))))
    (unless current-tag
      (when (yes-or-no-p (format "Create workspace for branch:%s: " branch))
	(workspace-create branch)))
    (workspace-load branch)))

(defun workspace-start ()
  "A Friendly start function, only starts if there is a branch"
  (let ((current-branch (workspace-local-branch)))
    (when current-branch
      (workspace-switch-to-tag current-branch))))

(defun workspace-start-on-init ()
  "If emacs started with no command line args, and this mode is loaded, then automatically start"
  (when (< (length command-line-args) 2)
    (setq inhibit-startup-message t)	; If we don't do this, it will put the startup buffer on top
    (message "Automatically starting workspace: %s" (workspace-local-branch))
    (workspace-start)))

(defun workspace-sync ()
  "Periodically check the local directory to see if the branch was changed (e.g. by command line)"
  (when (and (not (equal (workspace-local-branch) workspace-current-tag))
	   (equal (projectile-project-name) workspace-current-project))
    (cancel-timer workspace-sync-timer) ; kill the syncing until we load another workspace
    (let ((use-dialog-box nil))
      (if (y-or-n-p (format "VC branch changed on filesystem. Switch to workspace `%s'"  (workspace-local-branch)))
	  (workspace-switch-to-tag (workspace-local-branch))
	(message "Disabling sync!")))))
  
(defun workspace-sync-update-timer (value)
  "Update the sync timer"
  (when workspace-sync-timer
    (cancel-timer workspace-sync-timer))
  (setq workspace-sync-timer
	(run-with-idle-timer value t 'workspace-sync)))
	   
(provide 'workspace)
