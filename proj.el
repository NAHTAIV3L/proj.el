(require 'cl-lib)

(defvar proj-locations
  '("~/dev" "~/latex" "~/rpi")
  "Value for where to look for projects")

(defvar proj-find-params
  '("-mindepth 2" "-maxdepth 2" "-path '*/.git'" "-prune -o" "-type d" "-print")
  "parameters to run find with")

(defvar proj-grep-function #'grep
  "What function to run for proj-grep")

(defvar proj-current nil
  "Current project root")

;; per project state
(defvar proj-state (make-hash-table :test 'eq)
  "project state per project")

(defmacro proj--clean-path (path) `(string-replace (getenv "HOME") "~" ,path))

(defmacro proj--str-to-key (path) `(intern (concat ":" ,path)))
(defmacro proj--key-to-str (key) `(substring (symbol-name ,key) 1))

(defmacro proj--plist-get-def (plist prop def) `(or (plist-get ,plist ,prop) ,def))

(defmacro proj--current-state () `(gethash (proj--str-to-key proj-current) proj-state nil))

(defun proj--get-buffer-path (buffer)
  (if (or (string-search "magit" (buffer-name buffer))
          (with-current-buffer buffer (member major-mode '(shell-command-mode compilation-mode))))
      (with-current-buffer buffer default-directory)
    (or (with-current-buffer buffer dired-directory) (buffer-file-name buffer))))

(defun proj-previously-opened ()
  (if (hash-table-keys proj-state)
    (mapcar (lambda (key) (proj--clean-path (proj--key-to-str key)))
            (hash-table-keys proj-state))
    nil))

(defun proj--get-paths ()
  (delete proj-current
   (flatten-list (mapcar
     (lambda (p)
       (mapcar (lambda (str) (proj--clean-path (concat str "/")))
        (split-string (shell-command-to-string
          (concat "find " p " " (mapconcat (lambda (param) (concat param " ")) proj-find-params)))
         "\n" t)))
            proj-locations))))

(defun proj-set (dir &optional quiet)
  (interactive (list (read-directory-name "Set project directory: "
                                          default-directory)))
  (if (and (not (eq proj-current nil)) (file-equal-p dir proj-current))
	  (message "Project is already open")
	;; assign new current project
	(setq proj-current (file-truename (concat dir "/")))
	;; (add-to-list 'proj-previously-opened proj-current 'nil 'file-equal-p)
    (unless (gethash (proj--str-to-key proj-current) proj-state nil)
      (puthash (proj--str-to-key proj-current) (make-hash-table :test 'eq) proj-state))
	;; set compilation command and directory when we switch
    (let ((current-state (proj--current-state)))
      (setq compile-command (gethash :compile-command current-state "make -k"))
      (setq compilation-directory (gethash :compilation-directory current-state proj-current))
      (if (gethash :window-configuration current-state nil)
          (progn
            (set-window-configuration (gethash :window-configuration current-state))
            (other-window 1))
        (progn
          (unless quiet (dired proj-current))
          (delete-other-windows))))))

(defun proj-swap-to ()
  (interactive)
  (let* ((completion-extra-properties '(:category file))
		 (choice
		  (completing-read
		   (concat "Switch to project"
				   (when proj-current
					 (concat " (" (proj--clean-path proj-current) ")"))
				   ": ")
		   (append (proj--get-paths)
           (if (proj-previously-opened)
               (append '("NO PROJECT")
                       (mapcar (lambda (f) (proj--clean-path f)) (proj-previously-opened)))
             '("NO PROJECT")))
		   nil t nil nil proj-current)))
	(if (equal choice "NO PROJECT")
		(proj-set 'nil)
	  (proj-set choice))))

(defun proj-find-file (&optional filename)
  (interactive)
  (unless proj-current (proj-swap-to))
  (when filename (find-file filename))
  (let ((completion-extra-properties '(:category file))
        (default-directory proj-current))
    (find-file
     (completing-read
      (concat "Find file in " (proj--clean-path proj-current) ": ")
      (mapcar (lambda (str) (string-replace proj-current "" str))
              (split-string (shell-command-to-string
                             (concat "find " proj-current (concat " -path '"
                                                                    (substring proj-current 0
                                                                               (1- (length proj-current)))
                                                                    "*/.*' -prune -o")
                                     " -path '*/.git' -prune -o -type f -print"))
                            "\n" t))))))

(defun proj-find-file-all (&optional filename)
  (interactive)
  (unless proj-current (proj-swap-to))
  (when filename (find-file filename))
  (let ((completion-extra-properties '(:category file))
        (default-directory proj-current))
    (find-file
     (completing-read
      (concat "Find any file in " (proj--clean-path proj-current) ": ")
      (mapcar (lambda (str) (string-replace proj-current "" str))
              (split-string (shell-command-to-string
                             (concat "find " proj-current " -path '*/.git' -prune -o -type f -print"))
                            "\n" t))))))

(defun proj-switch-to-buffer (&optional buffer-or-name)
  (interactive)
  (unless proj-current (proj-swap-to))
  (switch-to-buffer
   (or buffer-or-name
       (let* ((other-buffer (other-buffer (current-buffer)))
              (other-name (buffer-name other-buffer))
              (pred (lambda (b)
                      (let ((path (proj--get-buffer-path (cdr b))))
                        (when path (file-in-directory-p (file-truename path) proj-current))))))
         (read-buffer
          (concat "Switch to buffer in " (proj--clean-path proj-current) ": ")
          (when (funcall pred (cons other-name other-buffer))
            other-name)
          nil pred)))))

(defun proj-kill-buffer (&optional buffer-or-name)
  (interactive)
  (unless proj-current (proj-swap-to))
  (kill-buffer
   (or buffer-or-name
       (let* ((buffer (current-buffer))
              (buffer-name (buffer-name buffer))
              (pred (lambda (b)
                      (if (eq (cdr b) buffer) t
                        (let ((path (proj--get-buffer-path (cdr b))))
                          (when path (file-in-directory-p (file-truename path) proj-current)))))))
         (read-buffer
          (concat "Kill buffer in " (proj--clean-path proj-current) ": ")
          (when (funcall pred (cons buffer-name buffer))
            buffer-name)
          nil pred)))))

(defun proj-dired ()
  (interactive)
  (if proj-current
      (dired proj-current)
    (proj-swap-to)))

(defun proj-grep ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
    (when proj-grep-function (call-interactively proj-grep-function))))

(defun proj-compile ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
    (call-interactively 'compile)))

(defun proj-recompile ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
    (call-interactively 'recompile)))

(defun proj-compile-action (_)
  "grab compilation command and directory whenever we compile"
  (when proj-current
    (let ((current-state (proj--current-state)))
    (puthash :compile-command compile-command current-state)
    (puthash :compilation-directory compilation-directory current-state))))
(add-hook 'compilation-start-hook 'proj-compile-action)

(defun proj--compilation-buffer-name-function (mode)
  (if proj-current
      (concat "*" (file-name-nondirectory (directory-file-name proj-current)) ": compilation*")
    (concat "*" (downcase mode) "*")))
(setq compilation-buffer-name-function 'proj--compilation-buffer-name-function)

;; window configuration hook
(defun proj--window-configuration-changed-action ()
  (when proj-current
      (puthash :window-configuration (current-window-configuration) (gethash (proj--str-to-key proj-current) proj-state))))
(add-hook `window-configuration-change-hook 'proj--window-configuration-changed-action)

(defun proj-copy-root-dir ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (kill-new proj-current)
  (gui-set-selection nil proj-current))

(defun proj-execute ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
	(execute-extended-command nil)))

(defun proj-shell-command ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
	(call-interactively 'shell-command)))

(defun proj-async-shell-command ()
  (interactive)
  (unless proj-current (proj-swap-to))
  (let ((default-directory proj-current))
	(call-interactively 'async-shell-command)))

(defvar proj-prefix-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "f" 'proj-find-file)
    (keymap-set map "F" 'proj-find-file-all)
    (keymap-set map "b" 'proj-switch-to-buffer)
    (keymap-set map "k" 'proj-kill-buffer)
    (keymap-set map "s" 'proj-set)
    (keymap-set map "p" 'proj-swap-to)
    (keymap-set map "d" 'proj-dired)
    (keymap-set map "c" 'proj-compile)
    (keymap-set map "r" 'proj-recompile)
    (keymap-set map "g" 'proj-grep)
    (keymap-set map "y" 'proj-copy-root-dir)
    (keymap-set map "x" 'proj-execute)
    (keymap-set map "!" 'proj-shell-command)
    (keymap-set map "&" 'proj-async-shell-command)
    map))

(keymap-set ctl-x-map "p" proj-prefix-map)

(provide 'proj)
