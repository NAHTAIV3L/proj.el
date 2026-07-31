(require 'cl-lib)
(require 'compile)

;; Configurable Variables
(defvar proj-locations '()
  "Value for where to look for projects comes in the form (dir . depth)")

(defvar proj-find-params '()
  "parameters to run find with")

(defvar proj-grep-function #'grep
  "What function to run for proj-grep")

(defvar proj-no-project-name "No Project"
  "String to show as no project")

;; Important State
(defvar proj-current proj-no-project-name
  "Current project root")

(defvar proj-state (make-hash-table :test 'eq)
  "project state per project")

(defvar proj-property-handlers '()
  "handlers for setting and getting per project state")

(defvar proj-history-list '()
  "history list for proj-swap-to")

(defvar proj-before-change-hook '()
  "hook to run before changing project state")

(defvar proj-after-change-hook '()
  "hook to run after changing project state")

;; Utils
(defmacro proj--clean-path (path) `(string-replace (getenv "HOME") "~" ,path))

(defmacro proj--str-to-key (path) `(intern (concat ":" ,path)))
(defmacro proj--key-to-str (key) `(substring (symbol-name ,key) 1))

(defmacro proj--current-state () `(gethash (proj--str-to-key proj-current) proj-state nil))

(defmacro proj--plist-map (func plist)
  `(cl-loop for (key value) on ,plist by 'cddr do
           (funcall ,func key value)))

(defun proj--get-buffer-path (buffer) (with-current-buffer buffer default-directory))

(defun proj--is-inactive () (equal proj-current proj-no-project-name))

(defun proj-previously-opened ()
  (if (hash-table-keys proj-state)
      (mapcar (lambda (key) (proj--clean-path (proj--key-to-str key)))
              (hash-table-keys proj-state))
    nil))

(defun proj--get-paths ()
  (mapcar
   (lambda (str)
     (if (equal str proj-no-project-name)
         str
       (proj--clean-path str)))
   (delete proj-current
           (mapcar
            (lambda (str)
              (if (equal str proj-no-project-name)
                  str
                (file-truename (concat str "/"))))
            (append (list proj-no-project-name)
                    (mapcar (lambda (key) (proj--key-to-str key))
                            (hash-table-keys proj-state))
                    (flatten-list
                     (mapcar
                      (lambda (p)
                        (let ((dir   (car p))
                              (depth (number-to-string (cdr p))))
                          (split-string
                           (shell-command-to-string
                            (concat "find " dir " "
                                    (mapconcat (lambda (param) (concat param " "))
                                               (append (list "-mindepth " depth "-maxdepth " depth
                                                             "-path '*/.git'" "-prune -o" "-type d" "-print")
                                                       proj-find-params))))
                           "\n" t)))
                      proj-locations)))))))

;; Property Helpers
(defmacro proj-add-property-handler (property handler)
  `(setq proj-property-handlers (plist-put proj-property-handlers ,property ,handler)))

(defmacro proj--gen-handler (&rest body)
  "calls every section with ACTION and VALUE defined"
  (let ((cases nil)
        (function-doc nil)
        (current-key nil)
        (current-forms nil))
    (when (stringp (car body))
      (setq function-doc (pop body)))
    (while body
      (setq current-key (pop body))
      (when (not (memq current-key '(:set-emacs-state :get-emacs-state :set-default-emacs-state)))
        (error "Invalid Symbol: %s" (symbol-name current-key)))
      (setq current-forms nil)
      (while (and body (not (keywordp (car body))))
        (push (pop body) current-forms))
      (push `(,current-key ,@(nreverse current-forms)) cases))

    (if function-doc
        `(lambda (action value)
           ,function-doc
           (pcase action
             ,@(nreverse cases)))
      `(lambda (action value)
         (pcase action
           ,@(nreverse cases))))))

(defmacro proj--var-handler-emacs-default (symbol)
  "Generates property handler for global variable SYMBOL, takes emacs default as default value."
  (let ((startup-value (symbol-value symbol)))
    `(proj--gen-handler
      :set-emacs-state
      (setq ,symbol value)
      :get-emacs-state
      ,symbol
      :set-default-emacs-state
      (setq ,symbol ,startup-value))))

(defmacro proj--var-handler-default (symbol default)
  "Generates property handler for global variable SYMBOL, specify DEFAULT value."
  `(proj--gen-handler
    :set-emacs-state
    (setq ,symbol value)
    :get-emacs-state
    ,symbol
    :set-default-emacs-state
    (setq ,symbol ,default)))

(defun proj--save-state ()
  "Saves state of current project"
  ;; add current proj to hash table if not in it
  (unless (gethash (proj--str-to-key proj-current) proj-state nil)
    (puthash (proj--str-to-key proj-current) (make-hash-table :test 'eq) proj-state))

  ;; saves state of current project to hash table
  (proj--plist-map
   (lambda (key value)
     (let ((current-state (proj--current-state))
           (val (funcall value :get-emacs-state nil)))
       (puthash key val current-state)))
   proj-property-handlers))

(defun proj--restore-state ()
  "Restores state of current project, or sets up the default"
  (if (gethash (proj--str-to-key proj-current) proj-state nil)
	  ;; restore emacs values
      (proj--plist-map
       (lambda (key value)
		 (let* ((current-state (proj--current-state))
				(val (gethash key current-state nil)))
           (funcall value :set-emacs-state val)))
       proj-property-handlers)
	;; or restore default values
	(proj--plist-map
	 (lambda (key value)
       (funcall value :set-default-emacs-state nil))
	 proj-property-handlers)))

;; User functions
(defun proj-set (dir)
  "Sets current project to dir and updates state. The most important function"
  (interactive (list (read-directory-name "Set project directory: "
                                          default-directory)))
  (if (or (equal dir proj-current) (file-equal-p dir proj-current))
	  (message "Project is already open")

    (run-hooks proj-before-change-hook)
	;; save state of current project
	(proj--save-state)

	;; set proj current
	(setq proj-current (if (equal dir proj-no-project-name)
						   dir
                         (file-truename (concat dir "/"))))

	;; restore state of new project
	(proj--restore-state)
    (run-hooks proj-after-change-hook)))

(defun proj-swap-to ()
  (interactive)
  (let* ((completion-extra-properties '(:category file))
         (history-delete-duplicates t)
         (choice
          (completing-read
           (concat "Switch to project"
                   (when proj-current
                     (concat " (" (proj--clean-path proj-current) ")"))
                   ": ")
           (proj--get-paths)
           nil t nil 'proj-history-list)))
    (proj-set choice)))

(defun proj-close ()
  (interactive)
  (unless (gethash (proj--str-to-key proj-current) proj-state nil)
    (puthash (proj--str-to-key proj-current) (make-hash-table :test 'eq) proj-state))

  (proj--plist-map
   (lambda (key value)
     (let ((current-state (proj--current-state))
           (val (funcall value :get-emacs-state nil)))
       (puthash key val current-state)))
   proj-property-handlers)

  (let* ((completion-extra-properties '(:category file))
         (choice
          (file-truename
           (concat
            (completing-read
             (concat "Kill project"
                     (when proj-current
                       (concat " (" (proj--clean-path proj-current) ")"))
                     ": ")
             (delete proj-no-project-name (proj-previously-opened))
             nil t nil nil (if (proj--is-inactive) nil proj-current))
            "/")))
         (pred
          (lambda (b)
            (let ((path (proj--get-buffer-path b)))
              (when path (file-in-directory-p (file-truename path) choice))))))
    (when (equal choice proj-current) (proj-set proj-no-project-name))
    (mapcar 'kill-buffer (seq-filter pred (buffer-list)))
    (let ((closed-project-state (gethash (proj--str-to-key choice) proj-state nil)))
      (when (and closed-project-state (gethash :window-configuration closed-project-state nil)
                 (puthash :window-configuration nil closed-project-state))))))

(defun proj-find-file ()
  (interactive)
  (if (proj--is-inactive)
      (call-interactively 'find-file)
    (let* ((completion-extra-properties '(:category file))
           (default-directory proj-current)
	   ;; uses git ls to find files if in git repo, and find if not
           (is-git (zerop (shell-command "git rev-parse --is-inside-work-tree")))
           (cmd (if is-git
                    "git ls-files --cached --others --exclude-standard"
                  (concat "find " proj-current " -path '*/.*' -prune -o -type f -print")))
           (cmd-output (split-string (shell-command-to-string cmd) "\n" t))
           ;; change files to absolute paths if git ls
           (files (if is-git
                      cmd-output
                    (mapcar (lambda (str) (string-replace proj-current "" str))
                            cmd-output))))
      (find-file
       (completing-read
        (concat "Find file in " (proj--clean-path proj-current) ": ")
        files)))))

(defun proj-find-file-all ()
  (interactive)
  (if (proj--is-inactive)
      (call-interactively 'find-file)
    (let ((completion-extra-properties '(:category file))
          (default-directory proj-current))
      (find-file
       (completing-read
        (concat "Find any file in " (proj--clean-path proj-current) ": ")
        (mapcar (lambda (str) (string-replace proj-current "" str))
                (split-string (shell-command-to-string
                               (concat "find " proj-current " -path '*/.git' -prune -o -type f -print"))
                              "\n" t)))))))

(defun proj-switch-to-buffer ()
  (interactive)
  (if (proj--is-inactive)
      (call-interactively 'switch-to-buffer)
    (switch-to-buffer
     (let* ((other-buffer (other-buffer (current-buffer)))
            (other-name (buffer-name other-buffer))
            (pred (lambda (b)
                    (let ((path (proj--get-buffer-path (cdr b))))
                      (when path
                        (or
                         (equal (car b) "*scratch*")
                         (file-in-directory-p (file-truename path) proj-current)))))))
       (read-buffer
        (concat "Switch to buffer in " (proj--clean-path proj-current) ": ")
        (when (funcall pred (cons other-name other-buffer))
          other-name)
        nil pred)))))

(defun proj-kill-buffer ()
  (interactive)
  (if (proj--is-inactive)
      (call-interactively 'kill-buffer)
    (kill-buffer
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
  (if (proj--is-inactive)
      (call-interactively 'dired)
    (dired proj-current)))

(defun proj-grep ()
  (interactive)
  (let ((default-directory (if (proj--is-inactive) default-directory proj-current)))
    (when proj-grep-function (call-interactively proj-grep-function))))

(defun proj-compile ()
  (interactive)
  (let ((default-directory (if (proj--is-inactive) default-directory proj-current)))
    (call-interactively 'compile)))

(fset 'proj-recompile 'recompile)

(defun proj--compilation-buffer-name-function (mode)
  (if (proj--is-inactive)
      (concat "*" (downcase mode) "*")
    (concat "*" (file-name-nondirectory (directory-file-name proj-current)) ": compilation*")))
(setq compilation-buffer-name-function 'proj--compilation-buffer-name-function)

(defun proj-copy-root-dir ()
  (interactive)
  (let ((value (if (proj--is-inactive)
                   default-directory
                 proj-current)))
    (kill-new value)
    (gui-set-selection nil value)))

(defun proj-execute ()
  (interactive)
  (let ((default-directory (if (proj--is-inactive) default-directory proj-current)))
	(execute-extended-command nil)))

(defun proj-shell-command ()
  (interactive)
  (let ((default-directory (if (proj--is-inactive) default-directory proj-current)))
	(call-interactively 'shell-command)))

(defun proj-async-shell-command ()
  (interactive)
  (let ((default-directory (if (proj--is-inactive) default-directory proj-current)))
	(call-interactively 'async-shell-command)))

;; assign keybinds
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

;; set up properties
(proj-add-property-handler :window-configuration (proj--gen-handler
                                                  :set-emacs-state
                                                  (if (and
                                                       value
                                                       (eq (window-configuration-frame value)
                                                           (selected-frame)))
                                                      (progn
                                                        (set-window-configuration value))
                                                    (dired proj-current)
                                                    (delete-other-windows))
                                                  :get-emacs-state
                                                  (current-window-configuration)
                                                  :set-default-emacs-state
                                                  (dired proj-current)
                                                  (delete-other-windows)))
(proj-add-property-handler :compile-command (proj--var-handler-emacs-default compile-command))
(proj-add-property-handler :compilation-directory (proj--var-handler-default compilation-directory proj-current))

(provide 'proj)
