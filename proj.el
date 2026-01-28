(require 'cl-lib)
(require 'seq)

(defvar proj-locations
  '("~/dev" "~/latex" "~/rpi")
  "Value for where to look for projects")

(defvar proj-find-params
  '("-mindepth 2" "-maxdepth 2" "-path '*/.git'" "-prune -o" "-type d" "-print")
  "parameters to run find with")

(defvar proj-current nil
  "Current project root")

(defvar proj-grep-function #'grep
  "What function to run for proj-grep")

(defvar proj-compile-commands '()
  "Compile commands for each project")

(defvar proj-compilation-directories '()
  "Compilation directories for each project")

(defmacro proj--clean-path (path) `(string-replace (getenv "HOME") "~" ,path))

(defun proj--get-buffer-path (buffer)
  (if (or (string-search "magit" (buffer-name buffer))
          (string-search "compilation" (buffer-name buffer)))
      (with-current-buffer buffer default-directory)
    (or (with-current-buffer buffer dired-directory) (buffer-file-name buffer))))

(defun proj--get-paths ()
  (delete proj-current
   (flatten-list (mapcar
     (lambda (p)
       (mapcar (lambda (str) (proj--clean-path (concat str "/")))
        (split-string (shell-command-to-string
          (concat "find " p " " (mapconcat (lambda (param) (concat param " ")) proj-find-params)))
         "\n" t)))
            proj-locations))))

(defun proj-set-dir (dir &optional quiet)
  (interactive "D")
  (setq proj-current (file-truename (concat dir "/")))
  (unless quiet (dired proj-current))
  ;; set compilation command and directory when we switch
  (setq compile-command (alist-get proj-current proj-compile-commands "make -k" nil #'equal))
  (setq compilation-directory (alist-get proj-current proj-compilation-directories proj-current nil #'equal)))

(defun proj-swap (&optional dir)
  (interactive)
  (if dir
      (proj-set-dir dir)
    (let* ((completion-extra-properties '(:category file))
           (choice
            (completing-read
             (concat "Switch to project"
                     (when proj-current
                       (concat " (" (proj--clean-path proj-current) ")"))
                     ": ")
             (append (proj--get-paths) '("Some other directory"))
             nil t nil nil proj-current)))
      (if (equal choice "Some other directory")
          (call-interactively 'proj-set-dir)
        (proj-set-dir choice)))))

(cl-defun proj-find-file (&optional filename &key all)
  (interactive (list nil :all current-prefix-arg))
  (unless proj-current (proj-swap))
  (when filename (find-file filename))
  (let ((completion-extra-properties '(:category file))
        (default-directory proj-current))
    (find-file
     (completing-read
      (concat "Find file in " (proj--clean-path proj-current) ": ")
      (mapcar (lambda (str) (string-replace proj-current "" str))
              (split-string (shell-command-to-string
                             (concat "find " proj-current (unless all " -path '*/.*' -prune -o") " -path '*/.git' -prune -o -type f -print"))
                            "\n" t))))))

(defun proj-switch-to-buffer (&optional buffer-or-name)
  (interactive)
  (unless proj-current (proj-swap))
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
  (unless proj-current (proj-swap))
  (kill-buffer
   (or buffer-or-name
       (let* ((other-buffer (other-buffer (current-buffer)))
              (other-name (buffer-name other-buffer))
              (pred (lambda (b)
                      (let ((path (proj--get-buffer-path (cdr b))))
                        (when path (file-in-directory-p (file-truename path) proj-current))))))
         (read-buffer
          (concat "Kill buffer in " (proj--clean-path proj-current) ": ")
          (when (funcall pred (cons other-name other-buffer))
            other-name)
          nil pred)))))

(defun proj-dired ()
  (interactive)
  (if proj-current
      (dired proj-current)
    (proj-swap)))

(defun proj-grep ()
  (interactive)
  (unless proj-current (proj-swap))
  (let ((default-directory proj-current))
    (when proj-grep-function (call-interactively proj-grep-function))))

(defun proj-compile ()
  (interactive)
  (unless proj-current (proj-swap))
  (let ((default-directory proj-current))
    (call-interactively 'compile)))

(defun proj-recompile ()
  (unless proj-current (proj-swap))
  (let ((default-directory proj-current))
    (call-interactively 'recompile)))

(defun proj-compile-action (_)
  "grab compilation command and directory whenever we compile"
  (when proj-current
	(progn
	  (setf (alist-get proj-current proj-compile-commands nil nil #'equal)
			compile-command)
	  (setf (alist-get proj-current proj-compilation-directories nil nil #'equal)
			compilation-directory))))
(add-hook 'compilation-start-hook 'proj-compile-action)

(defun proj--compilation-buffer-name-function (mode)
  (if proj-current
      (concat "*" (file-name-nondirectory (directory-file-name proj-current)) ": compilation*")
    (concat "*" (downcase mode) "*")))
(setq compilation-buffer-name-function 'proj--compilation-buffer-name-function)

(defun proj-copy-root-dir ()
  (interactive)
  (unless proj-current (proj-swap))
  (kill-new proj-current))

(defun proj-execute ()
  (interactive)
  (unless proj-current (proj-swap))
  (let ((default-directory proj-current))
	(execute-extended-command nil)))

(defun proj-shell-command ()
  (interactive)
  (unless proj-current (proj-swap))
  (let ((default-directory proj-current))
	(call-interactively 'shell-command)))

(defvar proj-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'proj-find-file)
    (define-key map "b" 'proj-switch-to-buffer)
    (define-key map "k" 'proj-kill-buffer)
    (define-key map "s" 'proj-set-dir)
    (define-key map "p" 'proj-swap)
    (define-key map "d" 'proj-dired)
    (define-key map "c" 'proj-compile)
    (define-key map "r" 'proj-recompile)
    (define-key map "g" 'proj-grep)
    (define-key map "y" 'proj-copy-root-dir)
    (define-key map "x" 'proj-execute)
    (define-key map (kbd "M-!") 'proj-shell-command)
    map))

(define-key ctl-x-map "p" proj-prefix-map)

(provide 'proj)
