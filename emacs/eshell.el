;;; eshell.el --- Eshell and shell helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq shell-command-prompt-show-cwd t)

(defvar chills/command-datetime-format "%Y-%m-%d %H:%M:%S"
  "Timestamp format used in async shell command buffer names.")

(defvar-local chills/async-shell-command nil
  "Command string used to create the current async shell command buffer.")

(defvar-local chills/async-shell-command-directory nil
  "Working directory used to create the current async shell command buffer.")

(defvar chills/async-shell-command-preserve-window nil
  "When non-nil, keep `async-shell-command' from selecting output windows.")

(defvar chills/last-async-shell-command-buffer nil
  "Most recently created async shell command output buffer.")

(defvar chills/last-async-shell-command-output-buffer-name nil
  "Most recently generated async shell output buffer name.")

(defun chills/command-datetime-string ()
  "Return the current timestamp string."
  (format-time-string chills/command-datetime-format))

(defun chills/set-output-buffer-name (args)
  "Wrap `async-shell-command' ARGS with a timestamped output buffer name."
  (let* ((command (car args))
         (time (chills/command-datetime-string))
         (output-buffer (concat "*Shell [" time "] (Running) " command)))
    (setq chills/last-async-shell-command-output-buffer-name output-buffer)
    (list command output-buffer (nth 2 args))))

(defun chills/process-callback (buffer process signal)
  "Handle PROCESS exit in BUFFER, then pass SIGNAL to `shell-command-sentinel'."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer buffer
      (rename-buffer
       (string-replace
        "(Running)"
        (concat "(Exit " (number-to-string (process-exit-status process)) ")")
        (buffer-name)))
      (insert (propertize
               (concat "\n~\n"
                       "~ Exited at " (chills/command-datetime-string))
               'font-lock-face 'font-lock-comment-face)))
    (shell-command-sentinel process signal)))

(defun chills/async-shell-command-rerun (&optional keep-old)
  "Re-run command for current async shell buffer.
With prefix argument KEEP-OLD, keep the current buffer.
Without KEEP-OLD, delete the current buffer after switching to the new run."
  (interactive "P")
  (unless chills/async-shell-command
    (user-error "No async shell command recorded for this buffer"))
  (let ((old-buffer (current-buffer))
        (target-window (selected-window))
        (default-directory (or chills/async-shell-command-directory default-directory))
        (chills/async-shell-command-preserve-window t)
        (chills/last-async-shell-command-buffer nil)
        (display-buffer-overriding-action
         '((display-buffer-no-window) (allow-no-window . t)))
        (async-shell-command-display-buffer t))
    (async-shell-command chills/async-shell-command)
    (when (and (window-live-p target-window)
               (buffer-live-p chills/last-async-shell-command-buffer))
      (set-window-buffer target-window chills/last-async-shell-command-buffer)
      (select-window target-window)
      (when (bound-and-true-p evil-mode)
        (evil-normal-state nil)))
    (when (and (not keep-old)
               (buffer-live-p old-buffer)
               (buffer-live-p chills/last-async-shell-command-buffer)
               (not (eq old-buffer chills/last-async-shell-command-buffer)))
      (kill-buffer old-buffer))))

(defun chills/add-info-after-exit (orig-fun &rest args)
  "Attach process metadata after calling ORIG-FUN with ARGS."
  (let* ((command (car args))
         (window (apply orig-fun args))
         (output-buffer (or (cadr args)
                            chills/last-async-shell-command-output-buffer-name))
         (buffer (or (and (window-live-p window) (window-buffer window))
                     (and (bufferp output-buffer) output-buffer)
                     (and (stringp output-buffer) (get-buffer output-buffer))))
         (proc (and buffer (get-buffer-process buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local chills/async-shell-command command
                    chills/async-shell-command-directory default-directory
                    compile-command command)
        (compilation-shell-minor-mode 1)))
    (when (buffer-live-p buffer)
      (setq chills/last-async-shell-command-buffer buffer))
    (when (process-live-p proc)
      (set-process-sentinel proc (apply-partially #'chills/process-callback buffer)))
    window))

(defun chills/select-window-normal-mode (window)
  "Select WINDOW and enter evil normal state when available."
  (when (and (window-live-p window)
             (not chills/async-shell-command-preserve-window))
    (select-window window)
    (when (bound-and-true-p evil-mode)
      (evil-normal-state nil)))
  window)

(unless (advice-member-p #'chills/set-output-buffer-name 'async-shell-command)
  (advice-add 'async-shell-command :filter-args #'chills/set-output-buffer-name))

(unless (advice-member-p #'chills/add-info-after-exit 'async-shell-command)
  (advice-add 'async-shell-command :around #'chills/add-info-after-exit))

(unless (advice-member-p #'chills/select-window-normal-mode 'async-shell-command)
  (advice-add 'async-shell-command :filter-return #'chills/select-window-normal-mode))

(defun chills/shell-command-mode-setup ()
  "Configure keybindings for async shell command output buffers."
  (local-set-key (kbd "g r") #'chills/async-shell-command-rerun)
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "gr") #'chills/async-shell-command-rerun)
    (evil-local-set-key 'motion (kbd "gr") #'chills/async-shell-command-rerun)))

(add-hook 'shell-command-mode-hook #'chills/shell-command-mode-setup)

(defalias 'eshell/f #'find-file)

(defun eshell/clear ()
  "Clear the Eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun chills/eshell--eshell-buffer-p (buffer)
  "Return non-nil when BUFFER is an Eshell buffer."
  (with-current-buffer buffer
    (derived-mode-p 'eshell-mode)))

(defun chills/eshell--buffers ()
  "Return Eshell buffers sorted by recency."
  (cl-remove-if-not #'chills/eshell--eshell-buffer-p (buffer-list)))

(defun chills/eshell--most-recent-buffer ()
  "Return the most recent Eshell buffer."
  (cl-find-if #'chills/eshell--eshell-buffer-p (buffer-list)))

(defun chills/eshell--same-directory-p (a b)
  "Return non-nil when directories A and B refer to the same path."
  (condition-case nil
      (file-equal-p (expand-file-name a) (expand-file-name b))
    (error
     (string=
      (directory-file-name (expand-file-name a))
      (directory-file-name (expand-file-name b))))))

(defun chills/eshell--buffer-for-directory (directory)
  "Return the most recent Eshell buffer in DIRECTORY."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'eshell-mode)
            (chills/eshell--same-directory-p default-directory directory))))
   (buffer-list)))

(defun chills/eshell--new-buffer (directory)
  "Create a new Eshell buffer in DIRECTORY."
  (let ((buffer (let ((default-directory directory))
                  (eshell t))))
    (with-current-buffer buffer
      (unless (chills/eshell--same-directory-p default-directory directory)
        (eshell/cd directory)
        (eshell-reset)))
    buffer))

(defun chills/eshell (&optional arg)
  "Open or switch to Eshell buffers.
Without ARG, switch to the most recent Eshell buffer, creating one in the
current directory when none exist.
With ARG, switch to an existing Eshell in the current directory or create one."
  (interactive "P")
  (let* ((directory default-directory)
         (target
          (if arg
              (or (chills/eshell--buffer-for-directory directory)
                  (chills/eshell--new-buffer directory))
            (or (chills/eshell--most-recent-buffer)
                (chills/eshell--new-buffer directory)))))
    (switch-to-buffer target)))

(defun chills/eshell-prompt ()
  "Custom Eshell prompt with PWD in the cursor color."
  (let ((pwd-color (face-attribute 'cursor :background nil 'default)))
    (concat
     (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,pwd-color))
     (propertize " $ " 'face 'default))))

(defun chills/eshell--process-from-entry (entry)
  "Return process object from Eshell process ENTRY."
  (cond
   ((processp entry) entry)
   ((and (consp entry) (processp (car entry))) (car entry))
   (t nil)))

(defun chills/eshell--running-processes ()
  "Return active process list for current Eshell buffer."
  (let (processes)
    (when (boundp 'eshell-process-list)
      (dolist (entry eshell-process-list)
        (let ((process (chills/eshell--process-from-entry entry)))
          (when (and process (process-live-p process))
            (push process processes)))))
    (nreverse processes)))

(defun chills/eshell--process-label ()
  "Return process label string for current Eshell buffer."
  (let* ((processes (chills/eshell--running-processes))
         (count (length processes)))
    (when (> count 0)
      (let ((name (process-name (car processes))))
        (if (> count 1)
            (format "%s +%d" name (1- count))
          name)))))

(defun chills/eshell--buffer-name ()
  "Return the target Eshell buffer name."
  (let* ((directory (abbreviate-file-name
                     (directory-file-name (expand-file-name default-directory))))
         (process (chills/eshell--process-label)))
    (if process
        (format "*eshell: %s [%s]*" directory process)
      (format "*eshell: %s*" directory))))

(defun chills/eshell--wrap-process-sentinels ()
  "Wrap sentinels for active Eshell processes to keep buffer names current."
  (dolist (process (chills/eshell--running-processes))
    (unless (process-get process 'chills/eshell-sentinel-wrapped)
      (let ((buffer (current-buffer))
            (original (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when original
             (funcall original proc event))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (chills/eshell--update-buffer-name)))))
        (process-put process 'chills/eshell-sentinel-wrapped t)))))

(defun chills/eshell--update-buffer-name (&rest _ignored)
  "Rename current Eshell buffer based on directory and active processes."
  (when (derived-mode-p 'eshell-mode)
    (chills/eshell--wrap-process-sentinels)
    (let ((new-name (chills/eshell--buffer-name)))
      (unless (string= (buffer-name) new-name)
        (rename-buffer new-name t)))))

(defun chills/eshell-mode-setup ()
  "Set up buffer-local Eshell behavior."
  (add-hook 'eshell-directory-change-hook #'chills/eshell--update-buffer-name nil t)
  (add-hook 'eshell-pre-command-hook #'chills/eshell--update-buffer-name nil t)
  (add-hook 'eshell-post-command-hook #'chills/eshell--update-buffer-name nil t)
  (chills/eshell--update-buffer-name))

(add-hook 'eshell-mode-hook #'chills/eshell-mode-setup)

(setq eshell-prompt-function #'chills/eshell-prompt
      eshell-highlight-prompt nil)

(when (fboundp 'spc-menu!)
  (spc-menu! "buffer" "s"
    "e" #'chills/eshell
    "s" '(switch-to-buffer (async-shell-command))))

(provide 'chills-eshell)
