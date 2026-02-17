;; File chooser portal integration for xdg-desktop-portal -> Emacs.
(with-eval-after-load 'filechooser
  (setq filechooser-use-popup-frame nil)

  ;; Keep D-Bus handler arity compatible with portal calls.
  (defun chills/filechooser-open-dired (prompt &optional dir filters &rest _ignore)
    (filechooser-with-dired prompt dir filters))

  (defun chills/filechooser-open-dired-single (prompt &optional dir filters &rest _ignore)
    "Choose a single file via Dired.
If multiple files are marked, return the first one to preserve
`org.freedesktop.impl.portal.FileChooser.OpenFile' single-file semantics."
    (let ((selection (filechooser-with-dired prompt dir filters)))
      (when selection
        (if (listp selection)
            (car selection)
          selection))))

  (setq filechooser-choose-file #'chills/filechooser-open-dired-single)
  ;; Keep a single Dired workflow for both single and multi-selection.
  (setq filechooser-choose-files #'chills/filechooser-open-dired)

  (defvar chills/filechooser-active-frame nil
    "Frame currently used by the portal filechooser request.")

  (defun chills/filechooser-cancel-response ()
    "Return a portal cancellation response."
    (filechooser--return-value nil))

  (defun chills/filechooser--portal-response-p (value)
    "Return non-nil when VALUE looks like a portal response tuple."
    (and (listp value)
         (= (length value) 2)
         (integerp (car value))
         (listp (cadr value))))

  ;; filechooser 0.2.3 can throw args-out-of-range in some Dired rows.
  (defun chills/filechooser--dired-jit-abbreviate-safe (beg end)
    "Safely ellipsize directory prefixes between BEG and END."
    (setq end (progn (goto-char end) (line-end-position)))
    (setq beg (progn (goto-char beg) (line-beginning-position)))
    (while (< (point) end)
      (when-let* ((file (dired-get-filename nil t))
                  (name (file-name-directory file))
                  (start (dired-move-to-filename)))
        (let* ((line-end (line-end-position))
               (stop (+ start (min (length name) (max 0 (- line-end start))))))
          (when (> stop start)
            (put-text-property start stop 'display ".../")
            (put-text-property start stop 'help-echo file))))
      (forward-line 1))
    `(jit-lock-bounds ,beg . ,end))

  (unless (advice-member-p #'chills/filechooser--dired-jit-abbreviate-safe
                           'filechooser--dired-jit-abbreviate)
    (advice-add 'filechooser--dired-jit-abbreviate :override
                #'chills/filechooser--dired-jit-abbreviate-safe))

  ;; Backport filechooser 0.2.4 abort behavior for popup-frame close/quit.
  (define-key filechooser-mininuffer-map [remap keyboard-quit] #'filechooser-abort)

  (defun chills/filechooser-wrap-request (orig &rest args)
    (let* ((display (or (getenv "WAYLAND_DISPLAY") (getenv "DISPLAY")))
           (bg (or (face-background 'default nil t) "#1e1e1e"))
           (fg (or (face-foreground 'default nil t) "#ffffff"))
           (params `((name . "filechooser-frame")
                     (minibuffer . t)
                     (width . 120)
                     (height . 36)
                     (background-color . ,bg)
                     (foreground-color . ,fg)))
           (frame (cond
                   ((and display (fboundp 'make-frame-on-display))
                    (make-frame-on-display display params))
                   (display
                    (make-frame (append params (list (cons 'display display)))))
                   (t
                    (make-frame params))))
           (special-event-map
            (define-keymap :parent special-event-map
              "<delete-frame>" #'filechooser-abort))
           response)
      (setq chills/filechooser-active-frame frame)
      (unwind-protect
          (let ((result
                 (catch 'exit
                   (catch 'done
                     (catch 'continue
                       (catch 'abort
                         (condition-case err
                             (with-selected-frame frame
                               (raise-frame frame)
                               (select-frame-set-input-focus frame)
                               (apply orig args))
                           ((quit error)
                            (message "filechooser request failed: %S" err)
                            (chills/filechooser-cancel-response)))))))))
            (setq response
                  (if (chills/filechooser--portal-response-p result)
                      result
                    (progn
                      (when result
                        (message "filechooser unexpected result: %S" result))
                      (chills/filechooser-cancel-response)))))
        (setq chills/filechooser-active-frame nil)
        (when (frame-live-p frame)
          (delete-frame frame t)))
      (or response (chills/filechooser-cancel-response))))

  (dolist (fn '(filechooser-handle-open-file
                filechooser-handle-save-file
                filechooser-handle-save-files))
    (unless (advice-member-p #'chills/filechooser-wrap-request fn)
      (advice-add fn :around #'chills/filechooser-wrap-request))))

;; Remove legacy global advices that can trap unrelated frame operations.
(advice-remove 'delete-frame #'chills/filechooser-around-delete-frame)
(advice-remove 'save-buffers-kill-terminal
               #'chills/filechooser-around-save-buffers-kill-terminal)

(provide 'filechooser-portal)
