;; File chooser portal integration for xdg-desktop-portal -> Emacs.
(with-eval-after-load 'filechooser
  (setq filechooser-use-popup-frame nil)

  ;; Keep D-Bus handler arity compatible with portal calls.
  (defun chills/filechooser-open-dired (prompt &optional dir filters &rest _ignore)
    (filechooser-with-dired prompt dir filters))

  (setq filechooser-choose-file #'chills/filechooser-open-dired)
  (setq filechooser-choose-files #'chills/filechooser-open-dired)

  (defvar chills/filechooser-active-frame nil
    "Frame currently used by the portal filechooser request.")

  (defun chills/filechooser-cancel-response ()
    "Return a portal cancellation response."
    (filechooser--return-value nil))

  (defun chills/filechooser--active-frame-p (&optional frame)
    (and (bound-and-true-p filechooser-current-operation)
         (frame-live-p chills/filechooser-active-frame)
         (or (null frame) (eq frame chills/filechooser-active-frame))))

  (defun chills/filechooser-around-delete-frame (orig frame &rest args)
    "Finish active filechooser recursive edit before deleting FRAME."
    (if (chills/filechooser--active-frame-p frame)
        (progn
          (condition-case nil
              (exit-recursive-edit)
            (error nil))
          (let ((chills/filechooser-active-frame nil))
            (apply orig frame args)))
      (apply orig frame args)))

  (defun chills/filechooser-around-save-buffers-kill-terminal (orig &rest args)
    "Cancel active filechooser request instead of killing daemon/client."
    (if (chills/filechooser--active-frame-p)
        (progn
          (condition-case nil
              (exit-recursive-edit)
            (error nil))
          (when (frame-live-p (selected-frame))
            (let ((chills/filechooser-active-frame nil))
              (delete-frame (selected-frame) t)))
          nil)
      (apply orig args)))

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
           response)
      (setq chills/filechooser-active-frame frame)
      (unwind-protect
          (setq response
                (condition-case _err
                    (with-selected-frame frame
                      (raise-frame frame)
                      (select-frame-set-input-focus frame)
                      (apply orig args))
                  ((quit error) (chills/filechooser-cancel-response))))
        (setq chills/filechooser-active-frame nil)
        (when (frame-live-p frame)
          (delete-frame frame t)))
      (or response (chills/filechooser-cancel-response))))

  (dolist (fn '(filechooser-handle-open-file
                filechooser-handle-save-file
                filechooser-handle-save-files))
    (unless (advice-member-p #'chills/filechooser-wrap-request fn)
      (advice-add fn :around #'chills/filechooser-wrap-request))))

(unless (advice-member-p #'chills/filechooser-around-delete-frame 'delete-frame)
  (advice-add 'delete-frame :around #'chills/filechooser-around-delete-frame))

(unless (advice-member-p #'chills/filechooser-around-save-buffers-kill-terminal
                         'save-buffers-kill-terminal)
  (advice-add 'save-buffers-kill-terminal :around
              #'chills/filechooser-around-save-buffers-kill-terminal))

(provide 'filechooser-portal)
