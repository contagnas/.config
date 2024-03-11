;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration
;; file or tangling and loading a literate org configuration file.

;; Don't attempt to find/apply special file handlers to files loaded during
;; startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "readme.elc" user-emacs-directory))
      (load-file (expand-file-name "readme.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (setq org-return-follows-link t)
    (require 'org)

    ;; This might not work exactly as expected - readme.org also contains early-init.el code,
    ;; which will have already been executed. Oh well?
    (org-babel-load-file (expand-file-name "readme.org" user-emacs-directory))))

;;; init.el ends here
