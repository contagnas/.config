;; This file is generate by readme.org. Do not edit manually!

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1
      warning-minimum-level :emergency)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq ;;file-name-handler-alist default-file-name-handler-alist
      gc-cons-percentage 0.1
      gc-cons-threshold 100000000
      warning-minimum-level :warning)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

(setq package-enable-at-startup nil)

(setq initial-frame-alist
      '(
        (background-color . "#000000")
        (menu-bar-lines . 0)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
