;;; early-init.el --- Early startup tuning -*- lexical-binding: t; -*-
;;; Commentary:
;; Early startup layout:
;; performance hacks, package.el, and UI defaults.

;;; setup/early-init/performance hacks
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
     (setq gc-cons-percentage 0.1
           gc-cons-threshold 100000000
           warning-minimum-level :warning)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;;; setup/early-init/package.el
(setq package-enable-at-startup nil)

;;; setup/early-init/ui
(setq initial-frame-alist
      '(
        (background-color . "#000000")
        (menu-bar-lines . 0)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
