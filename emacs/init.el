;;; init.el --- Emacs config -*- lexical-binding: t; -*-

;;; setup/install elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t
        use-package-always-ensure t))


(elpaca-wait)

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

;;; general/config/leader
(use-package general
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-define-key
   :keymaps 'override
   :states '(insert normal hybrid motion visual operator emacs)
   :prefix-map '+prefix-map
   :prefix "SPC"
   :global-prefix "M-S-SPC"
   )
  
  (general-create-definer spc
    :wk-full-keys nil
    :keymaps '+prefix-map)
  
  (spc
    "SPC" 'execute-extended-command
    "u"   'universal-argument
    "/"   'occur
    "!"   'shell-command
    ":"   'eval-expression
    "."   'repeat
    "h"   (general-simulate-key "C-h" :which-key "help")
    "r"   'async-shell-command
    )
  
  (general-create-definer global-leader
    :keymaps 'override
    :states '(insert normal hybrid motion visual operator)
    :prefix "SPC m"
    :non-normal-prefix "M-S-SPC m"
    "" '( :ignore t
          :which-key
          (lambda (arg)
            (cons (cadr (split-string (car arg) " "))
                  (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
  (defmacro spc-menu! (name prefix-key &rest body)
    "Create a definer named spc-KEY wrapping global-definer.
      Create prefix map: spc-KEY-map. Prefix bindings in BODY with PREFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "spc-" prefix-key))
           (prefix-map (intern (concat n "-map"))))
      `(progn
         (general-create-definer ,(intern n)
           :wrapping spc
           :prefix-map (quote ,prefix-map)
           :prefix ,prefix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,name))
         (,(intern n) ,@body))))
  (spc-menu! "buffer" "b"
    "d"  'kill-current-buffer
    "o" '((lambda () (interactive) (switch-to-buffer nil))
          :which-key "other-buffer")
    "p"  'previous-buffer
    "r"  'rename-buffer
    "R"  'revert-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n"  'next-buffer
    "s"  'scratch-buffer
    "TAB" '((lambda () (interactive) (switch-to-buffer nil))
            :which-key "other-buffer")
    ))

(elpaca-wait)

;;; general/config/files
(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC"
 :global-prefix "M-S-SPC"
 )

(general-create-definer spc
  :wk-full-keys nil
  :keymaps '+prefix-map)

(spc
  "SPC" 'execute-extended-command
  "u"   'universal-argument
  "/"   'occur
  "!"   'shell-command
  ":"   'eval-expression
  "."   'repeat
  "h"   (general-simulate-key "C-h" :which-key "help")
  "r"   'async-shell-command
  )

(general-create-definer global-leader
  :keymaps 'override
  :states '(insert normal hybrid motion visual operator)
  :prefix "SPC m"
  :non-normal-prefix "M-S-SPC m"
  "" '( :ignore t
        :which-key
        (lambda (arg)
          (cons (cadr (split-string (car arg) " "))
                (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(defun save-buffer-and-kill ()
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(spc-menu! "file" "f"
  "d"   '((lambda (&optional arg)
            (interactive "P")
            (let ((buffer (when arg (current-buffer))))
              (diff-buffer-with-file buffer)))
          :which-key "diff-with-file")
  "c" '((lambda () (interactive) (find-file (concat user-emacs-directory "init.el")))
        :which-key "emacs-config-file")
  "e"   '(:ignore t :which-key "edit")
  "f"   'find-file
  "l"   '((lambda (&optional arg)
            (interactive "P")
            (call-interactively (if arg #'find-library-other-window #'find-library)))
          :which-key "+find-library")
  "p"   'find-function-at-point
  "P"   'find-function
  "R"   'rename-file-and-buffer
  "s"   'save-buffer
  "v"   'find-variable-at-point
  "V"   'find-variable
  "q"   'save-buffer-and-kill)

(spc-menu! "window" "w"
  "s" 'split-window-vertically
  "v" 'split-window-horizontally
  "=" 'balance-windows
  "O" 'delete-other-windows
  "X" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window))
        :which-key "kill-other-buffer-and-window")
  "d" 'delete-window
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "o" 'other-window
  "t" 'window-toggle-side-windows
  "m" 'delete-other-windows
  "."  '(:ingore :which-key "resize")
  ".h" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider left")
  ".l" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider right")
  ".j" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider up")
  ".k" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider down")
  "x" 'kill-buffer-and-window)

;;; general/config/project
(spc-menu! "project" "p")

;;; general/config/quit
(spc-menu! "quit" "q"
  "r" 'restart-emacs)

;;; general/config/vim completion
(general-create-definer completion-def
  :prefix "C-x")

;;; general/config/backup files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))

(defun chills/apply-mode-line-face (&optional frame)
  "Apply consistent mode-line face styling for FRAME.
This is needed for daemon-created client frames."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (let ((bg-active (face-attribute 'mode-line :background nil 'default))
            (bg-inactive (face-attribute 'mode-line-inactive :background nil 'default)))
        (set-face-attribute 'mode-line nil :box `(:line-width 4 :color ,bg-active))
        (set-face-attribute 'mode-line-inactive nil :box `(:line-width 4 :color ,bg-inactive))))))

;; Apply now, and for every future frame created by emacsclient/daemon.
(chills/apply-mode-line-face)
(add-hook 'after-make-frame-functions #'chills/apply-mode-line-face)
(add-hook 'server-after-make-frame-hook #'chills/apply-mode-line-face)

;;; evil
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-undo-system 'undo-redo)
  :config
  (spc-w
    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right
    )
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  (evil-mode)
  )

;;; evil/evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

;;; evil/evil-surround
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1)
  )

;;; which-key
(use-package which-key
  :config (which-key-mode)
  )

;;; emacs
(use-feature emacs
  :demand t
  :custom
  (scroll-conservatively 101 "Scroll just enough to bring text into view")
  (enable-recursive-minibuffers t "Allow minibuffer commands in minibuffer")
  (frame-title-format '(buffer-file-name "%f" ("%b"))
                      "Make frame title current file's name.")
  (find-library-include-other-files nil)
  (indent-tabs-mode nil "Use spaces, not tabs")
  (inhibit-startup-screen t)
  (history-delete-duplicates t "Don't clutter history")
  (pgtk-use-im-context-on-new-connection nil "Prevent GTK from stealing Shift + Space")
  (sentence-end-double-space nil "Double space sentence demarcation breaks sentence navigation in Evil")
  (tab-stop-list (number-sequence 2 120 2))
  (tab-width 2 "Shorter tab widths")
  (completion-styles '(flex basic partial-completion emacs22))
  (blink-cursor-mode nil)
  )

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; magit
(use-package seq)
(use-package transient :after seq)

(use-package magit
  :after transient
  :general (spc "m" 'magit))

;;; org
(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook #'toc-org-mode))

;;; savehist
(use-feature savehist
  :init
  (savehist-mode))

;;; vert&co/vertico
(use-package vertico
  :init (vertico-mode)
  )

;;; vert&co/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; vert&co/consult
(use-package consult
  :demand t
  :general
  (spc-b
    "b" 'consult-buffer)
  (spc-
   "b" 'consult-buffer-other-window)
  (spc-p
    "s" 'consult-ripgrep)

  :config
  (setq consult-narrow-key "<"
        completion-in-region-function (lambda (&rest args)
                                        (apply (if vertico-mode
                                                   #'consult-completion-in-region
                                                 #'completion--in-region)
                                               args)))
  )

;;; vert&co/marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;;; vert&co/embark
(use-package embark-consult)

(use-package embark
  :after embark-consult
  :bind
  (("C-." . embark-act)))

;;; vert&co/corfu
(use-package wgrep)

(use-package corfu
  :custom
  (corfu-auto t)


  :init
  (global-corfu-mode))

;;; projectile
(use-package projectile)

(use-package consult-projectile
  :after projectile
  :general
  (spc-p
    "f" 'consult-projectile))

;;; eshell
(setq shell-command-prompt-show-cwd t)

(defvar command-datetime-format "%Y-%m-%d %H:%M:%S")

(defun command-datetime-string () (format-time-string command-datetime-format))

(defun set-output-buffer-name (args)
  "Wraps `async-shell-command` to use a more informative output-buffer name"
  (let* ((command (car args))
         (time (command-datetime-string))
         (output-buffer (concat "*Shell [" time "] (Running) " command)))
    (list command output-buffer)))

(advice-add 'async-shell-command :filter-args 'set-output-buffer-name)

(defun process-callback (buffer process signal)
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer buffer
      (rename-buffer (string-replace
                      "(Running)"
                      (concat "(Exit " (number-to-string (process-exit-status process)) ")")
                      (buffer-name)))
      (insert (propertize
               (concat "\n~\n"
                       "~ Exited at " (command-datetime-string))
               'font-lock-face 'font-lock-comment-face)))
    (shell-command-sentinel process signal)))

(defun add-info-after-exit (orig-fun &rest args)
  (let* ((window (apply orig-fun args))
         (buffer (window-buffer window))
         (proc (get-buffer-process buffer)))
    (progn
      (when (process-live-p proc)
        (set-process-sentinel proc (apply-partially 'process-callback buffer)))
      window)))


(advice-add 'async-shell-command :around 'add-info-after-exit)

(defun select-window-normal-mode (window)
  (select-window window)
  (when bound-and-true-p evil-mode
        (evil-normal-state nil)))
(advice-add 'async-shell-command :filter-return 'select-window-normal-mode)

(use-feature eshell
  :preface
  (defalias 'eshell/f 'find-file)
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  :general
  (spc-menu! "buffer" "s"
    "e" 'eshell
    "s" '(switch-to-buffer (async-shell-command))
    )
  )

(defun eshell-prompt ()
  "Custom Eshell prompt with PWD in the cursor's color."
  (let ((pwd-color (face-attribute 'cursor :background nil 'default)))
    (concat
     (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,pwd-color))
     (propertize " $ " 'face 'default))))

(setq eshell-prompt-function #'eshell-prompt)
(setq eshell-highlight-prompt nil)

;;; eshell/eat
(use-package eat
  :elpaca (eat :type git
               :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :config

  :hook (eshell-load-hook . eat-eshell-mode)
  )

;;; languages
(setq treesit-language-source-alist
      '(
        (gleam "https://github.com/gleam-lang/tree-sitter-gleam")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        ))

(defun treesit-install-if-missing (lang)
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(mapc #'treesit-install-if-missing (mapcar #'car treesit-language-source-alist))

;;; languages/tree-sitter remaps
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (gleam-mode . gleam-ts-mode)
        (python-mode . python-ts-mode)))

;;; languages/bazel
(use-package bazel)

;;; languages/rust
(use-package rust-mode)

;;; languages/nix
(use-package nix-mode
  )

;;; languages/yaml
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;;; languages/scala
(use-package scala-mode)

;;; languages/cue
(use-package cue-mode
  )

;;; languages/gleam
(use-package gleam-ts-mode
  :elpaca (gleam-ts-mode :host github :repo "gleam-lang/gleam-mode")
  )

(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          (setq python-shell-interpreter python-path)

          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          (setenv "VIRTUAL_ENV" venv-path)

          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

;;; languages/tsx
(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ))


(add-to-list 'major-mode-remap-alist '(typescript-mode . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;; languages/nushell
(use-package nushell-mode)

;;; envrc
(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config (envrc-global-mode 1))

(use-package direnv
  :config
  )

(load-file "~/.config/emacs/filechooser-portal.el")
