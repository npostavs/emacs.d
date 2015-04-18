;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic utils used for init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'np-utils)
(require 'np-recipes)
(autoload 'pkg-list "pkg-list" nil t)

(defvar override-global-map (make-sparse-keymap)); else bind-key will make full keymap
(defvar el-get-dir (expand-file-name "el-get/" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "el-get/" el-get-dir))
(add-to-list 'load-path (expand-file-name "use-package/" el-get-dir))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/npostavs/el-get/current/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync (mapcar #'el-get-source-name el-get-sources))

(require 'bind-key)
(require 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc emacs settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil
      inhibit-splash-screen t
      require-final-newline 'ask
      truncate-partial-width-windows 80
      sentence-end-double-space nil
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t

      history-delete-duplicates t
      comint-input-ignoredups t

      view-read-only t; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it

      delete-active-region nil ; just use <delete>

      gdb-many-windows t
      )

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              truncate-lines t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(fringe-mode '(4 . 4))

;; only show trailing whitespace in files
(define-and-add-hook find-file-hook
  (setq show-trailing-whitespace t))

;; typing the whole word is tiresome
(fset 'yes-or-no-p 'y-or-n-p)

;; improved rectangle selection without semi-shadowing C-{z,x,c}
(cua-selection-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; it's too annoying to accidentally hit C-[ three times when C-p was
;; meant.
(bind-key "ESC ESC ESC" 'keyboard-quit)

;; <backspace> is too far away
(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key* "M-h" 'backward-kill-word)
(bind-key* "C-M-h" 'backward-kill-sexp)
;; b scrolls back in view-mode (counterpart to SPC scrolling forward, from dillo)
(eval-after-load 'view
  '(bind-key "b" 'View-scroll-page-backward view-mode-map))
(eval-after-load 'apropos
  '(bind-key "b" 'scroll-down-command apropos-mode-map))
(setenv "MANWIDTH" "72")
(eval-after-load 'man
  '(bind-key "b" 'scroll-down-command Man-mode-map))
(eval-after-load 'info
  '(bind-key "b" 'Info-scroll-down Info-mode-map))
(eval-after-load 'help
  '(progn (bind-key "b" 'scroll-down-command help-mode-map)
          (bind-key "n" 'help-go-forward help-mode-map)
          (bind-key "p" 'help-go-back help-mode-map)))
;; single key non-homerow bindings for some less common operations
(bind-key* "<delete>" 'delete-region)
;; some compatibility with Windows/VisualC++ stuff
(bind-key "<f4>" 'next-error)
(bind-key "S-<f4>" 'previous-error)

(bind-key "C-<tab>" 'switch-to-next-buffer)
(bind-key "C-S-<iso-lefttab>" 'switch-to-prev-buffer)

(bind-key "<f7>" 'compile)
(bind-key "C-<f7>" 'recompile)
(bind-key "C-<f5>" 'gdb)
(bind-key "<f5>" 'gud-cont)
(bind-key "<f9>" 'gud-break)
(bind-key "<f10>" 'gud-next)
(bind-key "C-<f10>" 'gud-until)
(bind-key "<f11>" 'gud-step)
(bind-key "S-<f11>" 'gud-finish)

(unbind-key "C-x C-c")                             ; don't need this
(defalias 'quit-emacs 'save-buffers-kill-terminal) ; use M-x instead
(bind-key "M-<f4>" 'quit-emacs)
(bind-key "C-<f4>"
          ;; `kill-this-buffer' only works properly from menu
          (lambda () (interactive) (kill-buffer)))
(bind-key "C-S-<f4>" 'kill-buffer-and-window)

(define-key global-map [remap just-one-space] 'just-dwim-space)

;; don't want to burn single key seq on `forward-page' but moving
;; multiple pages with double key seq is annoying: use the repeat last
;; key trick (like C-x z z... or C-x e e...):
(defvar page-movement-map
  (let ((map (make-sparse-keymap)))
    (define-key map "[" 'backward-page)
    (define-key map "]" 'forward-page)
    map)
  "keymap for single key seq movement by page")

(defadvice forward-page (after with-repeat-key activate)
  (set-temporary-overlay-map page-movement-map))
(defadvice backward-page (after with-repeat-key activate)
  (set-temporary-overlay-map page-movement-map))

(define-key global-map [remap set-selective-display] 'toggle-fold)


(bind-key* "C-a" 'beginning-of-line-dwim)
(bind-key* "C-e" 'end-of-line-dwim)
(unbind-key "M-m")

;; scrolling
(bind-key* "<right>" (lambda () (interactive) (scroll-left 1 t)))
(bind-key* "<left>" (lambda () (interactive) (scroll-right 1 t)))
(bind-key "<kp-right>" 'scroll-left)
(bind-key "<kp-left>" 'scroll-right)
(bind-key* "<up>" (lambda () (interactive) (scroll-up -1)))
(bind-key* "<down>" (lambda () (interactive) (scroll-down -1)))
(put 'scroll-left 'disabled nil)

(bind-key* "M-<down>" 'windmove-down)
(bind-key* "M-<up>" 'windmove-up)
(bind-key* "M-<right>" 'windmove-right)
(bind-key* "M-<left>" 'windmove-left)

(bind-key "t" 'toggle-window-split ctl-x-4-map)
(bind-key "C-t" 'rotate-frame-window-buffers ctl-x-4-map)

(unbind-key "C-z") ; suspend-frame is also on C-x C-z
(bind-key "<XF86Sleep>" 'ignore)

(defun yank-primary ()
  "`mouse-yank-primary' sans mouse"
  (interactive)
  (push-mark (point))
  (insert (or (x-get-selection-value) (x-get-selection 'PRIMARY))))

(bind-key "<S-insert>" 'yank-primary)

(when (eq system-type 'windows-nt)
  (declare-function w32-send-sys-command nil)
  (defun w32-MAXIMIZE ()
    (interactive) (w32-send-sys-command #xF030))
  (defun w32-RESTORE ()
    (interactive) (w32-send-sys-command #xF120))
  (bind-key "<kp-subtract>" 'w32-RESTORE)
  (bind-key "<kp-add>" 'w32-MAXIMIZE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-foreground 'font-lock-doc-face "dark red")
(set-face-foreground 'font-lock-string-face "dark red")
(set-face-foreground 'font-lock-type-face "purple4")
(set-face-foreground 'font-lock-comment-face "#005000")
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground "maroon4" :underline t)
(set-face-attribute 'font-lock-keyword-face nil
                    :inherit 'default
                    :foreground 'unspecified :bold t)
(set-face-foreground 'font-lock-variable-name-face "DarkBlue")
(set-face-attribute 'font-lock-constant-face nil
                    :underline t :foreground 'unspecified
                    :inherit 'font-lock-variable-name-face)
(set-face-attribute 'font-lock-builtin-face nil
                    :underline t :foreground 'unspecified
                    :inherit 'font-lock-variable-name-face)
(set-face-attribute 'font-lock-warning-face nil
                    :foreground "red3" :bold t)

(when window-system
  (let* ((c (rgb-color-values (face-background 'default)))
         (offset #x001010)
         (c-name (format "#%06X" (- c offset))))
    (set-face-background 'trailing-whitespace c-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modes/packages
;;;
;; TODO? maybe add
;; FastNav and/or ace-jump? ace-jump seems simpler
;; https://github.com/lewang/le_emacs_MRU_yank
;; perpective
;; CEDET/Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish ; use-package makes use of this, load it first
  :config (defadvice diminish (after diminish-no-space activate)
            "drop space from lighter if first char is not a letter"
            (let ((to-what (car ad-return-value)))
              (when (string-match-p "^ [^[:alpha:]]" to-what)
                (setcar ad-return-value
                        (substring to-what 1))))))

;; visual-line-mode is part of simple.el so we can't `use-package' on it
(diminish 'visual-line-mode "V")
(add-hook 'text-mode-hook #'visual-line-mode)
(setq visual-line-fringe-indicators ; needs to restart mode when changed
      '(left-curly-arrow right-curly-arrow))

(use-package undo-tree
  :defer 5
  :config (progn (setq undo-tree-visualizer-timestamps t)
                 (global-undo-tree-mode +1))
  :diminish "↺")

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package multiple-cursors
  ;; TODO: maybe bind more mc/ comands?
  :bind (("C-z C-SPC" . mc/edit-lines)))

(use-package grep
  :defer t
  :config (when (eq system-type 'windows-nt)
            ;; findstr can handle the basic find|grep use case
            (grep-apply-setting 'grep-find-template
                                "findstr /S /N /D:. /C:<R> <F>")
            (setq find-name-arg nil)))

(use-package edebug
  :defer t
  ;; 24.3 is missing this edebug spec
  :config (def-edebug-spec cl-type-spec sexp))

(use-package calc
  :defer t
  :bind (("<kp-multiply>" . calc-dispatch)
         ("C-<kp-multiply>" . quick-calc))
  ;; the trail is distracting
  :config (setq calc-display-trail nil))

(use-package proced
  :bind ("C-c p" . proced)
  :config (progn
            (set-assq 'proced-format-alist
                      'min '(pid tree (args comm)))
            (set-assq 'proced-format-alist
                      'tiny '(pid tree user pcpu pmem (args comm)))
            (setq-default proced-format 'tiny
                          proced-tree-flag nil)))

(use-package ido
  :config
  (progn (ido-mode +1)
         (ido-everywhere +1)
         (setq ido-enable-flex-matching t
               ;; don't jump to some other directory when I mistype a filename
               ido-auto-merge-work-directories-length -1)))

(use-package ido-complete-space-or-hyphen
  :config (ido-complete-space-or-hyphen-enable))
(use-package ido-ubiquitous
  :config (progn (setq ido-ubiquitous-enable-old-style-default nil)
                 (ido-ubiquitous-mode +1)))

(use-package smex
  :init (setq smex-save-file (locate-user-emacs-file "smex-items"))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("Files" (filename . "[^/]\\'") (name . "\\`[^*].*[^*]\\'"))
             ("Dirs" (mode . dired-mode))
             ("Docs" (or (mode . help-mode)
                         (mode . Info-mode)
                         (mode . apropos-mode)
                         (mode . completion-list-mode)
                         (mode . Man-mode)))
             ("Mail/News" (or (filename . "newsrc-dribble")
                              (mode . message-mode)
                              (mode . gnus-article-mode)
                              (mode . gnus-group-mode)
                              (mode . gnus-summary-mode)))
             ("Procs" (predicate . (get-buffer-process (current-buffer))))
             ("Grep"  (mode . grep-mode))
             ("Magit" (name . "[.]magit>$")))))

    (define-and-add-hook ibuffer-mode-hook
      (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package dired-details
  :commands dired-details-install
  :config (setq-default dired-details-hidden-string "+ "))
(use-package dired
  :defer t
  :config (progn
            (setq dired-listing-switches "-alX")
            (require 'dired-x)
            (dired-details-install)))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain))


;;; loadhist misses some autoloads
(use-package loadhist
  :defer t
  :init (progn
          (autoload 'feature-file "loadhist"
            "Return the file name from which a given FEATURE was loaded.")
          (autoload 'read-feature "loadhist"
            "Read feature name from the minibuffer, prompting with string PROMPT.")))

;;; lisp related modes
(show-paren-mode +1) ; this is useful everywhere

(use-package paredit
  :diminish "(ed)"
  :config
  (progn
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (defadvice enable-paredit-mode (around demote-paredit-errors activate)
      (with-demoted-errors ad-do-it))))

(define-and-add-hook emacs-lisp-mode-hook
  (eldoc-mode +1)
  (setq tab-width 8))
(use-package eldoc
  :defer t
  :diminish ""
  :config (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(use-package elisp-slime-nav
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
  :defer t
  :diminish "")

(use-package yasnippet
  :defer 5
  :config (progn
            (setq yas-prompt-functions ; default x-prompt is just annoying
                  '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
            (setq yas-wrap-around-region t)
            (unbind-key "<tab>" yas-minor-mode-map)
            (unbind-key "TAB" yas-minor-mode-map)
            (bind-key "M-i" 'yas-expand yas-minor-mode-map)
            (yas-global-mode +1))
  :diminish (yas-minor-mode . "Y"))

(use-package pcre2el
  :defer 10
  :config (rxt-global-mode +1))

;;; programming related modes

(use-package pretty-symbols
  :init (dolist (mode '(emacs-lisp-mode-hook
                        lisp-mode-hook scheme-mode-hook js-mode-hook))
          (add-hook mode 'pretty-symbols-mode))
  :diminish "λ")

(use-package sh-script
  :defer t
  :config
  (progn (set-face-foreground 'sh-quoted-exec "maroon4")
         (set-face-foreground 'sh-heredoc (face-foreground 'font-lock-string-face))))

;; cc-mode
(use-package cc-mode
  :config
  (define-and-add-hook c-mode-common-hook
    (unless (fboundp 'global-subword-mode)
      (c-subword-mode +1))
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'access-label '/)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'statement-case-intro '+)
    (setq-default c-basic-offset 4)
    (setq fill-column 80))
  :defer t)

;; newer Emacsen have subword-mode seperate from cc-mode
(use-package subword
  :if (fboundp 'global-subword-mode)
  :init (global-subword-mode +1)
  :diminish "")

;; sml-mode
(use-package sml-mode
  :defer t)

;; lua-mode
(use-package lua-mode
  :defer t)

;; git
(use-package git-commit-mode ; from git-modes
  :defer t
  :config (progn
            (cond
             ((boundp 'git-commit-mode-hook) ; obsolete
              (remove-hook 'git-commit-mode-hook 'flyspell-mode))
             ((boundp 'git-commit-setup-hook)
              (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)))))

(use-package magit
  :bind ("C-c v" . magit-status)
  :config
  (progn
     ;; NOTE: require ido-ubiquitous
    (setq magit-completing-read-function #'magit-ido-completing-read)
    ;; remote usually redundant
    (setq magit-default-tracking-name-function
          #'magit-default-tracking-name-branch-only)

    ;; don't revert automatically
    (setq magit-refresh-file-buffer-hook nil ; obsolete
          magit-turn-on-auto-revert-mode nil ; obsolete
          magit-auto-revert-mode nil)

    ;; this is too expensive to have on by default
    (setq magit-backup-mode nil)

    ;; defaults for popups
    (setq magit-branch-arguments (remove "--track" magit-branch-arguments))
    (defadvice magit-push-popup (around magit-push-arguments-maybe-upstream
                                        activate)
      "Enable --set-upstream switch if there isn't a current upstream."
      (let ((magit-push-arguments
             (if (magit-get-remote) magit-push-arguments
               (cons "--set-upstream" magit-push-arguments))))
        ad-do-it))

    (set-face-foreground 'magit-hash
                         (face-foreground 'font-lock-type-face))
    ;; change buffer name formats so the "magit" goes at the end, that
    ;; way the important parts won't be cut off in the ibuffer list.
    (dolist (type '("branches" "cherry" "log" "reflog" "status" "wazzup"))
      (set (intern (format "magit-%s-buffer-name-format" type))
           (format "*%%a<%s.magit>" type)))
    ;; only have one buffer for each of these, not per repo
    (dolist (type '("commit" "diff" "process"))
      (set (intern (format "magit-%s-buffer-name-format" type))
           (format "*<%s.magit>" type)))
    (bind-key "SPC <t>"     'magit-invoke-popup-switch magit-popup-mode-map)
    (bind-key "SPC SPC <t>" 'magit-invoke-popup-option magit-popup-mode-map)
    (bind-key "C-c C-d" 'magit-describe-section magit-mode-map)
    (bind-key "`" (if (fboundp 'magit-toggle-margin)
                      'magit-toggle-margin
                    'magit-log-toggle-margin) magit-mode-map)
    ;; The `pop-to-ongoing' advice is too expensive: it takes close to
    ;; half a second.
    (ad-deactivate 'magit-commit-popup)
    (when (eq system-type 'windows-nt)
      ;; msys git uses a wrapper in .../Git/cmd/git.exe, going
      ;; directly to the executable in .../Git/bin/git.exe makes a
      ;; noticable difference in time: magit-refresh goes from 2.0s to
      ;; 1.3s
      (setq magit-git-executable
            (expand-file-name "../bin/git.exe"
                              (file-name-directory (executable-find "git")))))
    (font-lock-add-keywords 'emacs-lisp-mode
                            magit-font-lock-keywords)))

;; use GNU make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

(use-package i3-integration
  :load-path (lambda () `(,(concat el-get-dir "i3-emacs")))
  :if (and (eq window-system 'x)
           (executable-find "i3"))
  :config
  (defadvice magit-key-mode (after show-whole-popup activate)
    ;; The popup should fit the text exactly, but the last line is
    ;; clipped by about 1 pixel (because of fontifying?) so the whole
    ;; buffer ends up scrolling down. We'll just add an extra line.
    ;; See https://github.com/magit/magit/issues/800 for details.
    (set-window-text-height (selected-window)
                            (1+ (count-lines (point-min) (point-max))))))

;; org-mode
(use-package org-mode
  :defer t
  :init (setq orig-default-notes-file "~/.emacs.d/notes"
              org-startup-folded nil
              org-confirm-babel-evaluate nil
              org-export-copy-to-kill-ring nil))

;;; probably this should be replaced with projectile or something
(when (eq system-type 'windows-nt)
 (defvar project-list '()
   "list of (name dir) for `open-project'.")

 (defun open-project (name dir)
   (interactive (assoc (ido-completing-read "Project: " (mapcar #'car project-list) nil 'require-match)
                       project-list))
   (start-process-shell-command
    (format "%s-cmd" name) nil
    (format "start \"%s\" /D %s cmd.exe"
            name (shell-quote-argument dir)))
   (dired dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enable "advanced" commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (cmd '(scroll-left
               narrow-to-region
               upcase-region downcase-region
               dired-find-alternate-file
               erase-buffer
               set-goal-column))
  (put cmd 'disabled nil))

;;; safe locals
(put 'Package 'safe-local-variable #'symbolp)
(put 'require-final-newline 'safe-local-variable #'symbolp)
(put 'whitespace-line-column 'safe-local-variable #'integerp)
(put 'emacs-lisp-docstring-fill-column 'safe-local-variable #'integerp)
(put 'lexical-binding 'safe-local-variable #'booleanp)
(setq enable-local-eval nil)

;;; unsafe/annoying locals
(setq ignored-local-variables '(whitespace-style))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post init stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(require 'local-init nil t)
