;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic utils used for init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path user-emacs-directory)
(require 'np-utils)

(define-and-add-el-get-source
  '(:name use-package
          :website "https://github.com/jwiegley/use-package"
          :description "A use-package declaration for simplifying your .emacs"
          :type github
          :username "npostavs" :url-type ssh :checkout "origin/current"
          :features (bind-key use-package)))
(defvar override-global-map (make-sparse-keymap)); else bind-key will make full keymap
(defvar el-get-dir (concat user-emacs-directory "el-get/"))

(add-to-list 'load-path (concat el-get-dir "el-get"))
(dolist (p (eval-when-compile
             (require 'el-get)
             (el-get 'sync 'el-get 'use-package)
             (el-get-load-path 'use-package)))
  (add-to-list 'load-path p))

(require 'bind-key)
(require 'use-package)


(use-package el-get
  :if (not (featurep 'el-get)); this is just for experimenting with a compiled init
  :init (progn
          (add-to-list 'load-path el-get-dir); .loaddefs has paths relative to el-get-dir
          (require '.loaddefs))
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages))

;;; cut down el-get's post-init stuff
(define-and-add-el-get-source
  '(:name package
          :post-init
          (setq package-user-dir
                (expand-file-name
                 (convert-standard-filename
                  (concat (file-name-as-directory
                           default-directory)
                          "elpa"))))
          (make-directory package-user-dir t)))


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
      view-read-only t; all read-only buffers in view-mode
      )

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

(fringe-mode '(4 . 4))

;; only show trailing whitespace in files
(define-and-add-hook find-file-hook
  (setq show-trailing-whitespace t))

;; typing the whole word is tiresome
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <backspace> is too far away
(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key* "M-h" 'backward-kill-word)
(bind-key* "C-M-h" 'backward-kill-sexp)
;; b scrolls back in view-mode (counterpart to SPC scrolling forward, from dillo)
(eval-after-load 'view
  '(bind-key "b" 'View-scroll-page-backward view-mode-map))
(eval-after-load 'apropos
  '(bind-key "b" 'scroll-down-command apropos-mode-map))
(eval-after-load 'info
  '(bind-key "b" 'Info-scroll-down Info-mode-map))
(eval-after-load 'help
  '(progn (bind-key "b" 'scroll-down-command help-mode-map)
          (bind-key "n" 'help-go-forward help-mode-map)
          (bind-key "p" 'help-go-back help-mode-map)))
;; single key non-homerow bindings for some less common operations
(bind-key* "<delete>" 'delete-region)
(bind-key "<kp-multiply>" 'calc-dispatch)
(bind-key "C-<kp-multiply>" 'quick-calc)
;; some compatibility with Windows/VisualC++ stuff
(bind-key "<f4>" 'next-error)
(bind-key "S-<f4>" 'previous-error)
(bind-key "M-<f4>" 'kill-this-buffer)
(bind-key "C-<tab>" 'switch-to-next-buffer)
(bind-key "C-S-<iso-lefttab>" 'switch-to-prev-buffer)

(bind-key "<f7>" 'compile)
(bind-key "<f5>" 'gdb)

(unbind-key "C-x C-c")                             ; don't need this
(defalias 'quit-emacs 'save-buffers-kill-terminal) ; use M-x instead
(bind-key "M-<f4>" 'quit-emacs)
(bind-key "C-<f4>" 'kill-buffer-and-window)

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
(bind-key* "<right>" 'scroll-left)
(bind-key* "<left>" 'scroll-right)
(bind-key* "<up>"
  (lambda () (interactive) (scroll-up -1)))
(bind-key* "<down>"
  (lambda () (interactive) (scroll-down -1)))
(bind-key* "<right>" 'scroll-left)
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
  :defer t
  :diminish "↺"
  :idle (global-undo-tree-mode +1))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package multiple-cursors
  ;; TODO: maybe bind more mc/ comands?
  :bind (("C-z C-SPC" . mc/edit-lines)))

(use-package calc
  :defer t
  ;; the trail is distracting
  :config (setq calc-display-trail nil))

(use-package ido
  :config
  (progn (ido-mode +1)
         (ido-everywhere +1)
         (setq ido-enable-flex-matching t
               ;; don't jump to some other directory when I mistype a filename
               ido-auto-merge-work-directories-length -1)))

(define-and-add-el-get-source
  '(:name ido-complete-space-or-hyphen
          :description "Make ido completes like built-in M-x
          does, useful when use smex or use ido to complete other
          hyphen separated choices"
          :type github :username "doitian"))
(use-package ido-complete-space-or-hyphen
  :init (ido-complete-space-or-hyphen-enable))

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
             ("Files" (filename . "") (name . "^[^*]"))
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
             ("Procs" (predicate . (get-buffer-process (current-buffer)))))))

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


(define-and-add-el-get-source
  '(:name elisp-slime-nav
          :description "Slime-style navigation of Emacs Lisp source with M-. & M-,"
          :type github
          :username "purcell"))
(use-package elisp-slime-nav
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
  :defer t
  :diminish "")

(define-and-add-el-get-source
  '(:name yasnippet-snippets
          :description "a collection of yasnippet snippets for many languages"
          :type github
          :username "AndreaCrotti"))

(define-and-add-el-get-source
  '(:name yasnippet :depends (yasnippet-snippets)
          :pkgname nil
          :username "npostavs" :url-type ssh :checkout "origin/current"
          :pre-init nil :post-init nil))

(use-package yasnippet
  :init (setq yas-snippet-dirs
              `("~/.emacs.d/snippets"
                ,(expand-file-name "yasnippet-snippets" el-get-dir)
                ,(expand-file-name "yasnippet/snippets" el-get-dir)))
  :idle (yas-global-mode +1)
  :diminish (yas-minor-mode . "Y"))

(define-and-add-el-get-source
  '(:name pcre2el
          :description "Parse, convert, and font-lock PCRE, Emacs and rx regexps"
          :type github
          :username "joddie"))
(use-package pcre2el
  :idle (rxt-global-mode +1))

;;; programming related modes

(use-package mode-compile
  :defer t
  :init (progn (define-key global-map [remap compile] 'mode-compile)
               ;; if you really want to read the messages, check
               ;; *Messages* buffer
               (setq mode-compile-reading-time 0)))

(define-and-add-el-get-source
  '(:name pretty-symbols
          :description "Minor mode for drawing multi-character tokens as Unicode glyphs"
          :type github :username "drothlis"))
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
(define-and-add-el-get-source
  '(:name sml-mode
          :description
          "SML-mode is a major Emacs mode for editing Standard ML source code."
          :type elpa))

;; lua-mode
(use-package lua-mode
  :defer t)

;; git
(define-and-add-el-get-source
  `(:name git-modes
          :description "GNU Emacs modes for Git-related files (used by magit)."
          :type github
          :username "npostavs" :url-type ssh :checkout "origin/current"))
(use-package git-modes
  :defer t
  :config (remove-hook 'git-commit-mode-hook 'flyspell-mode))

(define-and-add-el-get-source
  `(:name magit
          :pkgname "magit"
          :username "npostavs" :checkout "origin/current" :url-type ssh
          :autoloads t
          :depends (git-modes)
          ,@(when (eq system-type 'windows-nt)
              '(:build nil))))

(use-package magit
  :bind ("C-c v" . magit-status)
  :config (setq magit-completing-read-function #'magit-ido-completing-read
                ;; default hook (revert) takes too long
                magit-refresh-file-buffer-hook nil))

;; use GNU make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

(define-and-add-el-get-source
  '(:name i3-emacs
          :description "i3 emacs integration"
          :website "https://github.com/vava/i3-emacs"
          :type github :username "npostavs" :url-type ssh :branch "origin/current"))

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
              org-startup-folded nil))


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
(put 'whitespace-style 'safe-local-variable #'listp)
(put 'whitespace-line-column 'safe-local-variable #'integerp)
(put 'lexical-binding 'safe-local-variable #'booleanp)
(setq enable-local-eval nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post init stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(require 'local-init nil t)
