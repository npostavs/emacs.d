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
          :username "npostavs" :url-type ssh :branch "current"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc emacs settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil
      inhibit-splash-screen t
      require-final-newline 'ask
      line-move-visual nil
      truncate-partial-width-windows 80
      sentence-end-double-space nil
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t
      view-read-only t; all read-only buffers in view-mode
      )

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

(setq-default indicate-empty-lines t
              word-wrap t
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
(eval-after-load "view"
  '(bind-key "b" 'View-scroll-page-backward view-mode-map))
;; single key non-homerow bindings for some less common operations
(bind-key* "<delete>" 'delete-region)
(bind-key "<kp-multiply>" 'calc-dispatch)
(bind-key "C-<kp-multiply>" 'quick-calc)
;; some compatibility with VisualC++
(bind-key "<f4>" 'next-error)
(bind-key "S-<f4>" 'previous-error)
(bind-key "M-<f4>" 'kill-this-buffer)
(bind-key "C-<tab>" 'switch-to-next-buffer)
(bind-key "C-S-<iso-lefttab>" 'switch-to-prev-buffer)

(bind-key "<f7>" 'compile)
(bind-key "<f5>" 'gdb)

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

;; don't need this
(unbind-key "C-x C-c")
(defalias 'quit-emacs 'save-buffers-kill-terminal); use M-x instead

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(bind-key "C-a" 'back-to-indentation-or-beginning)
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

(use-package diminish); use-package makes use of this, load it first

(use-package undo-tree
  :defer t
  :diminish "↺T"
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
  :config (progn (ido-mode +1)
                 (ido-everywhere)
                 (setq ido-enable-flex-matching t)))

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
  :config (dired-details-install))

;;; loadhist doesn't mention the handy `feature-file' in autoloads
(use-package loadhist
  :defer t
  :init (autoload 'feature-file "loadhist"
          "Return the file name from which a given FEATURE was loaded."))

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
  :diminish "ElD"
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

;;; programming related modes

(use-package mode-compile
  :init (define-key global-map [remap compile] 'mode-compile))

(define-and-add-el-get-source
  '(:name pretty-symbols
          :description "Minor mode for drawing multi-character tokens as Unicode glyphs"
          :type github :username "drothlis"))
(use-package pretty-symbols
  :init (dolist (mode '(emacs-lisp-mode-hook
                        lisp-mode-hook scheme-mode-hook js-mode-hook))
          (add-hook mode 'pretty-symbols-mode)))

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
 `(:name sml-mode
         ,@(when (eq system-type 'windows-nt) '(:build nil))
         :compile ("sml-compat.el" "sml-util.el" "sml-defs.el"
                   "sml-move.el" "sml-mode.el" "sml-proc.el")
         :info "sml-mode.info"))

;; lua-mode
(use-package lua-mode
  :defer t
  :config
  (defadvice lua-point-is-after-left-shifter-p
    (around lua-always-left-shifter-p activate)
    "pretend everything is left-shifter, see
https://github.com/immerrr/lua-mode/pull/19"
    (setq ad-return-value t)))

;; git
(define-and-add-el-get-source
  `(:name git-modes
          :description "GNU Emacs modes for Git-related files (used by magit)."
          :type github :username "magit"))

(define-and-add-el-get-source
  `(:name magit
          :depends (git-modes)
          ,@(when (eq system-type 'windows-nt)
              '(:build nil :autoloads nil))))

(use-package magit
  :bind ("C-c v" . magit-status)
  :init (use-package git-modes :defer t))

;; use GNU make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

(define-and-add-el-get-source
  '(:name i3-emacs
          :description "i3 emacs integration"
          :website "https://github.com/vava/i3-emacs"
          :type github :username "npostavs" :url-type ssh :branch "current"))

(use-package i3-integration
  :load-path (lambda () `(,(concat el-get-dir "i3-emacs")))
  :if (and (eq window-system 'x)
           (executable-find "i3")))


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
