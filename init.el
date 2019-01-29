;;; Must have this to avoid adding it automatically.
;; (package-initialize)



(require 'server)
(when (server-running-p)
  (if (y-or-n-p "Server already running, abort?")
      (keyboard-quit)
    (setq server-name
          (read-string "Different server name? "
                       (concat server-name
                               (number-to-string (random 1000)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic utils used for init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'np-utils)
(eval-after-load 'el-get
  ;; This must be reloaded when updating el-get: unloading
  ;; `el-get-custom' undefines the `el-get-sources' variable.
  '(load  "np-recipes"))
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
      sentence-end-double-space t ; explicitly choose default
      x-select-enable-clipboard t
      set-mark-command-repeat-pop t

      history-delete-duplicates t
      comint-input-ignoredups t

      view-read-only t; all read-only buffers in view-mode
      view-inhibit-help-message t ; don't tell me about it

      delete-active-region nil ; just use <delete>

      gdb-many-windows t

      epa-pinentry-mode 'loopback
      auth-sources '("~/.authinfo.gpg")

      ;; No more damn prompts!
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      )

;; Mail settings.
(setq send-mail-function #'smtpmail-send-it
      mail-host-address "gmail.com"
      smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587 ;25
      smtpmail-smtp-service 25 ;587

      ;; Allow toggling between text and HTML views of emails.
      ;; Possibly setting `gnus-buttonized-mime-types' and/or
      ;; `gnus-unbuttonized-mime-types' might be more sensible, but I
      ;; can't be bothered to figure it out right now.
      gnus-inhibit-mime-unbuttonizing t
      ;; Some mail comes with text/html and text/plain alternatives.
      ;; Don't take the text/html, if possible.
      mm-discouraged-alternatives '("text/html" "text/richtext")
      )

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              tab-width 4
              c-basic-offset 'set-from-style ; Just keep it as default
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
(setq cua-delete-selection nil) ; but don't enable `delete-selection-mode'!
(cua-selection-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; it's too annoying to accidentally hit C-[ three times when C-p was
;; meant.
(bind-key "ESC ESC ESC" 'keyboard-quit)
;; I sometimes lean on this by accident, resulting in deeply nested
;; M-x prompts which is super annoying.
(unbind-key "<menu>")

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
;; For some reason, these are only on `M-n' and `M-p' by default.
(bind-key "n" 'occur-next occur-mode-map)
(bind-key "p" 'occur-prev occur-mode-map)
;; Almost always want to see major mode keymap
(bind-key "b" 'describe-major-mode-bindings help-map)
;; single key non-homerow bindings for some less common operations
(bind-key* "<delete>" 'delete-region)
;; some compatibility with Windows/VisualC++ stuff
(bind-key "<f4>" 'next-error)
(bind-key "S-<f4>" 'previous-error)

(bind-key "TAB" 'forward-button special-mode-map)
(bind-key "<backtab>" 'backward-button special-mode-map)

(bind-key "C-<tab>" 'switch-to-next-buffer)
(bind-key "C-<iso-lefttab>" 'switch-to-prev-buffer)

(bind-key "<f7>" 'compile)
(bind-key "C-<f7>" 'recompile)
(setq compilation-error-regexp-alist
      ;; Prune compilation message regexps (see Bug#9065).
      '(ant bash python-tracebacks-and-caml cmake cmake-info java
            gcc-include gnu guile-file guile-line))
;; (absoft ada aix ant bash borland python-tracebacks-and-caml cmake
;;         cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek
;;         iar ibm irix java jikes-file maven jikes-line clang-include
;;         gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft
;;         omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example
;;         sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line
;;         gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint
;;         guile-file guile-line)
(bind-key "C-<f5>" 'gdb)
(bind-key "<f5>" 'gud-cont)
(bind-key "<f9>" 'gud-break)
(bind-key "<f10>" 'gud-next)
(bind-key "C-<f10>" 'gud-until)
(bind-key "<f11>" 'gud-step)
(bind-key "S-<f11>" 'gud-finish)

(unbind-key "C-x C-c")                             ; don't need this
(defalias 'quit-emacs 'save-buffers-kill-terminal) ; use M-x instead
(defun maybe-quit-emacs ()
  (interactive)
  (when (y-or-n-p "Quit Emacs?")
    (quit-emacs)))
(bind-key "M-<f4>" 'maybe-quit-emacs)
(bind-key "C-<f4>"
          ;; `kill-this-buffer' only works properly from menu
          (lambda () (interactive) (kill-buffer)))
(bind-key "C-S-<f4>" 'kill-buffer-and-window)

(define-key global-map [remap just-one-space] 'cycle-spacing)

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

(bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; scrolling
(bind-key* "<right>" (lambda () (interactive) (scroll-left 1 t)))
(bind-key* "<left>" (lambda () (interactive) (scroll-right 1 t)))
(bind-key "<kp-right>" 'scroll-left)
(bind-key "<kp-left>" 'scroll-right)
(bind-key* "<up>" #'scroll-down-line)
(bind-key* "<down>" #'scroll-up-line)
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
(bind-key "C-x r a" 'append-to-register)

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

;; The light orange citation faces are hardly readable.
(eval-after-load 'message
  '(set-face-foreground 'message-cited-text "dark red"))
(eval-after-load 'gnus-cite
  '(set-face-foreground 'gnus-cite-4 "dark red"))

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

;; debbugs.el uses this, but it's convenient to call independently.
(autoload 'gnus-read-ephemeral-bug-group "gnus-group"
  "Browse bug NUMBER as ephemeral group" 'interactive)

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
  ;; remove distracting display stuff
  :config (setq calc-display-trail nil
                calc-line-numbering nil))

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
               ;; so I can complete candidates without entering them
               ido-confirm-unique-completion t
               ;; don't jump to some other directory when I mistype a filename
               ido-auto-merge-work-directories-length -1)))

(use-package ido-complete-space-or-hyphen
  :config (ido-complete-space-or-hyphen-enable))
(use-package ido-ubiquitous
  :config (progn (setq ido-ubiquitous-enable-old-style-default nil)
                 (ido-ubiquitous-mode +1)))

(use-package smex
  :init (setq smex-save-file (locate-user-emacs-file "smex-items"))
  :bind (([remap execute-extended-command] . smex)
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
             ("Magit" (name . "\\`[*]magit.*:")))))

    (define-and-add-hook ibuffer-mode-hook
      (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-after-kill-buffer-p nil))

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
  :config (progn (setq ediff-window-setup-function #'ediff-setup-windows-plain)
                 (set-face-background 'ediff-current-diff-Ancestor "light blue")))

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
    ;; Don't shadow `M-s' prefix key
    (unbind-key "M-s" paredit-mode-map)
    (bind-key "M-s M-s" 'paredit-splice-sexp paredit-mode-map)
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
  :defer 30
  :config (progn
            (setq yas-prompt-functions ; default x-prompt is just annoying
                  '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
            (setq yas-wrap-around-region t)
            (unbind-key "<tab>" yas-minor-mode-map)
            (unbind-key "TAB" yas-minor-mode-map)
            (bind-key "M-i" 'yas-expand yas-minor-mode-map)
            (setq yas-fallback-behavior '(apply yas-insert-snippet))
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
  :init (setq-default c-basic-offset 4)
  :config
  (define-and-add-hook c-mode-common-hook
    (unless (fboundp 'global-subword-mode)
      (c-subword-mode +1))
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'access-label '/)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'statement-case-intro '+)
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
(use-package magit
  :bind ("C-c v" . magit-status)
  :init (setq
         ;; Avoid Emacs bug#20015,
         tramp-ssh-controlmaster-options
         "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
         ;; don't revert automatically,
         magit-refresh-file-buffer-hook nil ; obsolete
         magit-turn-on-auto-revert-mode nil ; obsolete
         magit-auto-revert-mode nil
         magit-revert-buffers nil       ; obsolete
         ;; and just delete for real.
         magit-delete-by-moving-to-trash nil)
  :config
  (progn
    ;; NOTE: require ido-ubiquitous
    (setq magit-completing-read-function #'magit-ido-completing-read)
    ;; remote usually redundant
    (setq magit-default-tracking-name-function
          #'magit-default-tracking-name-branch-only)

    ;; see https://github.com/magit/magit/pull/2091
    (setq magit-keep-region-overlay t)

    ;; I always keep my repos under ~/src
    (setq magit-repository-directories '(("~/src/" . 2)))

    (setq magit-popup-use-prefix-argument 'default)
    (setq magit-push-always-verify nil) ; obsolete

    ;; TODO: bind `magit-pop-revision-stack'?
    (use-package magit-extras
      :defer t
      :config
      (setq magit-pop-revision-stack-format
            (pcase-let ((`(,pt ,_eob ,index-regexp)
                         (default-value 'magit-pop-revision-stack-format)))
              `(,pt "[%N: %h]: %ci\n  %s\n  https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=%H"
                    ,index-regexp))))

    ;; Modify margin format: abbreviate time, shorten author name.
    (pcase-let ((`(,init ,_style ,width ,author ,_author-width)
                 magit-log-margin))
      (setq magit-log-margin
            `(,init age-abbreviated ,width ,author 10))
      (setq magit-status-margin
            `(nil age-abbreviated ,width ,author 10)))

    ;; this is too expensive to have on by default
    (setq magit-backup-mode nil)

    ;; defaults for popups
    (setq magit-branch-popup-show-variables nil)

    (magit-define-popup-switch 'magit-patch-popup ?w
      "Ignore all whitespace" "--ignore-all-space")

    (magit-define-popup-switch 'magit-patch-apply-popup ?r
      "Allow partial rejection" "--reject")

    (magit-define-popup-action 'magit-patch-popup ?E
      "Prepare patches for Emacs bug" 'magit-prepare-emacs-patches)

    (magit-define-popup-option 'magit-commit-popup ?D
      "Override author date" "--date=" )

    (set-face-foreground 'magit-hash
                         (face-foreground 'font-lock-type-face))
    (bind-key "SPC <t>"     'magit-invoke-popup-switch magit-popup-mode-map)
    (bind-key "SPC SPC <t>" 'magit-invoke-popup-option magit-popup-mode-map)
    (bind-key "C-c C-d" 'magit-describe-section magit-mode-map)
    (bind-key "`" (if (fboundp 'magit-toggle-margin)
                      'magit-toggle-margin 'magit-log-toggle-margin)
              magit-mode-map)
    (bind-key "C-c C-l" 'magit-toggle-buffer-lock magit-mode-map)

    ;; Show worktree section if there are worktrees, avoid overhead if
    ;; there aren't.
    (defvar-local np/magit-want-worktrees t)
    (put 'np/magit-want-worktrees 'permanent-local t)
    (defun np/magit-maybe-add-worktrees ()
      (if (and np/magit-want-worktrees
               (not (memq #'magit-insert-worktrees magit-status-sections-hook))
               (> (length (magit-list-worktrees)) 1))
          (magit-add-section-hook
           'magit-status-sections-hook #'magit-insert-worktrees
           'magit-insert-status-headers 'append 'local)
        (setq-local np/magit-want-worktrees nil)))
    (add-hook 'magit-status-mode-hook #'np/magit-maybe-add-worktrees)
    (bind-key "j w" #'magit-jump-to-worktrees magit-status-mode-map)

    ;; Show submodule section if there are submodules, avoid overhead if
    ;; there aren't.
    (defvar-local np/magit-want-submodules t)
    (put 'np/magit-want-submodules 'permanent-local t)
    (bind-key "j m" #'magit-jump-to-submodules magit-status-mode-map)

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
               narrow-to-page
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
(put 'bug-reference-bug-regexp 'safe-local-variable #'stringp)
(put 'autoload-compute-prefixes 'safe-local-variable #'booleanp)
(put 'c-noise-macro-names 'safe-local-variable
     (lambda (v) (and (listp v) (cl-every #'stringp v))))
(put 'git-commit-major-mode 'safe-local-variable
     (lambda (v) (memq v '(git-commit-elisp-text-mode
                      text-mode))))
(setq enable-local-eval nil)

;;; unsafe/annoying locals
(setq ignored-local-variables '(whitespace-style))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; post init stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(require 'local-init nil t)
