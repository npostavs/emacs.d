;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(eval-when-compile (require 'rx))

(defconst +quote-switching-char-table+
  (let* ((last (max ?' ?\"))
         (table (make-string (1+ last) 0)))
    (dotimes (i (1+ last))
      (aset table i
            (cond
              ((eq i ?') ?\")
              ((eq i ?\") ?')
              (t i))))
    table))

(defun switch-quotes-in-region (beg end)
  "replace double quotes with single quotes and vice versa in
region"
  (interactive "r")
  (translate-region beg end +quote-switching-char-table+))

(defmacro define-and-add-hook (add-to &rest body)
  "defun a hook function (named my-ADD-TO) and add it to hook
variable ADD-TO"
  (let ((hook-name (intern (concat "my-" (symbol-name add-to)) obarray)))
    `(progn
       (defun ,hook-name ()
         ,@body)
       (add-hook ',add-to #',hook-name))))

(defun rgb-color-values (color)
  "like `color-values' but returns the classic 3-byte rgb in a
single integer"
  (let ((c 0) (i 8))
    (dolist (x (color-values color) c)
      (setq c (+ c (lsh (logand x #xff00) i))
            i (- i 8)))))

(defun reload-feature (feature)
  "`unload-feature' followed by `require'"
  (interactive
   (list (read-feature "Reload feature: " t)))
  (unload-feature feature t)
  (require feature))

(defun beginning-of-line-dwim (&optional logical)
  "Go back to line's beginning or indentation, whichever is first.

If at beginning of logical line, go to indentation.
Prefix arg means just go to logical beginning unconditionally."
  (interactive "P")
  (if logical (beginning-of-line)
    (let* ((old (point))
           (ind (save-excursion (back-to-indentation) (point)))
           (beg (save-excursion (beginning-of-visual-line) (point)))
           (hi (max ind beg))
           (lo (min ind beg)))
      (goto-char (if (eq old hi) lo hi)))))

(defun end-of-line-dwim (&optional logical)
  "Go to successive visual line endings.

Prefix arg means just go to logical ending unconditionally."
  (interactive "P")
  (if logical
      (end-of-line)
    (when (and (= (point) (progn (end-of-visual-line)
                                 (when (not (or word-wrap (eolp)))
                                   (backward-char))
                                 (point)))
               (not (eolp)))
      (end-of-visual-line 2))))

(defun delete-this-file ()
  "Delete the file of the current buffer."
  (interactive)
  (delete-file (buffer-file-name)))

(defun set-assq (alist-var key value)
  "Set the value of KEY in ALIST-VAR to VALUE."
  (set alist-var (cons (cons key value)
                       (assq-delete-all key (symbol-value alist-var)))))


;; Based on `kmacro-call-macro' code.  There is a library form for
;; this sort of thing, but it looks a lot more complicated possibly
;; because it's working with older emacsen.
;; http://furius.ca/pubcode/pub/conf/lib/elisp/blais/repeatable.el

(defvar make-repeatable--command nil
  "The command that's currently repeating")

(defun make-repeatable (cmd &rest args)
  "call this to allow repeating function with last key of
sequence, just like C-x e e e..."
  (when (or (eq cmd make-repeatable--command)
            (> (length (this-single-command-keys)) 1))
   (set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (define-key map (vector last-input-event)
        (lambda () (interactive) (let ((make-repeatable--command cmd))
                              (apply cmd args))))
      map))))

;; `describe-bindings' orders the keymaps by precedence so the
;; major-mode goes next to last, which makes it a bit inconvenient
;; for quick lookup.
(defun describe-major-mode-bindings ()
  (interactive)
  (call-interactively 'describe-bindings)
  (with-current-buffer (help-buffer)
    (search-forward "Major Mode Bindings")
    (narrow-to-page)))


;;; debbugs stuff

(defconst debbugs-control-message-keywords
  '("serious" "important" "normal" "minor" "wishlist"
    "done" "donenotabug" "donewontfix" "doneunreproducible"
    "invalid" ; done+notabug+wontfix
    "unarchive" "unmerge" "reopen" "close"
    "merge" "forcemerge"
    "block" "unblock"
    "owner" "noowner"
    "reassign"
    "retitle"
    ;; 'notfixed <bugnum> <version>' works, even though it's
    ;; undocumented at debbugs.gnu.org.
    "fixed" "found" "notfound" "notfixed"
    "patch" "wontfix" "moreinfo" "unreproducible" "notabug"
    "pending" "help" "security" "confirmed" "easy"
    "usertag" "user"
    "documentation" ;; usertag:emacs.documentation
    ))
(defconst debbugs-control-message-commands-regexp
  (concat "^" (regexp-opt (cl-list* "#" "tags" "severity"
                                    debbugs-control-message-keywords))
          " .*$"))
(defconst debbugs-control-message-end-regexp
  (concat "^" (regexp-opt '("--" "quit" "stop"
                            "thank" "thanks" "thankyou" "thank you"))
          "$"))

(defun debbugs-implicit-ids ()
  (delq nil (list (debbugs-gnu-current-id t)
                  debbugs-gnu-bug-number ; Set on group entry.
                  (debbugs-gnu-guess-current-id)
                  (let ((bugnum-re "\\([0-9]+\\)\\(?:-done\\)?@debbugs.gnu.org")
                        (addr nil))
                    (and (eq major-mode 'message-mode)
                         (save-restriction
                           (message-narrow-to-headers)
                           (or (let ((addr (message-fetch-field "to")))
                                 (and addr (string-match bugnum-re addr)
                                      (match-string 1 addr)))
                               (let ((addr (message-fetch-field "cc")))
                                 (and addr (string-match bugnum-re addr)
                                      (match-string 1 addr))))))))))

;; Based on `debbugs-gnu-send-control-message', but don't send.
(autoload 'debbugs-gnu-current-id "debbugs-gnu.el")
(defun debbugs-gnu-make-control-message (message bugid &optional reverse)
  "Make a control message for the current bug report.
You can set the severity or add a tag, or close the report.  If
you use the special \"done\" MESSAGE, the report will be marked as
fixed, and then closed.

If given a prefix, and given a tag to set, the tag will be
removed instead."
  (interactive
   (save-excursion                 ; Point can change while prompting!
     (list (completing-read
            "Control message: " debbugs-control-message-keywords nil t)
           (let ((implicit-ids (debbugs-implicit-ids)))
             (string-to-number
              (completing-read "Bug #ID: " (mapcar #'prin1-to-string implicit-ids)
                               (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                               nil nil nil (car implicit-ids))))
           current-prefix-arg)))
  (let* ((version
          (when (member message '("done" "fixed" "found"))
            (save-excursion
              (read-string
               "Version: "
               (pcase (version-to-list emacs-version)
                 ;; Emacs development versions.
                 ((and `(,major ,minor ,micro . ,_))
                  (format "%d.%d" major (+ (if (> micro 1) 1 0) minor)))
                 (_ emacs-version))))))
	 (status (debbugs-gnu-current-status))
         (subject (format "Subject: control message for bug #%d" bugid)))
    (unless (eq major-mode 'message-mode)
      (set-buffer (pop-to-buffer "*Debbugs Control Message for #%d*" bugid))
      (insert "To: control@debbugs.gnu.org\n"
	      "From: " (message-make-from) "\n"
	      (format "Subject: control message for bug #%d\n" bugid)
	      mail-header-separator
	      "\n")
      (message-mode))
    (let ((ctrl-addr "control@debbugs.gnu.org")
          (id bugid)
          to-addr bcc-addr)
      (save-restriction
        (message-narrow-to-head)
        (setq  to-addr (message-fetch-field "to")
              bcc-addr (message-fetch-field "bcc"))
        (let* ((ctrl-re (regexp-quote ctrl-addr)))
          (unless (or (and  to-addr (string-match-p ctrl-re to-addr))
                      (and bcc-addr (string-match-p ctrl-re bcc-addr)))
            (message-add-header
             (format "%s: %s" (if to-addr "Bcc" "To") ctrl-addr)))))
      (message-goto-body)
      (while (looking-at-p debbugs-control-message-commands-regexp)
        (forward-line))
      (insert
       (save-excursion             ; Point can change while prompting!
         (cond
          ((member message '("unarchive" "unmerge" "noowner"))
           (format "%s %d\n" message id))
          ((equal message "reopen")
           (format "reopen %d\ntag %d - fixed patch\n" id id))
          ((member message '("merge" "forcemerge"))
           (format "%s %d %s\n" message id
                   (read-string "Merge with bug #: ")))
          ((member message '("block" "unblock"))
           (format
            "%s %d by %s\n" message id
            (mapconcat
             'identity
             (completing-read-multiple
              (format "%s with bug(s) #: " (capitalize message))
              (if (equal message "unblock")
                  (mapcar 'number-to-string
                          (cdr (assq 'blockedby status))))
              nil (and (equal message "unblock") status))
             " ")))
          ((equal message "owner")
           (format "owner %d !\n" id))
          ((equal message "retitle")
           (format "retitle %d %s\n" id (read-string "New title: ")))
          ((equal message "reassign")
           (format "reassign %d %s\n" id (read-string "Package(s): ")))
          ((equal message "close")
           (format "close %d\n" id))
          ((equal message "done")
           (format "tags %d fixed\nclose %d %s\n" id id version))
          ((member message '("found" "notfound" "fixed" "notfixed"))
           (format "%s %d %s\n" message id version))
          ((member message '("donenotabug" "donewontfix"
                             "doneunreproducible"))
           (format "tags %d %s\nclose %d\n" id (substring message 4) id))
          ((member message '("serious" "important" "normal"
                             "minor" "wishlist"))
           (format "severity %d %s\n" id message))
          ((equal message "invalid")
           (format "tags %d notabug wontfix\nclose %d\n"
                   id id))
          ((equal message "documentation")
           (concat (unless (save-excursion
                             (message-goto-body)
                             (re-search-forward "^user emacs$"))
                     "user emacs\n")
                   (format "usertag %d %s\n" id "documentation")))
          ((equal message "usertag")
           (format "user %s\nusertag %d %s\n"
                   (completing-read
                    "Package name or email address: "
                    (append
                     debbugs-gnu-all-packages (list user-mail-address))
                    nil nil (car debbugs-gnu-default-packages))
                   id (read-string "User tag: ")))
          (t
           (format "tags %d %c %s\n"
                   id (if reverse ?- ?+)
                   message)))))
      (unless (looking-at-p debbugs-control-message-end-regexp)
        (insert "quit\n\n")))))

(defvar magit-emacs-patch-range nil)

(defun magit-prepare-emacs-patches (range args)
  (interactive
   (list (-if-let (revs (magit-region-values 'commit))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (magit-read-range-or-commit "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (magit-patch-arguments)))
  (let ((bugnum nil))
    (with-temp-buffer
      (magit-git-insert "log" "--format=%B" range)
      (goto-char (point-min))
      (while (re-search-forward "[bB]ug ?#\\([0-9]+\\)" nil t)
        (push (match-string 1) bugnum)))
    (setq magit-emacs-patch-rev-list `(,(magit-toplevel) ,range ,args))
    (let ((grp-window (get-buffer-window "*Group*" t)))
      (when (window-live-p grp-window)
        (select-frame-set-input-focus (window-frame grp-window))
        (select-window grp-window)
        (gnus-read-ephemeral-bug-group
         (completing-read "Enter bug number: " bugnum)
         (cdr (assoc 'emacs gnus-bug-group-download-format-alist)))))))

(defun magit-attach-emacs-patches (range type disposition tag)
  (interactive
   (let ((disposition (completing-read "disposition: " '("inline" "attachment"))))
     (list magit-emacs-patch-rev-list
           (if (equal disposition "inline") "text/x-diff" "text/plain")
           disposition
           (y-or-n-p "tag + patch? "))))
  (pcase range
    (`(,repo-dir ,range ,args)
     (letrec ((dir (make-temp-file "patches-to-send" t))
              (deldir (lambda ()
                        (delete-directory dir t)
                        (remove-hook 'message-exit-actions deldir t)
                        (remove-hook 'kill-buffer-hook deldir t))))
       (add-hook 'message-exit-actions deldir nil t)
       (add-hook 'kill-buffer-hook deldir nil t)
       (let ((default-directory repo-dir))
         (magit-call-git "format-patch" range
                         (cons (concat "--output-directory=" dir) args)))
       (dolist (patch (directory-files dir t "\\`[^.]"))
         (mml-attach-file patch type "patch" disposition))
       (when tag
         (debbugs-gnu-make-control-message
          "patch" (car (debbugs-implicit-ids))))))
    (_ (user-error "Patches not prepared"))))

(defun magit-announce-pushed-emacs-patches (range)
  (interactive (list magit-emacs-patch-rev-list))
  (pcase range
    (`(,repo-dir ,range ,_args)
     (let ((default-directory repo-dir))
       (insert "Pushed: ")
       (dolist (rev (nreverse (magit-git-lines "rev-list" range)))
         (magit-pop-revision-stack rev repo-dir))
       (when (y-or-n-p "Close bug? ")
         (let ((emacs-version
                (with-temp-buffer
                  (insert-file-contents "configure.ac")
                  (re-search-forward "^ *AC_INIT(GNU Emacs, *\\([0-9.]+\\), *bug-gnu-emacs@gnu.org")
                  (match-string 1))))
           (debbugs-gnu-make-control-message
            "done" (car (debbugs-implicit-ids)))))))
    (_ (user-error "Patches not prepared"))))

(defvar debbugs-gnu-repos '("~/src/emacs/emacs-master/"
                            "~/src/emacs/emacs-bootstrapping/"
                            "~/src/emacs/emacs-26/"))
(defvar debbugs-gnu-repo-history nil)

(defun debbugs-read-repo (prompt)
  (completing-read prompt debbugs-gnu-repos
                   #'file-directory-p nil nil
                   'debbugs-gnu-repo-history (car debbugs-gnu-repo-history)))

(defconst debbugs-subject-noise-regexp
  (rx (opt (: (any "B" "b") "ug" (opt " ") "#" (+ digit) ": "))
      (opt "[PATCH") (opt " " (+ digit) "/" (+ digit)) "]"
      (opt " ")))

(defun debbugs-strip-noise-from-subject (subject)
  (cond ((stringp subject)
         (replace-regexp-in-string
          (concat "\\`" debbugs-subject-noise-regexp) "" subject))
        ((bufferp subject)
         (with-current-buffer subject
           (goto-char (point-min))
           (when (re-search-forward
                  (concat "^Subject: *\\("
                          debbugs-subject-noise-regexp
                          "\\)")
                  nil t)
             (replace-match "" t t nil 1))))
        (t (signal 'wrong-type-argument
                   (list '(or stringp bufferp) subject)))))

(defun debbugs-add-bugnum-if-needed (bugnum)
  (cl-check-type bugnum integer)
  (goto-char (point-min))
  (let ((diff-start (re-search-forward "^\\(?:---\\|[+][+][+]\\) ")))
    (goto-char (point-min))
    (unless (re-search-forward (format "[Bb]ug ?#%d" bugnum) diff-start t)
      (goto-char (point-min))
      (re-search-forward "^[Ss]ubject:")
      (end-of-line)
      (insert (format " (Bug#%d)" bugnum)))))

;; Adaped from `debbugs-gnu-apply-patch'.
(defun debbugs-gnu-grab-patch ()
  "Apply the patch from the current message."
  (interactive)
  (require 'debbugs-gnu)
  (add-hook 'emacs-lisp-mode-hook 'debbugs-gnu-lisp-mode)
  (add-hook 'diff-mode-hook 'debbugs-gnu-diff-mode)
  (add-hook 'change-log-mode-hook 'debbugs-gnu-change-mode)
  (gnus-summary-select-article nil t)
  (with-current-buffer gnus-article-buffer
    (let* ((bugnum (or (debbugs-gnu-current-id t)
		       debbugs-gnu-bug-number ; Set on group entry.
		       (debbugs-gnu-guess-current-id)))
           (from (gnus-fetch-field "from"))
           (subject (debbugs-strip-noise-from-subject
                     (gnus-fetch-field "subject")))
           (date (gnus-fetch-field "date"))
           (message (with-current-buffer gnus-article-buffer
                      (save-restriction
                        (gnus-narrow-to-body)
                        (buffer-string))))
           (chosen-patches
            (let ((wconf (current-window-configuration))
                  (patch-buffers nil))
              (unwind-protect
                  (progn
                    (delete-other-windows)
                    (dolist (handle (gnus-article-mime-handles))
                      (let ((num (pop handle)))
                        (when (string-match "diff\\|patch\\|plain"
                                            (mm-handle-media-type handle))
                          (let ((inhibit-read-only t)
                                (buf (mm-handle-buffer handle)))
                            (when (with-current-buffer buf
                                    (pcase (mm-handle-encoding handle)
                                      (`base64
                                       (base64-decode-region (point-min) (point-max)))
                                      (`quoted-printable
                                       (quoted-printable-decode-region (point-min) (point-max))))
                                    (goto-char (point-min))
                                    (re-search-forward
                                     "\\(?:^>?From:?\\)\\|diff --git" nil t))
                              (debbugs-clean-headers)
                              (push buf patch-buffers)
                              (set-window-dedicated-p
                               (display-buffer
                                buf
                                '(display-buffer-pop-up-window . nil))
                               t))))))
                    (unless patch-buffers
                      (gnus-summary-show-article 'raw)
                      (article-decode-charset)
                      (with-current-buffer (get-buffer gnus-article-buffer)
                        (let ((inhibit-read-only t))
                          (debbugs-clean-headers))
                        (push (current-buffer) patch-buffers)))
                    (balance-windows)
                    (mapcar (lambda (bufname)
                              (with-current-buffer bufname
                                (buffer-string)))
                            (let ((crm-separator ","))
                              (completing-read-multiple
                               "Apply patch(es)? "
                               (mapcar #'buffer-name patch-buffers) nil t))))
                (with-demoted-errors "Trying to restore pre-patch-grab window conf"
                  (set-window-configuration wconf)
                  ;; Restore the article buffer to non-`raw' form.
                  (gnus-summary-show-article)))))
           (patch-dir
            (make-temp-file (replace-regexp-in-string
                             "[^-[:alnum:]]" "_" subject)
                            t))
           (patch-num 0)
           (tmp-patch-files nil))
      (unwind-protect
          (progn
            (dolist (patch chosen-patches)
              (push (make-temp-file
                     (expand-file-name (format "%04d" (cl-incf patch-num))
                                       patch-dir)
                     nil ".patch")
                    tmp-patch-files)
              (with-temp-file (car tmp-patch-files)
                (insert patch)
                (goto-char (point-min))
                (if (re-search-forward "^\\(>?\\)From:?" nil t)
                    (progn
                      ;; 'git am' gets confused by '>From'.
                      (delete-region (match-beginning 1) (match-end 1)))
                  ;; Looks like a bare patch, supplement with
                  ;; email data.
                  (insert "From: " from "\n")
                  (insert "Date: " date "\n\n")
                  (insert "Subject: " subject "\n")
                  (insert message))
                (debbugs-strip-noise-from-subject (current-buffer))
                (debbugs-add-bugnum-if-needed bugnum)))
            (with-temp-buffer
              (setq default-directory (debbugs-read-repo "Apply to: "))
              (apply #'call-process "git" nil '(t t) nil
                             "am" "--3way" (nreverse tmp-patch-files))
              (message (buffer-string))))
        (delete-directory patch-dir t)))))

(defun debbugs-gnu-commit ()
  "`magit-commit', --author --date --message set from email."
  (interactive)
  (require 'debbugs-gnu)
  (gnus-summary-select-article nil t)
  (with-current-buffer gnus-article-buffer
    (let* ((from (gnus-fetch-field "from"))
           (subject (debbugs-strip-noise-from-subject
                     (gnus-fetch-field "subject")))
           (date (gnus-fetch-field "date"))
           (body (save-restriction
                   (gnus-narrow-to-body)
                   (buffer-string))))
      (with-temp-buffer
        (setq default-directory (debbugs-read-repo "Commit to: "))
        (magit-commit
         (list (concat "--author=" from)
               (concat "--date=" date)
               (concat "--message=" subject)
               (concat "--message=" body)
               "--edit"))))))

(defun debbugs-clean-headers ()
  "Remove irrelevant headers, so I can see the important stuff."
  (save-restriction
    (message-narrow-to-head)
    (message-remove-header
     (regexp-opt '("From" "To" "Subject" "Thread-Topic" "Thread-Index" "Date"
                   "Message-ID" "References" "In-Reply-To"
                   "X-Headers-End"))
     ;; Remove all non-matching headers
     t nil t)))

(defun forward-message-to-debbugs ()
  (interactive)
  (debbugs-clean-headers)
  (let* ((buf (current-buffer))
         (subject (message-fetch-field "Subject"))
         (from (message-fetch-field "From"))
         ;; "RE: bug#28888: 26.0.90; nt/INSTALL.W64"
         (bugnum (if (string-match "[bB]ug#\\([0-9]+\\)" subject)
                     (string-to-number (match-string 1 subject)))))
    (message-mail (format "%d@debbugs.gnu.org" (or bugnum 0)) subject
                  `(("Cc" . ,from)))
    (message-forward-make-body buf)
    (insert "[forwarding to list]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions grabbed from elsewhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; from https://github.com/thomasf/dotfiles-thomasf-emacs
(defun toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1)))))
  (make-repeatable 'toggle-fold))

;;; from http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;; from https://github.com/re5et/.emacs.d/blob/master/my/my-functions.el

(defun rotate-frame-window-buffers ()
  (interactive)
  (let ((window-and-buffers
         (mapcar
          (lambda (window)
            (cons window (window-buffer (next-window window))))
          (window-list))))
    (dolist (window-and-buffer window-and-buffers)
      (let ((window (car window-and-buffer))
            (buffer (cdr window-and-buffer)))
        (select-window window)
        (switch-to-buffer buffer))))
  (make-repeatable 'rotate-frame-window-buffers))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1)))))
  (make-repeatable 'toggle-window-split))




(provide 'np-utils)
