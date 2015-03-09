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

(defun np-github-clone (user pkg)
  "Clone a github repository from npostavs/PKG into ~/src/PKG.

Sets up remotes:
  origin = git@github.com:npostavs/PKG.git
  upstream = https://github.com/USER/PKG.git"
  (interactive (let* ((p (read-string "Package:"))
                      (u (read-string "User: " nil nil p)))
                 (list u p)))
  (let ((clone-dir (expand-file-name pkg "~/src"))
        (ssh-url (format "git@github.com:npostavs/%s.git" pkg))
        (https-url (format "https://github.com:%s/%s.git" user pkg)))
   (make-directory clone-dir)
   (with-current-buffer "*git-clone-output*"
     (call-process "git" nil t t
                   "clone" "-o" "upstream" https-url clone-dir)
     (call-process "git" nil t t
                   "remote" "add" "origin" ssh-url))
   (when (featurep 'magit)
     (magit-status clone-dir))))

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

(defun just-dwim-space (&optional n)
  "like `just-one-space', but flip between 1 and 0 spaces"
  (interactive "*P")
  (just-one-space 
   (if n (prefix-numeric-value n)
     (let* ((orig-pos (point))
            (skip-characters " \t")
            (x (- (save-excursion
                    (skip-chars-forward skip-characters)
                    (constrain-to-field nil orig-pos t))
                  (save-excursion
                    (skip-chars-backward skip-characters)
                    (constrain-to-field nil orig-pos)))))
       (if (zerop x) 1 0)))))

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


;; based on `kmacro-call-macro' code. There is a library form for this
;; sort of thing, but it looks a lot more complicated possibly because
;; it's working with older emacsen.
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
        `(lambda () (interactive) (let ((make-repeatable--command ',cmd))
                               (,cmd ,@args))))
      map))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions grabbed from elsewhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; from https://github.com/re5et/.emacs.d/blob/master/my/my-functions.el

(defun rotate-frame-window-buffers ()
  (interactive)
  (let ((map
         (mapcar
          (lambda (window)
            `(,window
              ,(window-buffer
                (next-window window))))
          (window-list))))
    (mapcar
     (lambda (window-to-buffer)
       (let ((window (car window-to-buffer))
             (buffer (cadr window-to-buffer)))
         (select-window window)
         (switch-to-buffer buffer))) map))
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
