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

(defvar el-get-sources nil)

(defun define-and-add-el-get-source (package)
  "Update PACKAGE in `el-get-source' (or add if absent)."
  (let ((srcs el-get-sources)
        (name (plist-get package :name)))
    (if (catch 'return
          (while srcs
            (when (eq name (plist-get (car srcs) :name))
              (setcar srcs package)
              (throw 'return t))
            (pop srcs))
          nil)
        el-get-sources        ; package updated with setcar
      ;; didn't find the package, let's add it
      (push package el-get-sources))))


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
      (goto-char (if (and (< lo old) (<= old hi))
                     lo hi)))))

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

;; based on `kmacro-call-macro' code. There is a library form for this
;; sort of thing, but it looks a lot more complicated possibly because
;; it's working with older emacsen.
;; http://furius.ca/pubcode/pub/conf/lib/elisp/blais/repeatable.el
(defun make-repeatable (cmd no-repeat &rest args)
  "call this to allow repeating function with last key of
sequence, just like C-x e e e..."
  (when (or (eq no-repeat 'repeating)
            (> (length (this-single-command-keys)) 1))
   (set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (define-key map (vector last-input-event)
        `(lambda () (interactive) (,cmd ,@args 'repeating)))
      map))))

;;; from https://github.com/thomasf/dotfiles-thomasf-emacs
(defun toggle-fold (&optional no-repeat)
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1)))))
  (make-repeatable 'toggle-fold no-repeat))


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
         (switch-to-buffer buffer))) map)))

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
          (if this-win-2nd (other-window 1))))))




(provide 'np-utils)
