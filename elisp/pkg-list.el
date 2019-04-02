(require 'el-get-elpa) ; for package compat functions

(defconst pkg-recipes-file "~/.emacs.d/elisp/np-recipes.el")

(defun pkg-list-recipe-checkout-alignment ()
  (with-current-buffer (find-file-noselect pkg-recipes-file)
    (save-excursion
      (goto-char (point-min))
      (search-forward ":checkout")
      (- (match-beginning 0) (line-beginning-position)))))

(defun pkg-list-recipe-checkout-align ()
  (interactive)
  (let ((col (pkg-list-recipe-checkout-alignment)))
    (search-forward ":checkout")
    (save-excursion
      (goto-char (match-beginning 0))
      (when (<= (current-column) col)
        (indent-to col)))))

(defun pkg-list-entries ()
  (load pkg-recipes-file nil 'nomessage 'nosuffix)
  (delq
   nil
   (mapcar
    (lambda (package)
      (let ((recipe (ignore-errors (el-get-package-def package))))
        (when recipe
          (let* ((type             (el-get-package-type recipe))
                 (checkout         (plist-get recipe :checkout))
                 (remote           (plist-get recipe :branch))
                 (compute-checksum (el-get-method type :compute-checksum))
                 (pkg-sym          (el-get-package-symbol package))
                 (current-rev
                  (cond
                   ((not (string= (el-get-package-status package) "installed"))
                    "nil")
                   (compute-checksum
                    (or (ignore-errors (funcall compute-checksum package))
                        "???"))
                   ((and (eq type 'elpa) package-alist)
                    (package-version-join
                     (package-desc-version
                      (car (el-get-elpa-descs
                            (assq (intern package) package-alist))))))
                   ((eq type 'builtin) "-")
                   (t "???")))
                 (face (if (or (eq type 'builtin)
                               (string= current-rev checkout))
                           'default 'font-lock-warning-face)))

            (unless remote
              (setq remote (if (member type '(git github))
                               "origin/master"
                             (propertize (upcase (symbol-name type))
                                         'face 'shadow))))

            (list pkg-sym
                  (apply #'vector
                         (append
                          (mapcar (lambda (s) (propertize s 'face face))
                                  (list package current-rev
                                        (el-get-as-string checkout)))
                          (list remote))))))))
    (delete-consecutive-dups
     (sort (append (el-get-list-package-names-with-status "installed")
                   (mapcar #'el-get-source-name el-get-sources))
           #'string<)))))

(defun pkg-list-check-remote ()
  (interactive)
  (let* ((pkg-entry (tabulated-list-get-entry))
         (pkg (el-get-package-symbol (elt pkg-entry 0)))
         (pkg-def (el-get-package-def pkg))
         (checkout (plist-get pkg-def :checkout))
         (remote (plist-get pkg-def :branch))
         (type (el-get-package-type pkg))
         (remote-rev
          (cl-case type
            ((git github)
             (let ((default-directory (el-get-package-directory pkg))
                   (remote (or remote "origin/master")))
               (call-process "git" nil nil nil "fetch")
               (with-temp-buffer
                 (call-process "git" nil '(t t) nil "rev-parse"
                               (concat remote "^{commit}"))
                 (goto-char (point-max))
                 (skip-syntax-backward "\\s-")
                 (delete-region (point) (point-max))
                 (buffer-string)))))))
    (cond
     ((equal remote-rev checkout)
      (message "%s = %s, %s is up to date." remote-rev checkout pkg))
     ((and remote-rev (string-match-p "[0-9a-f]\\{40\\}" remote-rev))
      (message "%s != %s" remote-rev checkout)
      (with-current-buffer (find-file-noselect pkg-recipes-file)
        (goto-char (point-min))
        (if (re-search-forward (format "(:name %s +:checkout"
                                       (regexp-quote (symbol-name pkg)))
                               nil t)
            (progn
              (skip-syntax-forward " ")
              (delete-region (point) (save-excursion (forward-sexp 1) (point)))
              (prin1 remote-rev (current-buffer))
              (save-buffer)
              (eval-buffer)
              (revert-buffer))
          (kill-new (with-temp-buffer
                      (insert (format "(:name %s :checkout \"%s\")" pkg remote-rev))
                      (search-backward ":checkout")
                      (indent-to (pkg-list-recipe-checkout-alignment))
                      (buffer-string)))
          (pop-to-buffer (current-buffer))
          (message "Couldn't find entry for %s, added one to kill-ring" pkg))))
     (t (message "remote = '%s', oops..." remote-rev)))))

(defun pkg-list-update-to-target ()
  (interactive)
  (let* ((pkg-entry (tabulated-list-get-entry))
         (pkg (el-get-package-symbol (elt pkg-entry 0))))
    (if (el-get-package-installed-p pkg)
        (el-get-update pkg)
      (el-get-install pkg))))

(define-derived-mode pkg-list-mode tabulated-list-mode "PkgList"
  "List and manage el-get installed packages..."
  (setq-local tabulated-list-format
              [("Package" 30 t)
               ("Current Revision" 16 nil)
               ("Target Revision" 16 nil)
               ("Remote branch" -1 nil)])
  (cd "~/.emacs.d/elisp/")
  (tabulated-list-init-header)
  (setq tabulated-list-entries #'pkg-list-entries))

(define-key pkg-list-mode-map "U" #'pkg-list-update-to-target)
(define-key pkg-list-mode-map "C" #'pkg-list-check-remote)
(define-key pkg-list-mode-map "A" #'pkg-list-recipe-checkout-align)

(defun pkg-list ()
  (interactive)
  (pop-to-buffer "*Pkg List*")
  (pkg-list-mode)
  (tabulated-list-print))

(provide 'pkg-list)
