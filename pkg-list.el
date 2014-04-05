(defun pkg-list-entries ()
  (mapcar
   (lambda (package)
     (let* ((type             (el-get-package-type package))
            (recipe           (el-get-package-def package))
            (checkout         (plist-get recipe :checkout))
            (remote           (plist-get recipe :branch))
            (compute-checksum (el-get-method type :compute-checksum))
            (pkg-sym          (el-get-package-symbol package))
            (current-rev
             (cond
              (compute-checksum
               (funcall compute-checksum package))
              ((eq type 'elpa)
               (package-version-join
                (package-desc-vers (cdr (assq (intern package) package-alist)))))
              ((eq type 'builtin) "BUILTIN")
              (t "???")))
            (face (if (or (eq type 'builtin)
                          (string= current-rev checkout))
                      'default 'font-lock-warning-face)))

       (unless remote
         (setq remote (if (member type '(git github))
                          "origin/master" "")))

       (list pkg-sym
             (apply #'vector
                    (mapcar (lambda (s) (propertize s 'face face))
                            (list package current-rev
                                  (el-get-as-string checkout)
                                  remote))))))
   (el-get-list-package-names-with-status "installed")))

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
               (replace-regexp-in-string "\n$" ""
                (shell-command-to-string (format "git rev-parse %s" remote))
                t t))))))
    (cond
     ((equal remote-rev checkout)
      (message "%s = %s, %s is up to date." remote-rev checkout pkg))
     ((string-match-p "[0-9a-f]\\{40\\}" remote-rev)
      (message "%s != %s" remote-rev checkout)
      (with-current-buffer (find-file-noselect "~/.emacs.d/np-recipes.el")
        (goto-char (point-min))
        (re-search-forward (format "(:name %s +:checkout"
                                   (regexp-quote (symbol-name pkg))))
        (skip-syntax-forward " ")
        (delete-region (point) (save-excursion (forward-sexp 1) (point)))
        (prin1 remote-rev (current-buffer))
        (save-buffer)
        (let ((eval-expression-print-level 1)
              (eval-expression-print-length 1))
         (eval-defun nil)))
      (revert-buffer))
     (t (message "remote = '%s', oops..." remote-rev)))))

(defun pkg-list-update-to-target ()
  (interactive)
  (let* ((pkg-entry (tabulated-list-get-entry))
         (pkg (el-get-package-symbol (elt pkg-entry 0))))
    (el-get-update pkg)))

(define-derived-mode pkg-list-mode tabulated-list-mode "PkgList"
  "List and manage el-get installed packages..."
  (setq-local tabulated-list-format
              [("Package" 30 t)
               ("Current Revision" 16 nil)
               ("Target Revision" 16 nil)
               ("Remote branch" -1 nil)])
  (tabulated-list-init-header)
  (setq tabulated-list-entries #'pkg-list-entries))

(define-key pkg-list-mode-map "U" #'pkg-list-update-to-target)
(define-key pkg-list-mode-map "C" #'pkg-list-check-remote)

(defun pkg-list ()
  (interactive)
  (pop-to-buffer "*Pkg List*")
  (pkg-list-mode)
  (tabulated-list-print))

(provide 'pkg-list)
