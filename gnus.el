(setq gnus-select-method
      '(nnimap "imap.gmail.com" (nnimap-stream ssl))

      gnus-save-newsrc-file nil
      gnus-startup-file "~/.emacs.d/newsrc"
      message-subscribed-address-functions '(gnus-find-subscribed-addresses)
      gnus-group-line-format "%M%S%p%P%5y:%B%ug%(%uG%)\n"

      gnus-treat-display-smileys nil

      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      message-user-agent 'gnus-user-agent

      user-mail-address "npostavs@gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587

      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "npostavs@gmail.com"))

      ;; Use STARTTLS without authentication against the server.
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))

      smtpmail-debug-info t
      smtpmail-debug-verb t)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun gnus-user-format-function-g(_)
  (save-match-data
    (if (string-match "\\(^[^/]+/\\).+$" gnus-tmp-group)
        (replace-match "\\1" nil nil gnus-tmp-group)
      gnus-tmp-group)))
(defun gnus-user-format-function-G(_)
  (save-match-data
    (if (string-match "\\(^[^/]+/\\)\\(.+\\)$" gnus-tmp-group)
        (replace-match  "\\2" nil nil gnus-tmp-group)
      "")))
