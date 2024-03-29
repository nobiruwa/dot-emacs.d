(provide 'reopen-as-root)

(defun file-other-p (filename)
  "Return t if file FILENAME created by others."
  (if (file-exists-p filename)
      (/= (user-real-uid) (nth 2 (file-attributes filename))) t))

(defun file-username (filename)
  "Return File Owner."
  (if (file-exists-p filename)
      (user-full-name (nth 2 (file-attributes filename)))
    (user-full-name (nth 2 (file-attributes (file-name-directory filename))))))

(defun th-rename-tramp-buffer ()
  "Rename a buffer's name, if remote prefix (such as `sudo:', `scp:') is not shown."
  (let* ((current-buffer-name (buffer-name))
         (current-file-name (buffer-file-name))
         (remotep (file-remote-p current-file-name 'method))
         (remote-prefix (format "%s:" remotep))
         (prefixp (string-prefix-p remote-prefix current-buffer-name)))
    (if (and remotep (not prefixp))
        (rename-buffer
         (format "%s:%s" remotep current-buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-other-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0) " is "
                             (if (file-exists-p (ad-get-arg 0)) "read-only." "newer file.")
                             "  Open it as "
                             (file-username (ad-get-arg 0)) "? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo:"
                                 (file-username file) "@" (system-name) ":" file))))
