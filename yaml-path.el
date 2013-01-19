(defun yaml-nav-parent-line
  ()
  (interactive)

  (let* ((current-i (current-indentation))
        (target-i (- current-i  yaml-indent-offset)))
    (if (= 0 current-i)
        nil
      (search-backward-regexp (concat "^ \\{" (format "%d" target-i) "\\}[^ #\n]"))
      (forward-char target-i)
      1)))


(defun yaml-nav-current-key
  ()  
  (save-excursion
    (move-beginning-of-line nil)
    (search-forward-regexp "[^ ]")
    (backward-char 1)
    (search-forward-regexp "[^:]+")
    (match-string 0)))

(defun yaml-nav-show-current-key
  ()
  (interactive)
  (message "%s" (yaml-nav-current-key)))

(defun yaml-nav-current-path
  ()
  (let ((p (list (yaml-nav-current-key))))
    (save-excursion
      (while (yaml-nav-parent-line)
        (setq p (cons (yaml-nav-current-key) p))))
    (strings-join "." p)))

(defun yaml-nav-show-current-path
  ()
  (interactive)
  (message "%s" (yaml-nav-current-path)))


