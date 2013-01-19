(defun hs-with-clojure-test (fn &rest args)
  "call fn with hideshow bindings marking (with-test (def ...)
tests) so that the tests (but not the def) is hidden, also hides
deftest and testing forms."  
  (let ((hs-block-start-regexp "(with-test\\|(deftest\\|(testing")
        (hs-block-end-regexp ")")
        (hs-adjust-block-beginning (lambda (initial)
                                     (save-excursion
                                       (if (looking-at "(with-test")
                                           (progn
                                             (forward-char)
                                             (forward-sexp 2)
                                             (forward-line)
                                             (point))
                                         (point))))))
    (apply fn args)))

(defun hs-hide-all-clojure-tests ()
  "Hide all clojure test forms."
  (interactive)
  (hs-with-clojure-test #'hs-hide-all))

(defun hs-hide-clojure-test (&optional end)
  "Hide the current clojure test form"
  (interactive "P")
  (hs-with-clojure-test #'hs-hide-block end))

(add-hook 'clojure-mode-hook
          (lambda ()
            (hs-minor-mode +1)
            (local-set-key (kbd "C-c h c")
                           'hs-toggle-hiding)
            (local-set-key (kbd "C-c h t")
                           'hs-hide-all-clojure-tests)
            (local-set-key (kbd "C-c h b")
                           'hs-hide-block)
            (local-set-key (kbd "C-c h s")
                           'hs-show-all)
            (local-set-key (kbd "C-c h a")
                           'hs-hide-all)

            (hs-hide-all-clojure-tests)))

