;;; slime-complete-locals.el --- complete functions taking locally mentioned symbols into account

;; Copyright (C) 2010 Joost Diepenmaat, Zeekat Softwareontwikkeling
;;
;; Author: Joost Diepenmaat - joost@zeekat.nl
;; Version: 0.2 / 2010-06-05
;; Keywords: abbrev, convenience, slime
;; URL: http://github.com/joodie/slime-complete-locals
;; Compatibility: GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code advices some of the SLIME completion function
;; to take into account any locally mentioned symbols.
;;
;; That way function arguments and let bindings are expanded
;; as well as the usual 'globally reachable' vars in the
;; inferior lisp.

;; To configure, put this file somewhere in your load-path then
;;
;;   (slime-setup 
;;    '(slime-fancy  ;; optional, turns on fancy stuff
;;      slime-company ;; optional, needs additional install
;;      slime-complete-locals)) 

;; Note that for this to work, for now, you need to make sure your
;; `slime-complete-symbol-function' is either `slime-complete-symbol*'
;; (which is the default) or `slime-simple-complete-symbol' or some
;; function that uses `slime-simple-completions' or
;; `slime-completions', like the `slime-company' completion functions.
;;
;; `slime-fuzzy-complete' / `slime-fuzzy-completions' aren't yet
;; supported.
;;
;; This code will work "out of the box" with `slime-company' mode:
;; http://www.emacswiki.org/emacs/CompanyMode and
;; http://www.emacswiki.org/emacs/CompanyModeBackends
;;
;; Also see
;; http://joost.zeekat.nl/2010/06/04/slime-hints-3b-lexical-completions-also-a-correction/
;;

;;; Code:

(setq slime-string-literal-regexp
  "\".*?[^\\]\"")

(setq slime-comment-regexp
  ";.*")

(setq slime-not-a-symbol-regexp
  "[][(){}]+")

(defun slime-get-local-symbols-buffer
  ()
  (save-excursion
    (let ((b (get-buffer-create "*slime-local-symbols-buffer*")))
      (set-buffer b)
      (erase-buffer)
      b)))

(defun slime-uniq-list (list)
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

(defun slime-count-list (list)
  (let ((result '()))
    (dolist (elem list)
      (let ((cell (assoc elem result)))
        (if cell
            (setcdr cell (+ 1 (cdr cell)))
          (setq result (cons (cons elem 1) result)))))
    (message "%s" (prin1-to-string result))
    result))

(defun slime-local-replace
  (reg rep)
  (while (re-search-forward reg nil t)
    (replace-match rep nil nil)))

;; this is pretty hackish. should be fairly quick, though
(defun slime-local-symbols
  ()
  (interactive)
  (save-excursion
    (destructuring-bind (s e) (slime-region-for-defun-at-point)
      (let ((tmp (slime-get-local-symbols-buffer)))
        (copy-to-buffer tmp s e)
        (set-buffer tmp)
        (set-text-properties 1 (point-max)  nil)        
        (slime-local-replace slime-string-literal-regexp " ")
        (goto-char (point-min))
        (slime-local-replace slime-comment-regexp "")
        (goto-char (point-min))
        (slime-local-replace slime-not-a-symbol-regexp " ")
        (slime-count-list (split-string (buffer-string)))))))

(defun slime-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun slime-simple-local-completions (prefix)
  (message "%s" (prin1-to-string prefix))
  (let ((len (length prefix)))
    (let ((res (mapcar #'car
                       (slime-filter
                        (lambda (pair) 
                          (let ((str (car pair)))
                            ;; if prefix only occurs once in the
                            ;; current defun, ignore it, since that
                            ;; single occurance is the text we're
                            ;; trying to complete!
                            (and
                             (or (not (string= prefix str)) 
                                 (< 1 (cdr pair)))
                             (<= len (length str))
                             (string= prefix (substring str 0 len)))))
                        (slime-local-symbols)))))
      (message "%s" (prin1-to-string res))
      res)))

(defun slime-add-simple-local-completions (prefix lst)
  (cons 
   (sort (slime-uniq-list (append (slime-simple-local-completions prefix) (car lst))) 
         (lambda (a b) (compare-strings a 0 nil b 0 nil))) 
   (cdr lst)))


(defadvice slime-simple-completions (around include-local-symbols (prefix))
  "include symbols from the current top-level form in the completion suggestions"
  ad-do-it
  (setq ad-return-value (slime-add-simple-local-completions prefix ad-return-value)))

(defadvice slime-completions (around include-local-symbols (prefix))
  "include symbols from the current top-level form in the completion suggestions"
  ad-do-it
  (setq ad-return-value (slime-add-simple-local-completions prefix ad-return-value)))

(defun slime-complete-locals-init ()
  (ad-activate 'slime-simple-completions)
  (ad-activate 'slime-completions))

(provide 'slime-complete-locals)
