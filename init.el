(when (not (string-match ":/opt/local/bin" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":" (getenv "HOME") "/bin:/usr/texbin")))

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))


(package-initialize)

;; org-mode global keys

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-default-notes-file  "~/org/notes.org")

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-bindings scpaste
                                  clojure-mode clojure-test-mode
                                  markdown-mode yaml-mode tuareg
                                  marmalade oddmuse scpaste
                                  company magit swank-cdt ecb_snap color-theme))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d")



(set-face-attribute 'default nil
                    :family "Inconsolata" :height 140 :weight 'normal :width 'normal)

(set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans"
                                                         :width 'normal
                                                         :size 13.0
                                                         :weight 'normal))

;; (set-face-attribute 'default nil
;;                     :family "Inconsolata" :height 140 :weight 'normal :width 'normal)

;; (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans Mono
;;                                                          :width 'normal
;;                                                          :size 16.0
;;                                                          :weight 'normal))

;; fixes ECB errors
(setq stack-trace-on-error t)



(menu-bar-mode +1)
;(load-library "clojure-refactoring-mode")

(require 'starter-kit-defuns)

(dolist (hook '(clojure-mode-hook
                lisp-mode
                javascript-mode))
  (add-hook hook
            '(lambda () (highlight-parentheses-mode t))))


(column-number-mode 1)
(hl-line-mode -1)


(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                          (lambda (_ _) nil))
             (enable-paredit-mode)))

(eval-after-load 'js
  '(progn (define-key js-mode-map "'" 'paredit-doublequote)))

(blink-cursor-mode 1)


;;;; THIS CRASHES IN OSX TERMINAL
;;
;; (defvar in-fullscreen
;;   0)
;; (when (and (fboundp 'ns-toggle-fullscreen)
;;            (= 0 in-fullscreen))
;;   (ns-toggle-fullscreen)
;;   (global-set-key (kbd "<s-return>") #'ns-toggle-fullscreen)
;;   (setq in-fullscreen 1))

;; make sure backspace is backspace in terminal.
(normal-erase-is-backspace-mode 1)

(global-set-key (kbd "<f5>") #'recompile)

;; disable C-z on X11 sessions
(when window-system
(global-unset-key "\C-z"))

(add-to-list 'load-path (expand-file-name "~/projects/emacs-rails"))
(require 'rails)

(require 'iswitch-menu)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'tex-site)

;; cleaning whitespace on save
(make-variable-buffer-local 'auto-whitespace-cleanup)
(add-hook 'before-save-hook
          (lambda () (when auto-whitespace-cleanup (whitespace-cleanup))))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

;; switch off automatic line breaks in rhtml
(add-hook 'rhtml-mode-hook
          '(lambda () (auto-fill-mode -1)))

(require 'rails)
(require 'rspec-mode)

(require 'nrepl)

(add-to-list 'load-path
             "~/projects/emacs-websocket")

(add-to-list 'load-path
             "~/projects/kite")

(require 'dired+)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


;; This should defer fontification, speeds up scrolling.
;; See http://tsengf.blogspot.nl/2012/11/slow-scrolling-speed-in-emacs.html

;(setq jit-lock-defer-time 0.05)
;(setq jit-lock-stealth-time 1) ; start fontification when no input


(defun three-columns
  ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode +1))

;; (defun pretty-stuff
;;   nil
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil
;;    `(("\\(->\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "→") nil)))
;;      ("\\(=>\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "⇒") nil)))
;;      ("\\(<=\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "≤") nil)))
;;      ("\\(>=\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "≥") nil)))
;;      ("\\(==\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "≡") nil)))
;;      ("\\(!=\\)" (0 (progn (compose-region (match-beginning 1) (match-end 1) "≢") nil))))))

;; (add-hook 'coffee-mode-hook 'pretty-stuff)

(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; (defvar clojure-symbol-mapping
;;   '(("->"   . "→")
;;     ("=="   . "⪮")
;;     ("=>"   . "⇒")
;;     ("<="   . "≤")
;;     (">="   . "≥")
;;     ("not=" . "≢")))

;; (defun symbol-map->font-lock
;;   (m)
;;   (list (concat "\\(" (car m) "\\)")
;;         `(0 (progn (compose-region (match-beginning 1)
;;                                    (match-end 1)
;;                                    ,(cdr m))))))

;; (defun set-symbols
;;   (mapping)
;;   (font-lock-add-keywords
;;       nil
;;       (mapcar #'symbol-map->font-lock
;;               mapping)))

;; (add-hook 'clojure-mode-hook '(lambda ()
;;                                 (set-symbols clojure-symbol-mapping)
;;                                 (clojure-test-mode +1)))

(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
  (require 'nrepl-ritz))

(add-hook 'clojure-mode-hook '(lambda ()
                                (clojure-test-mode +1)))



;(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

(defun complete-clojure-for-smart-tab
  (&rest)
  "ignores arguments"
  (interactive)
  (call-interactively #'complete-symbol))

;; make it possible to delete zip entries that have a backslash in
;; their name.

(defun archive-zip-fix-backslash-name
  (name)
  (replace-regexp-in-string "\\\\" "\\\\\\\\" name))

(defun archive-zip-expunge
  (archive files)
  (apply 'call-process
         "zip"
         nil
         nil
         nil
         (cons "-dq" (cons archive (mapcar 'archive-zip-fix-backslash-name files)))))



(global-rainbow-delimiters-mode +1)

(require 'simple-slides)

(defun slime-eval-print-last-expression-as-comment (string)
  "Evaluate sexp before point; print value into the current buffer as a comment"
  (interactive (list (slime-last-expression)))
  (insert " ; => ")
  (slime-eval-print string))

(add-hook 'slime-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x p")
                            'slime-eval-print-last-expression-as-comment)))

(defun slime-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-print string))


(require 'midnight)

(hl-line-mode +1)

(require 'powerline)
(powerline-default-theme)

(projectile-global-mode 1)

(require 'hideshow-clojure-tests)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "red" "forest green" "DarkGoldenrod3" "blue" "magenta" "dark cyan" "white"])
 '(archive-zip-expunge (quote ("zip" "-q" "-d")))
 '(auto-indent-next-pair-timer-interval (quote ((clojure-mode 1.5) (default 0.0005))))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs (company-gtags company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev)))
 '(company-begin-commands (quote (quote (self-insert-command))))
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(compilation-search-path (quote (nil)))
 '(compilation-window-height 20)
 '(css-indent-offset 2)
 '(custom-safe-themes (quote ("ddbe2d1e6208ce5ab21ec83cfcc3b31f009e6779a33f0c48514ca7abd5b79cee" "23029d4ad61c4ed4c3d6182f79e190cd60b99414cf8737eb24ab7ade6e7f9147" "b1c900df5fe6a3c052d00e197aa0bcf1b4c6bb05598a1b378b11c60da8f20faa" "938e431e2754b818626a24649fe8cefccf3c75f48f7379d492c1cb50e5149177" "c6f4dbba1e1444e4a0908f4fc507fccc0f1626818515eb3a82b1e7e11f5645bf" "821e340536c89aa46c2a3d792991c42ca178b65c18fc76c28602ff81c6795471" "e5570dc54a4ba2d8d249779f902f8de1f1a5dacc1a0c3b4c118ba69922195d2a" default)))
 '(dired-dwim-target t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(ffap-machine-p-known (quote reject))
 '(global-company-mode t)
 '(global-rainbow-delimiters-mode t)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(iswitch-menu-override-tmm-prompt t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-git-executable "/usr/local/bin/git")
 '(midnight-mode t nil (midnight))
 '(org-agenda-files (quote ("~/projects/contendis/planning.org")))
 '(org-use-speed-commands t)
 '(rails-always-use-text-menus t)
 '(rails-test:quiet t)
 '(safe-local-variable-values (quote ((auto-whitespace-cleanup . t) (whitespace-line-column . 80) (lexical-binding . t))))
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(slime-complete-symbol-function (quote slime-simple-complete-symbol))
 '(smart-tab-completion-functions-alist (quote ((emacs-lisp-mode . lisp-complete-symbol) (text-mode . dabbrev-completion) (clojure-mode . complete-clojure-for-smart-tab))))
 '(sql-mysql-program "/usr/local/bin/mysql")
 '(tool-bar-mode nil)
 '(yas/trigger-key "C-c TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:inverse-video t))))
 '(highlight ((t (:background "LightYellow1")))))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; this needs to be evaluated AFTER custom-set-variables
(load-theme 'joost)
(put 'erase-buffer 'disabled nil)
