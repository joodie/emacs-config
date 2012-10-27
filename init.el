(when (not (string-match ":/opt/local/bin" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":" (getenv "HOME") "/bin")))
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))


(package-initialize)



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

(load-library "guru-mode")

(add-hook 'prog-mode '(lambda () (guru-mode +1)))

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

(load-theme 'joost)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(custom-safe-themes (quote ("e5570dc54a4ba2d8d249779f902f8de1f1a5dacc1a0c3b4c118ba69922195d2a" "c973a0087f0c50f8014832f6f3b46c7a71bb1645bd05aee0f0c5cfd3b966e982" "fcb5ab7cb7c6248af9baec83807311ed2e02b31131a214465bce3cde1a31928f" "960163229791d5476bce25b951c72c696718b5985ce0611a8d2b9a5c1f771522" "4272a9d8aceeffd739a6d52a1ddc882d6f4ee9a546a598a3b2d64e8452e36df6" "516d3ad039684de5e55da7f3f79e25033665367d26353ae4e85409b4970b33e0" "f8be77f22462256ec1086b348d6efd28f75fff6ac502d36002460d197797ffe7" "125694a6a3f4ede7fa84c4179dcb3f10fcb55cfc938d1688647e8bf8b38f2cf9" "ec0da9651592a3b4eb1bc34dd8a47dcb0afd0d51b634f15462a9dedbba53600f" "79f01425c7a44ea876c813534a311d19985bab5dd18d80fb3d8cd1e633bd388f" "63436c7c5b4505f96f1263a0b6c9d2cdb63091a6605d671b92284f660bf9a134" "76f877336dff973860cf4c8fc53a15bf933e662d4a363235e3bb7f6a0061ff7b" "3157a15ee13cb18bf063233e1161912fb77a62cc97a991ae4212247d17936ecd" "efd8bf5f14d21ba5beda631dd4247e2680da9dd757e31a9b9ff799fbc1b422c6" "46cf34e44b48750b654eeb40e68021a70e97381d0b8548cff99f8c1627c5ee89" "1ef39d3ab971bb2820b4b3c0eb374bc5a6f745fce390d2c42beb00577f7ec668" "61ea6a54b8a6a5f7b4a827b1c314d025a41d55c616d3946177873835f5a10acb" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "32b4839be3c3fa65377ce14d32f73be4d34ff7543bb8a9091f6fa5b53ecae54c" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "7fe1e3de3e04afc43f9a3d3a8d38cd0a0efd9d4c" "d14db41612953d22506af16ef7a23c4d112150e5" default)))
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
 '(rails-always-use-text-menus t)
 '(rails-test:quiet t)
 '(safe-local-variable-values (quote ((auto-whitespace-cleanup . t) (whitespace-line-column . 80) (lexical-binding . t))))
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(slime-complete-symbol-function (quote slime-simple-complete-symbol))
 '(smart-tab-completion-functions-alist (quote ((emacs-lisp-mode . lisp-complete-symbol) (text-mode . dabbrev-completion) (clojure-mode . complete-clojure-for-smart-tab))))
 '(sql-mysql-program "/usr/local/bin/mysql")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "LightYellow1")))))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
