; automatically switch to lisp-mode for given files
(setq auto-mode-alist (cons '("dot-emacs" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("emacs-custom" . lisp-mode) auto-mode-alist))

;; ; dinamically add to load-path all folders from emacs-lib-folder
(setq emacs-lib-folder "~/.emacs.d")
(add-to-list 'load-path emacs-lib-folder)
(progn (cd emacs-lib-folder) (normal-top-level-add-subdirs-to-load-path))

; dinamically add to load-path all folders from emacs-lib-folder
;; (setq emacs-lib-folder "~/.emacs-mine")
;; (add-to-list 'load-path emacs-lib-folder)
;; (progn
;;   (cd emacs-lib-folder)
;;   (normal-top-level-add-subdirs-to-load-path))


; Misc settings
(transient-mark-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq scroll-step 1)
(tool-bar-mode -1)
(delete-selection-mode nil)
(setq inhibit-splash-screen -1)
(cua-selection-mode t)
(setq-default indent-tabs-mode nil)
(visual-line-mode t)
(scroll-bar-mode nil)
;(blink-cursor-mode t) ;; toggle the blinking cursor
;; (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")

;; show trailing whitespaces is some programming modes : -is this really working?
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace t))))
      '(text-mode-hook
        c-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        java-mode-hook
        python-mode-hook
        shell-script-mode-hook))

(global-set-key [(meta /)] 'dabbrev-expand)
(global-set-key [(control /)] 'hippie-expand)

; Shortening of often used commands
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'sh 'shell)

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'rof 'recentf-open-files)

(defalias 'eb 'eval-buffer)
2(defalias 'er 'eval-region)

(defalias 'hr 'highlight-regexp)
(defalias 'ur 'unhighlight-regexp)

(defalias 'dtw 'delete-trailing-whitespace)


; ibuffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "\*emacs\*")
			     (filename . "emacs-config")))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
         ("code" (filename . "code"))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
	 ("Subversion" (name . "\*svn"))
	 ("Magit" (name . "\*magit"))
	 ("ERC" (mode . erc-mode))
	 ("python" (mode . python-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

; Dired mode settings
;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook
          '(lambda ()
             (progn
               (require 'dired-x)
               ;; Enable toggling of uninteresting files.
               (setq dired-omit-files-p t)
               (setq dired-omit-files
                     (concat dired-omit-files "\\|\\.svn|\\.pyc")))))

; Enable linum mode
(linum-mode t)

;; ; Separate customize init file
(setq custom-file "~/.emacs-custom")
(load custom-file)

;; Don't make backup files next to the source, put them in a single place
(setq backup-directory-alist
      (quote (("." . "~/.emacs-backups"))))

;; Enable minor modes
(ido-mode t)
;; (ido-everywhere t)
;; (tabbar-mode t)

; Load required libraries
;; (add-to-list 'pymacs-load-path "/home/bhodorog/.emacs-mine/hg/python-mode")
(require 'pymacs)
(require 'column-marker)
(require 'dired+)


; Ropemacs
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport 't)
;; (setenv "PYTHONPATH" "$PYTHONPATH:/usr/share/pyshared/rope:/usr/share/pyshared/ropemacs:/usr/share/pyshared/ropemode:~/.emacs-mine/hg/python-mode" t)

;; ;; Switch between python.el and python-mode.el
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; ;; use ipython for python shell
;; (require 'ipython)

;; -----------------------------------------------------------------------------------
;;;; AUTOCOMPLETER STUFF

;; 1)
;; Pycomplete (from python-mode.el)
;; (eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path "/home/bhodorog/.emacs-mine/hg/python-mode"))
;; (require 'pycomplete)

;; 2)
;; Anything with ipython
;; ;;(require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))
;; (define-key py-mode-map (kbd "C-<tab>") 'anything-ipython-complete)

;; 3)
;; Rope
;; (define-key ropemacs-local-keymap [(meta /)] 'dabbrev-expand)
;; (define-key ropemacs-local-keymap [(control /)] 'hippie-expand)
;; (define-key ropemacs-local-keymap [(control c) (control /)] 'rope-code-assist)
;; ------

;; Auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs-mine/ac-dict")
;; (ac-config-default)
;; -----------------------------------------------------------------------------------

;; (defun py-outline-level ()
;;   (let (buffer-invisibility-spec)
;;     (save-excursion
;;       (skip-chars-forward "\t ")
;;       (current-column))))

;; (defun py-outline-level ()
;;   (1+ (/ (current-indentation) python-indent)))

(require 'yasnippet) ;; not yasnippet-bundle
;; (require 'yasnippet-autoloads "yasnippet-autoloads") 
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets")


;; Python mode combined with outline minor mode:
(add-hook 'python-mode-hook
          (lambda ()
            (interactive) (column-marker-2 79)
            (setq show-trailing-whitespace t)
            (which-function-mode t)
            (outline-minor-mode t)
            ;; (setq outline-regexp "[ \t]*# \\|[ \t]+\\(class\\|def\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\) ")
            ;; (setq outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\)")
            (yas/minor-mode t)
;            (linum-mode t)
            (auto-fill-mode t)
            ;; (setq py-python-command-args '("-pylab" "-colors" "LightBG"))
            (interactive) (column-marker-1 80)
            (setq coding-system-for-write 'utf-8)
;           python-mode related stuff
            ;; (setq outline-level 'py-outline-level)
            ;; (set (make-variable-buffer-local 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
            ;; (setq outline-regexp
            ;;       (rx (* space) (or "class" "def" "elif" "else" "except" "finally"
            ;;                         "for" "if" "try" "while" "with")
            ;;           symbol-end))
            ))


; Javascript mode
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

; move selected text up/down
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
        (exchange-point-and-mark))
     (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key "\M-\S-up" 'move-text-up)
(global-set-key "\M-\S-down" 'move-text-down)


; Use this only if color-theme gets some symbolp errors see
; http://www.emacswiki.org/emacs/ColorTheme#toc2

;; (defun color-theme-face-attr-construct (face frame)
;;   (if (atom face)
;;       (custom-face-attributes-get face frame)
;;     (if (and (consp face) (eq (car face) 'quote))
;;      (custom-face-attributes-get (cadr face) frame)
;;       (custom-face-attributes-get (car face) frame))))


;; flymake with pyflakes:
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; (load-library "flymake-cursor")
;; (global-set-key "\M-n" 'flymake-goto-next-error)
;; (global-set-key "\M-p" 'flymake-goto-prev-error)

;; (defun my-flymake-show-help ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;     (let ((help (get-char-property (point) 'help-echo)))
;;       (if help (message "%s" help)))))
;; (global-set-key [67108960] 'my-flymake-show-help)



(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))

;; enable rudel
;; (load-file "rudel-0.2-4/rudel-loaddefs.el")


;; Rudel stuff starts here
;; (add-to-list 'load-path "~/.emacs.d/cedet-1.0pre6")
;; (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4")
;; (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4/jupiter/")
;; (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4/obby/")
;; (load-library "cedet")
;; (load-library "rudel-loaddefs")
;; Rudel stuff ends here


; enable ipython
;; (setq python-python-command "ipython")
;; (setq python-python-command-args '("-cl" "-colors" "Linux"))
;; (setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")

;; (when (locate-library "ipython")
;;   (require 'ipython)
;;   (setq py-python-command-args '("-colors" "Linux")))


;; -----------------------------------------------------------------------------
;; Finally, set color-theme stuff


;; (load-library "cl-djcb-dark")
;; (load-library "cl-tango")
;; (require 'color-theme-less)
;; (require 'color- theme-subdued)
;; (require 'color-theme-gruber-darker)
;; (require 'color-theme)

(require 'color-theme-autoloads "color-theme-autoloads")
(require 'color-theme-monokai-autoloads "color-theme-monokai-autoloads")

;; (color-theme-initialize)
;; (require 'color-theme-less)
(require 'color-theme-monokai)

(if (window-system)
    (progn
      ;; (set-face-attribute 'default nil :font "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" :height 120)
      (color-theme-monokai)))

(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (if (window-system)
                   (progn
                     ;; (set-face-attribute 'default nil :font "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" :height 120)
                     (color-theme-monokai))))))

;; -----------------------------------------------------------------------------


;; Custom functions/hooks for persisting/loading frame geometry upon save/load
(defun save-frameg ()
  "Gets the current frame's geometry and saves to ~/.emacs.frameg."
  (let ((frameg-font (frame-parameter (selected-frame) 'font))
        (frameg-left (frame-parameter (selected-frame) 'left))
        (frameg-top (frame-parameter (selected-frame) 'top))
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height))
        (frameg-file (expand-file-name "~/.emacs.frameg")))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";;; This file stores the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '((font . \"" frameg-font "\")\n"
       (format "        (top . %d)\n" (max frameg-top 0))
       (format "        (left . %d)\n" (max frameg-left 0))
       (format "        (width . %d)\n" (max frameg-width 0))
       (format "        (height . %d)))\n" (max frameg-height 0)))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

(defun load-frameg ()
  "Loads ~/.emacs.frameg which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name "~/.emacs.frameg")))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-frameg)
      (add-hook 'kill-emacs-hook 'save-frameg)))


; Marmalade package archive
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
