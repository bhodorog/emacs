; Marmalade package archive
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Misc settings
(transient-mark-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq scroll-step 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode nil)
(setq inhibit-splash-screen -1)
(cua-selection-mode t)
(setq-default indent-tabs-mode nil)
(visual-line-mode t)
(scroll-bar-mode nil)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Customize desktop-mode
(setq desktop-save 'ask-if-exists)

; Shortening of often used commands
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'fgd 'find-grep-dired)
(defalias 'sh 'shell)

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'rof 'recentf-open-files)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'hr 'highlight-regexp)
(defalias 'ur 'unhighlight-regexp)

(defalias 'dtw 'delete-trailing-whitespace)
;; Alt+0 Alt+r - top of window
;; Alt+- Alt+r - bottom of window


;; Don't make backup files next to the source, put them in a single place
(setq backup-directory-alist
      (quote (("." . "~/.emacs-backups"))))

; Dired mode settings (ommit)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$|\\.pyc|\\.git"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

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

;; ; dinamically add to load-path all folders from bhg-emacs-lib-folder
(setq emacs-lib-folder "~/.emacs-bhg/emacs.d")
(add-to-list 'load-path emacs-lib-folder)
(progn (cd emacs-lib-folder) (normal-top-level-add-subdirs-to-load-path))


;; -----------------------------------------------------------------------------

;; (require 'color-theme-autoloads "color-theme-autoloads")
;; (require 'color-theme-monokai-autoloads "color-theme-monokai-autoloads")
;; (color-theme-initialize)
;; (require 'color-theme-less)

;; (require 'color-theme)
;; (require 'color-theme-monokai)
(require 'color-theme-solarized)
;;(load-theme 'solarized-[light|dark] t)

(if (window-system)
    (progn
      ;; (set-face-attribute 'default nil :font "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" :height 120)
      (color-theme-solarized-light)))

(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (if (window-system)
                   (progn
                     ;; (set-face-attribute 'default nil :font "-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" :height 120)
                     (color-theme-solarized-light))))))

;; -----------------------------------------------------------------------------
