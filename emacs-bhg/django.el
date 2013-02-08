;; ; dinamically add to load-path all folders from emacs-lib-folder
(add-to-list 'load-path "~/.emacs.d/django-mode")

(require 'django-html-mode)
;; (require 'django-mode)
;; (yas/load-directory "path-to/django-mode/snippets")
(add-to-list 'auto-mode-alist '("\\.snippet$" . django-html-mode))
