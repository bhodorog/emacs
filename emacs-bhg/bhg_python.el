(require 'column-marker)

;; Python mode combined with outline minor mode:
(add-hook 'python-mode-hook
          (lambda ()
            (interactive) (column-marker-2 79)
            (setq show-trailing-whitespace t)
            (which-function-mode t)
            (outline-minor-mode t)
            (auto-fill-mode t)
            (interactive) (column-marker-1 80)
            (setq coding-system-for-write 'utf-8)
            ))

(add-to-list 'exec-path "/usr/local/bin")
;; flymake with pyflakes:
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
