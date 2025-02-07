;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     lsp-mode and dap-mode should be installed and loaded first.
;;; Code:

;; C/C++
;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook #'eglot-ensure)
;; (add-hook 'c++-mode-hook #'eglot-ensure)

(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++"))

(global-set-key (kbd "<f5>") 'quickrun)

(use-package treesit-auto
  :demand
  :init
  (progn
    (setq treesit-font-lock-level 4))
  :config
  (global-treesit-auto-mode)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (sh-mode . bash-ts-mode)
          (js-mode . js-ts-mode)
          (css-mode . css-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
	      (java-mode . java-ts-mode)
          (python-mode . python-ts-mode))))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  ;; :init
  ;; (yas-global-mode 1)
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(yas/hippie-try-expand
            try-complete-file-name-partially
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(add-hook 'prog-mode-hook 'flymake-mode) ;错误的提示
(add-hook 'prog-mode-hook 'hs-minor-mode) ;代码的折叠
(add-hook 'prog-mode-hook 'prettify-symbols-mode) ;会将lambda等符号美化为λ


(use-package magit
  :ensure t
  :defer 3)


(provide 'init-programming)
;;; init-programming.el ends here
