;;; init-completion.el --- Initialize completion configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file sets up various completion frameworks and enhancements,
;; including orderless, vertico, marginalia, consult, corfu, and related packages.
;;

;;; Code:

;; orderless: enhanced completion style for flexible matching
(use-package orderless
  :ensure t
  :custom
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; pinyinlib: support Pinyin input for completion
(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; vertico: vertical interactive completion
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  :hook
  (after-init . vertico-mode))

;; marginalia: add rich annotations to minibuffer completions
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; consult: enhanced search and navigation commands
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g m" . consult-mark)
         ("C-c h" . consult-history)
         ("C-c i" . consult-info))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; corfu: modern completion UI with popup suggestions
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-min-width 80)
  (corfu-max-width 100)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  :hook
  (after-init . global-corfu-mode)
  :config
  (global-corfu-mode)
  (add-hook 'shell-mode-hook (lambda () (corfu-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (corfu-mode -1))))

;; nerd-icons-corfu: add icons to corfu completions
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; dabbrev: dynamic abbreviation expansion (swap M-/ and C-M-/)
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; nerd-icons-completion: add icons to completion candidates
(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode)
  :hook (vertico-mode . nerd-icons-completion-mode))

;; which-key: display available keybindings in popup
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(provide 'init-completion)
;;; init-completion.el ends here
