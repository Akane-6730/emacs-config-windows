;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into several modules.

;;; Code:

;; Set load paths for custom Lisp files and lsp-bridge
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")

;; (require 'init-benchmarking) ;; Measure startup time
;; Define constants for spell-check support and macOS detection
(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
   (init-gc-cons-threshold (* 128 1024 1024)))
 (setq gc-cons-threshold init-gc-cons-threshold)
 (add-hook 'emacs-startup-hook
      (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Add custom theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Mark all themes as safe and load the Sonokai theme
(setq custom-safe-themes t)
(load-theme 'sonokai t)

;; Package initialization and archives setup
(require 'package)
;; (require 'init-packages)
(setq package-archives
	  '(("melpa"  . "https://melpa.org/packages/")
	    ("gnu"    . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(setq package-check-signature nil)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Benchmark startup time
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Load basic settings and custom keybindings
(require 'init-basic)
(require 'init-keybindings)

;; Set italic style for types, comments, and keywords in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-type-face nil :slant 'italic)
            (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
            (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))



;; record command history
(use-package amx
 :ensure t
 :init (amx-mode))

(use-package ace-window
 :ensure t
 :defer t
 :bind (("C-x o" . 'ace-window)))

(use-package mwim
 :ensure t
 :bind
 ("C-a" . mwim-beginning-of-code-or-line)
 ("C-e" . mwim-end-of-code-or-line))

;; Provide keybinding menus using hydra
(use-package hydra
  :ensure t
  :defer t)
(use-package use-package-hydra
  :ensure t
  :after hydra)

(use-package avy
 :ensure t
 :bind
 (("C-j C-SPC" . avy-goto-char-timer)))

;; Enable flyspell mode in text and code folding in programming modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package flycheck
 :ensure t
 :config
 (setq truncate-lines nil)
 :hook
 (prog-mode . flycheck-mode))

(use-package flymake
  :ensure t
  :hook
  (prog-mode . flycheck-mode))


;; Manage projects with projectile
(use-package projectile
 :ensure t
 :bind
 (("C-c p" . projectile-command-map))
 ;; Recommended keymap prefix on macOS
 ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
 :config
 (projectile-mode +1)
 (setq projectile-mode-line "Projectile")
 (setq projectile-track-known-projects-automatically nil))

;; Load UI, completion, programming, org, and AI configurations
(require 'init-ui)
(require 'init-completion)
(add-hook 'emacs-startup-hook (lambda () (require 'init-programming)))
;; (add-hook 'emacs-startup-hook (lambda () (require 'init-org)))
;; (require 'init-programming)
(require 'init-org)
(require 'init-ai)
;; (with-eval-after-load 'org
;;   (require 'init-org))



(use-package lsp-bridge
  :ensure nil
  :hook ((prog-mode . lsp-bridge-mode)
         (org-mode . lsp-bridge-mode)))




(use-package fanyi
  :ensure t
  :commands (fanyi-dwim
	         fanyi-dwim2
	         fanyi-translate)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     ;; fanyi-longman-provider)
                     ))
  :config
  (setq fanyi-sound-player-support-https t))

;; eshell enhance
(use-package capf-autosuggest
  :ensure t
  :hook ((eshell-mode comint-mod) . capf-autosuggest-mode))

;; Automatically update packages
(use-package auto-package-update
  :ensure t
  :hook (after-init . auto-package-update-maybe)
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

;; Enhance deletion with hungry-delete
(use-package hungry-delete
  :ensure t
  :bind ("<backspace>" . hungry-delete-backward))



(provide 'init)

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "82aac99da1148720c54fd0142f8c8fce98ea2c6aed58124f2031f01c3a40a936" default))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (java . t)
     (js . t)
     (C . t)))
 '(package-selected-packages
   '(use-package rainbow-delimiters pinyinlib consult hungry-delete use-package-chords org-checklist taxy-magit-section ox-hugo gptel auto-package-update capf-autosuggest benchmark-init aider org-download org-roam-ui fanyi vertico flyspell-correct orderless dap-mode lsp-ui flycheck org-appear nerd-icons-corfu corfu sideline-lsp dired-rainbow projectile-variable dashboard nerd-icons-ibuffer org-dashboard nerd-icons-completion treemacs-nerd-icons yasnippet-snippets yasnippet quickrun restart-emacs org-bullets nerd-icons-dired doom-modeline keycast treesit-auto which-key mwim fontaine redo+ use-package-hydra lsp-treemacs treemacs-projectile treemacs highlight-symbol magit marginalia))
 '(warning-suppress-log-types '((unlock-file))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background unspecified))))
 '(mode-line-active ((t nil))))
