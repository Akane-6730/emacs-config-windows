;;; init-basic.el --- Basic Emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains essential basic settings for Emacs.
;; It includes settings for auto-pairing, line numbers, backups,
;; UI customization, coding system preferences, and font settings.

;;; Code:

;; General settings:
;; disable startup message, maximize frame, and disable lockfiles, backups, and auto-save.
(setq inhibit-startup-message t)
(toggle-frame-maximized)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)  ; use spaces instead of tabs
(setq-default tab-width 4)

;; Mode and hook settings: enable electric pairing, matching paren highlighting, column numbers,
;; auto-reverting, delete selection, code folding, global line numbers, and UI tweaks.
(electric-pair-mode t)
(add-hook 'prog-mode-hook #'show-paren-mode)
(column-number-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-display-line-numbers-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1))
(savehist-mode 1)
(global-hl-line-mode 1)
(setq-default cursor-type 'bar)

;; --- Encoding Settings ---

;; use UTF-8 as default and set locale and charset priority.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq system-time-locale "C")
(set-charset-priority 'unicode)

;; --- Font Settings ---
;; set default font in graphical mode, configure Chinese fonts if available,
;; adjust line number font sizes, and set symbol (Emoji) font.
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Consolas-18")
  (dolist (font '("LXGW Neo Xihei" "LXGW WenKai" "LXGW WenKai Mono" "LXGW ZhenXi"))
    (when (find-font (font-spec :family font))
      (set-fontset-font t 'han (font-spec :family font))
      (add-to-list 'face-font-rescale-alist `(,font . 1.25))))
  (set-face-attribute 'line-number nil :height 1.0)
  (set-face-attribute 'line-number-current-line nil :height 1.0))
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)

;; Proxy settings: set proxy for HTTP and HTTPS
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7897")
        ("https" . "127.0.0.1:7897")))

;; Saveplace: remember file locations between sessions
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Restart Emacs: enable easy restart functionality
(use-package restart-emacs
  :ensure t)

;; Enable auto-save with custom settings
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p "gpg" (file-name-extension (buffer-name)) t))))


(provide 'init-basic)
;;; init-basic.el ends here
