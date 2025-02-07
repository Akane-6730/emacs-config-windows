;;; init-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; code:

;; nerd-icons: setup icon fonts
(use-package nerd-icons
  :ensure t
  :defer t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; nerd-icons-ibuffer: add icons to ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-icon-size 1.0)
  (setq nerd-icons-ibuffer-human-readable-size t)
  nerd-icons-ibuffer-formats
  (setq inhibit-compacting-font-caches t))

;; nerd-icons-dired: add icons to dired mode
(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

;; rainbow-delimiters: colorize nested parentheses in programming modes
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; dashboard: configure the Emacs startup dashboard
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 7)
                            (projects . 5)))
    (setq dashboard-set-file-icons t))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "泣くのは弱いからじやない 耐えられるのは強いからじやない")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner "")
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons))

;; Use doom-modeline for a modern mode-line with time display
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (display-time-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
