;;; init-keybindings.el --- Custom key bindings for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains custom key bindings for Emacs.
;; It includes common key bindings for text editing, navigation,
;; and custom functions for easier cursor movement.
;;

;;; Code:

;; General keybindings: Insert newline and auto-indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; General keybindings: Bind C-; to correct the previous spelling error
(global-set-key (kbd "C-;") 'flyspell-correct-previous)

;; General keybindings: Increase and decrease text scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Custom functions: Move the cursor down/up 10 lines
(defun next-ten-lines ()
  "Move the cursor down 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines ()
  "Move the cursor up 10 lines."
  (interactive)
  (previous-line 10))

;; Bind custom functions: M-n for moving down, M-p for moving up 10 lines
(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

;; Unbind C-j to avoid conflicts (especially in terminal mode)
(global-set-key (kbd "C-j") nil)

;; Quick open config: Open the main Emacs init file
(defun open-init-file ()
  "Open the main Emacs init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Bind F2 to open the init file
(global-set-key (kbd "<f2>") 'open-init-file)

;; Bind F7 to the `gptel-rewrite' function
(global-set-key (kbd "<f7>") 'gptel-rewrite)

;; Bind C-x C-b to ibuffer for enhanced buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
