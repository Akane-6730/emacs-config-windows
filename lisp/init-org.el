;;; init-org.el -- Org mode setup -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Org mode with various packages and settings.
;; It includes setup for Org Babel, Org Roam, Org Bullets, Org Download,
;; and other useful Org-related tools.

;;; Code:

;; (setq org-directory (file-truename "~/org/"))
;; (setq pv/org-refile-file (concat org-directory "refile.org"))
;; (setq pv/org-agenda-files `(,(concat org-directory "Agenda/")))
;; (setq pv/org-bibtex-library `(,(concat org-directory "References/")))
;; (setq pv/org-bibtex-files `(,(concat org-directory "References/references.bib")))

(eval-after-load 'org
  '(progn

     (add-hook 'org-mode-hook #'visual-line-mode)


     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (java . t)
        (js . t)))
     (setq org-confirm-babel-evaluate nil)


     (require 'org-tempo)
     (dolist (pair '(("sh" . "src shell")
                     ("el" . "src emacs-lisp")
                     ("py" . "src python")
                     ("java" . "src java")))
       (add-to-list 'org-structure-template-alist pair))


     (add-hook 'org-mode-hook
               (lambda ()
                 (setq-local electric-pair-inhibit-predicate
                             `(lambda (c)
                                (if (char-equal c ?\<)
                                    t
                                  (funcall ,electric-pair-inhibit-predicate c))))))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-appear
  :ensure t
  :hook (org-mode)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autolinks t))

(use-package flyspell-correct
  :ensure t
  ;; :hook (org-mode . flyspell-correct-mode)
  )

(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell")
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (setq ispell-personal-dictionary "c:/msys64/mingw64/lib/aspell-0.60/en_GB"))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "D:/CS")) ;~/org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  ;; :vc (:fetcher "github" :repo "org-roam/org-roam-ui")
  :defer t
  :commands (org-roam-ui-open))

(use-package org-download
    :ensure t
    :hook ((dired-mode org-mode) . org-download-enable)
    :config
    (add-hook 'dired-mode-hook 'org-download-enable)
    (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")
    (defun org-download-annotate-default (link)
      "Annotate LINK with the time of download."
      (make-string 0 ?\s))

    (setq-default org-download-heading-lvl nil
                  org-download-image-dir "./img"
                  ;; org-download-screenshot-method "screencapture -i %s"
                  org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after org           ; 确保在 org-mode 之后加载
  :config
  (require 'ox-hugo)   ; 显式加载 ox-hugo
  (setq org-hugo-base-directory (expand-file-name "D:/quartz/content"))  ; 使用 expand-file-name 处理路径
  )

(provide 'init-org)

;;; init-org.el ends here
