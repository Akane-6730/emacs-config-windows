
;; AI assistance
(use-package gptel
  :ensure t
  :defer t
  :config
  (setq gptel-model   'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"     ;Any name you want
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key ""
        :models '(deepseek-chat deepseek-coder))))

;; (use-package aider
;;   :config
;;   (setq aider-args '("--model" "deepseek")))

(provide 'init-ai)
