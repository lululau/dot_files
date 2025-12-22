(with-eval-after-load 'gptel
  (setq gptel-backend  (gptel-make-openai "Dashscope"
                         :host "dashscope.aliyuncs.com"
                         :endpoint "/compatible-mode/v1/chat/completions"
                         :stream t
                         :key 'gptel-api-key-from-auth-source
                         :models '(qwen-plus)))
  (gptel-make-openai "GLM"
    :host "open.bigmodel.cn"
    :endpoint "/api/coding/paas/v4/chat/completions"
    :stream t
    :key 'gptel-api-key-from-auth-source
    :models '(glm-4.6))
  )
