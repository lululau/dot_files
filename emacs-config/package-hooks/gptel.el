;; -*- lexical-binding: t; -*-

;; This file is loaded from `dotspacemacs/user-init', i.e. before packages are
;; installed/activated. Registering `(with-eval-after-load 'gptel ...)' directly
;; here would fire during the package phase -- e.g. on a gptel reinstall, when
;; byte-compiling/activating the freshly downloaded gptel loads the feature --
;; before `gptel-agent' (an elpa package) and `buffer-tools-for-gptel' (a craft)
;; are on `load-path'. That aborts gptel activation with:
;;   "Cannot open load file ... gptel-agent".
;; It also silently breaks the config in normal sessions when gptel happens to
;; load before those deps join `load-path' (the block stops at the first
;; `require', leaving gptel-backend/presets undefined).
;;
;; Defer registration until Spacemacs has finished `dotspacemacs/user-config'
;; (crafts + elpa packages are on `load-path' by then). If gptel is already
;; loaded at that point, `with-eval-after-load' runs the body immediately.
(defun lx/gptel-user-config ()
  (with-eval-after-load 'gptel

    (gptel-make-openai "Dashscope"
                       :host "dashscope.aliyuncs.com"
                       :endpoint "/compatible-mode/v1/chat/completions"
                       :stream t
                       :key 'gptel-api-key-from-auth-source
                       :models '(qwen-plus))

    (require 'gptel-agent)
    (require 'buffer-tools-for-gptel)

    (setq gptel-backend (gptel-make-openai "GLM-4.7"
      :host "open.bigmodel.cn"
      :endpoint "/api/coding/paas/v4/chat/completions"
      :stream t
      :key 'gptel-api-key-from-auth-source
      :models '(glm-4.7)))

    (gptel-make-openai "GLM-4.5-Air"
      :host "open.bigmodel.cn"
      :endpoint "/api/coding/paas/v4/chat/completions"
      :stream t
      :key 'gptel-api-key-from-auth-source
      :models '(glm-4.5-air))

    (mcp-hub-start 'gptel-mcp-register-tool)


    (gptel-make-preset 'qwen-with-agent-tools
      :description nil :backend "Dashscope" :model 'qwen-plus :system 'default :tools
      '("Bash" "Eval" "WebSearch" "WebFetch" "YouTube" "Diagnostics" "Mkdir" "Edit" "Insert" "Write" "Glob" "Read" "Grep"
        "TodoWrite" "Agent" "ViewBuffer" "EditBuffer" "ReplaceBuffer" "BufferSearch" "ListBuffers")
      :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media nil :include-reasoning t)


    (gptel-make-preset 'qwen-with-mcp-tools
      :description nil :backend "Dashscope" :model 'qwen-plus :system 'default :tools
      '("resolve-library-id" "get-library-docs" "complex_search" "append_content" "patch_content" "simple_search" "get_file_contents" "list_files_in_vault"
        "list_files_in_dir" "maps" "calendar" "webSearch" "reminders" "mail" "messages" "notes" "contacts" "Bash" "Eval"
        "WebSearch" "WebFetch" "YouTube" "Diagnostics" "Mkdir" "Edit" "Insert" "Write" "Glob" "Read" "Grep" "TodoWrite"
        "Agent" "ViewBuffer" "EditBuffer" "ReplaceBuffer" "BufferSearch" "ListBuffers")
      :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media nil :include-reasoning t)


    (gptel-make-preset 'glm-with-agent-tools
      :description nil :backend "GLM-4.7" :model 'glm-4.7 :system 'default :tools
      '("Bash" "Eval" "WebSearch" "WebFetch" "YouTube" "Diagnostics" "Mkdir" "Edit" "Insert" "Write" "Glob" "Read" "Grep"
        "TodoWrite" "Agent" "ViewBuffer" "EditBuffer" "ReplaceBuffer" "BufferSearch" "ListBuffers")
      :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media nil :include-reasoning t)


    (gptel-make-preset 'glm-with-mcp-tools
      :description nil :backend "GLM-4.7" :model 'glm-4.7 :system 'default :tools
      '("resolve-library-id" "get-library-docs" "complex_search" "append_content" "patch_content" "simple_search" "get_file_contents" "list_files_in_vault"
        "list_files_in_dir" "maps" "calendar" "webSearch" "reminders" "mail" "messages" "notes" "contacts" "Bash" "Eval"
        "WebSearch" "WebFetch" "YouTube" "Diagnostics" "Mkdir" "Edit" "Insert" "Write" "Glob" "Read" "Grep" "TodoWrite"
        "Agent" "ViewBuffer" "EditBuffer" "ReplaceBuffer" "BufferSearch" "ListBuffers")
      :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media nil :include-reasoning t)))

;; (add-hook 'spacemacs-post-user-config-hook #'lx/gptel-user-config)
