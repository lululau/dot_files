;;; gpt-commit.el --- Commit messages with GPT in Emacs -*- lexical-binding: t; -*-

;; Author: Youngwook Kim <youngwook.kim@gmail.com>
;; URL: https://github.com/ywkim/gpt-commit
;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (magit "2.90") (request "0.3.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GPT-Commit is an Emacs package that automates the generation of
;; conventional commit messages.  By leveraging the power of GPT
;; (Generative Pre-trained Transformer) models, it suggests structured
;; commit messages following the conventional commit format.
;;
;; With GPT-Commit, you no longer need to spend time crafting commit
;; messages manually.  It analyzes the changes in your Git repository and
;; generates meaningful commit messages automatically, ensuring
;; consistent and descriptive commit logs.
;;
;; Features:
;; - Automatic generation of conventional commit messages
;; - Integration with Git and Magit for seamless workflow
;; - Easy configuration and customization
;;
;; GPT-Commit streamlines the commit process and promotes best practices
;; for commit message formatting.  By using consistent commit messages,
;; you can enhance project clarity, facilitate collaboration, and improve
;; the overall maintainability of your codebase.
;;
;; (require 'gpt-commit)
;; (setq gpt-commit-openai-key "YOUR_OPENAI_API_KEY")
;; (setq gpt-commit-model-name "gpt-3.5-turbo-16k")
;; (add-hook 'git-commit-setup-hook 'gpt-commit-message)


;;; Code:

(provide 'gpt-commit)

(require 'magit)
(require 'request)

(defvar gpt-commit-openai-key nil "API key for the OpenAI.")

(defcustom gpt-commit-model-name "gpt-3.5-turbo"
  "Model name to use for GPT chat completions."
  :type 'string
  :group 'gpt-commit)

(defcustom gpt-commit-api-url "https://api.openai.com/v1/chat/completions"
  "API endpoint for GPT chat completions."
  :type 'string
  :group 'gpt-commit)

(defcustom gpt-commit-max-token 4096
  "Maximum token length for GPT completions."
  :type 'integer
  :group 'gpt-commit)

(defcustom gpt-commit-system-prompt-en
  "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

You are a specialized Git commit message generator. The user provides the result of running `git diff --cached`. Your task is to create clear, structured, and informative commit messages that follow a specific format:

1. First line: A concise title (60-72 characters) that summarizes the change using imperative mood
2. Second line must be a blank line, with no spaces
3. Then from third line, a bulleted list of specific changes, each starting with a present-tense action verb

RULES:
- Title must be specific and descriptive
- Use imperative mood in title (e.g., \"Add\", \"Fix\", \"Update\", not \"Added\", \"Fixed\", \"Updated\")
- Keep the title under 72 characters
- Each bullet point should start with \"- \" followed by a present-tense action verb
- Bullet points should be concise but informative about what changed and why
- Keep total bullet points at most 3-5, for simple changes 1 bullet point
- Organize bullet points in order of importance
- Highlight important technical details that would be relevant to other developers
- Do not include unnecessary details or explanations that belong in documentation
- Focus on WHAT changed and WHY, not HOW

Avoid vague messages like \"Fix bug\" or \"Update code\" - be specific about what was fixed or updated."

  "System prompt for GPT chat completions in English."
:type 'string
:group 'gpt-commit)


(defcustom gpt-commit-system-prompt-zh
  "用户提供 `git diff --cached` 的运行结果。你需要生成一个符合规范的中文提交信息。回复中不要包含其他内容。以下是关于规范提交的说明。

你是一个专门的 Git 提交信息生成器。用户提供 `git diff --cached` 的运行结果。你的任务是创建清晰、结构化且信息丰富的中文提交信息，遵循以下格式：

1. 第一行：简明扼要的标题（60-72 字符），概括变更内容，使用祈使语气
2. 第二行必须是一个空行, 且不包含任何空格
3. 然后从第三行开始是具体变更的列表，每项以现在时的动作动词开头

规则：
- 标题必须具体且具有描述性
- 标题使用祈使语气（例如：\"添加\"、\"修复\"、\"更新\"，而不是\"添加了\"、\"修复了\"、\"更新了\"）
- 标题保持在 72 字符以内
- 每个要点应以 \"- \" 开头，后跟现在时的动作动词
- 要点应简洁但能说明改变了什么以及为什么改变
- 总要点数最多 3-5 个，简单变更只需 1 个要点
- 按重要性组织要点
- 突出对其他开发者相关的重要技术细节
- 不要包含不必要的细节或不属于文档的解释
- 专注于改变了什么（WHAT）和为什么（WHY），而不是如何（HOW）

避免使用模糊的消息，如\"修复错误\"或\"更新代码\"——要具体说明修复或更新了什么。"

  "System prompt for GPT chat completions in Chinese."
:type 'string
:group 'gpt-commit)

(defun gpt-commit-parse-response (data)
  "Parse the GPT response DATA."
  (let* ((choices (cdr (assoc 'choices data)))
         (choice (elt choices 0))
         (message (assoc 'message choice))
         (content (cdr (assoc 'content message))))
    (decode-coding-string content 'utf-8)))

(defun gpt-commit-openai-chat-completions-api (messages callback)
  "Call OpenAI's Chat Completions API with MESSAGES and CALLBACK."
  (let* ((headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " gpt-commit-openai-key))))
         (json-string (json-serialize `((model . ,gpt-commit-model-name)
                                        (messages . ,messages))))
         (payload (encode-coding-string json-string 'utf-8)))
    (request gpt-commit-api-url
             :type "POST"
             :headers headers
             :data payload
             :parser 'json-read
             :timeout 60
             :success
             (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (gpt-commit-parse-response data))))
             :error
             (cl-function
              (lambda (&rest args &key data error-thrown &allow-other-keys)
                (message "Error: %s %s" error-thrown data))))))

(defun gpt-commit--retrieve-staged-diff ()
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n"))
         (max-token gpt-commit-max-token)
         (max-char (- (* 3 max-token) (length gpt-commit-system-prompt-en)))
         (total (length changes)))
    (if (> total max-char)
        (substring changes 0 max-char)
    changes)))

(defun gpt-commit-generate-message (existing-prefix callback)
  "Generate a commit message using GPT and pass it to the CALLBACK.
EXISTING-PREFIX is text that should be used as a prefix for the generated message."
  (let* ((changes (gpt-commit--retrieve-staged-diff))
         (prompt (if (and existing-prefix (not (string-empty-p existing-prefix)))
                     (concat "Existing prefix: " existing-prefix "\n\n" gpt-commit-system-prompt-en "\n\nIMPORTANT: The generated message MUST start with the exact existing prefix provided above.")
                   gpt-commit-system-prompt-en))
         (messages `[((role . "system")
                      (content . ,prompt))
                     ((role . "user")
                      (content . ,changes))]))
    (gpt-commit-openai-chat-completions-api messages callback)))

(defun gpt-commit--extract-comment-lines ()
  "Extract all comment lines (starting with #) from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((comment-lines '()))
      (while (not (eobp))
        (when (looking-at "^#")
          (push (buffer-substring-no-properties (point) (line-end-position)) comment-lines))
        (forward-line 1))
      (nreverse comment-lines))))

(defun gpt-commit--clear-buffer ()
  "Clear entire buffer content."
  (delete-region (point-min) (point-max)))

(defun gpt-commit-message ()
  "Automatically generate a conventional commit message using GPT-Commit.

This function is a hook intended to be added to `git-commit-setup-hook'.
When called, it analyzes the changes in the Git repository and generates
a conventional commit message using the GPT model.

If there's already existing text in the commit buffer, that text will be
used as a prefix for the generated commit message.

The generated commit message follows the conventional commit format,
providing a structured description of the changes made in the commit.

To use this feature, make sure you have set the OpenAI API key and
GPT model name in the respective variables:
- `gpt-commit-openai-key'
- `gpt-commit-model-name'

Example usage:
  (require 'gpt-commit)
  (setq gpt-commit-openai-key \"YOUR_OPENAI_API_KEY\")
  (setq gpt-commit-model-name \"gpt-3.5-turbo-16k\")
  (add-hook 'git-commit-setup-hook 'gpt-commit-message)"

  (interactive)
  (let ((existing-prefix (git-commit-buffer-message))
        (buffer (current-buffer)))
    (when (bound-and-true-p copilot-mode)
      (copilot-mode -1))
    (message "Generating commit message with GPT-Commit...")
    (gpt-commit-generate-message
     existing-prefix
     (lambda (commit-message)
       (when commit-message
         (message "Commit message generation completed.")
         (with-current-buffer buffer
           (let ((comment-lines (gpt-commit--extract-comment-lines)))
             (gpt-commit--clear-buffer)
             (insert commit-message)
             (when comment-lines
               (insert "\n\n\n")
               (dolist (line comment-lines)
                 (insert line "\n")))
             (mwim-beginning-of-code-or-line))))))))

(defun gpt-commit-generate-message-zh (existing-prefix callback)
  "Generate a Chinese commit message using GPT and pass it to the CALLBACK.
EXISTING-PREFIX is text that should be used as a prefix for the generated message."
  (let* ((changes (gpt-commit--retrieve-staged-diff))
         (prompt (if (and existing-prefix (not (string-empty-p existing-prefix)))
                     (concat "已存在的前缀: " existing-prefix "\n\n" gpt-commit-system-prompt-zh "\n\n重要提示：生成的消息必须以上面提供的已存在前缀开头。")
                   gpt-commit-system-prompt-zh))
         (messages `[((role . "system")
                      (content . ,prompt))
                     ((role . "user")
                      (content . ,changes))]))
    (gpt-commit-openai-chat-completions-api messages callback)))

(defun gpt-commit-message-in-zh ()
  "Automatically generate a conventional commit message in Chinese using GPT-Commit.

This function is similar to `gpt-commit-message', but generates commit messages
in Chinese instead of English. It is intended to be added to `git-commit-setup-hook'.
When called, it analyzes the changes in the Git repository and generates
a conventional commit message using the GPT model.

If there's already existing text in the commit buffer, that text will be
used as a prefix for the generated commit message.

The generated commit message follows the conventional commit format,
providing a structured description of the changes made in the commit.

To use this feature, make sure you have set the OpenAI API key and
GPT model name in the respective variables:
- `gpt-commit-openai-key'
- `gpt-commit-model-name'

Example usage:
  (require 'gpt-commit)
  (setq gpt-commit-openai-key \"YOUR_OPENAI_API_KEY\")
  (setq gpt-commit-model-name \"gpt-3.5-turbo-16k\")
  (add-hook 'git-commit-setup-hook 'gpt-commit-message-in-zh)"

  (interactive)
  (let ((existing-prefix (git-commit-buffer-message))
        (buffer (current-buffer)))
    (when (bound-and-true-p copilot-mode)
      (copilot-mode -1))
    (message "正在使用 GPT-Commit 生成中文提交信息...")
    (gpt-commit-generate-message-zh
     existing-prefix
     (lambda (commit-message)
       (when commit-message
         (message "提交信息生成完成。")
         (with-current-buffer buffer
           (let ((comment-lines (gpt-commit--extract-comment-lines)))
             (gpt-commit--clear-buffer)
             (insert commit-message)
             (when comment-lines
               (insert "\n\n\n")
               (dolist (line comment-lines)
                 (insert line "\n")))
             (mwim-beginning-of-code-or-line))))))))

;;; gpt-commit.el ends here
