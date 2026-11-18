;; -*- lexical-binding: t; -*-

(with-eval-after-load 'tramp-rpc
  ;; ls-lisp (which tramp-rpc delegates listings to) mangles GNU-style
  ;; switches: "--quoting-style=..." wipes the rest of the string and
  ;; "--sort=none" turns into -S.  Normalize them before parsing, so
  ;; switches edited later via `dired-sort-toggle-or-edit' work too.
  (advice-add 'tramp-rpc-handle-insert-directory :around
              (lambda (orig filename switches &optional wildcard full-directory-p)
                (funcall orig
                         filename
                         (lx/dired-rpc--ls-lisp-switches (or switches ""))
                         wildcard full-directory-p))))

(with-eval-after-load 'tramp-rpc-deploy
  ;; Git 安装模式下指向 quelpa 源码目录并启用源码编译模式，
  ;; 确保使用匹配最新 commit 的服务端二进制（避免协议版本 protocol revision 不兼容）
  (setq tramp-rpc-deploy-source-directory
        (expand-file-name "~/.emacs.spacemacs.d/.cache/quelpa/build/tramp-rpc")
        tramp-rpc-deploy-git-build-policy 'build)

  ;; 修复 tramp-rpc-deploy--download-file 的 HTTP 报头与 Body 分隔符解析缺陷
  (defun tramp-rpc-deploy--download-file (url dest)
    "Download URL to DEST synchronously.
Returns t on success, nil on failure."
    (condition-case err
        (let ((url-request-method "GET")
              (url-show-status nil))
          (message "Downloading %s..." url)
          (with-timeout (tramp-rpc-deploy-download-timeout
                         (signal 'remote-file-error
                                 (list (format "Download timed out after %d seconds"
                                               tramp-rpc-deploy-download-timeout))))
            (let ((buffer (url-retrieve-synchronously url t t)))
              (unless buffer
                (signal 'remote-file-error (list "No HTTP response" url)))
              (unwind-protect
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (unless (looking-at "HTTP/[0-9.]+ 200\\(?:[ \t]\\|$\\)")
                      (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                          (signal 'remote-file-error (list "HTTP error" (match-string 1)))
                        (signal 'remote-file-error (list "Invalid HTTP response"))))
                    (if (re-search-forward "\r?\n\r?\n" nil t)
                        (point)
                      (when (boundp 'url-http-end-of-headers)
                        (goto-char url-http-end-of-headers)
                        (while (memq (char-after) '(13 10))
                          (forward-char 1))))
                    (let ((coding-system-for-write 'binary))
                      (write-region (point) (point-max) dest nil 'silent))
                    t)
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))))))
      (error
       (message "Download failed: %s" (error-message-string err))
       nil)))

  ;; 修复 macOS (Darwin) 远端 scpx PTY MAX_CANON=1024 导致超长单行激活命令卡死的问题：
  ;; 原版将临时/目标路径重复拼接 9 次（>2000 字节），改用短变量与局部函数压缩至 ~550 字节内。
  (defun tramp-rpc-deploy--activation-command (temporary destination checksum)
    "Build a compact atomic remote activation transaction under PTY MAX_CANON."
    (let ((tmp (tramp-shell-quote-argument temporary))
          (dest (tramp-shell-quote-argument destination))
          (digest (tramp-shell-quote-argument checksum)))
      (format
       (concat
        "_t=%s _d=%s _c=%s && "
        "_r(){ test -f \"$1\" && ! test -L \"$1\"; } && "
        "_r \"$_t\" && chmod +x \"$_t\" && _r \"$_t\" && "
        "(test ! -e \"$_d\" && ! test -L \"$_d\" || _r \"$_d\") && "
        "a=$({ sha256sum \"$_t\" || shasum -a 256 \"$_t\"; } 2>/dev/null | cut -d' ' -f1) && "
        "test \"$a\" = \"$_c\" && mv -f \"$_t\" \"$_d\" && "
        "_r \"$_d\" && test -x \"$_d\"")
       tmp dest digest)))

  ;; 支持在 macOS (aarch64-darwin) 上为异构目标自动交叉编译：
  ;; - Linux 目标 (x86_64-linux / aarch64-linux) 使用 cargo-zigbuild
  ;; - macOS x86_64 目标 (x86_64-darwin) 直接使用 rustup 的 cargo build --target x86_64-apple-darwin
  ;; 均需优先使用 rustup 的 cargo/rustc（避免 Homebrew rust 缺少交叉编译 target）。
  (defun lx/tramp-rpc--cross-build-method (arch)
    "Return cross-compile subcommand (`zigbuild' or `build') for ARCH, or nil."
    (unless (string= arch (tramp-rpc-deploy--detect-local-arch))
      (cond
       ((and (member arch '("x86_64-linux" "aarch64-linux"))
             (executable-find "cargo-zigbuild")
             (executable-find "zig"))
        "zigbuild")
       ((and (string= arch "x86_64-darwin")
             (eq system-type 'darwin)
             (file-directory-p "/opt/homebrew/opt/rustup/bin"))
        "build"))))

  (advice-add 'tramp-rpc-deploy--can-build-for-arch-p :around
              (lambda (orig arch)
                (or (funcall orig arch)
                    (not (null (lx/tramp-rpc--cross-build-method arch))))))

  (advice-add 'tramp-rpc-deploy--build-binary :around
              (lambda (orig arch)
                (let ((subcmd (lx/tramp-rpc--cross-build-method arch)))
                  (if (not subcmd)
                      (funcall orig arch)
                    (unless (tramp-rpc-deploy--source-root)
                      (signal 'remote-file-error (list "Source directory not configured")))
                    (let* ((rustup-bin "/opt/homebrew/opt/rustup/bin")
                           (has-rustup (file-directory-p rustup-bin))
                           (exec-path (if has-rustup (cons rustup-bin exec-path) exec-path))
                           (process-environment
                            (if has-rustup
                                (cons (concat "PATH=" rustup-bin ":" (getenv "PATH"))
                                      process-environment)
                              process-environment))
                           (default-directory (tramp-rpc-deploy--source-root))
                           (source-hash (or (and tramp-rpc-deploy--plan
                                                 (tramp-rpc-deploy-plan-source-hash tramp-rpc-deploy--plan))
                                            (tramp-rpc-deploy--source-tree-hash)))
                           (target (tramp-rpc-deploy--arch-to-rust-target arch))
                           (cache-path (tramp-rpc-deploy--local-cache-path arch))
                           (build-output (expand-file-name
                                          (format "target/%s/release/%s"
                                                  target tramp-rpc-deploy-binary-name)
                                          default-directory))
                           (build-buffer (get-buffer-create "*tramp-rpc-build*")))
                      (unless (executable-find "cargo")
                        (signal 'remote-file-error (list "Rust toolchain (cargo) not found")))
                      (message "Cross-compiling tramp-rpc-server for %s via cargo %s..." arch subcmd)
                      (with-current-buffer build-buffer
                        (erase-buffer))
                      (let ((exit-code
                             (call-process "cargo" nil build-buffer nil
                                           subcmd "--release"
                                           "--target" target
                                           "--manifest-path"
                                           (expand-file-name "Cargo.toml" default-directory))))
                        (if (zerop exit-code)
                            (progn
                              (unless (equal source-hash (tramp-rpc-deploy--source-tree-hash))
                                (signal 'remote-file-error
                                        '("Server sources changed during build; retry deployment")))
                              (tramp-rpc-deploy--promote-cached-binary
                               build-output cache-path "source-build")
                              (message "Built tramp-rpc-server for %s via cargo %s" arch subcmd)
                              cache-path)
                          (with-current-buffer build-buffer
                            (signal
                             'remote-file-error
                             (list (format "Cross-build (%s) failed (exit %d):\n%s"
                                           subcmd exit-code (buffer-string)))))))))))))
