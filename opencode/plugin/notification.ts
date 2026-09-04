import type { Plugin } from "@opencode-ai/plugin"

export const NotificationPlugin: Plugin = async ({ project, client, $, directory, worktree }) => {
  const home = process.env.HOME ?? ""
  const dir = new URL(".", import.meta.url).pathname
  const icon = process.env.INSIDE_EMACS
    ? dir + "emacs-opencode.png"
    : dir + "opencode.png"
  return {
    event: async ({ event }) => {
      const raw = project?.name || directory || "OpenCode"
      const projectName = home && raw.startsWith(home) ? "~" + raw.slice(home.length) : raw

      if (event.type === "session.idle") {
          await $`terminal-notifier -title "OpenCode" -message ${"Task completed: " + projectName} -contentImage ${icon} -group opencode`.quiet().nothrow()
      }

      if (event.type === "session.error") {
        const errorMsg = event.error?.message || "An error occurred"
          await $`terminal-notifier -title "OpenCode Error" -message ${errorMsg} -contentImage ${icon} -group opencode-error`.quiet().nothrow()
      }
    }
  }
}
