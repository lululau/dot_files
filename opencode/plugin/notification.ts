import type { Plugin } from "@opencode-ai/plugin"

export const NotificationPlugin: Plugin = async ({ project, client, $, directory, worktree }) => {
  const icon = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRVpnDpI_ibz--hgmbaxDohIIilrmfHAy_Hx53vlwS2qGOVkcz3Adj1xHo&s=10"
  return {
    event: async ({ event }) => {
      const home = process.env.HOME ?? ""
      const raw = project?.name || directory || "OpenCode"
      const projectName = home && raw.startsWith(home) ? "~" + raw.slice(home.length) : raw

      if (event.type === "session.idle") {
          await $`harkctl notify ${"Task completed: " + projectName} --title "OpenCode" --image ${icon}`.quiet().nothrow()
      }

      if (event.type === "session.error") {
        const errorMsg = event.error?.message || "An error occurred"
          await $`harkctl notify ${errorMsg} --title "OpenCode Error" --image ${icon}`.quiet().nothrow()
      }
    }
  }
}
