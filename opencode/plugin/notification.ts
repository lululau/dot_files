import type { Plugin } from "@opencode-ai/plugin"

export const NotificationPlugin: Plugin = async ({ project, client, $, directory, worktree }) => {
  return {
    event: async ({ event }) => {
      const projectName = project?.name || directory || "OpenCode"

      if (event.type === "session.idle") {
          await $`terminal-notifier -title "OpenCode" -message "Task completed: ${projectName}" -sender ai.opencode.desktop && afplay /System/Library/Sounds/Bottle.aiff`
      }

      if (event.type === "session.error") {
        const errorMsg = event.error?.message || "An error occurred"
          await $`terminal-notifier -title "OpenCode Error" -message "${errorMsg}" -sender ai.opencode.desktop && afplay /System/Library/Sounds/Sosumi.aiff`
      }
    }
  }
}
