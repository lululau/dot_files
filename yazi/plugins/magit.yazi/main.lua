return {
    entry = function()
        local output = Command("git"):arg("status"):stderr(Command.PIPED):output()
        if output.stderr ~= "" then
            ya.notify({
                title = "Magit",
                content = "Not in a git directory\nError: " .. output.stderr,
                level = "warn",
                timeout = 5,
            })
        else
            permit = ui.hide()
            local status, err = Command("magit")
							:stdin(Command.INHERIT)
							:stdout(Command.INHERIT)
							:stderr(Command.INHERIT)
							:spawn():wait()
            if not status then
              ya.notify({
                  title = "Magit",
                  content = "Failed to run magit (code : " .. err .. ")",
                  level = "error",
                  timeout = 5,
              })
            else
              permit:drop()
              ya.manager_emit('escape', {visual = true, select = true})
            end
        end
    end,
}
