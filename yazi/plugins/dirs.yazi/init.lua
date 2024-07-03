return {
  entry = function(self, args)
    local dir = args[1]
    ya.manager_emit("cd", {dir})
  end
}
