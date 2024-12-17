return {
  entry = function(self, job)
    local dir = job.args[1]
    ya.manager_emit("cd", {dir})
  end
}
