return {
  entry = function(self, job)
    local dir = job.args[1]
    ya.emit("cd", {dir})
  end
}
