local state = ya.sync(function() return tostring(cx.active.current.cwd) end)

local function fail(s, ...) ya.notify { title = "Fzf", content = string.format(s, ...), timeout = 5, level = "error" } end

local file_exists = function(name)
  local f = io.open(name, "r")
  if f ~= nil then io.close(f) return true else return false end
end

local function entry()
	local _permit = ya.hide()
	local cwd = state()

  local autojump_db_file = os.getenv("HOME") .. "/.local/share/autojump/autojump.txt"
  if file_exists(os.getenv("HOME") .. "/Library/autojump/autojump.txt") then
    autojump_db_file = os.getenv("HOME") .. "/Library/autojump/autojump.txt"
  end
  local command = "sort -n -r '" .. autojump_db_file .. "' | cut -f2 | sed 's/$/\\//' | fzf"
	local child, err = Command("bash")
                      :args({"-c", command})
                      :cwd(cwd)
                      :stdin(Command.INHERIT)
                      :stdout(Command.PIPED)
                      :stderr(Command.INHERIT)
                      :spawn()

	if not child then
		return fail("Spawn `fzf` failed with error code %s. Do you have it installed?", err)
	end

	local output, err = child:wait_with_output()
	if not output then
		return fail("Cannot read `fzf` output, error code %s", err)
	elseif not output.status:success() and output.status:code() ~= 130 then
		return fail("`fzf` exited with error code %s", output.status:code())
	end

	local target = output.stdout:gsub("\n$", "")
	if target ~= "" then
		ya.manager_emit(target:match("[/\\]$") and "cd" or "reveal", { target })
	end
end

return { entry = entry }
