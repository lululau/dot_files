-- require("full-linemode")
require("dirs")
require("starship"):setup()

function Linemode:full()
	local year = os.date("%Y")
	local time = (self._file.cha.modified or 0) // 1

	if time > 0 and os.date("%Y", time) == year then
		time = os.date("%b %d %H:%M", time)
	else
		time = time and os.date("%b %d  %Y", time) or ""
	end

  local perms = self._file.cha:permissions() or ""
	local size = self._file:size()
	return ui.Line(string.format("%s %s %s ", perms, size and ya.readable_size(size) or "-", time))
end
