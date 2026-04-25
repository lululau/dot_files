return {
	entry = function()
		if os.getenv("BARE_YAZI_IN_TERM") then
			Command("/usr/bin/osascript")
				:arg("-e")
				:arg('tell application "System Events" to key code 41 using {command down, control down, option down}')
				:stdout(Command.NULL)
				:stderr(Command.NULL)
				:spawn():wait()
		else
			ya.emit("quit", {})
		end
	end,
}
