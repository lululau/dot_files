local hovered = ya.sync(function()
    local tab, paths = cx.active, {}
    paths[1] = tostring(tab.current.hovered.url)
    return paths
end)

return {
	entry = function()
		local urls = hovered()
    local editor = os.getenv("CALLING_EDITOR") or "cursor"
    Command(editor):arg(urls):spawn():wait()
    ya.emit("quit", {})
	end,
}
