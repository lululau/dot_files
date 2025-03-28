local hovered = ya.sync(function()
    local tab, paths = cx.active, {}
    paths[1] = tostring(tab.current.hovered.url)
    return paths
end)

return {
	entry = function()
		local urls = hovered()
		Command("cursor"):args(urls):spawn():wait()
    ya.mgr_emit("quit", {})
	end,
}
