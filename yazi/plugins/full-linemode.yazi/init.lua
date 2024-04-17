function Folder:linemode(area, files)
	local mode = cx.active.conf.linemode
	if mode == "none" then
		return {}
	end

	local lines = {}
	for _, f in ipairs(files) do
		local spans = { ui.Span(" ") }
		if mode == "size" then
			local size = f:size()
			spans[#spans + 1] = ui.Span(size and ya.readable_size(size) or "")
		elseif mode == "mtime" then
			local time = f.cha.modified
			spans[#spans + 1] = ui.Span(time and os.date("%y-%m-%d %H:%M", time // 1) or "")
		elseif mode == "permissions" then
			spans[#spans + 1] = ui.Span(f.cha:permissions() or "")
		elseif mode == "all" or mode == "full" then
			local size = f:size()
			local time = f.cha.modified
			spans[#spans + 1] = ui.Span(f.cha:permissions() or "")
			spans[#spans + 1] = ui.Span("  ")
			spans[#spans + 1] = ui.Span(size and ya.readable_size(size) or "")
			spans[#spans + 1] = ui.Span("  ")
			spans[#spans + 1] = ui.Span(time and os.date("%y-%m-%d %H:%M", time // 1) or "")
		end

		spans[#spans + 1] = ui.Span(" ")
		lines[#lines + 1] = ui.Line(spans)
	end
	return ui.Paragraph(area, lines):align(ui.Paragraph.RIGHT)
end

return Folder
