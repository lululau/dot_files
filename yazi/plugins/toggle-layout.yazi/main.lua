--- @since 25.5.31
--- @sync entry

-- 布局配置常量
local LAYOUTS = {
	hidden = { parent = 0, current = 1, preview = 0 },    -- 0,1,0 - 只显示当前面板
	compact = { parent = 1, current = 1, preview = 0 },   -- 1,1,0 - 父目录+当前，无预览
	full = { parent = 1, current = 4, preview = 3 }       -- 1,4,3 - 标准三栏布局
}

-- 辅助函数：根据布局区域宽度获取初始布局类型
local function get_initial_layout_type(area)
	-- 如果没有区域信息，默认使用完整模式
	if not area or not area.w then
		return "full"
	end

	-- 基于布局区域的宽度进行判断
	local width = area.w
	if width <= 80 then
		return "hidden"
	elseif width <= 120 then
		return "compact"
	else
		return "full"
	end
end

-- 辅助函数：应用布局
local function apply_layout(layout_type_or_state)
	-- layout_type_or_state 可以是：
	-- - 字符串布局类型 ("hidden", "compact", "full") 用于初始化
	-- - 布尔值切换状态 (true/false) 用于手动切换

	local parent_ratio, current_ratio, preview_ratio
	local layout_type

	if type(layout_type_or_state) == "string" then
		-- 初始化模式：直接使用指定的布局类型
		layout_type = layout_type_or_state
		parent_ratio, current_ratio, preview_ratio =
			LAYOUTS[layout_type].parent,
			LAYOUTS[layout_type].current,
			LAYOUTS[layout_type].preview
	else
		-- 手动切换模式：只在隐藏和完整模式之间切换
		local is_hidden_mode = layout_type_or_state
		if is_hidden_mode then
			layout_type = "hidden"
			parent_ratio, current_ratio, preview_ratio = 0, 1, 0
		else
			layout_type = "full"
			parent_ratio, current_ratio, preview_ratio = 1, 4, 3
		end
	end

	-- 保存原始布局函数
	if not _G.original_layout then
		_G.original_layout = Tab.layout
	end

	-- 实现新布局
	Tab.layout = function(self)
		-- 根据当前区域智能判断初始布局（仅在首次调用时）
		if not _G.layout_initialized and self._area then
			_G.initial_layout_type = get_initial_layout_type(self._area)

			-- 设置切换状态：如果初始是隐藏模式则为true，否则为false
			_G.toggle_layout_state = (_G.initial_layout_type == "hidden")
			_G.layout_initialized = true

			-- 应用初始布局
			local layout = LAYOUTS[_G.initial_layout_type]
			parent_ratio, current_ratio, preview_ratio =
				layout.parent, layout.current, layout.preview
		end

		local total = parent_ratio + current_ratio + preview_ratio
		self._chunks = ui.Layout()
			:direction(ui.Layout.HORIZONTAL)
			:constraints({
          ui.Constraint.Ratio(parent_ratio, total),
          ui.Constraint.Ratio(current_ratio, total),
          ui.Constraint.Ratio(preview_ratio, total),
                  })
			:split(self._area)
	end

	-- 触发布局更新
	ya.emit("app:resize", {})
end

-- 插件主体
return {
	-- 初始化：设置标记，等待布局时进行智能判断
	setup = function()
		-- 重置初始化标记，确保下次布局时重新判断
		_G.layout_initialized = false
		-- 调用 apply_layout 来触发布局初始化，传入 false 作为默认值
		-- Tab.layout 函数会根据实际宽度重新设置正确的布局
		apply_layout(false)
	end,

	-- 手动切换布局
	entry = function()
		-- 确保布局已初始化
		_G.layout_initialized = true

		-- 获取当前状态
		local is_hidden_mode = _G.toggle_layout_state or false

		-- 切换状态
		is_hidden_mode = not is_hidden_mode
		_G.toggle_layout_state = is_hidden_mode

		-- 应用新布局
		apply_layout(is_hidden_mode)
	end
}
