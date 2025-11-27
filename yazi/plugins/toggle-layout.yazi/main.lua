--- @since 25.5.31
--- @sync entry

-- 辅助函数：根据布局区域判断是否应该使用隐藏模式
local function should_use_hidden_mode(area)
	-- 如果没有区域信息，默认使用完整模式
	if not area or not area.w then
		return false
	end

	-- 基于布局区域的宽度进行判断
	return area.w < 80
end

-- 辅助函数：应用布局
local function apply_layout(is_hidden_mode)
	-- 根据状态设置布局比例
	local parent_ratio, current_ratio, preview_ratio
	if is_hidden_mode then
		-- 隐藏模式：0,1,0 - 只显示当前面板
		parent_ratio, current_ratio, preview_ratio = 0, 1, 0
	else
		-- 完整模式：1,4,3 - 标准三栏布局
		parent_ratio, current_ratio, preview_ratio = 1, 4, 3
	end

	-- 保存原始布局函数
	if not _G.original_layout then
		_G.original_layout = Tab.layout
	end

	-- 实现新布局
	Tab.layout = function(self)
		-- 根据当前区域智能判断初始状态（仅在首次调用时）
		if not _G.layout_initialized and self._area then
			_G.toggle_layout_state = should_use_hidden_mode(self._area)
			_G.layout_initialized = true

			-- 重新计算布局比例
			if _G.toggle_layout_state then
				parent_ratio, current_ratio, preview_ratio = 0, 1, 0
			else
				parent_ratio, current_ratio, preview_ratio = 1, 4, 3
			end
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
		apply_layout(is_hidden_mode)
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
