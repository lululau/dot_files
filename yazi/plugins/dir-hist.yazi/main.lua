--- @since 25.5.28
local M = {}

-- 获取当前目录路径
local get_current_dir_path = ya.sync(function()
  local path = tostring(cx.active.current.cwd)
  if ya.target_family() == "windows" and path:match("^[A-Za-z]:$") then
    return path .. "\\"
  end
  return path
end)

-- 获取状态属性
local get_state_attr = ya.sync(function(state, attr)
  return state[attr]
end)

-- 设置状态属性
local set_state_attr = ya.sync(function(state, attr, value)
  state[attr] = value
end)

-- 获取目录历史
local get_directory_history = ya.sync(function(state)
  return state.directory_history
end)

-- 添加路径到历史记录
local add_to_history = ya.sync(function(state, tab_idx, path)

  if not state.directory_history[tab_idx] then
    state.directory_history[tab_idx] = {}
  end

  local history = state.directory_history[tab_idx]
  local history_size = state.history_size or 50

  -- 移除重复的路径
  for i = #history, 1, -1 do
    if history[i] == path then
      table.remove(history, i)
    end
  end

  -- 添加到开头
  table.insert(history, 1, path)

  -- 限制历史记录大小
  while #history > history_size do
    table.remove(history, #history)
  end
end)

-- 获取标签历史
local get_tab_history = ya.sync(function(state, tab_idx)
  return state.directory_history[tab_idx] or {}
end)

-- 获取当前标签索引
local get_current_tab_idx = ya.sync(function(state)
  return cx.tabs.idx
end)

-- 切换到上一个目录
local switch_to_previous = ya.sync(function(state)
  local tab_idx = get_current_tab_idx()
  local history = get_tab_history(tab_idx)

  if #history == 0 then
    ya.notify { title = "Directory History", content = "No previous directory", timeout = 2, level = "warn" }
    return
  end

  local previous_path = history[1]
  
  -- 标记这是插件切换，避免被 sub cd 事件记录
  state.plugin_switching = true
  state.switching_to_path = previous_path
  
  -- 从历史记录中移除这个路径
  table.remove(history, 1)
  
  ya.emit("cd", { previous_path })
  ya.notify { title = "Directory History", content = 'Switched to: ' .. previous_path, timeout = 1, level = "info" }
end)

-- 在当前目录和上一个目录之间切换
local toggle_between_current_and_previous = ya.sync(function(state)
  local tab_idx = get_current_tab_idx()
  local history = get_tab_history(tab_idx)
  local current_path = get_current_dir_path()

  if #history == 0 then
    ya.notify { title = "Directory History", content = "No previous directory", timeout = 2, level = "warn" }
    return
  end

  local previous_path = history[1]

  -- 如果当前路径和上一个路径不同，则切换到上一个路径
  if current_path ~= previous_path then
    -- 保存当前路径到历史记录
    add_to_history(tab_idx, current_path)
    
    -- 标记这是插件切换，避免被 sub cd 事件记录
    state.plugin_switching = true
    state.switching_to_path = previous_path
    
    -- 切换到上一个路径
    ya.emit("cd", { previous_path })
    ya.notify { title = "Directory History", content = 'Switched to: ' .. previous_path, timeout = 1, level = "info" }
  else
    -- 如果已经在历史路径上，则切换回当前路径
    if #history > 1 then
      local original_path = history[2]
      
      -- 标记这是插件切换，避免被 sub cd 事件记录
      state.plugin_switching = true
      state.switching_to_path = original_path
      
      ya.emit("cd", { original_path })
      ya.notify { title = "Directory History", content = 'Switched back to: ' .. original_path, timeout = 1, level = "info" }
    else
      ya.notify { title = "Directory History", content = "No other directory to switch to", timeout = 2, level = "warn" }
    end
  end
end)

return {
  setup = function(state, opts)
    -- 设置默认配置
    state.history_size = opts.history_size or 50

    -- 初始化状态
    state.directory_history = {}
    state.last_paths = {}
    state.initialized_tabs = {}

    -- 监听目录变化事件
    ps.sub("cd", function(body)
      local tab = body.tab or cx.tabs.idx
      local new_path = get_current_dir_path()

      -- 初始化标签
      if not state.initialized_tabs[tab] then
        state.last_paths[tab] = new_path
        state.initialized_tabs[tab] = true
        return
      end

      local previous_path = state.last_paths[tab]

      -- 检查是否是插件切换
      if state.plugin_switching and state.switching_to_path == new_path then
        -- 这是插件切换，不添加到历史记录，清除标记
        state.plugin_switching = false
        state.switching_to_path = nil
        state.last_paths[tab] = new_path
        return
      end

      -- 如果路径发生变化，添加到历史记录
      if previous_path and previous_path ~= new_path then
        add_to_history(tab, previous_path)
      end

      state.last_paths[tab] = new_path
    end)
  end,

  entry = function(self, job)
    local action = job.args[1]
    if not action then return end

    if action == "toggle" then
      -- Alt-减号：在当前目录和上一个目录之间切换
      toggle_between_current_and_previous()
    elseif action == "previous" then
      -- Alt-p：切换到上一个目录
      switch_to_previous()
    elseif action == "list" then
      -- 列出目录历史
      local tab_idx = get_current_tab_idx()
      local history = get_tab_history(tab_idx)

      if #history == 0 then
        ya.notify { title = "Directory History", content = "No directory history", timeout = 2, level = "info" }
        return
      end

      local content = "Directory History:\n"
      for i, path in ipairs(history) do
        content = content .. i .. ". " .. path .. "\n"
      end

      ya.notify { title = "Directory History", content = content, timeout = 5, level = "info" }
    end
  end,
}
