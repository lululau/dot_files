local action_state = require('telescope.actions.state')
local actions = require('telescope.actions')
local conf = require('telescope.config').values
local entry_display = require('telescope.pickers.entry_display')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')

local aj = {
}

function aj.open(path)
    vim.cmd('Dired ' .. path)
end

function aj.get_paths()
    local paths = {}
    local f = io.popen("AUTOJUMP_SOURCED=1 autojump -s | sed -n '/^_______/!p; /^_______/q' | tac")
    for line in f:lines() do
        local score, path = line:match("(%d+):%s+(.+)")
        table.insert(paths, {path=path, score=score})
    end
    f:close()
    return paths
end

local function get_all_autojump_paths()
  local p = {}
  local paths = aj.get_paths()

  for _, k in pairs(paths) do
    table.insert(p, k)
  end
  return p
end

local function show_changes(opts)
  opts = opts or {}
  local displayer = entry_display.create({
    separator = ' ',
    items = {
      { remaining = true },
    },
  })
  local function make_display(entry)
    return displayer({
      { entry.value.path, 'TelescopeResultsFunction' },
    })
  end
  pickers
    .new(opts, {
      prompt_title = 'Autojump Directories',
      finder = finders.new_table({
        results = get_all_autojump_paths(),
        entry_maker = function(entry)
          return {
            value = entry,
            display = make_display,
            ordinal = entry.path,
          }
        end,
      }),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function(prompt_bufnr)
        actions.select_default:replace(function()
          local entry = action_state.get_selected_entry()
          actions.close(prompt_bufnr)
          aj.open(entry.value.path)
        end)
        return true
      end,
    })
    :find()
end

local function run()
  show_changes()
end

return require('telescope').register_extension({
  exports = {
    autojump = run,
  },
})
