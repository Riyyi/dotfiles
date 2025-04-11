local F = require("core.functions")

-- Module for managing buffers, these are used for navigation

local M = {}

-- Buffers, categorized into groups
-- {
--     { group = "Project", buffers = { "buffer1", "buffer2" }, active_index = 1 },
--     { group = "Other",   buffers = { "buffer3", "buffer4" }, active_index = 1 },
-- }
M.buffers = {}

M.buffer_default_group = "Other"

M.buffer_matcher = {
	-- match = truthy, function() -> thruthy, or string matching buffer filename
	-- group = string, function() -> string
	{ match = F.find_project_root, group = F.find_project_root },
	{ match = true,                group = "Other" }, -- default
}

--------------------------------------------

local get_buffer_name = function()
	local buf = vim.api.nvim_get_current_buf()
	local buffer_name = vim.api.nvim_buf_get_name(buf)
	if not buffer_name or buffer_name == "" then return nil end
	return buffer_name
end

M.add_buffer = function()
	local buffer_name = get_buffer_name()
	if not buffer_name then return end

	-- Evaluate group
	local eval_group = function(config_group)
		if not config_group then return M.buffer_default_group end
		if type(config_group) == "function" then return config_group() end
		return config_group
	end

	-- Add if buffer does not exist yet
	local add = function(config_group)
		local group = eval_group(config_group)

		local index
		for i, buffer_group in ipairs(M.buffers) do
			if buffer_group.group == group then index = i end
		end

		-- Group is new entirely, so we only have 1 buffer
		if not index then
			table.insert(M.buffers, { group = group, buffers = { buffer_name }, active_index = 1 })
			return #M.buffers, 1 -- group index, buffer index
		end

		-- Buffer previously added, set active
		for i, buffer in ipairs(M.buffers[index].buffers) do
			if buffer == buffer_name then
				M.buffers[index].active_index = i
				return index, i -- group index, buffer index
			end
		end

		-- Buffer is new, add it to the list
		table.insert(M.buffers[index].buffers, buffer_name)
		local count = #M.buffers[index].buffers
		M.buffers[index].active_index = count

		return index, count -- group index, buffer index
	end

	-- Walk matcher configuration
	local matcher = M.buffer_matcher or {}
	if matcher and type(matcher) == "function" then
		matcher = matcher()
	end
	if #matcher == 0 then add(M.buffer_default_group) end
	for _, config in ipairs(M.buffer_matcher) do
		if type(config.match) == "function" and config.match() then
			return add(config.group)
		end

		-- Match path's filename
		if type(config.match) == "string" and config.match == buffer_name:match("([^/]+)$") then
			return add(config.group)
		end

		if type(config.match) == "boolean" and config.match then
			return add(config.group)
		end
	end

	return add(M.buffer_default_group)
end

local get_group_from_buffer = function(buffer_name)
	for i, buffer_group in ipairs(M.buffers) do
		for j, buffer in ipairs(buffer_group.buffers) do
			if buffer == buffer_name then
				return i, j -- group index, buffer index
			end
		end
	end

	return nil
end

local focus_buffer = function(new_buffer_name)
	for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
		if vim.api.nvim_buf_get_name(buffer) == new_buffer_name then
			vim.api.nvim_set_current_buf(buffer)
			break
		end
	end
end

M.remove_buffer = function(buffer_name)
	if not buffer_name or buffer_name == "" then return end

	local group_index, buffer_index = get_group_from_buffer(buffer_name)
	if not group_index or not buffer_index then return end

	local buffer_group = M.buffers[group_index]

	-- Remove the buffer and decrease active index
	table.remove(buffer_group.buffers, buffer_index)
	buffer_group.active_index = math.max(buffer_group.active_index - 1, 1)
	local new_buffer_name = buffer_group.buffers[buffer_group.active_index]

	-- If there are now no buffers in the group, delete the group
	if #buffer_group.buffers == 0 then
		table.remove(M.buffers, group_index)
		if #M.buffers == 0 then return end
		if #M.buffers < group_index then group_index = group_index - 1 end
		new_buffer_name = M.buffers[group_index].buffers[M.buffers[group_index].active_index]
	end

	-- Make the new index the active buffer
	focus_buffer(new_buffer_name)
end

local buffer_action_in_active_group = function(action, direction)
	local buffer_name = get_buffer_name()
	if not buffer_name then return end

	-- Get active group from the current buffer
	local group_index, buffer_index = get_group_from_buffer(buffer_name)
	if not group_index then group_index, buffer_index = M.add_buffer() end

	-- Get buffer set
	local buffers = M.buffers[group_index].buffers
    if #buffers < 2 then return end

	-- Determine the other index
	local other_index = buffer_index + direction
	if buffer_index == 1 and direction == -1 then
		other_index = #buffers
	elseif buffer_index == #buffers and direction == 1 then
		other_index = 1
	end

	if action == "move" then
		for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
			-- TODO: Probably not needed if the other autocommands are implemented
			-- Remove inactive buffers
			if not vim.api.nvim_buf_is_loaded(buffer) then
				for i, group_buffer in ipairs(buffers) do
					if buffer == group_buffer then
						table.remove(buffers, i)
						if i > 1 and i <= other_index then
							other_index = other_index - 1
						end
					end
				end

				goto continue
			end

			-- Make buffer active
			if vim.api.nvim_buf_get_name(buffer) == buffers[other_index] then
				vim.api.nvim_set_current_buf(buffer)
				M.buffers[group_index].active_index = other_index
				break
			end

			::continue::
		end
	elseif action == "swap" then
		local tmp = buffers[other_index]
		buffers[other_index] = buffers[buffer_index]
		buffers[buffer_index] = tmp
	end

	M.buffers[group_index].buffers = buffers
end

M.buffer_move_left = function()
	buffer_action_in_active_group("move", -1)
end

M.buffer_move_right = function()
	buffer_action_in_active_group("move", 1)
end

M.buffer_swap_left = function()
	buffer_action_in_active_group("swap", -1)
end

M.buffer_swap_right = function()
	buffer_action_in_active_group("swap", 1)
end

--------------------------------------------
-- Group move

local buffer_group_move = function(direction)
	local buffer_name = get_buffer_name()
	if not buffer_name then return end

	-- Get active group from the current buffer
	local group_index, _ = get_group_from_buffer(buffer_name)
	if not group_index then group_index, _ = M.add_buffer() end

	-- Check groups bounds
    if #M.buffers < 2 then return end

	-- Determine the other index
	local other_index = group_index + direction
	if group_index == 1 and direction == -1 then
		other_index = #M.buffers
	elseif group_index == #M.buffers and direction == 1 then
		other_index = 1
	end

	-- Get active index from other group
	local other_active_index = M.buffers[other_index].active_index

	-- Make buffer active
	-- TODO: What do if this buffer no longer exists?
	local other_buffer = M.buffers[other_index].buffers[other_active_index]
	focus_buffer(other_buffer)
end

M.buffer_group_move_up = function()
	buffer_group_move(-1)
end

M.buffer_group_move_down = function()
	buffer_group_move(1)
end

--------------------------------------------
-- Telescope functionss

local pick = function(title, items, entry_maker, action)
	local actions = require("telescope.actions")
	local action_state = require("telescope.actions.state")
	local conf = require("telescope.config").values
	local finders = require("telescope.finders")
	local pickers = require("telescope.pickers")

	pickers.new({}, {
		prompt_title = title,
		finder = finders.new_table {
			results = items,
			entry_maker = entry_maker,
		},
		previewer = false,
		sorter = conf.generic_sorter({}),
		attach_mappings = function(prompt_bufnr)
			actions.select_default:replace(function()
				actions.close(prompt_bufnr)

				local selection = action_state.get_selected_entry()
				action(selection)
			end)

			return true
		end,
	}):find()
end

M.buffer_pick_buffer = function()
	local buffer_name = get_buffer_name()
	if not buffer_name then return end

	-- Get active group from the current buffer
	local group_index, _ = get_group_from_buffer(buffer_name)
	if not group_index then group_index, _ = M.add_buffer() end

	-- Get all buffers from the active group
	local buffers = {}
	for i, buffer in ipairs(M.buffers[group_index].buffers) do
		table.insert(buffers, { key = buffer, value = tostring(i) })
	end

	pick("Group buffers",
		 buffers,
		 function(buffer)
			-- Only show last part of the path
			local display = buffer.key:match("^.+/(.+)$") or buffer.key
			return {
				display = display,       -- shown in the picker
				ordinal = buffer.value,  -- used for sorting/filtering
				value = buffer.value,    -- actual value
			}
		 end,
		 function(selection)
			local new_buffer_name = M.buffers[group_index].buffers[tonumber(selection.value)]

			-- Make new buffer active
			focus_buffer(new_buffer_name)
		 end
	)
end

M.buffer_pick_group = function()
	-- Get all groups
	local groups = {}
	for i, buffer_group in ipairs(M.buffers) do
		table.insert(groups, { key = buffer_group.group, value = tostring(i) })
	end

	pick("Groups",
		 groups,
		 function(group)
			-- Only show last part of the path
			local display = group.key:match("^.+/(.+)$") or group.key
			return {
				display = display,      -- shown in the picker
				ordinal = group.value,  -- used for sorting/filtering
				value = group.value,    -- actual value
			}
		 end,
		 function(selection)
			local buffer_group = M.buffers[tonumber(selection.value)]
			local new_buffer_name = buffer_group.buffers[buffer_group.active_index]

			-- Make new buffer active
			focus_buffer(new_buffer_name)
		 end
	)
end

return M
