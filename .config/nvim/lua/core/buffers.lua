local F = require("core.functions")

-- Module for managing buffers, these are used for navigation

local M = {}

-- Buffer table structure:
-- {
--     "Group" = { "buffer1", "buffer2", active_index = 1 }
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
		if not M.buffers[group] then M.buffers[group] = {} end
		for i, buffer in ipairs(M.buffers[group]) do
			if buffer == buffer_name then
				M.buffers[group]["active_index"] = i
				return group, i
			end
		end

		table.insert(M.buffers[group], buffer_name)
		local count = #M.buffers[group]
		M.buffers[group]["active_index"] = count

		return group, count
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
	for group, buffers in pairs(M.buffers) do
		for i, buffer in ipairs(buffers) do
			if buffer == buffer_name then return group, i end
		end
	end
	return nil
end

local buffer_action_in_active_group = function(action, direction)
	local buffer_name = get_buffer_name()
	if not buffer_name then return end

	local group, index = get_group_from_buffer(buffer_name)
	if not group then group, index = M.add_buffer() end

	local buffers = M.buffers[group]
    if #buffers < 2 then return end

	-- Determine the other index
	local other_index = index + direction
	if index == 1 and direction == -1 then
		other_index = #buffers
	elseif index == #buffers and direction == 1 then
		other_index = 1
	end

	if action == "move" then
		for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
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
				buffers.active_index = other_index
				break
			end

			::continue::
		end
	elseif action == "swap" then
		local tmp = buffers[other_index]
		buffers[other_index] = buffers[index]
		buffers[index] = tmp
	end

	M.buffers[group] = buffers
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

	-- TODO: How to get groups deterministically?

	-- Get active group from the current buffer

	-- Check groups bounds

	-- Change active group
end

M.buffer_group_move_up = function()
	buffer_group_move(-1)
end

M.buffer_group_move_down = function()
	buffer_group_move(1)
end

-- TODO: functions that can
--   v Navigate buffer left
--   v Navigate buffer right
--   - Navigate group up (use active_index when swapping)
--   - Navigate group down
--   v Move buffer left
--   v Move buffer right
--   - Remove buffer from currently active group (decrease active_index)

return M


-- (cond
-- ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
-- ((or (memq major-mode '(magit-process-mode
-- 						magit-status-mode
-- 						magit-diff-mode
-- 						magit-log-mode
-- 						magit-file-mode
-- 						magit-blob-mode
-- 						magit-blame-mode))
-- 	(string= (buffer-name) "COMMIT_EDITMSG")) "Magit")
-- ((project-current) (dot/project-project-name))
-- ((memq major-mode '(org-mode
-- 					emacs-lisp-mode)) "Org Mode")
-- ((derived-mode-p 'dired-mode) "Dired")
-- ((derived-mode-p 'prog-mode
-- 				'text-mode) "Editing")
-- (t "Other")))))
