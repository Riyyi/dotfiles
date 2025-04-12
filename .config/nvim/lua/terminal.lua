-- Module for managing popup terminal

local M = {}

M.persistent_term = nil

local create_command = function(name, command)
	if not name or type(name) ~= "string" then return end

	vim.api.nvim_create_user_command(name, function()
		local cmd = type(command) == "function" and command() or command
		M.send_cmd_to_term(cmd)
	end, {})
end

M.setup = function()
	-- Setup keybinds
	require("keybinds").toggleterm_nvim()

	create_command("RunScript", "sleep 2 && ls /")
	create_command("ScanScript", function()
		local file_path = vim.fn.expand("%:p")
		LOG(file_path)

		return "ls"
	end)
end

M.send_cmd_to_term = function(cmd)
	local term = require("toggleterm.terminal").Terminal
	local default = term:new({ id = 1 })
	if not default:is_open() then default:open() end
	default:send(cmd)
end

return M
