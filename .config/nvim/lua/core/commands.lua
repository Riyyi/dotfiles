local F = require("core.functions")

local create_command = function(name, command)
	if not name or type(name) ~= "string" then return end

	vim.api.nvim_create_user_command(name, function()
		local cmd = type(command) == "function" and command() or command
		F.execute_command(cmd)
	end, {})
end

create_command("RunScript", "ls")
create_command("ScanScript", function()
	local file_path = vim.fn.expand("%:p")
	LOG(file_path)

	return "ls"
end)
