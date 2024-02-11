local M = {}

local plugin_path = vim.fn.stdpath("cache") .. "/lazy"

-- Install lazy.nvim plugin manager
-- See `:help lazy.nvim.txt`
M.install = function()
	local lazy_path = plugin_path .. "/lazy.nvim"
	if not vim.loop.fs_stat(lazy_path) then
	vim.fn.system {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazy_path,
	}
	end
	vim.opt.rtp:prepend(lazy_path)
end

M.setup = function(modules)
	M.install()

	local options = {
		root = plugin_path, -- directory where plugins will be installed
	}

    require("lazy").setup(modules, options)
end

return M
