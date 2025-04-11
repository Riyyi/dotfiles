local F = require("core.functions")

--------------------------------------------
--- Commands ---

-- Prevent typo
vim.cmd [[
	cnoreabbrev <expr> W (getcmdtype() is# ":" && getcmdline() is# "W") ? ("w") : ("W")
	cnoreabbrev <expr> Q (getcmdtype() is# ":" && getcmdline() is# "Q") ? ("q") : ("Q")
	cnoreabbrev <expr> WQ (getcmdtype() is# ":" && getcmdline() is# "WQ") ? ("wq") : ("WQ")
	cnoreabbrev <expr> Wq (getcmdtype() is# ":" && getcmdline() is# "Wq") ? ("wq") : ("Wq")
]]

--------------------------------------------
--- Autocommands ---

-- Cut off trailing whitespace and trailing blank lines
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
	pattern = "*",
	callback = F.trim_buffer,
})

-- Highlight on yank
--  See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
	pattern = "*",
	group = highlight_group,
	callback = function() vim.highlight.on_yank() end,
})

-- Show message when autosaving
local autosave_group = vim.api.nvim_create_augroup("autosave", {})
vim.api.nvim_create_autocmd("User", {
	pattern = "AutoSaveWritePost",
	group = autosave_group,
	callback = function(opts)
		if opts.data.saved_buffer ~= nil then
			local filename = vim.api.nvim_buf_get_name(opts.data.saved_buffer)
			vim.notify("Wrote " .. filename, vim.log.levels.INFO)
		end
	end,
})

vim.api.nvim_create_autocmd("BufWinEnter", {
	callback = function()
	-- Buffer tracking
		require("core.buffers").add_buffer()
	end,
    desc = "Track all full window buffers visited",
})

vim.api.nvim_create_autocmd({ "BufDelete", "BufWipeout" }, {
	callback = function(opts)
		-- Buffer tracking
		require("core.buffers").remove_buffer(opts.match)
	end,
    desc = "Track all full window buffers killed",
})
