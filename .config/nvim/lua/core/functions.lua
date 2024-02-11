local M = {}

M.is_buffer_a_file = function()
	local buffer_name = vim.fn.bufname()

	return buffer_name ~= "" and vim.fn.filereadable(buffer_name) == 1
end

M.get_file_path = function()
	if not M.is_buffer_a_file() then
		return nil
	end
	local file_path = vim.fn.expand("%:p")

	return vim.fn.fnamemodify(file_path, ":h") .. "/"
end

M.get_netrw_path = function() -- b:netrw_curdir
	if vim.fn.expand("#" .. vim.fn.bufnr()) == "0" then
		return nil
	end

	return vim.fn.fnamemodify(vim.fn.bufname(), ":p")
end

M.get_current_directory = function()
	return M.get_file_path() or M.get_netrw_path() or vim.fn.getcwd()
end

M.find_project_root = function()
	local current_directory = M.get_current_directory()

	local directory = current_directory
	while directory ~= "/" do
		local git_directory = directory .. "/.git"
		local project_file = directory .. "/.project"

		if vim.fn.isdirectory(git_directory) == 1 or vim.fn.filereadable(project_file) == 1 then
			return directory
		end

		directory = vim.fn.fnamemodify(directory, ":h")
	end

	return nil, current_directory
end

-- This will merge tables with index-value pairs and keep the unique values
M.table_merge_unique = function(...)
	local result = {}
	local seen_values = {}
	for _, value in ipairs(vim.tbl_flatten(...)) do
		if not seen_values[value] then
			seen_values[value] = true
			table.insert(result, value)
		end
	end

	return result
end

-- Cut off trailing whitespace and trailing blank lines
-- https://vi.stackexchange.com/questions/37421/how-to-remove-neovim-trailing-white-space
-- https://stackoverflow.com/questions/7495932/how-can-i-trim-blank-lines-at-the-end-of-file-in-vim
M.trim_buffer = function()
	local save_cursor = vim.fn.getpos(".")
	pcall(function() vim.cmd([[%s/\s\+$//e]]) end)
	pcall(function() vim.cmd([[%s#\($\n\s*\)\+\%$##]]) end)
	vim.fn.setpos(".", save_cursor)
end

return M
