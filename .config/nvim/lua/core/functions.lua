local M = {}

M.is_buffer_a_file = function()
	local buffer_name = vim.fn.bufname()

	return buffer_name ~= "" and vim.fn.filereadable(buffer_name) == 1
end

M.normalize_path = function(path)
	if not path then return nil end
	return IS_WINDOWS() and path:gsub("\\", "/") or path
end

M.get_file_path = function()
	if not M.is_buffer_a_file() then
		return nil
	end
	local file_path = vim.fn.expand("%:p")

	return M.normalize_path(vim.fn.fnamemodify(file_path, ":h")) .. "/"
end

M.get_netrw_path = function() -- b:netrw_curdir
	if vim.fn.expand("#" .. vim.fn.bufnr()) == "0" then
		return nil
	end

	return M.normalize_path(vim.fn.fnamemodify(vim.fn.bufname(), ":p"))
end

M.get_current_directory = function()
	return M.get_file_path() or M.get_netrw_path() or M.normalize_path(vim.fn.getcwd())
end

M.find_project_root = function()
	local current_directory = M.get_current_directory()
	if current_directory:match("^term://") then return nil, current_directory end

	local directory = current_directory
	while directory ~= "/" and not directory:match("^%a:[/\\]?$") do
		local git_path = vim.loop.fs_stat(directory .. "/.git")
		if git_path then
			return M.normalize_path(directory:gsub("/$", "")) -- remove trailing slash
		end

		local project_file = vim.loop.fs_stat(directory .. "/.project")
		if project_file and project_file.type == "file" then
			return M.normalize_path(directory:gsub("/$", "")) -- remove trailing slash
		end

		directory = M.normalize_path(vim.fn.fnamemodify(directory, ":h"))
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
