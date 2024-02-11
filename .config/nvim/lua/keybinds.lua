local K = vim.keymap.set

local builtin
local wk = require("which-key")

local F = require("keybind-functions")

local M = {}

--------------------------------------------
--- Keybindings ---

M.setup = function()
	F.setup()

	-- Prevent dependency loop
	builtin = require("telescope.builtin")

	-- Change default behavior
	K("n", "Y", "y$")
	K("v", "p", "pgvy")

	K("n", "J", "mzJ`z")    -- dont move cursor to end of line

	K("n", "<C-d>", "<C-d>zz") -- stay in the middle
	K("n", "<C-u>", "<C-u>zz")
	K("n", "n", "nzzzv")
	K("n", "N", "Nzzzv")

	K("x", "p", "\"_dP") -- dont overwrite clipboard when pasting visually

	-- Remap for dealing with word wrap
	K("n", "k", [[v:count == 0 ? "gk" : "k"]], { expr = true, silent = true })
	K("n", "j", [[v:count == 0 ? "gj" : "j"]], { expr = true, silent = true })

	-- Tab/Shift+Tab functionality
	K("n", "<Tab>", ">>_")
	K("n", "<S-Tab>", "<<_")
	K("i", "<S-Tab>", "<C-D>")
	K("v", "<Tab>", ">gv")
	K("v", "<S-Tab>", "<gv")

	-- Move highlighted blocks
	K("v", "J", ":m '>+1<CR>gv=gv")
	K("v", "K", ":m '<-2<CR>gv=gv")

	-- Switch to previous buffer
	K("n", "<M-`>", "<C-6>")

	-- Center buffer with Ctrl+L
	K("n", "<C-l>", "zz")

	-- Close buffer with Alt-w
	K("n", "<M-w>", F.buffer_close)

	-- Hungry delete
	K("i", "<BS>", F.hungry_delete_backspace())
	K("i", "<Del>", F.hungry_delete())

	----------------------------------------
	--- Leader keys ---


	K("n", "<leader><space>", builtin.commands, { desc = "Execute command" })


	-- F.wk("<leader>b", "buffer/bookmark")
	K("n", "<leader>bb", builtin.buffers, { desc = "Switch buffer" })
	K("n", "<leader>bd", F.buffer_dashboard, { desc = "Dashboard" })


	F.wk("<leader>c", "comment")
	-- numToStr/Comment.nvim
	K("n", "<leader>cc", "<Plug>(comment_toggle_linewise_current)", { desc = "Comment toggle linewise" })
	K("n", "<leader>cp", "<Plug>(comment_toggle_blockwise_current)", { desc = "Comment toggle blockwise" })
	K("x", "<leader>cc", "<Plug>(comment_toggle_linewise_visual)", { desc = "Comment toggle linewise (visual)" })
	K("x", "<leader>cp", "<Plug>(comment_toggle_blockwise_visual)", { desc = "Comment toggle blockwise (visual)" })


	F.wk("<leader>e", "eval")
	K("n", "<leader>eb", ":w<CR>:source %<CR>", { desc = "Evaluate buffer" })


	F.wk("<leader>f", "file")
	K("n", "<leader>fc", F.file_config, { desc = "Config file" })
	K("n", "<leader>ff", F.file_find, { desc = "Find file" })
	K("n", "<leader>fh", F.file_find_home, { desc = "Find file in ~" })
	K("n", "<leader>f/", F.file_find_root, { desc = "Find file in root" })
	K("n", "<leader>fr", F.file_find_recent, { desc = "Find recent file" })
	K("n", "<leader>fs", F.file_save, { desc = "Save file" })


	F.wk("<leader>h", "help")
	K("n", "<leader>hh", builtin.help_tags, { desc = "Describe" })
	K("n", "<leader>hk", builtin.keymaps, { desc = "Describe key" })
	K("n", "<leader>hm", builtin.man_pages, { desc = "Describe manpage" })


	F.wk("<leader>g", "goto")
	K("n", "<leader>ge", F.goto_trouble, { desc = "Goto trouble" })
	K("n", "<leader>gt", F.goto_todos_telescope, { desc = "Goto todos (Telescope)" })
	K("n", "<leader>gT", F.goto_todos_trouble, { desc = "Goto todos (Trouble)" })


	F.wk("<leader>p", "project")
	K("n", "<leader>pf", F.project_file, { desc = "Find project file" })
	K("n", "<leader>pg", F.project_grep, { desc = "Find in project" })
	K("n", "<leader>pp", F.project_list, { desc = "List projects" })


	F.wk("<leader>s", "search")
	K("n", "<leader>ss", F.search_buffer, { desc = "Search buffer" })
	K("n", "<leader>sq", ":nohlsearch<CR>", { desc = "Stop search", silent = true })


	F.wk("<leader>v", "git") -- version control
	K("n", "<leader>vr", F.vc_select_repo, { desc = "Select repo" })
	K("n", "<leader>vv", F.vc_status, { desc = "Neogit status" })


	K({ "n", "v" }, "<leader>w", "<C-W>", { remap = true }) -- keymap and which-key should *both* be triggered
	wk.register({
		w = {
			name = "window",
			s = "Split window",
			v = "Split window vertically",
			w = "Switch windows",
			q = "Quit a window",
			o = "Close all other windows",
			T = "Break out into a new tab",
			x = "Swap current with next",
			["-"] = "Decrease height",
			["+"] = "Increase height",
			["<lt>"] = "Decrease width",
			[">"] = "Increase width",
			["|"] = "Max out the width",
			["_"] = "Max out the height",
			["="] = "Equally high and wide",
			h = "Go to the left window",
			l = "Go to the right window",
			k = "Go to the up window",
			j = "Go to the down window",
		},
	}, { mode = { "n", "v" }, prefix = "<leader>", preset = true })
	-- https://github.com/folke/which-key.nvim/issues/270
	-- https://github.com/folke/which-key.nvim/blob/main/lua/which-key/plugins/presets/misc.lua
end

--------------------------------------------
--- Plugin specific ---

-- This function gets run when an LSP connects to a particular buffer.
M.lspconfig_on_attach = function(_, bufnr)
	local nnoremap = function(keys, func, desc)
		vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
	end

	F.wk("<leader>l", "lsp", bufnr)
	nnoremap("<leader>la", vim.lsp.buf.code_action, "Code action")
	nnoremap("<leader>lf", F.lsp_format_buffer, "Format buffer")
	nnoremap("<leader>lr", vim.lsp.buf.rename, "Rename")

	F.wk("<leader>lg", "goto", bufnr)
	nnoremap("<leader>lga", builtin.lsp_dynamic_workspace_symbols, "Workspace symbols")
	nnoremap("<leader>lgd", vim.lsp.buf.declaration, "Declaration")
	nnoremap("<leader>lgg", vim.lsp.buf.definition, "Definition") -- builtin.lsp_definitions
	nnoremap("<leader>lgi", builtin.lsp_implementations, "Implementation")
	nnoremap("<leader>lgr", builtin.lsp_references, "References")
	nnoremap("<leader>lgs", builtin.lsp_document_symbols, "Document symbols")
	nnoremap("<leader>lgt", builtin.lsp_type_definitions, "Type definition")

	-- See `:help K` for why this keymap
	nnoremap("K", vim.lsp.buf.hover, "Hover Documentation")
	nnoremap("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")
end

-- Keybindings for nvim-cmp popup
M.nvim_cmp = function()
	local cmp = require("cmp")
	local luasnip = require("luasnip")
	return cmp.mapping.preset.insert({
		["<M-h>"] = cmp.mapping.abort(),
		["<M-j>"] = cmp.mapping.select_next_item(),
		["<M-k>"] = cmp.mapping.select_prev_item(),
		["<M-l>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),

		["<Esc>"] = cmp.mapping.abort(),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
		["<C-Space>"] = cmp.mapping.complete(),

		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_locally_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.locally_jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),
	})
end

-- Telescope picker generic mappings
M.telescope_default_mappings = function()
	local actions = require("telescope.actions")
	return {
		i = {
			["<M-h>"] = actions.close,
			["<M-j>"] = actions.move_selection_next,
			["<M-k>"] = actions.move_selection_previous,
			["<M-l>"] = actions.select_default,
		}
	}
end

return M
