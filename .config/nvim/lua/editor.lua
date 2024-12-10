return {

	-- Detect tabstop and shiftwidth automatically
	"tpope/vim-sleuth",

	-- "gc" to comment visual regions/lines
	"numToStr/Comment.nvim",

	-- Highlight, edit, and navigate code
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
		build = ":TSUpdate",
		opts = {
			ensure_installed = {
				"bash", "c", "cmake", "cpp", "c_sharp", "css", "go",
				"haskell", "html", "java", "javascript", "jsdoc", "json",
				"latex", "lua", "make", "markdown", "php", "python",
				"query", "regex", "rust", "toml", "tsx", "typescript",
				"vim", "vimdoc", "yaml",
			},
			sync_install = false,
			auto_install = true,

			-- Default install directory is <plugin_path>/parser
			-- parser_install_dir

			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = {
				enable = true,
			},
		}
	},

	-- Auto-save
	{
		"okuuva/auto-save.nvim",
		cmd = "ASToggle",                   -- defer, until run command
		event = { "InsertLeave", "TextChanged" }, -- defer, until event trigger
		opts = {
			debounce_delay = 5000, -- delay for `defer_save`, in ms
			condition = function(buf)
				-- Dont save special-buffers
				return vim.fn.getbufvar(buf, "&buftype") == ""
			end,
		},
		config = function(_, opts)
			require("auto-save").setup(opts)

			-- Cut off trailing whitespace and trailing blank lines
			local group = vim.api.nvim_create_augroup("AutoSave", { clear = false })
			local core = require("core.functions")
			vim.api.nvim_create_autocmd("User", {
				pattern = "AutoSaveWritePre",
				group = group,
				callback = function(event)
					if event.data.saved_buffer ~= nil then
						core.trim_buffer()
					end
				end,
			})
		end,
	},

	-- Autopair / electric pair
	{                                        -- https://github.com/altermo/ultimate-autopair.nvim
		"altermo/ultimate-autopair.nvim",
		event = { "InsertEnter", "CmdlineEnter" }, -- defer
		branch = "development",
		opts = {
			bs = { -- See: `:h ultimate-autopair-map-backspace-config`
				-- Call the backspace logic from the config, instead of automap
				-- https://github.com/altermo/ultimate-autopair.nvim/issues/60
				map = "<Plug>ultimate-autopair-BS",
			},
		},
	},

}
