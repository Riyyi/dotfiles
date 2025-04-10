return {

	-- Fuzzy Finder (files, LSP, etc)
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			-- Fuzzy Finder Algorithm which requires local dependencies to be built.
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = IS_WINDOWS() and "mingw32-make" or "make",
				cond = function()
					return vim.fn.executable(IS_WINDOWS() and "mingw32-make" or "make") == 1
				end,
			},
			-- Extensions
			"nvim-telescope/telescope-file-browser.nvim",
			"smartpde/telescope-recent-files",
		},
		extensions = {
			"fzf",
		},
		config = function()
			require("telescope").setup({
				defaults = {
					sorting_strategy = "ascending",

					layout_strategy = "config",
					layout_config = {
						height = 10, -- amount of results
						config = { -- apply settings to the "config" layout
							search_condensed = true,
						}
					},
					border = true,

					history = {
						path = vim.fn.stdpath("cache") .. "/telescope_history",
					},

					mappings = require("keybinds").telescope_default_mappings(),
				},
				extensions = {
					file_browser = {
						hide_parent_dir = true, -- hide "../"
						prompt_path = true, -- set path as prompt prefix
					},
				},
			})
			require("telescope").load_extension("fzf")
			require("telescope").load_extension "file_browser"
			require("telescope").load_extension("recent_files")

			--- ┌──────────────────────────────────────────────────┐
			--- │                                                  │
			--- │               ┌───────────────────┐              │
			--- │               │                   │              │
			--- │               │                   │              │
			--- │               │      Preview      │              │
			--- │               │                   │              │
			--- │               │                   │              │
			--- │               └───────────────────┘              │
			--- │                                                  │
			--- ├──────────────────────────────────────────────────┤
			--- │                      Prompt                      │
			--- ├──────────────────────────────────────────────────┤
			--- │                     Results                      │
			--- │                                                  │
			--- └──────────────────────────────────────────────────┘
			require("telescope.pickers.layout_strategies").config = function(picker, max_columns, max_lines,
																			 layout_config)
				local p_window = require "telescope.pickers.window"
				local initial_options = p_window.get_initial_window_options(picker)
				local results = initial_options.results
				local prompt = initial_options.prompt
				local preview = initial_options.preview
				-- Layout config isnt filled by Telescope automatically
				layout_config = require("telescope.config").values.layout_config or {}

				results.title = ""
				results.borderchars = { "─", "│", "─", "│", "├", "┤", "╯", "╰" }

				local bs = picker.window.border and 2 or 0
				local search_condensed = bs ~= 0 and layout_config.config.search_condensed and 2 or 0

				-- Height
				prompt.height = 1
				results.height = layout_config.height or 10
				local search_height = (prompt.height + results.height + (bs * 3))
				preview.height = math.floor((max_lines - search_height) * 0.8)

				-- Width
				prompt.width = max_columns - (bs ~= 0 and search_condensed == 0 and bs or 0)
				results.width = max_columns - (bs ~= 0 and search_condensed == 0 and bs or 0)
				preview.width = math.floor(max_columns * 0.75)

				-- Line (position), coordinates start at top-left
				prompt.line = max_lines - results.height + search_condensed - bs -- take overlapping border into account
				results.line = max_lines - results.height + search_condensed - bs + (bs / 2) + prompt.height
				preview.line = math.floor((max_lines - search_height - preview.height + bs) / 2) + 1

				return {
					preview = picker.previewer and preview.width > 0 and preview,
					prompt = prompt,
					results = results,
				}
			end
		end,
	},

	-- Telescope pickers sorted by recency and frequency
	{
		"prochri/telescope-all-recent.nvim",
		dependencies = {
			"kkharji/sqlite.lua",
		},
		opts = {
			database = {
				folder = vim.fn.stdpath("cache"),
			},
			pickers = { -- extension_name#extension_method
				["projects#projects"] = {
					disable = false,
					use_cwd = false,
					sorting = "frecency",
				},
			},
		}
	},

}
