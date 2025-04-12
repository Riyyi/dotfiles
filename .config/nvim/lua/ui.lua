return {

	-- Theme
	{
		"RRethy/nvim-base16",
		lazy = false, -- make sure to load this during startup if it is your main colorscheme
		priority = 1000,
		config = function()
			require("base16-colorscheme").with_config({
				-- TODO: maybe make Telescope borders visible?
				-- telescope = false,
			})
			vim.cmd.colorscheme "base16-tomorrow-night"

			local colors = require("base16-colorscheme").colors
			-- local blue_m1 = "#9cc4e6"
			local blue    = colors.base0D -- #81a2be
			-- local blue_p1 = "#5f819d"
			local blue_p2 = "#445666"
			local blue_p3 = "#2a3640"
			local cyan = colors.base0C -- #8abeb7
			local fg = colors.base05 -- #c5c8c6
			local fg_dark = "#515151"
			-- local green = colors.base0B -- #b5bd68
			local green_p1 = "#8c9440"
			-- local green_p2 = "#5f875f"
			local green_bg = "#404324"
			-- local orange = colors.base09 -- #de935f
			-- local magenta = colors.base0E -- #b294bb
			local red = colors.base08 -- #cc6666
			local red_bg = "#4e2626"
			local yellow = colors.base0A -- #f0c674

			-- Cursor
			vim.api.nvim_command("highlight CursorLineNr                                      guifg=" .. yellow .. " gui=bold")
			-- Diffview
			vim.api.nvim_command("highlight DiffviewDiffAdd         guibg=" .. green_bg)
			vim.api.nvim_command("highlight DiffviewDiffChange      guibg=" .. blue_p3) -- Unchanged part on a change line
			vim.api.nvim_command("highlight DiffviewDiffDelete      guibg=" .. red_bg .. "    guifg=" .. fg_dark)
			vim.api.nvim_command("highlight DiffviewDiffText        guibg=" .. blue_p2) -- Changed part on a change line
			-- Git gutter
			vim.api.nvim_command("highlight GitSignsAdd                                       guifg=" .. green_p1)
			vim.api.nvim_command("highlight GitSignsChange                                    guifg=" .. yellow)
			-- Rainbow delimiters
			vim.api.nvim_command("highlight RainbowDelimiterBlue                              guifg=" .. blue)
			vim.api.nvim_command("highlight RainbowDelimiterCyan                              guifg=" .. cyan)
			vim.api.nvim_command("highlight RainbowDelimiterGreen                             guifg=" .. green_p1)
			vim.api.nvim_command("highlight RainbowDelimiterOrange                            guifg=" .. fg)
			vim.api.nvim_command("highlight RainbowDelimiterRed                               guifg=" .. red)
			vim.api.nvim_command("highlight RainbowDelimiterYellow                            guifg=" .. yellow)
		end,
	},

	-- Set lualine as statusline
	{
		-- See `:help lualine.txt`
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				icons_enabled = false,
				theme = "base16",
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
				globalstatus = true,
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "encoding", "filename" },
				lualine_c = {
					{
						-- TODO: nvim alternate file stuff and closing files is busted
						"project",
						fmt = function()
							local path = require("core.functions").find_project_root() or ""
							return path:match("([^/]+)$")
						end,
					},
				},
				lualine_x = { "diagnostics", "fileformat" },
				lualine_y = { "filetype" },
				lualine_z = { "progress", "location" },
			},
		},
	},

	-- Add file type icons to plugins
	{ "nvim-tree/nvim-web-devicons", lazy = true },

	-- Dashboard
	{
		"nvimdev/dashboard-nvim",
		event = "VimEnter",
		opts = {
			config = {
				header = {
					"      .          .      ",
					"    ';;,.        ::'    ",
					"  ,:::;,,        :ccc,  ",
					" ,::c::,,,,.     :cccc, ",
					" ,cccc:;;;;;.    cllll, ",
					" ,cccc;.;;;;;,   cllll; ",
					" :cccc; .;;;;;;. coooo; ",
					" ;llll;   ,:::::'loooo; ",
					" ;llll:    ':::::loooo: ",
					" :oooo:     .::::llodd: ",
					" .;ooo:       ;cclooo:. ",
					"   .;oc        'coo;.   ",
					"     .'         .,.     ",
					"",
					"   _______________________________________________________",
					"",
					"",
				},
				shortcut = {
					{ desc = "Neovim master race!" },
				},
				footer = {},
			},
		}
	},

	-- Show project errors
	{
		"folke/trouble.nvim",
		cmd = "Trouble", -- defer
		opts = {
			mode = "lsp_references",
			use_diagnostic_signs = true,
		},
		config = function()
			-- Gutter/fringe icons
			--   https://github.com/folke/trouble.nvim/issues/52
			local signs = {
				Error = "»",
				Warn = "»",
				Hint = "»",
				Info = "»",
				Other = "»",
			}
			for type, icon in pairs(signs) do
				local hl = "DiagnosticSign" .. type
				vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
			end
		end,
	},

	-- Show project TODOs
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {},
	},

	-- Useful plugin to show you pending keybinds.
	"folke/which-key.nvim",

	-- Rainbow delimiters
	{
		"HiPhish/rainbow-delimiters.nvim",
		opts = {
			highlight = {
				"RainbowDelimiterOrange", -- white
				"RainbowDelimiterCyan",
				"RainbowDelimiterYellow",
				"RainbowDelimiterGreen",
				"RainbowDelimiterBlue",
				"RainbowDelimiterOrange", -- white
				"RainbowDelimiterCyan",
				"RainbowDelimiterYellow",
				"RainbowDelimiterGreen",
				"RainbowDelimiterBlue",
				"RainbowDelimiterRed",
			},
		},
		config = function(_, opts)
			require("rainbow-delimiters.setup").setup(opts)
		end,
	},

	-- Rainbow-mode
	{ -- https://github.com/norcalli/nvim-colorizer.lua
		"norcalli/nvim-colorizer.lua",
		opts = {},
	}, -- :ColorizerToggle

	-- Popup terminal
	{
		"akinsho/toggleterm.nvim",
		config = function()
			vim.cmd [[let &shellcmdflag = '-s']] -- required for Windows
			require("toggleterm").setup({
				auto_scroll = true,
				direction = "float",
				float_opts = {
					border = "curved",
					row = function() return vim.o.lines - 4 end,
					width = function() return vim.o.columns - 6 end,
					height = 20,
					winblend = 0,
				},
				hide_numbers = true,
				open_mapping = nil,
				persist_mode = true,
				size = 20,
				start_in_insert = true,
			})
		end,
	}
}
