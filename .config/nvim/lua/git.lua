return {

	-- Adds git related signs to the gutter/fringe
	{
		"lewis6991/gitsigns.nvim",
		opts = {
			-- See `:help gitsigns.txt`
			signs = {
				add          = { text = "█" },
				change       = { text = "█" },
				delete       = { text = "█" },
				topdelete    = { text = "█" },
				changedelete = { text = "█" },
				untracked    = { text = "┆" },
			},
		},
	},

	-- Magit clone
	{
		"NeogitOrg/neogit",
		cmd = "Neogit", -- defer
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",

			"sindrets/diffview.nvim", -- diff integration
			-- { "sindrets/diffview.nvim", opts = { enhanced_diff_hl = true } }, -- diff integration
		},
		opts = {},
	},

}
