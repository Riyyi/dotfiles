return {

	-- Autocompletion
	{
		"hrsh7th/nvim-cmp",
		lazy = true, -- defer
		dependencies = {
			-- Snippet Engine & its associated nvim-cmp source
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",

			-- Adds LSP completion capabilities
			"hrsh7th/cmp-nvim-lsp", -- source for neovim"s built-in LSP client
			"hrsh7th/cmp-nvim-lua", -- source for neovim"s Lua API
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",

			-- Adds a number of user-friendly snippets
			"rafamadriz/friendly-snippets",

			-- Icons in the completion menu
			"onsails/lspkind.nvim",
		},
		config = function()
			local luasnip = require("luasnip")
			require("luasnip.loaders.from_vscode").lazy_load()
			luasnip.config.setup({})

			require("cmp").setup({
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				completion = {
					completeopt = "menu,menuone,noinsert",
				},
				sources = {
					{ name = "nvim_lsp" },
					{ name = "nvim_lua" },
					{ name = "luasnip" },
					{ name = "buffer" },
					{ name = "path" },
				},
				formatting = {
					fields = { "kind", "abbr", "menu" },
					-- https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance#how-to-get-types-on-the-left-and-offset-the-menu
					-- https://github.com/onsails/lspkind.nvim#option-2-nvim-cmp
					format = function(entry, vim_item)
						local kind = require("lspkind").cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry,
							vim_item)
						local strings = vim.split(kind.kind, "%s", { trimempty = true })
						kind.kind = strings[1] or ""
						kind.menu = " (" .. (strings[2] or "") .. ")"

						return kind
					end,
					expandable_indicator = true,
				},
				mapping = require("keybinds").nvim_cmp(),
			})
		end,
	},

	-- LSP Configuration & Plugins
	{ -- https://github.com/neovim/nvim-lspconfig
		"neovim/nvim-lspconfig",
		dependencies = {
			-- Improve the built-in LSP UI
			"nvimdev/lspsaga.nvim",
			-- Additional lua configuration, makes Nvim stuff amazing!
			"folke/neodev.nvim",
			-- C# "Goto Definition" with decompilation support
			"Hoffs/omnisharp-extended-lsp.nvim",
		},
		config = function()
			-- Setup neovim Lua configuration
			require("lspsaga").setup() -- ?? does this do anything with doc hover
			require("neodev").setup()

			-- Vim process
			local pid = vim.fn.getpid()

			-- Setup language servers
			-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
			local servers = {
				clangd = {}, -- C/C++
				intelephense = {}, -- PHP
				lua_ls = { -- Lua, via lua-language-server
					settings = {
						Lua = {
							workspace = { checkThirdParty = false },
							telemetry = { enable = false },
							diagnostics = {
								-- Ignore Lua_LS's noisy `missing-fields` warnings
								disable = { "missing-fields" },
								-- Ignore Lua_LS's noise `undefined global` warnings
								globals = { "vim" },
							},
						},
					},
				},
				omnisharp = { -- C#, via omnisharp-roslyn-bin
					cmd = { "/usr/bin/omnisharp", "--languageserver", "--hostPID", tostring(pid) },
					handlers = {
						["textDocument/definition"] = require('omnisharp_extended').handler,
					},
				},
				ts_ls = {
					init_options = {
						plugins = {
							{
								name = '@vue/typescript-plugin',
								location = '/home/rick/.cache/.bun/install/global/node_modules/@vue/language-server',
								languages = { 'vue' },
							},
						},
					},
					filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx', 'vue', },
					-- filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue' },
					-- requires: bun install -g typescript-language-server
					-- TODO: Update which-key config, maybe other packages
					-- Sources:
					--   https://github.com/vuejs/language-tools?tab=readme-ov-file#hybrid-mode-configuration-requires-vuelanguage-server-version-200
					--   https://github.com/neovim/nvim-lspconfig/blob/master/lua/lspconfig/configs/ts_ls.lua#L4
				},
				volar = {
					-- init_options = {
					-- 	vue = {
					-- 		hybridMode = false,
					-- 	},
					-- },
				},
				-- volar = {
				-- 	-- init_options = {
				-- 	-- 	typescript = {
				-- 	-- 		serverPath = '/home/rick/.cache/.bun/install/global/node_modules/typescript/lib//tsserverlibrary.js'
				-- 	-- 	}
				-- 	-- },
				-- 	-- filetypes = {'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json'}
				-- },
			}

			-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

			for server, _ in pairs(servers) do
				local opts = vim.tbl_extend("keep", servers[server], {
					capabilities = capabilities,
					on_attach = require("keybinds").lspconfig_on_attach,
				})
				-- P(server)
				-- P(opts)
				require("lspconfig")[server].setup(opts)
			end
		end,
	},

	-- Project
	{
		"ahmedkhalf/project.nvim",
		opts = {
			detection_methods = { "lsp", "pattern" },
			patterns = { ".git", ".project" },
			datapath = vim.fn.stdpath("cache"),
		},
		config = function(_, opts)
			-- Add syncing between project.nvim and dashboard-nvim project entries.
			-- NOTE: Must come before the project.nvim setup(),
			--       so the glue's VimLeavePre autocmd is defined before it.
			local F = require("core.functions")
			vim.api.nvim_create_autocmd("VimLeavePre", {
				pattern = "*",
				callback = function()
					-- Fetch project.nvim projects
					local project = require("project_nvim.project")
					local history = require("project_nvim.utils.history")
					local recent = history.recent_projects or {}
					local session = history.session_projects or {}
					local recent_plus_session = F.table_merge_unique(recent, session)

					-- Fetch dashboard-nvim projects
					local utils = require("dashboard.utils")
					local path = utils.path_join(vim.fn.stdpath("cache"), "dashboard/cache")
					local projects = utils.read_project_cache(path) or {}

					-- Add any projects that project.nvim uniquely knows about to dashboard-nvim
					local all_projects = F.table_merge_unique(recent_plus_session, projects)
					vim.fn.writefile({ "return " .. vim.inspect(all_projects) }, path)

					-- Add any projects that dashboard-nvim uniquely knows about to project.nvim
					for _, value in ipairs(projects) do
						if not vim.tbl_contains(recent_plus_session, value) then
							pcall(project.set_pwd, value, "manual") -- skip non-existent directories, dont error
						end
					end
				end,
			})

			require("project_nvim").setup(opts)
			require("telescope").load_extension("projects")
		end
	}


}
