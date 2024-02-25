--- Behavior ---

-- vim.opt.autochdir = true
vim.opt.clipboard = "unnamedplus"
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.opt.mouse = "a"
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes" -- always show gutter/fringe
vim.opt.ttimeoutlen = 0

-- Case-insensitive searching UNLESS \C or capital in search
vim.opt.ignorecase = true
vim.opt.smartcase = true

--- Editing ---

vim.opt.number = true
vim.opt.breakindent = true

vim.opt.backspace = "indent,eol,start"
vim.opt.expandtab = false
vim.opt.shiftround = true
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.tabstop = 4

vim.opt.smartindent = true

vim.opt.wrap = false

--- Files

vim.opt.backup = true
vim.opt.backupdir = vim.fn.stdpath("cache") .. "/backup"
vim.opt.swapfile = false
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("cache") .. "/undodir"

vim.opt.shadafile = vim.fn.stdpath("cache") .. "/netrwhist"

--- UI ---

vim.opt.colorcolumn = "81"
vim.opt.completeopt = "menuone,noselect"
vim.opt.cursorline = true
vim.opt.fillchars = vim.opt.fillchars + "diff:╱"
vim.opt.showtabline = 0
vim.opt.termguicolors = true
vim.opt.title = true
