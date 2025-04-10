IS_WINDOWS = function()
	return string.find(vim.loop.os_uname().sysname, "NT")
end

-- Usage:
--   :lua P("Hello World!")
P = function(v)
	print(vim.inspect(v))
	return v
end

RELOAD = function(...)
	return require("plenary.reload").reload_module(...)
end

R = function(name)
	RELOAD(name)
	return require(name)
end

LOG = function(v)
	vim.fn.writefile({ vim.inspect(v) }, vim.fn.stdpath("cache") .. "/log", "a")
end
