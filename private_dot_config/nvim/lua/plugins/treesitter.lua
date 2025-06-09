return {
	'nvim-treesitter/nvim-treesitter',
	build = ":TSUpdate",
	config = function () 
		local configs = require("nvim-treesitter.configs")

		configs.setup({
			ensure_installed = { "c", "nix", "ocaml", "lua", "vim", "vimdoc", "query", "elixir", "heex", "javascript", "html", "python" },
			ignore_installed = { "org" },
			sync_install = false,
			highlight = { enable = true },
			indent = { enable = true },  
		})
	end
}
