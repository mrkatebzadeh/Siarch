return {
	"nvim-neorg/neorg",
	build = ":Neorg sync-parsers",
	tag = "v7.0.0",
	dependencies = { "nvim-lua/plenary.nvim" },
	config = function()
		require("neorg").setup({
			load = {
				["core.defaults"] = {},
				["core.concealer"] = {},
				["core.completion"] = { config = { engine = "nvim-cmp", name = "[Norg]" } },
				["core.integrations.nvim-cmp"] = {},
				["core.dirman"] = {
					config = {
						workspaces = {
							notes = "~/Dropbox/notes",
							research = "~/Dropbox/research",
							home = "~/Dropbox/home",
						},
					},
				},
				["core.keybinds"] = {
					config = {
						default_keybinds = true,
						neorg_leader = "<Leader>n",
					},
				},
				["core.esupports.metagen"] = { config = { type = "auto", update_date = true } },
				["core.qol.toc"] = {},
				["core.qol.todo_items"] = {},
				["core.looking-glass"] = {},
				["core.presenter"] = { config = { zen_mode = "zen-mode" } },
				["core.export"] = {},
				["core.export.markdown"] = { config = { extensions = "all" } },
				["core.summary"] = {},
				["core.tangle"] = { config = { report_on_empty = false } },
			},
		})
	end,
}
