return {
	"ThePrimeagen/harpoon",
	commit = "ccae1b9",
	config = function()
		local harpoon = require("harpoon")
		require("telescope").load_extension("harpoon")
	end,
}
