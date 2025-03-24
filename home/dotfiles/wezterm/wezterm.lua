local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.default_prog = { 'nu' }

config.color_scheme = "Catppuccin Frappe"
config.font = wezterm.font("FiraCode Nerd Font Mono")
config.window_decorations = "RESIZE"

config.hide_tab_bar_if_only_one_tab = true

config.window_background_opacity = 0.9
config.macos_window_background_blur = 20

config.enable_scroll_bar = true

config.max_fps = 120
config.animation_fps = 120
config.cursor_blink_ease_in = "EaseOut"
config.cursor_blink_ease_out = "EaseOut"
config.default_cursor_style = "BlinkingBlock"
config.cursor_blink_rate = 650

config.visual_bell = {
	fade_in_function = "EaseIn",
	fade_in_duration_ms = 250,
	fade_out_function = "EaseOut",
	fade_out_duration_ms = 250,
	target = "CursorColor",
}

return config
