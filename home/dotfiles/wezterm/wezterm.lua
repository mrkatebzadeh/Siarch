local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.default_prog = { 'nu' }

config.color_schemes = {
  doom = {
    foreground = "#999999",
    background = "#2b2f2d",
    cursor_bg = "#ebcb8b",
    cursor_fg = "#424855",
    cursor_border = "#ebcb8b",
    selection_fg = "#424855",
    selection_bg = "#88c0d0",
    scrollbar_thumb = "#666b75",
    split = "#88c0d0",
    ansi = {
      "#353a44",
      "#d08770",
      "#1c8362",
      "#ebcb8b",
      "#88c0d0",
      "#cc79a7",
      "#88c0d0",
      "#999999",
    },
    brights = {
      "#666b75",
      "#d08770",
      "#a3be8c",
      "#e69f00",
      "#88c0d0",
      "#8c6bb1",
      "#88c0d0",
      "#d8dadd",
    },
    tab_bar = {
      background = "#353a44",
      active_tab = {
        bg_color = "#88c0d0",
        fg_color = "#424855",
      },
      inactive_tab = {
        bg_color = "#424855",
        fg_color = "#999999",
      },
      inactive_tab_hover = {
        bg_color = "#666b75",
        fg_color = "#d8dadd",
      },
      new_tab = {
        bg_color = "#353a44",
        fg_color = "#999999",
      },
      new_tab_hover = {
        bg_color = "#88c0d0",
        fg_color = "#424855",
      },
    },
  },
}

config.color_scheme = "doom"

-- config.font = wezterm.font("Lilex Nerd Font")
config.font = wezterm.font("FiraCode Nerd Font")
config.font_size = 14.0
config.window_decorations = "RESIZE"
config.window_padding = { left = 10, right = 10, top = 10, bottom = 10 }

config.send_composed_key_when_left_alt_is_pressed = true
config.hide_tab_bar_if_only_one_tab = true

config.window_background_opacity = 0.85
config.macos_window_background_blur = 20

config.enable_scroll_bar = false

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
config.enable_wayland = false
config.set_environment_variables = {
    PATH = os.getenv("HOME") .. "/.siarch/pkgs/scripts/hypr/src:" .. os.getenv("HOME") .. "/.cargo/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:" .. os.getenv("PATH"),
}

return config
