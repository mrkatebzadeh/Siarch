hl.bind("CTRL+SUPER+ALT+Slash", hl.dsp.exec_cmd("xdg-open ~/.config/hypr/custom/keybinds.lua"), {description = "Edit user keybinds"} )

-- macOS compat (skipped ALT+Q/Space/W/D/E per request)
hl.bind("CTRL + ALT + Space", hl.dsp.exec_cmd("pkill rofi || rofimoji --use-icons"), { release = true })
hl.bind("CTRL + ALT + Q", hl.dsp.exec_cmd("hyprlock"))
hl.bind("CTRL + ALT + F", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))

-- ALT -> CTRL sendshortcut
-- hl.bind("ALT + A", hl.dsp.send_shortcut({ mods = "CTRL", key = "A" }))
-- hl.bind("ALT + B", hl.dsp.send_shortcut({ mods = "CTRL", key = "B" }))
-- hl.bind("ALT + C", hl.dsp.send_shortcut({ mods = "CTRL", key = "INSERT" }))
-- hl.bind("ALT + F", hl.dsp.send_shortcut({ mods = "CTRL", key = "F" }))
-- hl.bind("ALT + G", hl.dsp.send_shortcut({ mods = "CTRL", key = "G" }))
-- hl.bind("ALT + H", hl.dsp.send_shortcut({ mods = "CTRL", key = "H" }))
-- hl.bind("ALT + I", hl.dsp.send_shortcut({ mods = "CTRL", key = "I" }))
-- hl.bind("ALT + J", hl.dsp.send_shortcut({ mods = "CTRL", key = "J" }))
-- hl.bind("ALT + K", hl.dsp.send_shortcut({ mods = "CTRL", key = "K" }))
-- hl.bind("ALT + L", hl.dsp.send_shortcut({ mods = "CTRL", key = "L" }))
-- hl.bind("ALT + M", hl.dsp.send_shortcut({ mods = "CTRL", key = "M" }))
-- hl.bind("ALT + N", hl.dsp.send_shortcut({ mods = "CTRL", key = "N" }))
-- hl.bind("ALT + O", hl.dsp.send_shortcut({ mods = "CTRL", key = "O" }))
-- hl.bind("ALT + P", hl.dsp.send_shortcut({ mods = "CTRL", key = "P" }))
-- hl.bind("ALT + R", hl.dsp.send_shortcut({ mods = "CTRL", key = "R" }))
-- hl.bind("ALT + S", hl.dsp.send_shortcut({ mods = "CTRL", key = "S" }))
-- hl.bind("ALT + T", hl.dsp.send_shortcut({ mods = "CTRL", key = "T" }))
-- hl.bind("ALT + U", hl.dsp.send_shortcut({ mods = "CTRL", key = "U" }))
-- hl.bind("ALT + V", hl.dsp.send_shortcut({ mods = "SHIFT", key = "INSERT" }))
-- hl.bind("ALT + X", hl.dsp.send_shortcut({ mods = "CTRL", key = "X" }))
-- hl.bind("ALT + Y", hl.dsp.send_shortcut({ mods = "CTRL", key = "Y" }))
-- hl.bind("ALT + Z", hl.dsp.send_shortcut({ mods = "CTRL", key = "Z" }))
-- hl.bind("ALT + SHIFT + Z", hl.dsp.send_shortcut({ mods = "CTRL", key = "Y" }))
-- hl.bind("ALT + SHIFT + T", hl.dsp.send_shortcut({ mods = "CTRL SHIFT", key = "T" }))

-- Emacs-style CTRL sendshortcut (binde, no mods sent)
-- hl.bind("CTRL + A", hl.dsp.send_shortcut({ mods = "", key = "home" }), { repeating = true })
-- hl.bind("CTRL + E", hl.dsp.send_shortcut({ mods = "", key = "end" }), { repeating = true })
-- hl.bind("CTRL + F", hl.dsp.send_shortcut({ mods = "", key = "right" }), { repeating = true })
-- hl.bind("CTRL + B", hl.dsp.send_shortcut({ mods = "", key = "left" }), { repeating = true })
-- hl.bind("CTRL + P", hl.dsp.send_shortcut({ mods = "", key = "up" }), { repeating = true })
-- hl.bind("CTRL + N", hl.dsp.send_shortcut({ mods = "", key = "down" }), { repeating = true })
-- hl.bind("CTRL + H", hl.dsp.send_shortcut({ mods = "", key = "backspace" }), { repeating = true })

-- hl.bind(
    -- "ALT + C",
    -- hl.dsp.send_shortcut({ mods = "CTRL SHIFT", key = "C" })
-- )

-- hl.bind(
    -- "ALT + V",
    -- hl.dsp.send_shortcut({ mods = "SHIFT", key = "INSERT" })
-- )
