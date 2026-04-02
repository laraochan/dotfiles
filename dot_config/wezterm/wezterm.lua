local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.font_size = 14
-- config.color_scheme = 'carbonfox'
config.color_scheme = 'Material (terminal.sexy)'
-- config.color_scheme = 'Horizon Dark (Gogh)'
config.use_ime = true

return config
