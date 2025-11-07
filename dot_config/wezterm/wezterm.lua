local wezterm = require('wezterm')
local config = wezterm.config_builder()

config.font = wezterm.font('JetBrainsMono Nerd Font')
config.font_size = 16
config.color_scheme = 'Panda (Gogh)'

return config
