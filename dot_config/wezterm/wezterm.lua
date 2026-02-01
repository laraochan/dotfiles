local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.color_scheme = 'Solarized (dark) (terminal.sexy)'
config.ime_preedit_rendering = 'Builtin'

return config
