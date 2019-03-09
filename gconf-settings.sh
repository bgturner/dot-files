#!/bin/bash
#
# Update desktop settings
#

# Get the UUID of the default gnome-terminal profile.
profile=$(gsettings get org.gnome.Terminal.ProfilesList default)
profile=${profile:1:-1} # remove leading and trailing single quotes

# Update Terminal Color scheme.
gsettings set "org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/" background-color '#1c1c1c'

# Disable menu bar in gnome-terminal
#
# Note: since this is a global preference, we don't need to target a 
# specific profile.
#
gsettings set "org.gnome.Terminal.Legacy.Settings" default-show-menubar false

# Make capslock an additional ctrl
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"

# Register custom keyboard shortcuts.
#
# Note: this will wipe out any existing custom shortcuts.
# If you want to merge these with existing shortcuts, programmatically
# generate the key `custom0` for each shortcut.
gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/']"

# Set the shortcut values.
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name 'Launch Terminal'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command 'gnome-terminal'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding '<Super>t'

# Adjust title bar button settings
#
# Note: the colon indicates where the window title will be. To have the 
# buttons to the right of the title, place the colon before the button 
# definition:
#
#		':close,minimize,maximize'
#
gsettings set org.gnome.desktop.wm.preferences button-layout 'close:'

