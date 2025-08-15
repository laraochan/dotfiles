#!/bin/bash
set -eu

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPTS_DIR="$DOTFILES_DIR/scripts"

echo "Start: setup.sh"

$SCRIPTS_DIR/setup_symlink.sh
$SCRIPTS_DIR/setup_dpp.sh

echo "End: setup.sh"
