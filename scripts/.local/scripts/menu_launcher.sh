#!/bin/sh

MENU_DIR="~/.local/bin"

flock -n /tmp/menusuite.lock -c "${MENU_DIR}/menus/$*"
