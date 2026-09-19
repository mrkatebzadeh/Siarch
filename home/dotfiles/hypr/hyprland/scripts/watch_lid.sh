#!/usr/bin/env bash
# Copyright (C) 2026 M.R. Siavash Katebzadeh <mr@katebzadeh.xyz>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

set -u

lid_state_file=${LID_STATE_FILE:-/proc/acpi/button/lid/LID/state}
last=

while :; do
    if read -r _ state < "$lid_state_file"; then
        if [ "$state" = open ]; then
            if [ "$state" != "$last" ]; then
                hyprctl eval 'hl.monitor({ output = "eDP-1", mode = "preferred", position = "auto", scale = 1, disabled = false })'
            fi
        elif [ "$state" = closed ]; then
            if hyprctl monitors -j | jq -e '.[] | select(.name != "eDP-1")' >/dev/null 2>&1; then
                # Level-triggered (not just on transition): `hyprctl reload`
                # re-applies the generic monitor block and re-enables eDP-1
                # without any lid transition, so re-assert every tick.
                if hyprctl monitors -j | jq -e '.[] | select(.name == "eDP-1")' >/dev/null 2>&1; then
                    hyprctl eval 'hl.monitor({ output = "eDP-1", disabled = true })'
                fi
            elif [ "$state" != "$last" ]; then
                systemctl suspend || loginctl suspend
            fi
        fi
        last=$state
    fi

    [ "${1:-}" = --once ] && exit
    sleep 1
done
