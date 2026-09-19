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

set -eu

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/bin"
printf 'state: open\n' > "$tmp/lid-state"

cat > "$tmp/bin/hyprctl" <<'EOF'
#!/bin/sh
if [ "$1" = "monitors" ]; then
    cat "$MONITORS_JSON"
else
    printf '%s\n' "$*" >> "$CALLS"
fi
EOF
chmod +x "$tmp/bin/hyprctl"
for cmd in systemctl loginctl; do
    printf '#!/bin/sh\nprintf "%%s\\n" "%s $*" >> "$CALLS"\n' "$cmd" > "$tmp/bin/$cmd"
    chmod +x "$tmp/bin/$cmd"
done

export CALLS="$tmp/calls"
export MONITORS_JSON="$tmp/monitors.json"
run_once() {
    : > "$CALLS"
    PATH="$tmp/bin:$PATH" LID_STATE_FILE="$tmp/lid-state" \
        bash "$(dirname "$0")/watch_lid.sh" --once
}

# open + eDP-1 absent -> enable
printf 'state: open\n' > "$tmp/lid-state"
printf '[]\n' > "$MONITORS_JSON"
run_once
grep -Fq 'eval hl.monitor({ output = "eDP-1", mode = "preferred", position = "auto", scale = 1, disabled = false })' "$CALLS"

# closed + eDP-1 re-enabled (e.g. by `hyprctl reload`) -> disable again
printf 'state:      closed\n' > "$tmp/lid-state"
printf '[{"name": "eDP-1"}, {"name": "DP-3"}]\n' > "$MONITORS_JSON"
run_once
grep -Fq 'eval hl.monitor({ output = "eDP-1", disabled = true })' "$CALLS"

# closed + eDP-1 already disabled -> no-op (no eval spam)
printf '[{"name": "DP-3"}]\n' > "$MONITORS_JSON"
run_once
! grep -Fq 'eval ' "$CALLS"

# closed + no external monitor -> suspend once, no monitor eval
printf '[{"name": "eDP-1"}]\n' > "$MONITORS_JSON"
run_once
grep -Eq '^(systemctl|loginctl) suspend' "$CALLS"
! grep -Fq 'eval ' "$CALLS"
