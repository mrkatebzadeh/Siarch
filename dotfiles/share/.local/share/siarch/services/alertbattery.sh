#!/bin/bash
bat_files="/sys/class/power_supply/BAT0"
bat_status=$(cat ${bat_files}/status)
capacity=$(cat "${bat_files}/capacity")

if [[ ${bat_status}=="Discharging" && ${capacity} -le 5 ]]; then
    echo "Battery alert - ${capacity}%"
    notify-send -u critical \
        --icon=$HOME/.local/share/siarch/icons/empty-battery-64.png \
        "Hibernating ... in 5 seconds" \
        "${capacity}% battery remaining"
    sleep 5
    systemctl suspend-then-hibernate
elif [[ ${bat_status}=="Discharging" && ${capacity} -le 35 ]]; then
    echo "Battery alert - ${capacity}%"
    notify-send -u critical \
        --icon=$HOME/.local/share/siarch/icons/empty-battery-64.png \
        "Low battery" \
        "${capacity}% battery remaining"
fi
