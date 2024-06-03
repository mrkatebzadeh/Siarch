#!/bin/bash

DEVICES="$(system_profiler SPBluetoothDataType -json -detailLevel basic 2>/dev/null | jq '.SPBluetoothDataType' | jq '.[0]' | jq '.device_connected' | jq '.[] | to_entries[] | select(.value.device_minorType == "Headphones")')"

if [ "$DEVICES" = "" ]; then
	echo ""
else
	LEFT="$(echo $DEVICES | jq '.value.device_batteryLevelLeft' | tr -d \")"
	RIGHT="$(echo $DEVICES | jq '.value.device_batteryLevelRight' | tr -d \")"

	if [ $LEFT = 0 ]; then
		LEFT="-"
	fi

	if [ $RIGHT = 0 ]; then
		RIGHT="-"
	fi

	echo "􀲌 ${LEFT} 􀹫  $RIGHT 􀲋"

fi
