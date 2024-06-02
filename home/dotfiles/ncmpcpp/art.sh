#!/usr/bin/env sh

# Path to music directory
 MUSIC_DIR="$HOME/Music"
# Path to output cover
COVER="/tmp/cover.png"
COVER_NOTIFICATION="/tmp/cover_notification.png"
# Size of cover
COVER_SIZE=297
# Size in pixel of borders to crop out
CROP_BORDER=20
# Radius or rounded borders
BORDER_RADIUS=10

ffmpeg_cover() {
    ffmpeg -loglevel 0 -y -i "$1" -vf "crop=min(in_w-$CROP_BORDER\,in_h-$CROP_BORDER):out_w,scale=-2:$COVER_SIZE" "$COVER" 
}

rounded_cover() {
    convert -quiet "$COVER" \
     \( +clone  -alpha extract \
        -draw "fill black polygon 0,0 0,$BORDER_RADIUS $BORDER_RADIUS,0 fill white circle $BORDER_RADIUS,$BORDER_RADIUS $BORDER_RADIUS,0" \
        \( +clone -flip \) -compose Multiply -composite \
        \( +clone -flop \) -compose Multiply -composite \
     \) -alpha off -compose CopyOpacity -composite "$COVER" 
}

notification() {
    convert "$COVER" -resize 144x144 "$COVER_NOTIFICATION"
    notify-send -i "$COVER_NOTIFICATION" "$(mpc --format %title% current)"
}

main() {
    file="$MUSIC_DIR/$(mpc --format %file% current)"

    [ -n "$file" ] && ffmpeg_cover "$file" > /dev/null 2>&1 && rounded_cover > /dev/null 2>&1

    notification > /dev/null 2>&1
}

main
