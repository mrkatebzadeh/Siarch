#!/usr/bin/env bash

#-------------------------------#
# Display current cover         #
#-------------------------------#

source "`ueberzug library`"

COVER="/tmp/cover.png"
WIDTH=$(tmux display -p '#{pane_width}')
HEIGHT=$(tmux display -p '#{pane_height}')
COVERSIZE=$(identify /tmp/cover.png | awk '{print $3}')
COORD=(${COVERSIZE//x/ })
X_COVER=${COORD[0]}
Y_COVER=${COORD[1]}
X_PADDING=$(((WIDTH - X_COVER)/2))
Y_PADDING=$(((HEIGHT - Y_COVER)/2))

X_PADDING=2
Y_PADDING=2
function add_cover() {
    ImageLayer::add [identifier]="cover" [x]="$X_PADDING" [y]="$Y_PADDING" [path]="$COVER" 
}

function remove_cover() {
    ImageLayer::remove [identifier]="cover" 
} 

function you_wait() {
    while inotifywait -q -q -e close_write "$COVER"; do
        add_cover
    done
}

clear

if [ ! -f "$COVER" ]; then
    cp "$HOME/.config/ncmpcpp/default_cover.png" "$COVER"
fi

ImageLayer 0< <(
    add_cover
    you_wait
)
