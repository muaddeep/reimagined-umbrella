#!/bin/sh

CURRENT_DIR=$(pwd)
CONFIG_DIR=$CURRENT_DIR/configs

CP_COMMAND="cp -r"
if rsync -h  > /dev/null 2>&1; then
    CP_COMMAND="rsync -r --existing"
fi

if [ ! -d "$CONFIG_DIR" ]; then
    echo "$CONFIG_DIR - no such file or directory"
    exit
fi

ls $CONFIG_DIR | while read i; do
    echo $CP_COMMAND $HOME/.$i $CONFIG_DIR/$i
    $CP_COMMAND $HOME/.$i $CONFIG_DIR/$i
done

