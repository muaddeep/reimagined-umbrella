#!/bin/sh

CURRENT_DIR=$(pwd)
CONFIG_DIR=$CURRENT_DIR/configs

if [ ! -d "$CONFIG_DIR" ]; then
    echo "$CONFIG_DIR - no such file or directory"
    exit
fi

ls $CONFIG_DIR | while read i; do
    echo "copying $CONFIG_DIR/$i to $HOME/.$i";
    cp $CONFIG_DIR/$i $HOME/.$i
done

