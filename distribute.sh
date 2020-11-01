#!/bin/bash

INIT_EL=~/.emacs.d/init.el
EL_FILES=("init.el" "custom.el")
HAS_INIT_FILE=1

if [ ! -f "./init.el" ]; then
    echo "No init.el file found in working directory."
    HAS_INIT_FILE=0
fi

# Get rid of backup file.
if [ -f "$INIT_EL.old" ] && [ $HAS_INIT_FILE -eq 1 ]; then
    echo "Detected backup init.el file. Clearing..."
    rm "$INIT_EL.old"
fi

# Move current file to backup file.
if [ -f "$INIT_EL" ] && [ $HAS_INIT_FILE -eq 1 ]; then
    echo "Detected init.el file. Moving to backup..."
    mv "$INIT_EL" "$INIT_EL.old"
fi

# Move source controlled file to operational init.el file.
if [ $HAS_INIT_FILE -eq 1 ]; then
    echo "Distributing init.el file."
    cp "./init.el" "$INIT_EL"
fi

# TODO: Add .bashrc movement here and other dotfiles
