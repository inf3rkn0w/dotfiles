#!/bin/bash

INIT_EL=~/.emacs.d/init.el

# Get rid of backup file.
if [ -f "$INIT_EL.old" ]; then
    rm "$INIT_EL.old"
fi

# Move current file to backup file.
if [ -f "$INIT_EL" ]; then
    mv "$INIT_EL" "$INIT_EL.old"
fi

# Move source controlled file to operational init.el file.
if [ -f "./init.el" ]; then
    mv "./init.el" "$INIT_EL"
fi
