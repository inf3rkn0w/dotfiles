#!/bin/sh

# Link configurations in place to be read
mkdir -p ~/.emacs.d/exwm # Create if not exists
ln -s ../.emacs.d/exwm.el ~/.emacs.d/exwm/exwm.el
ln -s ../.emacs.d/init.el ~/.emacs.d/init.el

# Startup items to allow boot into EXWM
ln -s ../.emacs.d/EXWM.desktop /usr/share/xsessions/EXWM.desktop
ln -s ./start-exwm.sh ~/.emacs.d/exwm/start-exwm.sh

