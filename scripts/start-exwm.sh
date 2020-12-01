#!/bin/sh

# Run the screen compositor (for background stuff)
compton &

# Enable screen locking on suspend
xss-lock -- slock &

exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
