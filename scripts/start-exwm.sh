#!/bin/sh

compton &

exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
