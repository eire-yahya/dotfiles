#!/usr/bin/env bash

swww init &
swww img ~/pics/reis.jpg --resize no &

hyprctl hyprsunset temperature 3000
pkill waybar
waybar &

dunst
