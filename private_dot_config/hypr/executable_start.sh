#! /usr/bin/env bash

swww init &
swww img ~/Pictures/northofireland.jpeg &

waybar &

dunst

wlsunset -s 04:00 
wlsunset -S 20:00 
wlsunset -t 4000  
wlsunset -l 52.6 -L -8.6
wlsunset 
