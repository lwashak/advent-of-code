#!/usr/bin/env bash

# GHC Options
echo "-Wall" >> $HIE_BIOS_OUTPUT
echo "-dynamic" >> $HIE_BIOS_OUTPUT

# Dynamically load all modules from ./Utils/
echo "-i Utils" >> $HIE_BIOS_OUTPUT
printf '%s\n' $(ls Utils/*.hs) | sed 's/^\(\w*\/\)*\(\w\+\)\.hs$/Utils.\2/g' >> $HIE_BIOS_OUTPUT

# Deps
echo "hie-bios.sh" >> $HIE_BIOS_DEPS
echo "hie.yaml" >> $HIE_BIOS_DEPS

