#!/bin/bash
# Change the offset to match your executables start address
# Change the Paths to match where ever you want to put the hex output
# On linux and Windows requires srecord is installed
#
srec_cat ../../../bin/piafun.raw -binary -offset 0x0200  -Address_Length 2 -o piafun.hex -Intel
mv -f *.hex ../../../drives/disk0/piafun.hex