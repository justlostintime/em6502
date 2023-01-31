This directory provides the needed integeration to cc65
runtime environment

Before running the included integration.sh bash script
please be sure to install cc65 from your distribution

if you are unable to find it there then you may install it from
Latest from : http://download.opensuse.org/repositories/home:/strik/Debian_11/

or from source from
https://github.com/cc65/cc65
with

git clone https://github.com/cc65/cc65.git
cd cc65
make
sudo make install

should do it.

Also to produce the hex loadable by the ctmon65 monitor
you need to install srecord from your distributions repository
sudo apt install srecord

The integration.sh script will replace kim1 target with the ctmon65 target
by substituting kim1 for ctmon65. this is required as the targets are hard coded into
cc65 so we masquerade as a kim1 target!.

A simple example program is available in the cc65/example directory
run 
make
Convert2Hex.sh

this will place the outputed into the emulators
drives/disk0 directory
and can be loaded from the monitor with
L hello.hex
then run with
J 0200

Or in the emulator just activate the helloworld emulation

