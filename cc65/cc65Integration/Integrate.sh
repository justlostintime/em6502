#!/bin/bash
# before running this script be sure you have installed CC65
# Latest from : http://download.opensuse.org/repositories/home:/strik/Debian_11/ 
# But try your own distribution first, it should be available in most cases
# This is configures for mint/ubuntu/deb distro
sudo mv /usr/share/cc65/asminc/kim1.inc /usr/share/cc65/asminc/kim1saved.inc
sudo mv /usr/share/cc65/include/kim1.h /usr/share/cc65/asminc/kim1saved.h
sudo mv /usr/share/cc65/cfg/kim1.cfg /usr/share/cc65/asminc/kim1saved.cfg
sudo mv /usr/share/cc65/lib/kim1 /usr/share/cc65/asminc/kim1saved
sudo ln ./asminc/kim1.inc /usr/share/cc65/asminc/kim1.inc
sudo ln ./cfg/kim1.cfg /usr/share/cc65/cfg/kim1.cfg
sudo ln ./include/kim1.h /usr/share/cc65/include/kim1.h
sudo ln ./lib/kim1.lib /usr/share/cc65/lib/kim1.lib