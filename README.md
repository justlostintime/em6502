# 6502/65C02 Emulator/ Development Environment
This emulator Development environment is written in gambas3
you will need the latest gambas build from gambas daily builds or 3.18
The upgrade to 3.18 happened again because of bytecode updates and syntax updates and range checking
that have been added to Gambas3. There also seems to be a good bit of performance improvement as well.

Gambas 3.18 can be found at:

Ubuntu the packages for ppa gambas can be found at
https://launchpad.net/~gambas-team/+archive/ubuntu/gambas3

Debian the package can be installed from repository:
deb http://http.us.debian.org/debian/ bookworm main

opensuse 15.4 at : 
https://download.opensuse.org/repositories/home:/munix9/15.4/x86_64/gambas3-3.18.0-lp154.11.1.x86_64.rpm
Opensuse tumbleweed at:
https://download.opensuse.org/repositories/home:/munix9/openSUSE_Tumbleweed/x86_64/gambas3-3.18.0-11.1.x86_64.rpm

or :
https://download.opensuse.org/repositories/home:/munix9/openSUSE_Tumbleweed/aarch64/gambas3-3.18.0-11.2.aarch64.rpm

Arch/Manjaro at and built from
git clone https://gitlab.com/gambas/gambas.git
or
https://gitlab.com/gambas/gambas/-/archive/3.18.0/gambas-3.18.0.tar.bz2

# em6502 can be updated and installed from following repository for Deb/Mint/Ubuntu
```
sudo -i
curl -s --compressed "https://raw.githubusercontent.com/justlostintime/ppa/main/ubuntu/KEY.gpg" | gpg --dearmor > /etc/apt/trusted.gpg.d/westwood-archive-key.gpg
sudo curl -s --compressed -o /etc/apt/sources.list.d/gsh.list "https://raw.githubusercontent.com/justlostintime/ppa/main/ubuntu/gsh.list"
sudo apt update
sudo apt install em6502
```

## Version 1.2.2
Lots of bug fixes and addition, see the commit history.
Itegrated the cc65 c compiler, and created support libraries for ctmon65
Added a number of C program examples, timers, interrupts, dynamic memory allocation etc.

The packages generation for opensuse seems to be a little broken for 15.4
You must use zypper install em6502-1....
Then ignore the unsigned package message.   

## Some version before 1.1.19
Some versions of the emulator release before 1.1.19 may have required a newer version of the Gambas bytecode
than is available in the stable Gambas package. Version 1.1.19 fixes this issue.
Sorry for any issues this caused.

## Special Note 09/04/2022
  As of this week updates to how Gabas3 implements the gui for gtk/Qt a patch in the em6502 code was required, please
  Download the latest version.
  
## NEW
  Added full CLI interface support, possible to directly run a computer config without the gui.
  '''
  em6502 -c Default
  '''
  Added profiling at the instruction level, look under new debug menu. the profiling displays the total 
  cycle count for each executed instruction in your program.
  
## Adding this PPA to your system
You can update your system with this PPA by adding 
ppa:gambas-team/gambas-daily to your system's Software Sources. (Read about installing)
```
   sudo add-apt-repository ppa:gambas-team/gambas-daily
   sudo apt-get update
   
   sudo apt install gambas3
```
Symbolic debugging, memory monitoring etc.

Everything one needs to develop 6502 applications\
It includes a portable system monitor and tiny basic.\
Corsham Techologies, LLC 6502 monitor CTMON65\
CTMON65 rev 0.3\
09/20/2018 by Bob Applegate K2UT, bob@corshamtech.com\
Bob's Tiny BASIC v1.0.1

https://github.com/CorshamTech/6502-Tiny-BASIC

The basic and monitor have been modified and extended\
by me. Both are open sourced!\
It also provides emulators for ct65 devices including the sd interface.\
for now.\
This environment will emulate a clock speed of about 3-4mhz at best on a 1.2ghz cpu
![Screenshot at 2022-02-28 12-54-45](https://user-images.githubusercontent.com/2708327/156057767-10a277e2-0128-4bdc-aa26-4d23ec63ec10.png)



