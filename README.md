# 6502Emulator-DevEnv
This emulator Development environment is written in gambas3
you will need the latest gambas build from gambas daily builds or 3.17

## NEW
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

Everything one needs to develope 6502 applications\
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
![6502emulator](https://user-images.githubusercontent.com/2708327/154163068-837ff439-4a15-4a51-9040-f2183fc6d24f.png)


