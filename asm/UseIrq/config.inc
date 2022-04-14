      Processor 6502
;*********************************************************
; FILE: config.inc
;
; General configuration file
;*********************************************************
;
FALSE		equ	0
TRUE		equ	!FALSE
;
; SS-50 bus constants
;
IO_BASE		equ	$E000
IO_SIZE		equ	16
;
; Memory usage
;
ZERO_PAGE_START	equ	$00f0
ROM_START	equ	$f000
RAM_START	equ	$df00
;
; If enabled, turn on buffered input code.
;
BUFFERED_INPUT	equ	FALSE
;
MAX_ARGC	equ	5
;
; If enabled, the debugger will display the flag register
; in ASCII.  Nice, but takes more code.
;
FULL_STATUS	equ	TRUE
;
; Enable EXTENDED_CMDS to allow linking external commands
; to the command handler.
;
EXTENDED_CMDS	equ	FALSE
;
; Define to enable SD related functions
;
SD_ENABLED	equ	TRUE
;
; Size of the keyboard buffer
;
BUFFER_SIZE	equ	132
;==============================================
; Device specific information
;=============================================
;Timer based upon emulator system clock
SysTime        equ $E020                              ; write 1 to enable 0 to disable
SysTime_Period equ SysTime+1                          ; Write the timeout period 2 bytew Low/high order
SysTime_Ack    equ SysTime+3                          ; Set to one if irq pending, write any value to ACK irq

;Timer based upon instruction cycle clock
CycleTime        equ $E030                            ; write 1 to enable 0 to disable
Cycletime_Period equ Cycletime+1                      ; Write the timeout period 2 bytes Low/high order
CycleTime_Ack    equ CycleTime+3                      ; Set to one if irq pending, write any value to ACK irq


