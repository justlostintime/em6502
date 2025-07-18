; This is the io blocks and function for reading and writing
; to devices attached to this computer.
; This supports devices mapped at e000 thru efff in slot sizes of 16 byte
;
;=====================================================================
; Device configuration equates
ConsoleID       equ    0
SerialID        equ    [1<<3]
ClockID         equ    [2<<3]
TimerID         equ    [3<<3]
DiskID          equ    [4<<3]

IO_MAX_DEVICES        equ    10
IO_VECT_LEN           equ    6

IO_DEVICE_CLOSED      equ    1    ; The device is not open
IO_DEVICE_INVALID     equ    2    ; The Device number provided is invalid

;========================================================================================
; Uninitialized data segment
                seg.u     TBData

BInVec          ds      2         ; This is used by fuction to vector to current input rtn
BOutVec         ds      2         ; This is used by functions to vector to the current output rtn
BStatVec        ds      2         ; This is used by inteface to read write status/config information
BActiveDevice   db      1         ; the index of the current device block
BActiveDriver   db      1         ; Index of the device driver block

;============================================================================================
                Seg     Code
; IRQ BASIC Code Service RTN Support
SaveIrqReg      db      0         ; Store current setting
IRQStatus       db      0         ; 1 = enabled, 0 = dissabled
IRQPending      db      0         ; Irq recieved, Called at next Basic Line
IRQEntry        db      0,0       ; Basic code offset of IRQ Handler

;============================================================================================
; Define the device interface blocks
DeviceDriverBlocks
ConsoleDevice                           ; Block 0
                dw      ConsoleID       ; device idenifier Console
                dw      cin             ; read function vector
                dw      cout            ; write function vector
                dw      cstatus         ; Get current Status info/Write config

SerialDevice                            ; Block 1
                dw      SerialID        ; device idenifier Serial
                dw      SerialIn        ; read function vector
                dw      SerialOut       ; write function vector
                dw      SerialStatus    ; Get current Status info/Write config

ClockDevice                             ; Block 2
                dw      ClockID         ; Day/date Clock
                dw      ClockRead       ; Read the date from clock
                dw      ClockWrite      ; Set the date/time of clock
                dw      #0              ; No Status/Config Function

TimerDevice                             ; Block 3
                dw      TimerID         ; Timer/interrupt interface
                dw      TimerStart      ; Start the timer
                dw      TimerStop       ; Stop the timer
                dw      TimerStatus     ; Config/read status

DiskDevice      dw      DiskID          ; Block 4
                dw      DIN             ; Disk Input / read function
                dw      DOUT            ; Disk Output / write function
                dw      DSTAT           ; Disk Status/open/close etc information
;
;======================================================================
; Define the Device/Slot,driver control blocks
; entry format: ControlWord,DeviceDriver,PortAddress,StatusWord
; Fixed at 10 devices memory usage is getting pretty big!!!!!!
; Control word : bit 0 = active 1, Free 0

DeviceIoBlocks
                dw      1, ConsoleID, $E000, 0        ; Console device is 0 file
                dw      1, ClockID,   $E010, 0        ; Port for the day/time clock
                dw      1, TimerID,   $E010, 0        ; Timer interface
                dw      1, DiskID,    $E010, 0        ; Disk Driver interface
                dw      1, SerialID,  $E020, 0        ; Second terminal(default Basic debug)
                dw      0, SerialID,  $E030, 0        ; Unused slot
                dw      0, SerialID,  $E040, 0        ; Unused slot
                dw      0, SerialID,  $E050, 0        ; Unused slot
                dw      0, SerialID,  $E060, 0        ; Unused slot
                dw      0, SerialID,  $E070, 0        ; Unused slot
;
;======================================================================
;
                Seg     Code
;======================================================================
; This is the Basic IRQ handler, works with task manager, assumes timer interupt
;
ServiceIrq:     pha
                txa
                pha
                ldx     #0
ServiceLoop:
                inc     timercounter,x
                bne     ServiceCont
                inx
                cpx     #4
                bne     ServiceLoop
ServiceCont:
                lda     IRQStatus
                beq     RetIrq
                lda     IRQPending
                bne     RetIrq
                lda     #1
                sta     IRQPending
RetIrq:          
                pla
                tax
                pla
                rti
;======================================================================
; Jump to the output/input function in BOutVec/BInVec
;
VOUTCH          jmp     (BOutVec)          ; Primary block io vectors
VGETCH          jmp     (BInVec)
VSTAT           jmp     (BStatVec)
;
;======================================================================
; IO Service functions
; Validate the device index and set x to offest in table
; does not return to ioInterface if invalid, returns to original caller
; should be called immediatly after entering the ioInterface call
ioValidateDevice:
                cpx    #IO_MAX_DEVICES
                bcc    ioValidIndex
                ldx    #IO_DEVICE_INVALID
                bcs    ioInvalidDevice
ioValidIndex:
                txa
                asl                           ; Multiply by 8
                asl
                asl
                tax                           ; Point to actual offset in the table
                cpx    BActiveDevice          ; is it already active ?
                beq    ioValidDevice          ; Shortcut if this is the active device already
                lda     #1                    ; Active flag
                and     DeviceIoBlocks,x      ; Check if the device is active
                bne     ioValidDevice         ; The device is active and valid index
                ldx     #IO_DEVICE_CLOSED

ioInvalidDevice:
                pla                           ; Remove return address of IO interface
                pla
                sec                           ; ensure that carry is set
                rts

ioValidDevice:
                clc
                rts
;
;===================================================================================
; Set the io device jmp vectors
; input x contains the vector to the active Device IO Block
; output a, x undefined y unchanged
ioSetDeviceVectors:
                cpx     BActiveDevice         ; Check if already set
                beq     ioSetDevExit          ; if already set then do nothing

                stx     BActiveDevice         ; set the active device vector
                tya
                pha

                ldy     DeviceIoBlocks+2,x   ; Get the device driver index
                sty     BActiveDriver        ; Pointer to active Device driver
                ldx     #0                   ; Transfer the 6 pointers to the Vectors
ioSetDevLoop:
                lda     DeviceDriverBlocks+2,y
                sta     BInVec,x
                inx
                iny
                cpx     #IO_VECT_LEN         ; Transfer the vector length to copy
                bne     ioSetDevLoop

                pla
                tay
ioSetDevExit:
                rts
;
;======================================================================
; Generic call interface for devices
; input x = DeviceIoBlockIndex
; Carry set if error, x contains the error code
; all other parameters are dependant upon the actual device interface
;
; ioPutCH  a contains the character to send
ioPutCH:      jsr ioValidateDevice
              pha
              jsr ioSetDeviceVectors
              pla
              jsr    VOUTCH
              clc
              rts

; io Getch returns the character read from device
ioGetCH:      jsr   ioValidateDevice

              rts
;
;======================================================================
;TTY interface functions,
;      a contains the character to send
;      x contains the Device ID (equals index into io blocks)  of the io block to used
;
SerialIn:

SerialOut:

SerialStatus:
           rts
;
;======================================================================
; Date/Time clock interface
ClockRead:

ClockWrite:
          rts
;
;======================================================================
;
TimerStart:

TimerStop:

TimerStatus:
         rts

