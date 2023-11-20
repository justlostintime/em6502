;This is the timer management functions
; a = 0 turn off irq, stop timer
; a = 1 turn on irq, set parm contained in x
; a = 4 get low value
; a = 2 get high value

timerinterface  equ $E010
cTimerControl   equ $1E
cTimerStop      equ 0
cTimerStart     equ 1
cTimerLow       equ 2
cTimerHigh      equ 4

; il interface to the timer

iTimer
      jsr     popR0
      ldx     R0            ; Set time parameter
      jsr     popR0
      lda     R0            ; control parameter 0-4
      jsr     iTimerif
      sta     R0
      stx     R0+1
      jmp     pushR0nextIl

; Actual system interface to the timer
; x is value 9 = 1 second, 1-5 = value * 10ms 6 = 10ms, 7=250ms, 8=500ms
; a is 0,1,2,4
iTimerif
      cmp  #cTimerLow                ; Do they want the low byte
      bcs  iTimerValue               ; Just get the value we need
      pha                            ; Save the command we will use
      lda  #cTimerControl            ; Load the timer control command
      sta  timerinterface            ; Write it to the timer port
      pla                            ; Get the actual command back
      sta  timerinterface            ; write it to the port
      cmp  #cTimerStart              ; if the command was start timer then write value
      beq  iTimerParm                ; if not then get ack/nak and continue
      sei                            ; Disable the interupts
      jmp  iTimerAck                 ; get ack and exit
      
iTimerParm
      stx  timerinterface            ; Write the program value otherwise
      cli                            ; enable the interupts, this is start/restart timer
      
iTimerAck
      lda  timerinterface            ; get the ack nak
      ldx  #0                        ; the ack value is single byte so pad with x
      rts

iTimerValue                          ; get the value from the offsets provided
      php
      sei
      tax                            ; the control is also the value
      lda  [timercounter-2],x        ; get the high byte of value
      pha
      lda  [timercounter-1],x        ; get the low part of value
      tax
      pla
      plp                            ; restore the interupt flag if it was enabled
      rts



