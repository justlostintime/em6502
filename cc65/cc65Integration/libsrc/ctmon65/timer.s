;
; this provides the timer function used by all other calls for
; wait, sleep, etc
      .setcpu         "65C02"
      .import        _piowrite, _pioread,pusheax,ldeax0sp,incsp4
      .importzp       ptr1, ptr2, ptr3, sreg
      .include        "errno.inc"
      .include        "fcntl.inc"
      .include        "ctmon65.inc"    ; this is mapped to the ctmon65.h file

      .export        _init_timer
      .export        _get_timer
      .export        _get_timer_tick_length

.DATA
ticklength:   .byte 0                ; this is the external timer parameter see ctmon65h
timervalue:   .byte 0,0,0,0
settimerreq:  .byte $1E,$06
settimerrsp:  .byte 00,00
timerlengthtable: .word 0,10,20,30,40,50,100,250,500,1000

.CODE
; int __fastcall__ get_timer_tick_length()
; return the value of the tick as an integer
; tick is in the format of the start timer request see ctmon65.h
.proc _get_timer_tick_length
    lda   ticklength
    asl
    tay
    lda   timerlengthtable,y
    ldx   timerlengthtable+1,y
    rts
.endproc

; unsigned long __fastcall__ get_timer()
; returns the current timer value
.proc _get_timer

; example return long from c code
; return 0x67891234;
;ldx     #$12
;lda     #$89
;sta     sreg
;lda     #$67
;sta     sreg+1
;lda     #$34
;rts
;debug print called message
;    jsr    PUTSIL
;    .byte  "_get_timer called ",$0a,$0d,0
    ldx     timervalue+1
    lda     timervalue+2
    sta     sreg
    lda     timervalue+3
    sta     sreg+1
    lda     timervalue
    ldy     #0
    rts
.endproc

; int __fastcall__ init_timer(int clocktick)
; Setup the pio timer function
; set the correct vectors to call this
; setup a 4 byte timervalue
; Setup a list of callback functions

.proc _init_timer
      sta   settimerreq+1              ; save the actual tick value
      sta   ticklength                 ; save parameter to read back
      lda   #<(_IRQCALL)
      sta   IRQVEC
      lda   #>(_IRQCALL)
      sta   IRQVEC+1

      lda   #<(settimerreq)
      sta   ptr1
      lda   #>(settimerreq)
      sta   ptr1+1
      lda   #2
      sta   ptr2
      lda   #0
      sta   ptr2+1
      jsr   _piowrite                 ; send the buffer with the request
;      jsr   PUTSIL
;      .byte "Start time request sent",$0a,$0d,0
      lda   #1
      sta   ptr2
      lda   #0
      sta   ptr2+1
      lda   #<(settimerrsp)
      sta   ptr1
      lda   #>(settimerrsp)
      sta   ptr1+1
      jsr   _pioread
;      jsr   PUTSIL
;      .byte "Start time Response recieved",$0a,$0d,0
      lda   settimerrsp
      cmp   #$82                  ; chck fo an ack, not nak
      beq   done
;      jsr   PUTSIL
;      .byte "Error Start time request",$0a,$0d,0
      lda   #$0
      ldx   #$0
      ldy   #$0
      stx     ___oserror      ; Clear __oserror
      rts
done:
       lda  settimerreq+1       ; if 0 then turn irq off
       cmp  #0
       bne  enableirq
       sei                                    ; disable IRQVEC
       bra  irqexit
enableirq:
       cli                                    ; enable interupts
;      jsr   PUTSIL
;      .byte "Timer Started no error",$0a,$0d,0
irqexit:
      ldy   #0
      lda   #$ff
      ldx   #$ff
      sty   ___oserror      ; Clear __oserror
      rts
.endproc

.proc _IRQCALL
    inc  timervalue
    bne  done
    inc  timervalue+1
    bne  done
    inc  timervalue+2
    bne  done
    inc  timervalue+3
done:
    rti
.endproc
