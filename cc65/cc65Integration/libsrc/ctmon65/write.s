;
; int __fastcall__ write (int fd, const void* buf, int count);
;
      .import         popax, popptr1, _piowrite
      .importzp       ptr1, ptr2, ptr3, ptr4 ,tmp1

      .include        "errno.inc"
      .include        "fcntl.inc"
      .include        "ctmon65.inc"    ; this is mapped to the ctmon65.h file


.export         _write
.export         _diskupdateptr
.export         _diskwritedebug

.proc   _write

        sta     ptr3
        stx     ptr3+1           ; Count in ptr3

        sta     ptr2
        stx     ptr2+1           ; Increment and store in ptr2

        inc     ptr2
        inc     ptr2+1
putNoInc:
        jsr     popptr1         ; Buffer address in ptr1
        jsr     popax           ; get file descriptor
        cmp     #3              ; Check for the one fd number we support for disk
        beq     writetodisk
        cmp     #4              ; Write to pio
        beq     writetopio

begin:  dec     ptr2
        bne     outch
        dec     ptr2+1
        beq     done

outch:  ldy     #0
        lda     (ptr1),y
        jsr     OUTCHR          ; Send character using Monitor call
;       cmp     #$07            ; Check for '\a'
;       bne     chklf           ; ...if BEL character
;jsr    BEEP                    ; Make beep sound
chklf:  cmp     #$0A            ; Check for 'n'
        bne     next            ; ...if LF character
        lda     #$0D            ; Add a carriage return
        jsr     OUTCHR

next:   inc     ptr1
        bne     begin
        inc     ptr1+1
        jmp     begin

done:   lda     ptr3            ; Return count
        ldx     ptr3+1
        rts

;===========================================================
; Call to Diskwrite , A contains the number of bytes to write
; to the file, X (MSB) and Y (LSB) point to the
; buffer where to get the data.  On return, C will
; be set if an error was detected, or C will be clear
; if no error.  Note that if A contains 0 on entry,
; no bytes are written.
writetodisk:
        jsr     _diskwritedebug
        dec     ptr2              ; need to readjust as not console out
        dec     ptr2+1
writeloop:
        lda     ptr2+1            ; check if  256 or more bytes left to write
        bne     writeblock
        lda     ptr2              ; no more bytes left to write
        beq     done              ; Nothing left to write
        bne     writedata         ; Write whatever is left to write
writeblock:
        lda     #255              ; write a block of 255 bytes

writedata:
        ldx     ptr1+1
        ldy     ptr1
        sta     tmp1
        jsr     DISKWRITE
        bcs     writeerror
        jsr     _diskupdateptr
        jmp     writeloop         ; Loop back to see if anything else to write

writeerror:
        pla
        lda     #EIO
        jmp     ___directerrno

writetopio:

        jsr     _diskwritedebug
        dec     ptr2              ; need to re-adjust as not console out
        dec     ptr2+1
        jsr     _piowrite         ; ptr 2 = the length to write
        jmp     done

.endproc


; tmp1 = length, ptr1 = address, ptr2 = remaining length
; sets   carry if count is not zero and clear carry if zero
.proc   _diskupdateptr
        lda     tmp1
        clc
        adc     ptr1              ; Update the buffer pointer to next block
        sta     ptr1
        lda     #0
        adc     ptr1+1
        sta     ptr1+1
        ldy     tmp1
        sec
        lda     ptr2
        sbc     tmp1
        sta     ptr2
        lda     ptr2+1
        sbc     #0
        sta     ptr2+1
        bne     setcarry
        lda     ptr2
        bne     setcarry
        clc
        rts
setcarry:
        sec
        rts
.endproc

.proc   _diskwritedebug
        rts
.endproc
