;
; int __fastcall__ read (int fd, void* buf, unsigned count);
;
      .import         popax, popptr1, _diskupdateptr,_diskwritedebug, _pioread
      .importzp       ptr1, ptr2, ptr3, tmp1

      .include        "errno.inc"
      .include        "fcntl.inc"
      .include        "ctmon65.inc"    ; this is mapped to the ctmon65.h file

.export         _read

.proc           _read

        sta     ptr3
        stx     ptr3+1           ; Count in ptr3

        sta     ptr2
        stx     ptr2+1           ; Increment and store in ptr2

        inc     ptr2
        inc     ptr2+1

getNoInc:
        jsr     popptr1          ; Buffer address in ptr1
        jsr     popax            ; File Descriptor
        cmp     #3               ; file descriptor three is read disk
        beq     ReadFromDisk
        cmp     #4
        beq     readfrompio

begin:  dec     ptr2
        bne     getch
        dec     ptr2+1
        beq     done             ; If buffer full, return

getch:  jsr     INTCHR           ; Get character using Monitor ROM call
        jsr     OUTCHR           ; Echo it
        and     #$7F             ; Clear top bit
        cmp     #$07             ; Check for '\a'
        bne     chkcr            ; ...if BEL character
        ;jsr     BEEP             ; Make beep sound TODO
chkcr:  cmp     #$0D             ; Check for '\r'
        bne     putch            ; ...if CR character
        lda     #$0A             ; Replace with '\n'
        jsr     OUTCHR           ; and echo it

putch:  ldy     #$00             ; Put char into return buffer
        sta     (ptr1),y
        inc     ptr1             ; Increment pointer
        bne     begin
        inc     ptr1+1
        bne     begin

done:   lda     ptr3
        ldx     ptr3+1
        rts                      ; Return count
;
;===========================================================
; Read from an open disk file
; On entry to bios, A contains the number of bytes to read
; from the file, X (MSB) and Y (LSB) points to the
; buffer where to put the data.  On return, C will
; be set if EOF was reached (and no data read), or
; C will be clear and A contains the number of bytes
; actually read into the buffer.
ReadFromDisk:
        jsr    _diskreaddebug
        lda     #0
        sta     ptr3
        sta     ptr3+1            ; we actually have to count the bytes we read
        dec     ptr2              ; have to readjust as not console out
        dec     ptr2+1
readloop:
        lda     ptr2+1            ; check if more than 256 bytes left to read
        bne     readblock
        lda     ptr2              ; no more bytes left to write
        beq     done              ; Nothing left to write
        bne     readdata          ; read whatever is left to write
readblock:
        lda     #$FF              ; read a block of 255 bytes
readdata:
        ldx     ptr1+1
        ldy     ptr1
        jsr     DISKREAD
        bcs     readdone          ; eof read
        sta     tmp1              ; Save the number of bytes read
        clc
        adc     ptr3              ; update the actual bytes read
        sta     ptr3
        lda     #0
        adc     ptr3+1
        sta     ptr3+1
        jsr     _diskupdateptr    ; update the buffer pointer to next byte
        jmp     readloop

readdone:
        lda     ptr3              ; return the number of bytes read, 0 = eof
        ldx     ptr3+1
        rts
;
;===================================================================================
;
readfrompio:

        jsr     _diskwritedebug
;        jsr     PUTSIL                                  ; debug
;        .byte   $0a,$0d,"Reading from pio",$0a,$0d,"Buffer Address :",$00  ; debug
;       
;        lda     ptr1+1
;        jsr     HEXA
;        lda     ptr1
;        jsr     HEXA
;        jsr     PUTSIL
;        .byte   $0a,$0d,$00
        
        dec     ptr2              ; need to readjust as not console out
        dec     ptr2+1
        jsr     _pioread
        jmp     readdone

.endproc

.proc _diskreaddebug
      jsr _diskwritedebug
.endproc
