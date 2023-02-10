;
; Close the file, ctmon65 can only support a single open file
; it always has the file descriptor 3
; int __fastcall__ close (int fd);
;
        .setcpu         "65C02"
        .export         _close

        .importzp       tmp2
        .include        "errno.inc"
        .include        "ctmon65.inc"

.proc   _close

        cmp             #3            ; we only support a single open file #3
        bne             chkpio        ; LOOK FOR PIO INTERFACE or console
        jsr             DISKCLOSE     ; close the file

closegood:
        lda     #0              ; return no error
        ldx     #0
        stx     ___oserror      ; Clear __oserror
        rts
chkpio:
        cmp     #5              ; we should never be more than 4
        bcc     closegood       ; if less than we never close console or pio

BadDescriptor:
        lda     #EBADF
        jsr     ___directerrno
        rts



.endproc
