;
; Close the file, ctmon65 can only support a single open file
; it always has the file descriptor 3
; int __fastcall__ close (int fd);
;
        .export         _close

        .importzp       tmp2
        .include        "errno.inc"
        .include        "kim1.inc"

.proc   _close

        cmp             #3            ; we only support a single open file #3
        bne             BadDescriptor
        jsr             DISKCLOSE     ; close the file
closegood:
        lda     #0              ; return no error
        ldx     #0
        stx     ___oserror      ; Clear __oserror
        rts

BadDescriptor:
        lda     #EBADF
        jsr     ___directerrno
        rts



.endproc
