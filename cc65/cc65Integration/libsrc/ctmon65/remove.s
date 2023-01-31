;
; int __fastcall__ __sysremove(char *filename);
;
         .export         _remove
         .import         addysp, popax
         .importzp       sp, tmp2, tmp3

         .include        "errno.inc"
         .include        "fcntl.inc"
         .include        "kim1.inc"        ; actually ctmon65 interface

; On entry to this function a LSB  x MSB
; Upon calling bios interface
; X (MSB) and Y (LSB) point to a null-terminated
; filename to open.  On return, C is clear if the file
; is open, or C set if an error (usually means the
; file does not exist.
.proc    _remove
         tay
         jsr       DISKRMFILE
         bcc       removeNoError
         lda       #ENOENT
         jmp     ___directerrno  ; Set _errno, clear __oserror, return -1
         
removeNoError:
         ldx       #0
         tax
         rts
.endproc

