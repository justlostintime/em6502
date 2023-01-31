;
; _open a file, ctmon can only have a single file open at a time
;
         .export         _open
         .import         addysp, popax
         .importzp       sp, tmp2, tmp3

         .include        "errno.inc"
         .include        "fcntl.inc"
         .include        "kim1.inc"        ; actually ctmon65 interface

;=====================================================================
; _open a single file for reading
; int open (const char* name, int flags, ...);  /* May take a mode argument */
;
.PROC   _open

;Throw away any unused parameters,
; On entry y contains the number of bytes passed
        dey                   ; Parm count < 4 shouldn't be needed to be...
        dey                   ; ...checked (it generates a c compiler warning)
        dey
        dey
        beq     parmok        ; Branch if parameter count ok
        jsr     addysp        ; Fix stack, throw away unused parameters

; Parameters ok. Pop the flags and save them into tmp3
parmok:
        jsr     popax           ; Get flags

; Check the flags. We cannot have both, read and write flags set, and we cannot
; open a file for writing without creating it.

        and     #(O_RDWR | O_CREAT)
        cmp     #O_RDONLY       ; Open for reading?
        beq     doread          ; Yes: Branch
        cmp     #(O_WRONLY | O_CREAT)     ; Open for writing?
        beq     dowrite

; Invalid open mode
; Error entry: Set oserror and errno using error code in A and return -1
oserror:
       jsr     popax
       lda     #EINVAL
       jmp     ___directerrno
;=========================================================================
; This opens a file on the SD for reading.  On entry,
; X (MSB) and Y (LSB) point to a null-terminated
; filename to open.  On return, C is clear if the file
; is open, or C set if an error (usually means the
; file does not exist.
doread:
; Get the filename from stack and parse it. Bail out if is not ok
        jsr     popax           ; Get name
        tay
        jsr     DISKOPENREAD
        bcc     GoodEvent
        lda     #ENOENT
        jmp     ___directerrno

dowrite:
        jsr     popax
        tay
        jsr     DISKOPENWRITE
        bcc     GoodEvent
        lda     #ENOENT
        jmp     ___directerrno


GoodEvent:
        ; Done. Return the handle in a/x

        lda     #3              ; Handle is always 3
        ldx     #0
        stx     ___oserror      ; Clear __oserror
        rts

.endproc