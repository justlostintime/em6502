;
; _open a file, ctmon can only have a single file open at a time
;
         .setcpu         "65C02"
         .export         _open
         .import         addysp, popax, popptr1, pushax
         .importzp       sp, tmp2, tmp3, ptr1, ptr2

         .include        "errno.inc"
         .include        "fcntl.inc"
         .include        "ctmon65.inc"        ; actually ctmon65 interface

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
        jsr     addysp        ; Fix stack, throw away unused parameters mode

; Parameters ok. Pop the flags and save them into tmp3
parmok:
        jsr     popax           ; Get flags

; Check the flags. We cannot have both, read and write flags set, and we cannot
; open a file for writing without creating it.

        and     #(O_RDWR | O_CREAT)       ; Mask for io operations
        cmp     #O_RDONLY                 ; Open for reading?
        beq     doread                    ; Yes: Branch
        cmp     #O_RDWR                   ; Open for read then write/ will be read
        beq     doread                    ; if readwrite then do read
        cmp     #(O_WRONLY | O_CREAT)     ; Open for writing/create?
        beq     dowrite
        cmp     #O_WRONLY                 ; open for append write
        beq     dowrite

; Invalid open mode
; Error entry: Set oserror and errno using error code in A and return -1
oserror:
       jsr     popax                      ; Pop the file name to open
       lda     #EINVAL
       jmp     ___directerrno
;=========================================================================
; This opens a file on the SD for reading.  On entry,
; X (MSB) and Y (LSB) point to a null-terminated
; filename to open.  On return, C is clear if the file
; is open, or C set if an error (usually means the
; file does not exist.
doread:
        jsr     checkdevice
        lda     #4
        cmp     tmp3
        beq     GoodEvent

; Get the filename from stack and parse it. Bail out if is not ok
        jsr     popax               ; Get name
        tay
        jsr     DISKOPENREAD
        bcc     GoodEvent
        lda     #ENOENT
        jmp     ___directerrno

dowrite:
        jsr     checkdevice
        lda     #4                   ; is it a pio device arduino
        cmp     tmp3
        beq     GoodEvent

OpenFileName:
        jsr     popax
        tay
        jsr     DISKOPENWRITE
        bcc     GoodEvent
        lda     #ENOENT
        jmp     ___directerrno


GoodEvent:
        ; Done. Return the handle in a/x
        jsr     popax           ; discard the file name again
        lda     tmp3            ; Handle is always 3 if a file
        ldx     #0
        stx     ___oserror      ; Clear __oserror
        rts

.endproc

piodevice:  .byte  "/dev/pio"

.proc  checkdevice

       jsr      popptr1              ; get thefile name
       lda      ptr1
       ldx      ptr1+1
       jsr      pushax               ; put it back onto the stack

       ldy      #0
       lda      (ptr1),y
       ;jsr      OUTCHR               ; debug output the character
       cmp      #'/'
       beq      devpath
       ;jsr      PUTSIL               ; debug
       ;.byte    $0A,$0D,"Open Request is a disk file",$0A,$0D,$00   ; debug
       lda      #3                   ; set default file id to 3 - disk file
       sta      tmp3
       rts
devpath:
       ;jsr      PUTSIL              ; debug
       ;.byte    $0A,$0D,"The file name may be a device",$0A,$0D,$00 ; debug
       lda      #>(piodevice)
       sta      ptr2+1
       lda      #<(piodevice)
       sta      ptr2

       ldy      #1
       ldx      #7
devloop:
       lda      (ptr1),y
       ;jsr      OUTCHR              ; debug
       cmp      (ptr2),y
       bne      devdone
       iny
       dex
       bne      devloop
       lda      #4                   ; set device file id = 4 - pio
       sta      tmp3

devdone:
       rts

.endproc
