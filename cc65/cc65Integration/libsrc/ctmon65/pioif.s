;
; int __fastcall__ read (int fd, void* buf, unsigned count);
;
      .import         popax, popptr1, _diskupdateptr,_diskwritedebug
      .importzp       ptr1, ptr2, ptr3, tmp1

      .include        "errno.inc"
      .include        "fcntl.inc"
      .include        "ctmon65.inc"    ; this is mapped to the ctmon65.h file


      .export         _piowrite
      .export         _pioread

;
;Low level sd/pio/clock/timer arduino card functions
; XPARINITvec     := $f01e
; XPARSETWRITEvec := $f021
; XPARSETREADvec  := $f024
; XPARWRITEvec    := $f027
; XPARREADvec     := $f02a
;
;====================================================================
; On input ptr1 points to buffer to send
;          ptr2 contains length
;
;Used by function
;          tmp1 must be set to length, or one in our case
;          used by _diskupdateptr
;
; on return
;          ptr3 contains number of bytes sent/read

.proc      _piowrite
          jsr   XPARSETWRITEvec
          lda   #1
          sta   tmp1
          ldy   #0
          sty   ptr3
          sty   ptr3+1

piowriteloop:
          lda   (ptr1)
          jsr   XPARWRITEvec
          jsr   _diskupdateptr
          jsr   incptr3
          ldx   ptr2
          bne   piowriteloop
          ldx   ptr2+1
          bne   piowriteloop
          rts
.endproc

.proc     _pioread

;Debug print start and length of request
;         jsr    PUTSIL
;         .byte  "Read pio starts: ",0
;         lda    ptr2+1
;         jsr    HEXA
;         lda    ptr2
;         jsr    HEXA
;         lda    #$20
;         jsr    OUTCHR
         
         jsr    XPARSETREADvec
         lda    #1
         sta    tmp1
         ldy    #0
         sty    ptr3
         sty    ptr3+1
pioreadloop:
         jsr    XPARREADvec
         sta    (ptr1)

;  debug section print character
;         jsr    HEXA
;         lda    #$20
;         jsr    OUTCHR

         jsr    _diskupdateptr
         jsr    incptr3
         ldx    ptr2
         bne    pioreadloop
         ldx    ptr2+1
         bne    pioreadloop
;debug print completeion message
;         jsr    PUTSIL
;         .byte  "Read pio completes ",$0a,$0d,0
         rts
.endproc

.proc     incptr3
          inc ptr3
          bne done
          inc ptr3+1
done:
          rts
.endproc
