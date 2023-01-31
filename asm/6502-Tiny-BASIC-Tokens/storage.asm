;
;=====================================================
;=====================================================
;=====================================================
; This file contains the functions for saving and
; restoring programs from some sort of mass storage
; device.  This particular version is for using the
; Corsham Tech SD Card System.
;=====================================================
;=====================================================
;=====================================================

         seg.u      TBData
diskBufLength       ds    1
diskBufOffset       ds    1
DiskFileName        ds    64

        SEG Code

;
;=====================================================
; Open a file for reading as a program.  The next
; thing on the line should be the filename.
;
iOPENREAD
          if    XKIM || CTMON65
                jsr     setFileName       ;Set the file name to open
                jsr     DiskOpenRead      ;attempt to open file
                bcc     Ropenok           ;branch if opened ok
;
; Open failed
;
Rdfail          ldx     #ERR_READ_FAIL
Rdfail2         lda     #0
                jmp     iErr2
;
; Clear counts and offsets so the next read will
; cause the file to be read.
;
Ropenok         lda     #0
                sta     diskBufOffset
                sta     diskBufLength
                jmp     NextIL
          endif

;===============================================================
; Set file name
setFileName:
                ldy     CUROFF
                lda     (CURPTR),y
                cmp     #tString                        ;Must be a quoted string
                bne     setFileNameNotFound             ;Must be a filename

                clc
                tya
                adc     CURPTR
                sta     R0                              ;LSB
                lda     CURPTR+1
                adc     #0
                sta     R0+1
                lda     #DiskFileName&$ff
                sta     R1
                lda     #DiskFileName>>8
                sta     R1+1
                jsr     qstrcpy                          ; on exit R0 contains the total copy length index accross source not dest
                lda     R0
                clc
                adc     CUROFF                           ; add the current offset
                sta     CUROFF                           ; Update the buffer pointer after complete

                ldy     #DiskFileName&$ff
                ldx     #DiskFileName>>8
                clc
                rts

setFileNameNotFound:
                pla
                pla                                     ; remove the return address from the stack
                lda     #0
                ldx     #ERR_NO_FILENAME
                jmp     iErr2

;
;==============================JUSTLOSTINTIME 08/02/2022========
;Remove a file from the disk
iRMFILE
          if    XKIM || CTMON65
                jsr     setFileName
                jsr     DiskRmFile          ;attempt to remove file
                bcc     wrmOk               ;branch if removed ok
                lda     #0
                ldx     #ERR_FILE_NOT_FOUND
                jmp     iErr2
wrmOk
                jmp     NextIL

        endif
;
;=====================================================
iOPENWRITE
      if        XKIM || CTMON65
                jsr     setFileName
                jsr     DiskOpenWrite       ;attempt to open file
                bcc     Wopenok             ;branch if opened ok
;
; Open failed
;
Wdfail          lda     #0
                ldx     #ERR_WRITE_FAIL
                jmp     iErr2
;
Wopenok         jmp     NextIL
      endif
;
;=====================================================
; Gets a line of input from the disk file and puts it
; into LINBUF.
;
; On exit:
;    CURPTR points to LINBUF
;    LINBUF contains the line with 0 at the end.
;    Y has offset to first non-space character
;    CURROFF has the same as Y.
;
iDGETLINE
    if      XKIM || CTMON65
                ldx     #LINBUF&$ff
                stx     CURPTR
                ldx     #LINBUF>>8
                stx     CURPTR+1
;
                ldx     #0                    ;offset
iDgetLoop       stx     getlinx
                jsr     getNextFileByte
                bcs     iGetEOF
                cmp     #CR
                beq     iGetEOL
                cmp     #LF
                beq     iGetEOL
                ldx     getlinx
                sta     LINBUF,x
                inx
                bne     iDgetLoop
;
; Handle end of line.  If the line has nothing, loop
; back and get another line.
;
iGetEOL         ldx     getlinx               ;blank line?
                beq     iDgetLoop             ;yes, ignore it
;
; This can fall through when there is a line, or
; called directly when EOF is encountered.
;
iGetEOF         ldx     getlinx
                lda     #0
                sta     LINBUF,x
                ldy     #0
                jsr     SkipSpaces
                jsr     ParseInputLine
                lda     #TOKENBUFFER&$ff
                sta     CURPTR
                lda     #TOKENBUFFER>>8
                sta     CURPTR+1
                lda     #1
                sta     CUROFF
                jmp     NextIL
    endif

;
; THIS IS CALLED TO DISPLAY THE CONTENTS OF THE
; DISK
;
iDDIR
          if    XKIM || CTMON65
                jsr     DiskDir
;
; Get/Display each entry
;
DiskDirLoop     ldx     #DiskFileName>>8              ;pointer to buffer
                ldy     #DiskFileName&$ff
                jsr     DiskDirNext                   ;get next entry
                bcs     DiskDirEnd                    ;carry = end of list
                jsr     puts
                db      "   ",0
; Print the line to the console
                ldx     #DiskFileName>>8              ;pointer to buffer
                ldy     #DiskFileName&$ff
                lda     0
                jsr     PrtStr                        ;else print name
                jsr     crlf

                jmp     DiskDirLoop                   ;do next entry

DiskDirEnd      jmp     NextIL
          endif
;
;=====================================================
; Does a LIST to a Disk file.
;
iDLIST
          if    XKIM || CTMON65
                jsr     SetOutDisk
                jmp     iLST2
          endif
;
;=====================================================
; Closes any pending disk file.  Okay to call if there
; is no open file.
;
iDCLOSE
          if     XKIM || CTMON65
                jsr     DiskClose
                jmp     NextIL
          endif
;
;=====================================================
; This gets the next byte from an open disk file.  If
; there are no more bytes left, this returns C set.
; Else, C is clear and A contains the character.
;
getNextFileByte
          if    XKIM || CTMON65
                ldx     diskBufOffset
                cpx     diskBufLength
                bne     hasdata                 ;branch if still data
;
; There is no data left in the buffer, so read a
; block from the SD system.
;
                lda     #BUFFER_SIZE
                ldx     #buffer>>8
                ldy     #buffer&$ff
                jsr     DiskRead
                bcs     getNextEof
;
; A contains the number of bytes actually read.
;
                sta     diskBufLength               ;save length
                cmp     #0                          ;shouldn't happen
                beq     getNextEof
;
                ldx     #0
hasdata         lda     buffer,x
                inx
                stx     diskBufOffset
                clc
                rts
;
getNextEof      lda     #0
                sta     diskBufOffset
                sta     diskBufLength
                sec
                rts
;
;=====================================================
; Set output vector to the disk output function
;
SetOutDisk      lda     #DOUT&$ff
                sta     BOutVec
                lda     #DOUT/256
                sta     BOutVec+1
                rts
;
;=====================================================
; input a contains charater to write to open file
; output:
;          C flag clear if no error
;
DOUT            stx    DiskFileName             ; Save the x value, fulename not used
                sty    DiskFileName+1           ; Save the y value  filename not actually used
                sta     buffer                  ; Store the byte to send into the buffer 
                lda     #1                      ; set number of bytes to send to 1
                ldy     #buffer&$ff             ; Load the low order address of buffer to y
                ldx     #buffer>>8              ; Load the high order address of buffer to x
                jsr     DiskWrite               ; Place the character to disk if a file is open
                ldx     DiskFileName            ; Restore the x value that was saved
                ldy     DiskFileName+1          ; Restore the y value saved
                rts
;=======================================================
; output:
;        c flag is clear if no error, a contains bytes read
;        c flag set Reached eof, a undefined
;
DIN             stx    DiskFileName             ; Save the x value, filename not used just storage
                sty    DiskFileName+1           ; Save the y value  filename not actually used
                lda     #1                      ; set number of bytes to read to 1
                ldy     #buffer&$ff             ; Load the low order address of buffer to y
                ldx     #buffer>>8              ; Load the high order address of buffer to x
                jsr     DiskRead
                lda     buffer                  ; Get the byte just read
                ldx     DiskFileName
                ldy     DiskFileName+1
                rts

;========================================================
; Dstat / open/close/stat files
DSTAT
          rts
;========================================================
          endif