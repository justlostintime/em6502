;
;=====================================================
;=====================================================
;=====================================================
; This marks the start of support functions used by
; the IL opcodes.  These are support functions, NOT
; the IL code.
;=====================================================
;GOSUBSTACKSIZE  equ     16        ;Depth of gosub nesting
;=====================================================
                Seg Code
;=====================================================
; This gets the next two bytes pointed to by ILPC and
; returns them; X contains LSB, A contains MSB.  ILPC
; is advanced by two, and Y contains 0 on return.

;
getILWord       jsr     getILByte           ;LSB
                tax
;
;=====================================================
; This gets the next byte pointed to by ILPC and
; returns it in A.  On return, X is unchanged but Y
; contains 0.
;
getILByte       ldy     #0
                lda     (ILPC),y          ;get byte
                php                       ;save status
                inc     ILPC              ;inc LSB
                bne     getILb2           ;branch if no overflow
                inc     ILPC+1            ;inc MSB
getILb2         plp                       ;restore status
                rts
;
;=====================================================
; Decrement ILPC by one.
;
decIL           lda     ILPC
                bne     decIL2
                dec     ILPC+1
decIL2          dec     ILPC
                rts
;
;=====================================================
; Push the ILPC onto the return stack.  Actually, this
; pushes the address of ILPC+2 since that's the next
; address to execute.
;
pushILPC      ldy     ILSTACKPTR
              cpy     #ILSTACKSIZE<<1
              bcs     pushErr
              lda     ILPC
              clc
              adc     #2
              sta     (ILSTACK),y
              php     ;save C bit
              iny
              lda     ILPC+1
              plp     ;restore C
              adc     #0
              sta     (ILSTACK),y
              iny
              sty     ILSTACKPTR
              clc
              rts
pushErr
              sec
              rts
;
;=====================================================
; Pull the top entry from return stack and put into
; ILPC.
;
popILPC       ldy     ILSTACKPTR
              beq     pushErr
              dey
              lda     (ILSTACK),y
              sta     ILPC+1
              dey
              lda     (ILSTACK),y
              sta     ILPC
              sty     ILSTACKPTR
              clc
              rts
;
;=====================================================
; This searches for a specific line number that is in
; R0.  There are three possible return conditions:
; Line numbers are now the third byte, the first byte is now
; a pointer to the next line, of course no longer than 255 byte
; per line.
;
; Exact match was found:
;    * Z set
;    * CURPTR points to two-byte line number for that
;      line.
;
; Next highest line found:
;    * Z cleared
;    * C set
;    * CURPTR points to two-byte line number for that
;      line.
;
; End of program reached:
;    * Z cleared
;    * C cleared
;    * CURPTR points to first free byte at end of
;      program.  Ie, it has save value as PROGRAMEND.
;
; A, X, and Y are all undefined on return.
;

findLine
                lda     ProgramStart         ;Start of program -> CURPTR
                sta     CURPTR
                lda     ProgramStart+1
                sta     CURPTR+1
;
; At end of code?
;
iXFER1
                lda     CURPTR                ; chk CURPTR = END PROGRAM
                cmp     ProgramEnd            ; at end of program then stop run
                bne     xfer2                 ; not end
                lda     CURPTR+1
                cmp     ProgramEnd+1
                bne     xfer2                 ;Not at end
;
; Line not found and the end of the program was
; reached.  Return Z and C both clear.
;
                lda     #1                    ;clear Z
                clc                           ;clear C
                rts
;
; Check for an exact line number match
;
xfer2           lda     R0
                ldy     #1                ; changed to skip extra length byte
                cmp     (CURPTR),y
                bne     xfernotit
                iny
                lda     R0+1
                cmp     (CURPTR),y
                bne     xfernotit         ; not a matching line number
;
; This is exactly the line we want.
;
                rts                       ;it matches exactly
;
; See if this line is greater than the one we're
; searching for.
;
xfernotit       ldy     #2              ;Changed from to skip leading length and least significat digit
                lda     (CURPTR),y      ;compare MSB first
                cmp     R0+1
                bcc     xfer3
                bne     xfer4
                dey
                lda     (CURPTR),y      ;compare LSB
                cmp     R0
                bcc     xfer3
;
; This line is greater than the one we want, so
; return Z clear and C set.
;
xfer4:          sec             ;We found a line number greater
                rts             ;both conditions set
;
; Not the line (or droid) we're looking for.  Move to
; the next line.
;
xfer3           jsr     FindNextLine
                jmp     iXFER1
;
;=====================================================
; This advances CURPTR to the next line.  If there
; are no more lines, this leaves CURPTR equal to
; ProgramEnd.  Returns CUROFF set to 3.  This assumes
; CURPTR is pointing to a valid line on entry.  This
; pointer points to the two-byte line number.
; Update this points to the 1 byte line length  ****************
;
FindNextLine
                ldy     #3                ;skip line number and length byte
                sty     CUROFF            ;this is the new offset
                ldy     #0
                lda     (CURPTR),y        ;Get the length
                clc
                adc     CURPTR
                sta     CURPTR
                lda     CURPTR+1
                adc     #0
                sta     CURPTR+1
FindNext4       rts
;
;=====================================================
; This compares CURPTR to PROGRAMEND and returns Z set
; if they are equal, Z clear if not.
;
AtEnd           lda     CURPTR
                cmp     ProgramEnd
                bne     atendexit
                lda     CURPTR+1
                cmp     ProgramEnd+1
atendexit       rts
;

;
;=====================================================
; Convert an ASCII string to a number.  On input,
; (CURPTR),Y points to the first digit.  This gets
; digit-by-digit until finding a non-number.  Returns
; Y pointing to the non-digit, and R0 contains the
; number.  This does NOT check for valid ranges, so
; a value like "123456789" will produce something,
; but not what you had expected.
;
getDecimal      lda     #0
                sta     R0
                sta     R0+1
                sta     dpl             ;temporary negative flag
;
; See if it's negative...
;
                ;sty     $0013         Removed as no idea why here JUSTLOSTINTIME
                lda     (CURPTR),y
                cmp     #'-
                bne     getDecLoop
                inc     dpl             ;it's negative
;
getDecLoop      lda     (CURPTR),y
                beq     getDdone         ;Added this incase we hit eol JUSTLOSTINTIME
                cmp     #'0
                bcc     getDdone
                cmp     #'9+1
                bcs     getDdone
                sec
                sbc     #'0             ;convert to binary
                pha
;
; Now multiply R0 by 10.  Remember that
; 2*N + 8*N = 10*N.
;
                asl     R0
                rol     R0+1            ;*2
                lda     R0
                sta     R1
                lda     R0+1
                sta     R1+1
                asl     R0
                rol     R0+1            ;*4
                asl     R0
                rol     R0+1            ;*8
                clc                     ;now add the partial sums...
                lda     R0              ;...to get *10
                adc     R1
                sta     R0
                lda     R0+1
                adc     R1+1
                sta     R0+1
;
; Add in the new digit
;
                pla
                clc
                adc     R0
                sta     R0
                bcc     getD2
                inc     R0+1
;
; Move to next character
;
getD2           iny
                bne     getDecLoop
;
; All done with digits, so now deal with it being
; negative.  If zero, then don't check for negative
; flag.  Ie, -0 is stored as 0.
;
getDdone        lda     R0
                ora     R0+1
                beq     getDone2            ;zero
                lda     dpl
                beq     getDone2            ;positive
;
; Invert all the bits, then add one.
;
                lda     R0
                eor     #$ff
                sta     R0
                lda     R0+1
                eor     #$ff
                sta     R0+1
;
                inc     R0
                bne     getDone2
                inc     R0+1
getDone2
; removed next few lines as no idea why they are here JUSTLOSTINTIME
                ;lda     R0
                ;sta     $0010
                ;lda     R0+1
                ;sta     $0011
                ;lda     dpl
                ;sta     $012

                rts

;=====================================================
; Gets a line of input into LINBUF.
;
; On entry:
;    A contains the prompt character, or 0 if none.
;    X = 1 Background read
;    x = 0 Forground read with wait
;
; On exit:
;    CURPTR points to LINBUF
;    LINBUF contains the line with 0 at the end.
;    Y has offset to first non-space character
;    CURROFF has the same as Y.
;
GetLine         jsr     ReadPrompt
                cpx     #0
                beq     GetLineRetry
                ldx     taskPtr
                lda     taskTable,x
                and     #TASKWAITIO     ;Task Active and waiting for IO
                bne     taskWaitingIO
                ora     #TASKWAITIO     ;Mark Task as waiting for IO
                sta     taskTable,x     ;Mark the state for task as waiting io
                dec     taskWaitingIO   ;Start polling the input and make task wait
                beq     taskWaitingIO   ;Get out of here and wait for io to complete

;
; Now read a line and wait for the CR
;
GetLineRetry
                lda     #0                ;Wait for input to complete
                jsr     ReadLine

;
; Point to the line we just read
; Set the current pointer to point to the input line
;
ReadComplete    ldy     #0
                sty     CUROFF
                ldx     #LINBUF&$ff
                stx     CURPTR
                ldx     #LINBUF>>8
                stx     CURPTR+1
;
; Output a CR/LF
;
                jsr     CRLF
;
; If a blank line, prompt again.
;
                jsr     SkipSpaces
                lda     (CURPTR),y
                bne     GetLineDone            ;We have data then exit
                jsr     ReadPromptRetry
                ldx     taskPtr                ;if this task is waiting for IO
                lda     taskTable,x            ;then get out, wait for line to
                and     #TASKWAITIO            ;Complete again
                bne     taskWaitingIO
                jmp     GetLineRetry           ;If the IO is wait then jump to start

GetLineDone
                ldx     taskPtr
                lda     #TASKACTIVE
                sta     taskTable,x          ;IO is complete

taskWaitingIO
                rts

;
;=======================================================================
; Display the prompt character
; On entry
;          A contains the prompt character
; On exit
;          The readbuffer index is reset to 0
;
ReadPrompt      sta     promptChar

;
; Prompt
;

ReadPromptRetry lda     promptChar
                ora     #0                ;any prompt?
                beq     getlinenp
                jsr     VOUTCH
                lda     #$20
                jsr     VOUTCH             ;Space after prompt
;
getlinenp       ldx     #0                ;offset into LINBUF
                stx     getlinx
                rts
;
;===============================================================
; This fuction is the driver for the line input
; on call if a = 0 then it waits for all input
;            a = 1 then nowait for input
; On exit
;                     c clear if not complete line
;                     c set if it was a complete line

ReadLine
                sta     inputNoWait
                cmp     #0
                beq     getline1
                jsr     ISCHAR           ; if there is no character just get out
                beq     GetLineNoWait
getline1        jsr     VGETCH
        if  CTMON65
                pha
                jsr     VOUTCH              ;echo echo echo
                pla
        endif
                cmp     #CR
                beq     getlind           ;end of line
                cmp     #BS               ;backspace?
                beq     getlinebs
                ldx     getlinx
                sta     LINBUF,x
                inx
                stx     getlinx
                lda     inputNoWait
                beq     getline1
                bne     GetLineNoWait
;
; CR was hit
;
getlind         lda     #0                  ; set the end pf buffer
                ldx     getlinx
                sta     LINBUF,x

                sec                         ; Carry set then cr received
                rts

GetLineNoWait
                clc                         ; Carry clear no end of line
                rts
;
; Backspace was hit
;
getlinebs       ldx     getlinx
                beq     getlineEOL          ;at start of line
                dex
                stx     getlinx
getlinepbs      jsr     puts
                db      27,"[K",0
                jmp     getline1
getlineEOL      lda     #SPACE
                jsr     VOUTCH
                bne     getlinepbs
;
;=====================================================
; Count the length of the line currently in LINBUF
; starting at offset Y.  Returns the length in X.  The
; starting offset in Y should point past the ASCII
; line number.  Also counts the trailing NULL and two
; extra bytes for where the line number will be.
; Update must now include leading length byte not the null at end ****************
;
getLineLength
		ldx	#0	;size
getLineL2	lda	LINBUF,y
		beq	getLineL3
		iny
		inx
		bne	getLineL2
getLineL3	inx		;count null at end
		inx		;line number LSB
		inx		;MSB
		inx             ;change: count new leading line length
		stx	lineLength
		rts
;
;=====================================================
; Count the length of the line pointed to by CURPTR.
; This also counts the line number and the terminating
; null.  Ie, this string returns 8:
;
; <lineLow><lineHi>Hello<null>
;
; Another way of looking at it: add the return value
; to the CURPTR and it'll point to the next line's
; line number.  Returns the value in Y.
; Update to ject get the leading byte length ********************
;
;getCURPTRLength
;		ldy	CURPTR
;		ldy	#3	;change: skip line number and leading length byte
;getCLineL2  	lda	(CURPTR),y
;		beq	getCLineL3
;		iny
;		bne	getCLineL2
;getCLineL3	iny		;count null at end
;		rts

;
;=====================================================
; This saves ILPC.  This saves to a single save area,
; so it can't be called more than once.
;
saveIL		lda	ILPC
		sta	tempIL
		lda	ILPC+1
		sta	tempIL+1
		rts
;
;=====================================================
; This restores ILPC.
;
restoreIL	lda	tempIL
		sta	ILPC
		lda	tempIL+1
		sta	ILPC+1
		rts
;
;=====================================================
; This pushes R0 onto the stack.
;
pushR0          sty     rtemp1
                ldy     MATHSTACKPTR
                cpy     #MATHSTACKSIZE<<1
                bcs     pusherr
                lda     R0
                sta     (MATHSTACK),y
                iny
                lda     R0+1
                sta     (MATHSTACK),y
                iny
                sty     MATHSTACKPTR
                ldy     rtemp1
                clc
                rts

;=====================================================
; This pushes curptr basic current line onto the call stack.
; and CUROFF. Also marks entry type as 1 = GOSUB

pushLN
                STA     rtemp1+1                ; Store type of push being done
                sty     rtemp1
                lda     MESSAGEPTR              ; stack and msg Q grow together see if they cross!
                cmp     GOSUBSTACKPTR
                bcc     pusherr                 ; No error
                ldy     GOSUBSTACKPTR           ; Get the Go Stack Pointer
                ldx     #0                      ; Start of bytes to copy
pushLoop
                lda     CURPTR,x                ; Get the current pointer Start address
                sta     (GOSUBSTACK),y          ; put it onto the stack
                iny                             ; Next destination
                inx                             ; Next Source byte
                cpx     #3                      ; 4 bytes per entry on the stack
                bne     pushLoop                ; Jump if not done for next byte

pushDone        lda     rtemp1+1                ; Type of stack entry
                sta     (GOSUBSTACK),y          ; Store Type of stack entry
                iny                             ; Next entry

                sty     GOSUBSTACKPTR           ; Save the new stack pointer
                ldy     rtemp1
                clc
                rts
pusherr:
                sec
                rts
;=====================================================
; This pops Top Off gosub call Stack and
; places it in CURPTR/CUROFF.
; This checks if the type = 1 GOSUB
; if not it removes what ever is on the stack
; until it finds the next return. Allowing
; a return from within a for/next
; on exit a contains the type of return from, gosub_rtn, gosub_rtn_value....
popLN           sty     rtemp1
                ldy     GOSUBSTACKPTR         ; Get the Gosub/for stack pointer
                ldx     #3                    ; each stack entry is 3 bytes

popContinue:
                cpy     #4                    ; if less than 4 on stack then error
                bcc     poperr                ; Process an error

                dey                           ; Position to read entry type
                lda     (GOSUBSTACK),y        ; get the stack entry type
                sta     rtemp1+1              ; Save to be returned
                cmp     #GOSUB_RTN            ; Type is a gosub entry
                beq     popLoop               ; Restore the line
                cmp     #GOSUB_RTN_VALUE      ; Also restore the line
                bne     popSkipEntry          ; No then just skip this

popLoop:
                dey
                dex
                lda     (GOSUBSTACK),y
                sta     CURPTR,x
                cpx     #0
                bne     popLoop               ; Loop until all moved


PopDone:        sty     GOSUBSTACKPTR
                ldy     rtemp1
                lda     rtemp1+1               ; get the type of return
                clc
                rts

poperr:         sec
                rts

popSkipEntry:   dey
                dey
                dey
                jmp popContinue

;
;=====================================================
; This pushes R1 onto the stack
;
pushR1          sty     rtemp1
                ldy     MATHSTACKPTR
                cpy     #MATHSTACKSIZE<<1
                bcs     poperr
                lda     R1
                sta     (MATHSTACK),y
                iny
                lda     R1+1
                sta     (MATHSTACK),y
                iny
                sty     MATHSTACKPTR
                ldy     rtemp1
                clc
                rts
;
;=====================================================
; This pops Top Of Stack and places it in R0.
;
popR0           sty     rtemp1
                ldy     MATHSTACKPTR
                beq     poperr
                dey
                lda     (MATHSTACK),y
                sta     R0+1
                dey
                lda     (MATHSTACK),y
                sta     R0
                sty     MATHSTACKPTR
                ldy     rtemp1
                clc
                rts

;
;=====================================================
; This pops TOS and places it in R1.
;
popR1           sty     rtemp1
                ldy     MATHSTACKPTR
                beq     poperr
                dey
                lda     (MATHSTACK),y
                sta     R1+1
                dey
                lda     (MATHSTACK),y
                sta     R1
                sty     MATHSTACKPTR
                ldy     rtemp1
                rts
;
;=====================================================
; This pops TOS and places it in MQ.
;
popMQ         sty     rtemp1
              ldy     MATHSTACKPTR
              beq     poperr
              dey
              lda     (MATHSTACK),y
              sta     MQ+1
              dey
              lda     (MATHSTACK),y
              sta     MQ
              sty     MATHSTACKPTR
              ldy     rtemp1
              rts
;
;=====================================================
; This assists with multiplication and division by
; looking at R0 and R1 and saving a flag as to what
; sign the result will be.  Math is always done on
; positive numbers, so this converts negative numbers
; into positives.  On exit, R0 and R1 are both
; positive.  If the signs were different then 'signs'
; will be non-zero.
;
SaveSigns     lda     #0
              sta     sign      ;assume positive
              lda     R0+1      ;MSB
              bpl     SaveSigns1
              inc     sign      ;it's negative
              eor     #$ff      ;flip bits
              sta     R0+1
              lda     R0
              eor     #$ff
              sta     R0
              inc     R0
              bne     SaveSigns1
              inc     R0+1
SaveSigns1    lda     R1+1
              bpl     SaveSigns2
              pha
              lda     sign
              eor     #1
              sta     sign
              pla
              eor     #$ff      ;flip bits
              sta     R1+1
              lda     R1
              eor     #$ff
              sta     R1
              inc     R1
              bne     SaveSigns2
              inc     R1+1
SaveSigns2    rts
;
;=====================================================
; This looks at the value of 'signs' and will convert
; both R0 and R1 to negative if set.
;
RestoreSigns
              lda     sign
              beq     restoresigns2
;
              lda     R0
              bne     restoresigns3
              dec     R0+1
restoresigns3
              dec     R0
              lda     R0
              eor     #$ff
              sta     R0
              lda     R0+1
              eor     #$ff
              sta     R0+1
;
              lda     R1
              bne     restoresigns4
              dec     R1+1
restoresigns4
              dec     R1
              lda     R1
              eor     #$ff
              sta     R1
              lda     R1+1
              eor     #$ff
              sta     R1+1
;
restoresigns2
              rts
;
;=====================================================
; Skip over spaces.  Returns Y with the offset to
; either the last character in the line, or the first
; non-space character.
;

skipsp2         iny
SkipSpaces      lda     (CURPTR),y
                beq     Skip3               ;end of line
                cmp     #SPACE
                beq     skipsp2
Skip3           rts
;*********************************************************
; Output a CR/LF combination to the console.  Preserves
; all registers.
;
tbcrlf          pha
                lda     #CR
                jsr     VOUTCH
                lda     #LF
                jsr     VOUTCH
                pla
                rts
;
;=====================================================
; Some logic to print the Line of basic code being executed
idbgBasic       bit     ILTrace
                bvc     dbgBasicNone
                tya
                pha
                jsr     SetOutDebug

                lda     CURPTR
                sta     dpl
                lda     CURPTR+1
                sta     dpl+1

                jsr     PrintProgramLine

                lda     ILTrace
                and     #$01                ; Check if the Basic debug should be interactive
                beq     dbgBasicDone
                jsr     SetInDebug
                jsr     puts
                db      "Press s - Stop",CR,LF,"d - display Vars",CR,LF,"anything else to step",CR,LF," > ",0
dbgBasicLoop
                jsr     VGETCH
                jsr     CRLF
                jsr     SetInDebugEnd

                cmp     #'s                 ; Quit program
                beq     dbgBasicStop

                cmp     #'d                 ; Display Variables
                bne     dbgBasicDone

                jsr     PrintAllVars
                clc
                bcc     dbgBasicLoop        ; Next char

dbgBasicDone    jsr     SetOutDebugEnd
                pla
                tay
dbgBasicNone    jmp     NextIL

dbgBasicStop
                jsr     SetOutDebugEnd
                pla
                tay
                jmp     iFIN
;
;=====================================================
; This is some debug logic which displays the current
; value of the ILPC and the line buffer.
;
dbgLine         bit     ILTrace
                bmi     dbgPrt
                rts
dbgPrt
                jsr     SetOutDebug
                jsr     puts
                db      "ILPC:",0
                lda     ILPC+1
                jsr     OUTHEX
                lda     ILPC
                jsr     OUTHEX
                lda     #SPACE
                jsr     VOUTCH

                ldy     #0
                lda     (ILPC),y               ;Get the il pcode value
  if IL_DEBUG_TEXT
                jsr     PrintILText
  else
                jsr     OUTHEX
  endif
                jsr     puts
                db      " ILSP:",0
                lda     ILSTACKPTR
                jsr     OUTHEX
                lda     #SPACE
                jsr     VOUTCH

; Display the CURPTR value and offset
;
                jsr     puts
                db      ", CURPTR: ",0
                lda     CURPTR+1
                jsr     OUTHEX
                lda     CURPTR
                jsr     OUTHEX
                lda     #'+
                jsr     VOUTCH
                lda     CUROFF
                jsr     OUTHEX
;
                jsr     CRLF
                jsr     SetOutDebugEnd
                jsr     ILChkRange
                bcs     dbgLineErr
                clc
                rts

dbgLineErr
                jsr     SetOutDebug
                jsr     puts
                db      "Outside Valid IL Address Range",CR,LF,0
                jsr     SetOutDebugEnd
                sec
                rts

ILChkRange      lda   ILPC+1
                cmp   #IL>>8
                bcc   ILBadRange
                bne   ILChkHigh

                lda   ILPC
                cmp   #IL&$ff
                bcc   ILBadRange

ILChkHigh       lda   ILPC+1
                cmp   #ILEND>>8
                bcc   ILGoodRange
                bne   ILBadRange

                lda   ILPC
                cmp   #ILEND&$ff
                bcs   ILBadRange

ILGoodRange     clc
                rts
ILBadRange
                sec
                rts


;=====================================================
; Set output vector to the console output function
;
SetOutConsole
                pha
                lda     #OUTCH&$ff
                sta     BOutVec
                lda     #OUTCH>>8
                sta     BOutVec+1
                pla
                rts

SetInConsole
                pha
                lda     #GETCH&$ff
                sta     BInVec
                lda     #GETCH>>8
                sta     BInVec+1
                pla
                rts



;====================================================
;Clear the terminal assume it is ansii or vt100
;
iCLEARSCREEN
                jsr     puts
                db      $1b,'[,'2,'J,0
                jmp     NextIL

;====================================================
; Push true and false onto math stack
pushTrue
                lda    #$ff
pushTF          sta    R0
                sta    R0+1
                jsr    pushR0
                rts
pushFalse       lda    #0
                beq    pushTF

;======================================================
; Copy stack top to R1
CopyStackR1
                tya
                pha
                ldy    MATHSTACKPTR
                dey
                lda    (MATHSTACK),y
                sta    R1+1
                dey
                lda    (MATHSTACK),y
                sta    R1
                pla
                tay
                rts


;====================================================
;Swap the out debug call for standard calls
DebugIOSave     ds     2
DebugInSave     ds     2
SetOutDebug
                lda    BOutVec
                sta    DebugIOSave
                lda    BOutVec+1
                sta    DebugIOSave+1
                lda    #OUTDEBUG&$ff         ; Put the Debug output
                sta    BOutVec
                lda    #OUTDEBUG>>8
                sta    BOutVec+1
                rts
SetInDebug
                lda    BInVec
                sta    DebugInSave
                lda    BInVec+1
                sta    DebugInSave+1
                lda    #INDEBUG&$ff
                sta    BInVec
                lda    #INDEBUG>>8
                sta    BInVec+1
                rts
SetOutDebugEnd
                lda    DebugIOSave
                sta    BOutVec
                lda    DebugIOSave+1
                sta    BOutVec+1
                rts
SetInDebugEnd
                lda    DebugInSave
                sta    BInVec
                lda    DebugInSave+1
                sta    BInVec+1
                rts
;
;====================================================
; Set the input and output terminal address
; The math stack stack byte is the output io slot
; The math stack  is the input io slot

iSetTerminal
                jsr   popR0                              ; Process the output io addresses
                jsr   CalcSlot
                lda   R0
                ora   #1
                sta   TerminalOutputPort
                lda   R0+1
                sta   TerminalOutputPort+1

                jsr   popR0                             ; Process the input io address
                jsr   CalcSlot
                lda   R0
                sta   TerminalStatusPort
                ora   #1
                sta   TerminalInputPort
                lda   R0+1
                sta   TerminalInputPort+1
                sta   TerminalStatusPort+1
                jmp    NextIL

;===================================================
; Calculate the slot address the the slot number
; R0 contains the slot number 0-255

CalcSlot
                txa
                pha

                ldx     #4
CalcSlotLoop:
                clc
                rol     R0
                rol     R0+1
                dex
                bne     CalcSlotLoop

                lda     #$E0
                ora     R0+1
                sta     R0+1
                pla

                tax
                rts
;
;====================================================
; Output to the Terminal/Debug console
;     x = high address byte
;     y = low address byte
;     a = Terminator for string
TerminalWrite
DebugWrite
                jsr     SetOutDebug
                jsr     PrtStr
                jsr     SetOutDebugEnd
                rts

TerminalIOblock
OUTDEBUG
                .byte   $8D                           ; STA
TerminalOutputPort
DEBUGPORT       .word   $E021                         ; Dont check anything just output the byte
                RTS

TerminalRead
INDEBUG
                .byte   $AD                           ; LDA
TerminalStatusPort
DEBUGPORTSTATUS .word   $E020

                and     #$01
                beq     INDEBUG

                .byte   $AD                              ; LDA
TerminalInputPort
DEBUGPORTIN     .word   $E021
                rts
TerminalIOblockEnd
;======================================================================
;Copy Quoted string to buffer, terminate with 0 byte
; R0  Source tring points to tString type
; x is terminator
; R1 points to destinition location
; On exit R0 contains length of copy Plus Term and leading bytes

qstrcpy
                jsr     pushR0
                jsr     IncR0                           ; point past the tString
                jsr     IncR0                           ; Point Past the opening "
                ldx     #'"                             ; copy Termination
                jsr     pstrcpy
                jsr     IncR0                           ; point to "
                jsr     IncR0                           ; Point to next free byte
                jsr     popR1
                sec
                lda     R0
                sbc     R1
                sta     R0
                lda     R0+1
                sbc     R1+1
                sta     R0+1
                rts

;=========================================================================
;Copy string from R0 to R1, terminator in x
; On exit    R0 contains the length of the copy
pstrcpy:
                ldy     #0
                stx     R2

strcpyLoop:
                lda     (R0),y
                cmp     R2
                beq     strcpyDone
                sta     (R1),y
                jsr     IncR0
                jsr     IncR1
                bcc     strcpyLoop
strcpyDone:
                lda     #0
                sta     (R1),y

                rts

;=========================================================================
; on exit c is set on overflow
IncR1:
                pha
                clc
                lda     #1
                adc     R1
                sta     R1
                bcc     IncR1Done
                lda     #0
                adc     R1+1
                sta     R1+1
IncR1Done:
                pla
                rts
;=========================================================================
; on exit c is set on overflow
IncR0:
                pha
                clc
                lda     #1
                adc     R0
                sta     R0
                bcc     IncR0Done
                lda     #0
                adc     R0+1
                sta     R0+1
IncR0Done:
                pla
                rts
















