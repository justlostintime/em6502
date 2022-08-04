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
getILWord	jsr	getILByte	;LSB
		tax
;
;=====================================================
; This gets the next byte pointed to by ILPC and
; returns it in A.  On return, X is unchanged but Y
; contains 0.
;
getILByte	ldy	#0
		lda	(ILPC),y	;get byte
		php			;save status
		inc	ILPC		;inc LSB
		bne	getILb2		;branch if no overflow
		inc	ILPC+1		;inc MSB
getILb2		plp			;restore status
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
              rts
;
;=====================================================
; Pull the top entry from return stack and put into
; ILPC.
;
popILPC       ldy     ILSTACKPTR
              dey
              lda     (ILSTACK),y
              sta     ILPC+1
              dey
              lda     (ILSTACK),y
              sta     ILPC
              sty     ILSTACKPTR
              rts
;
;=====================================================
; This searches for a specific line number that is in
; R0.  There are three possible return conditions:
; Line numbers are now the third byte, the first byte is now **************
; a pointer to the next line, of course no longer that 53 byte
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
                lda     #ProgramStart&$ff     ;Start of program -> CURPTR
                sta     CURPTR
                lda     #ProgramStart>>8
                sta     CURPTR+1
;
; At end of code?
;
iXFER1
                lda     CURPTR                ; chk CURPTR = END PROGRAM
                cmp     PROGRAMEND            ; at end of program then stop run
                bne     xfer2                 ; not end
                lda     CURPTR+1
                cmp     PROGRAMEND+1
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
xfernotit       ldy     #2              ;Changed from to skip leading length and lesat significat digit
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
xfer3		jsr	FindNextLine
		jmp	iXFER1
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
		ldy	#3		;skip line number and length byte
		sty	CUROFF		;this is the new offset
		ldy     #0
		lda     (CURPTR),y       ;Get the length
		clc
		adc	CURPTR
		sta	CURPTR
		bcc	FindNext4	;exit
		inc	CURPTR+1
FindNext4	rts
;
;=====================================================
; This compares CURPTR to PROGRAMEND and returns Z set
; if they are equal, Z clear if not.
;
AtEnd		lda	CURPTR
		cmp	PROGRAMEND
		bne	atendexit
		lda	CURPTR+1
		cmp	PROGRAMEND+1
atendexit	rts
;
;=====================================================
; Print the contents of R0 as a signed decimal number.
; Does leading zero suppression.
;
PrintDecimal
		lda	R0+1	;MSB has sign
		bpl	pplus		;it's a positive number
;
; Negative numbers need more work.  Invert all the bits,
; then add one.
;
		lda	#'-
		jsr	VOUTCH		;print the negative sign
;
		lda	R0		;invert bits
		eor	#$ff
		sta	R0
		lda	R0+1
		eor	#$ff
		sta	R0+1
		inc	R0		;add one
		bne	pplus
		inc	R0+1
;
; Print the value in R0 as a positive number.
;
pplus		ldx	#0	;start of subtraction table
		stx	diddigit	;no digits yet
pploop		ldy	#0		;result of division
pploop2		lda	R0		;LSB
		sec
		sbc	dectable,x
		sta	R0
		lda	R0+1
		sbc	dectable+1,x
		bpl	pplusok	;no underflow
;
; Else, underflow.  Add back in the LSB of the
; table to R0.
;
		clc
		lda	R0
		adc	dectable,x
		sta	R0
;
; Print the value in Y.  Actually, see if Y is zero and
; whether any digit has been printed yet.  If Y isn't
; zero or we've printed a digit, go ahead and print.
;
		stx	printtx
		tya
		ora	#0		;set flags
		bne	pprintit	;non-zero, print
;
		lda	diddigit
		beq	pprintno	;don't print
;
pprintit	tya
		ora	#'0
		sta	diddigit
		jsr	VOUTCH
pprintno	ldx	printtx
;
; Move to the next table entry
;
		inx
		inx
		cpx	#dectableend-dectable
		bne	pploop	;not at end
;
; At the end.  R0 contains the final value
; to print.
;
		lda	R0
		ora	#'0
		jmp	VOUTCH
;
; Finish doing the subtraction.
;
pplusok		sta	R0+1
		iny
		bne	pploop2
;
; Table of powers-of-ten
;
dectable	dw	10000
            dw	1000
            dw	100
            dw	10
dectableend	equ	*
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
getDecimal      lda	#0
                sta	R0
                sta	R0+1
                sta	dpl	;temporary negative flag
;
; See if it's negative...
;
                sty $0013
                lda	(CURPTR),y
                cmp	#'-
                bne	getDecLoop
                inc	dpl	;it's negative
;
getDecLoop	lda	(CURPTR),y
                cmp	#'0
                bcc	getDdone
                cmp	#'9+1
                bcs	getDdone
                sec
                sbc	#'0	;convert to binary
                pha
;
; Now multiply R0 by 10.  Remember that
; 2*N + 8*N = 10*N.
;
                asl	R0
                rol	R0+1	;*2
                lda	R0
                sta	R1
                lda	R0+1
                sta	R1+1
                asl	R0
                rol	R0+1	;*4
                asl	R0
                rol	R0+1	;*8
                clc		;now add the partial sums...
                lda	R0	;...to get *10
                adc	R1
                sta	R0
                lda	R0+1
                adc	R1+1
                sta	R0+1
;
; Add in the new digit
;
                pla
                clc
                adc	R0
                sta	R0
                bcc	getD2
                inc	R0+1
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
getDdone        lda	R0
                ora     R0+1
                beq	getDone2	;zero
                lda	dpl
                beq	getDone2	;positive
;
; Invert all the bits, then add one.
;
                lda	R0
                eor	#$ff
                sta	R0
                lda	R0+1
                eor	#$ff
                sta	R0+1
;
                inc	R0
                bne	getDone2
                inc	R0+1
getDone2
                lda R0
                sta $0010
                lda R0+1
                sta $0011
                lda dpl
                sta $012

                rts
;
;=====================================================
; Print the string that immediately follows the JSR to
; this function.  Stops when a null byte is found,
; then returns to the instruction immediately
; following the null.
;
; Thanks to Ross Archer for this code.
; http://www.6502.org/source/io/primm.htm
;
	if KIM
puts		sty	putsy
		pla		;low part of "return" address
           			;(data start address)
		sta	dpl
		pla
		sta	dpl+1	;high part of "return" address
             			;(data start address)
             			;Note: we're pointing one short
psinb       	ldy	#1
		lda	(dpl),y	;Get next string character
		inc	dpl	;update the pointer
		bne	psinc	;if not, we're pntng to next char
		inc	dpl+1	;account for page crossing
psinc		ora	#0	;Set flags according to contents of
             			;   Accumulator
		beq	psix1	;don't print the final NULL
		jsr	OUTCH	;write it out
		jmp	psinb	;back around
psix1		inc	dpl
		bne	psix2
		inc	dpl+1	;account for page crossing
psix2		ldy	putsy
		jmp	(dpl)	;return to byte following NULL
	endif
;
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
                cmp     #$03            ;Task Active and waiting for IO
                beq     taskWaitingIO
                ora     #$02            ;Mark Task as waiting for IO
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
                cmp     #3                     ;Complete again
                beq     taskWaitingIO
                jmp     GetLineRetry           ;If the IO is wait then jump to start

GetLineDone
                ldx     taskPtr
                lda     #1
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
                jsr     OUTCH
                lda     #$20
                jsr     OUTCH             ;Space after prompt
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
getline1        jsr     GETCH
        if  CTMON65
                pha
                jsr     cout              ;echo echo echo
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
                jsr     OUTCH
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
; Save Context Store the context to the TASK Table
; on entry y contains the task table entry to save to
; on exit y points to next task table entry
;         x contains the number of bytes copied
ContextSave     ldx   #0
                iny             ;inc past the task flags
ContextSvLoop   lda   CONTEXT,x
                sta   taskTable,y
                iny
                inx
                cpx   #CONTEXTLEN
                bcc   ContextSvLoop
                rts
;
; Load Context transfer context from task table to the Current Context
; on entry y contains the task table entry to transfer
; on exit y points to the original task table entry
;         x contains the number of byts copied
ContextLoad     tya
                pha
                ldx   #0
                iny                 ;inc past the task flags
ContextLDLoop   lda   taskTable,y
                sta   CONTEXT,x
                iny
                inx
                cpx   #CONTEXTLEN
                bcc   ContextLDLoop
                pla
                tay
                rts
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
                lda     R0
                sta     (MATHSTACK),y
                iny
                lda     R0+1
                sta     (MATHSTACK),y
                iny
                sty     MATHSTACKPTR
                ldy     rtemp1
                rts

;=====================================================
; This pushes curptr basic current line onto the call stack.
; and CUROFF. Also marks entry type as 1 = GOSUB

pushLN
                sty     rtemp1
                ldy     GOSUBSTACKPTR           ; Get the Go Stack Pointer
                cpy     #GOSUBSTACKSIZE*4       ; Is there space available
                beq     pusherr                 ; No error
                ldx     #0                      ; Start of bytes to copy
pushLoop
                lda     CURPTR,x                ; Get the current pointer Start address
                sta     (GOSUBSTACK),y          ; put it onto the stack
                iny                             ; Next destination
                inx                             ; Next Source byte
                cpx     #3                      ; 4 bytes per entry on the stack
                bne     pushLoop                ; Jump if not done for next byte

pushDone        lda     #1                      ; Type of stack entry, 1 gosub, 2 for , 3 next
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
popLN           sty     rtemp1
                ldy     GOSUBSTACKPTR         ; Get the Gosub/for stack pointer
                ldx     #3                    ; each stack entry is 3 bytes

popContinue
                cpy     #4                    ; if less than 4 on stack then error
                bcc     poperr                ; Process an error

                dey                           ; Position to read entry type
                lda     (GOSUBSTACK),y        ; get the stack entry type
                cmp     #1                    ; Type is a gosub entry
                bne     popSkipEntry          ; No then just skip this

popLoop
                dey
                dex
                lda     (GOSUBSTACK),y
                sta     CURPTR,x
                cpx     #0
                bne     popLoop               ; Loop until all moved


PopDone         sty     GOSUBSTACKPTR
                ldy     rtemp1
                clc
                rts
poperr          sec
                rts

popSkipEntry    dey
                dey
                dey
                jmp popContinue

;
;=====================================================
; This pushes R1 onto the stack
;
pushR1          sty     rtemp1
                ldy     MATHSTACKPTR
                lda     R1
                sta     (MATHSTACK),y
                iny
                lda     R1+1
                sta     (MATHSTACK),y
                iny
                sty     MATHSTACKPTR
                ldy     rtemp1
                rts
;
;=====================================================
; This pops Top Of Stack and places it in R0.
;
popR0           sty     rtemp1
                ldy     MATHSTACKPTR
                dey
                lda     (MATHSTACK),y
                sta     R0+1
                dey
                lda     (MATHSTACK),y
                sta     R0
                sty     MATHSTACKPTR
                ldy     rtemp1
                rts

;
;=====================================================
; This pops TOS and places it in R1.
;
popR1           sty     rtemp1
                ldy     MATHSTACKPTR
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
SaveSigns	lda	#0
		sta	sign	;assume positive
		lda	R0+1	;MSB
		bpl	SaveSigns1
		inc	sign	;it's negative
		eor	#$ff	;flip bits
		sta	R0+1
		lda	R0
		eor	#$ff
		sta	R0
		inc	R0
		bne	SaveSigns1
		inc	R0+1
SaveSigns1	lda	R1+1
		bpl	SaveSigns2
		pha
		lda	sign
		eor	#1
		sta	sign
		pla
		eor	#$ff	;flip bits
		sta	R1+1
		lda	R1
		eor	#$ff
		sta	R1
		inc	R1
		bne	SaveSigns2
		inc	R1+1
SaveSigns2	rts
;
;=====================================================
; This looks at the value of 'signs' and will convert
; both R0 and R1 to negative if set.
;
RestoreSigns
		lda	sign
		beq	restoresigns2
;
		lda	R0
		bne	restoresigns3
		dec	R0+1
restoresigns3
		dec	R0
		lda	R0
		eor	#$ff
		sta	R0
		lda	R0+1
		eor	#$ff
		sta	R0+1
;
		lda	R1
		bne	restoresigns4
		dec	R1+1
restoresigns4
		dec	R1
		lda	R1
		eor	#$ff
		sta	R1
		lda	R1+1
		eor	#$ff
		sta	R1+1
;
restoresigns2
		rts
;
;=====================================================
; Skip over spaces.  Returns Y with the offset to
; either the last character in the line, or the first
; non-space character.
;
skipsp2		iny
SkipSpaces	lda	(CURPTR),y
		beq	Skip3	;end of line
		cmp	#SPACE
		beq	skipsp2
Skip3		rts
;
;=====================================================
; This is some debug logic which displays the current
; value of the ILPC and the line buffer.
;
dbgLine		jsr	puts
		db	"ILPC: ",0
		lda	ILPC+1
		jsr	OUTHEX
		lda	ILPC
		jsr	OUTHEX
		lda	#SPACE
		jsr	OUTCH
		ldy	#0
		lda	(ILPC),y
		jsr	OUTHEX
;
; Display the CURPTR value and offset
;
		jsr	puts
		db	", CURPTR: ",0
		lda	CURPTR+1
		jsr	OUTHEX
		lda	CURPTR
		jsr	OUTHEX
		lda	#'+
		jsr	OUTCH
		lda	CUROFF
		jsr	OUTHEX
;
		jmp	CRLF
;
;=====================================================
; This function might go away eventually, but was
; added to provide data for other pieces of code.
; It has some ties to the operating environment that
; will need to be customized for the target system.
;
GetSizes
;
; Here is machine specific code to get the highest
; memory location that can be used by BASIC.
;
	if ProgramStart < $2000
		lda	#$ff
		sta	HighMem	;$13ff for KIM-1
		lda	#$DE    ;#$13
		sta	HighMem+1
	else
		lda	#$ff
		sta	HighMem	;$CFFF otherwise
		lda	#$cf
		sta	HighMem+1
	endif
;
; This computes the available memory remaining.
;
		sec
		lda	HighMem
		sbc	PROGRAMEND
		sta	FreeMem
		sta	R0
		lda	HighMem+1
		sbc	PROGRAMEND+1
		sta	FreeMem+1
		sta	R0+1
;
; This computes the size of the current user program.
;
		sec
		lda	PROGRAMEND
		sbc	#ProgramStart&$ff
		sta	UsedMem
		lda	PROGRAMEND+1
		sbc	#ProgramStart>>8
		sta	UsedMem+1
;
		rts
;
;=====================================================
; Set output vector to the console output function
;
SetOutConsole
                lda     #OUTCH&$ff
                sta     BOutVec
                lda     #OUTCH/256
                sta     BOutVec+1
                rts
;
;=====================================================
; Jump to the output function in BOutVec
;
VOUTCH          jmp     (BOutVec)

;====================================================
PrtTerm         equ     rtemp1

; Print Y has the offset to use
PrtQuoted       lda     CURPTR
                sta     PrtFrom
                lda     CURPTR+1
                sta     PrtFrom+1
                lda     #'"
                sta     PrtTerm
                jmp     PrtLoop

; Print a string pointed to by x= h, y=l terminated by a
; Return y as the length

PrtStr          stx      PrtFrom+1
                sty      PrtFrom
                sta      PrtTerm
                ldy      #0

PrtLoop         lda     (PrtFrom),y
                cmp     PrtTerm
                beq     PrtEnd
                cmp     #0              ; always end if 0 is found
                beq     PrtEnd
                jsr     OUTCH
                iny
                jmp     PrtLoop
PrtEnd          iny                     ;return byte after the copy
                rts

;====================================================
;Clear the terminal assume it is ansii or vt100
;
iCLEARSCREEN
                jsr     puts
                db      $1b,'[,'3,'J,0
                jmp     NextIL
