input            processor 6502
;=====================================================
; Bob's Tiny BASIC
;
; While working on the Corsham Technologies KIM Clone
; project, I wanted to include a TINY BASIC since that
; was a highly desirable feature of early computers.
;
; Rather than negotiating copyright issues for
; existing BASICs, I decided to just write one from
; scratch.
;
; 10/07/2017
;
; This implements a stripped down Tiny BASIC
; interpreter using the Interpretive Language (IL)
; method as described in the first few issues of
; Dr Dobb's Journal.  The IL interpreter can be used
; to write various languages simply by changing the
; IL code rather than the interpreter itself.
;
; 10/15/2021 v0.4 - Bob Applegate
;		* Fixed major bug in findLine that
;		  caused corrupted lines, crashes, etc.
;		* If no parameter given to RND, assume
;		  32766.
;		* No more error 5 when a program
;		  reaches the end without an END.
;
; 02/15/2022 v0.5 JustLostInTime@gmail.com
;               * Unexpanded version to play with everything
;               * Add some usefull system level functions
;               * allow a larger number of tiny basic formats
;               * Add byte at start of line holding length
;                 for faster execution of goto and gosub
;               * Re-added gosub
;               * allow ; or , at end if print stmt
;                 without CRLF being added.
;               * Added extended function erase to
;                 use the extended ctmon65 rm file
;               * Fix quoted text to not have to backtrack
;               * Add IRQ handler, Call Gosub and Iret at end
;
; www.corshamtech.com
; bob@corshamtech.com
; JustLostInTime@gmail.com
;
;=====================================================
;
; Create TRUE and FALSE values for conditionals.
;

FALSE		equ	0
TRUE		equ	~FALSE
;
;---------------------------------------------------------
; One of these must be set to indicate which environment
; Tiny BASIC will be running in.  Here are the current
; environments:
;
; KIM - This is a bare KIM-1.  You'll need to add a few
; more K of RAM.
;
; XKIM - The Corsham Technologies xKIM extended monitor,
; which enhances, without replacing, the standard KIM
; monitor.  It gives access to routines to save/load files
; to a micro SD card.
;
; CTMON65 is a from-scratch monitor written for the
; Corsham Tech SS-50 6502 CPU board, but the monitor can
; easily be ported to other systems.  It has support for
; using a micro SD card for file storage/retrieval.
;
KIM             equ     FALSE           ;Basic KIM-1, no extensions
XKIM            equ     FALSE           ;Corsham Tech xKIM monitor
CTMON65         equ     TRUE            ;Corsham Tech CTMON65
;
;   Need to define some macros for the dasm assembler
;
    MACRO dw
        .word {0}
    ENDM

    MACRO db
        .byte {0}
    ENDM

;
; If set, include disk functions.
;
DISK_ACCESS       equ     TRUE
;
; If ILTRACE is set then dump out the address of every
; IL opcode before executing it.
;
ILTRACE           equ     FALSE
;
; If FIXED is set, put the IL code and the user
; program space at fixed locations in memory.  This is
; meant only for debugging.
;
FIXED             equ     FALSE
;
; Sets the arithmetic stack depth.  This is *TINY*
; BASIC, so keep this small!
;
MATHSTACKSIZE     equ     20      ;number of entries in math stack
ILSTACKSIZE       equ     20      ;number of entries in ilstack
GOSUBSTACKSIZE    equ     16      ;Depth of gosub/For-Next nesting max is 64 times TASKTABLE LENGTH must < 256
TASKCOUNT         equ     9       ;Task Table count, up to 64 tasks
TASKCYCLESDEFAULT equ     200     ;Default Task Switch 0-255 uses a single byte
MESSAGESMAX       equ     2       ;MAX of 2 Messages
;
; Common ASCII constants
;
BEL             equ     $07
BS              equ     $08
TAB             equ     $09
LF              equ     $0A
CR              equ     $0D
quote           equ     $22
SPACE           equ     $20
COMMA           equ     ',
SEMICOLON       equ     ';
COLON           equ     ':
DOLLAR          equ     '$
;
; These are error codes
;
ERR_NONE                equ     0       ;No Errror
ERR_EXPR                equ     1       ;expression error
ERR_UNDER               equ     2       ;stack underflow
ERR_OVER                equ     3       ;stack overflow
ERR_EXTRA_STUFF         equ     4       ;Stuff at end of line
ERR_SYNTAX              equ     5       ;various syntax errors
ERR_DIVIDE_ZERO         equ     6       ;divide by zero
ERR_READ_FAIL           equ     7       ;error loading file
ERR_WRITE_FAIL          equ     8       ;error saving file
ERR_NO_FILENAME         equ     9       ;Forgot to include the file name
ERR_FILE_NOT_FOUND      equ     10      ;The file name provided not found
ERR_STACK_UNDER_FLOW    equ     11      ;the gosub stack underflow
ERR_STACK_OVER_FLOW     equ     12      ;Stack overflow
ERR_BAD_LINE_NUMBER     equ     13      ;Bad line number specified Not found
ERR_NO_EMPTY_TASK_SLOT  equ     14      ;Unable to create a new task no/slots
ERR_INDEX_OUT_OF_RANGE  equ     15      ;Subscript out of range
ERR_INVALID_PID         equ     16      ;Invalid PID provided
ERR_OUT_OF_MSG_SPACE    equ     17      ;Out of space for new messsages
;
;=====================================================
; Zero page storage.
;
            SEG.U       ZEROPAGE
            org         $0040

ILTrace                 ds      1       ;non-zero means tracing

; The context is used to locate a task switch
; it copies from here till all task fields are saved/swapped
; The max number of tasks is 256 / context length
;
CONTEXT                 equ     *

VARIABLES               ds      2           ; 2 bytes pointer to, 26 A-Z
ILPC                    ds      2           ; IL program counter
ILSTACK                 ds      2           ; IL call stack
ILSTACKPTR              ds      1
MATHSTACK               ds      2           ; MATH Stack pointer
MATHSTACKPTR            ds      1
GOSUBSTACK              ds      2           ; pointer to gosub stack
GOSUBSTACKPTR           ds      1           ; current offset in the stack, moved to task table
MESSAGEPTR              ds      1           ; Pointer to active message, from bottom of ilstack 
;
; CURPTR is a pointer to curent BASIC line being
; executed.  Always points to start of line, CUROFF
; is the offset to the current character.
; The order of these fields is important
CURPTR                  ds      2           ; Pointer to current Basic line
CUROFF                  ds      1           ; Current offset in Basic Line
;
;The order of these fields in important

;
; R0, R1 and MQ are used for arithmetic operations and
; general use.
;
REGISTERS               equ     *           ;IL MATH REGISTERS
REG0                    equ     0
R0                      ds      2           ;arithmetic register 0
REG1                    equ     2           ;offset of R1
R1                      ds      2           ;arithmetic register 1
REGMQ                   equ     4           ;offset og MQ
MQ                      ds      2           ;used for some math
REGISTERSEND            equ     *
REGISTERSLEN            equ     REGISTERSEND-REGISTERS

CONTEXTEND              equ     *                      ; End of swap context
CONTEXTLEN              equ     CONTEXTEND - CONTEXT   ; length of the context
variablesEnd            equ     26*2                   ; length of variable space

dpl                     ds      2       ;Used as a pointer to call il instructions
tempIL                  ds      2       ;Temp IL programcounter storage
tempIlY                 ds      1       ;Temp IL Y register storage
offset                  ds      1       ;IL Offset to next inst when test fails
lineLength              ds      1       ;Length of current line

taskIOPending           ds      1       ; 1 = pending Set when a task wants to read keyboard/ write to screen
taskRDPending           ds      1       ; 1 = background read is pending

;
; This is zero if in immediate mode, or non-zero
; if currently running a program.  Any input from
; the main loop clears this, and the XFER IL
; statement will set it.
;
RunMode                 ds      1       ;Basic program is running or stop
;
; Used for line insertion/removal.
;
FROM                    ds      2       ;Used for basic prog insert/remove and print text

; THE ADDRESS USED BY THE PRINTER FUNCTION
; TO PRINT A GENERIC STRING X,Y ADDRESS, Ac = TERMINATOR
;
PrtFrom                 EQU     FROM
;

;
;=====================================================
;
                        SEG     Code
                        org     $0200
;
; Cold start is at $0200.  Warm start is at $0203.
;
TBasicCold              jmp     cold2     ;jump around vectors
warm                    jmp     warm2     ;Entry point for worm restart
;
; These are the user-supplied vectors to I/O routines.
; If you want, you can just patch these in the binary
; file, but it would be better to change the source
; code.
;
                if      KIM
OUTCH           jmp     $1ea0             ;output char in A
GETCH           jmp     $1e5a             ;get char in A (blocks)
CRLF            jmp     $1e2f             ;print CR/LF
OUTHEX          jmp     $1e3b             ;print A as hex
MONITOR         jmp     $1c4f             ;return to monitor
                endif
                if      XKIM
                include   "xkim.inc"
                SEG     Code
OUTCH           jmp     $1ea0
GETCH           jmp     xkGETCH
CRLF            jmp     $1e2f             ;print CR/LF
OUTHEX          jmp     xkPRTBYT
MONITOR         jmp     extKIM
puts            equ     putsil
BUFFER_SIZE     equ     132
                endif

                if      CTMON65
                include   "ctmon65.inc"

                SEG     Code
;
OUTCH           jmp     cout
GETCH           jmp     cin
CRLF            jmp     crlf
OUTHEX          jmp     HexA
MONITOR         jmp     WARM
ISCHAR          jmp     cstatus
puts            equ     putsil
                endif
;
cold2           jsr     puts
                db      CR,LF
                db      "Bob's Tiny BASIC v1.0.2 IRQs/Tasks"
                db      CR,LF
                db      "https://github.com/CorshamTech/6502-Tiny-BASIC"
                db      CR,LF,0
;
                jsr     GetSizes           ;setup the free space available

calcstack       lda     #1
                sta     taskCounter                 ; Initialize number of tasks to 1
                sta     taskTable                   ; mark the main task as active
                jsr     taskSetStacks               ; setup all the task stacks/Variables
                lda     #IL&$ff
                sta     ILPC
                lda     #IL>>8
                sta     ILPC+1
;
                lda     #ProgramStart&$ff           ;user prog
                sta     PROGRAMEND
                lda     #ProgramStart>>8
                sta     PROGRAMEND+1
;
; Initialize the pseudo-random number sequence...
;
                lda     #$5a
                sta     rtemp1
                lda     #%10011101
                sta     random
                lda     #%01011011
                sta     random+1
;
;   Insert a Basic irq handler for the basic Language
                lda     #ServiceIrq&$ff
                sta     IRQvec
                lda     #ServiceIrq>>8
                sta     IRQvec+1
                jmp     coldtwo

;
; This is the Basic IRQ handler
ServiceIrq      pha
                lda     IRQStatus
                BEQ     RetIrq
                lda     IRQPending
                bne     RetIrq
                lda     #1
                sta     IRQPending
RetIrq          pla
                rti
;
;
; This is the warm start entry point
;
warm2           jsr     CRLF
                lda     errGoto
                sta     ILPC
                lda     errGoto+1
                sta     ILPC+1
;
; And continue with both starts here
;
coldtwo         jsr     SetOutConsole
;
; The ILTrace flag is now run-time settable.
;
                lda     #ILTRACE&$ff
                sta     ILTrace
;

                lda     #0
                sta     RunMode
                sta     LINBUF

; Clear everything from the stacks

                sta     taskIOPending                ; No one waiting for io
                sta     taskRDPending                ; No one waiting for bg io
                jsr     taskReset
;
                lda     #LINBUF&$ff
                sta     CURPTR
                lda     #LINBUF>>8
                sta     CURPTR+1      ;fall through...

;=====================================================
; This is the top of the IL interpreter.  This fetches
; and executes the instruction currently pointed to
; by ILPC and adjusts ILPC to point to the next
; instruction to execute.
;
NextIL          dec     taskCurrentCycles
                bne     NextIlNow
                jsr     iTaskSwitch           ;check for a task switch
NextIlNow       lda     ILTrace
                beq     NextIL2
                jsr     dbgLine
NextIL2         ldy     CUROFF
                jsr     SkipSpaces
                sty     CUROFF
;Task IO Management
                lda     taskRDPending         ; if it is zero then Nothing pending
                beq     NextILStr
                jsr     ReadLine              ; else Pending and poll keyboard
                bcc     NextILStr             ; if carry is clear then no end of line yet
                dec     taskRDPending         ; Carry is set if CR has been recievec
;
NextILStr       jsr     getILByte
;
; When the handler is called, these are the conditions
; of several important items:
;
;    (ILPC) will point to the byte AFTER the IL
;    opcode being executed.
;
;    (CURPTR),CUROFF will point to the start of the
;    next word in the input buffer.  Ie, the next word
;    in the user program.
;
		asl
		cmp	#ILTBLend-ILTBL+2
		bcc	ILgood
;
; This handles an illegal IL opcode.  This is serious
; and there's no way to recover.
;
ILbad		jsr	puts
		db	CR,LF
		db	"Illegal IL "
		db	0
;
; Well this is awkward, we need to back up the IL
; by one since it no longer points to the current
; opcode.
;
		jsr	decIL
;
		ldy	#0
		lda	(ILPC),y
		jsr	OUTHEX
		jsr	puts
		db	" at ",0
		lda	ILPC+1
		jsr	OUTHEX
		lda	ILPC
		jsr	OUTHEX
		jsr	CRLF
		jmp	MONITOR
;
; Just jump to the address (ILPC),y.  Have to do
; some goofy stuff.
;
ILgood		tay		       ;move index into Y
		lda	ILTBL,y
		sta	dpl
		lda	ILTBL+1,y
		sta	dpl+1
		jmp	(dpl)	       ;go to handler
;
;=====================================================
; This is the IL jump table.  The IL opcode is
; mulitplied by two, then looked-up in this table.
; There is absolutely nothing special about the order
; of entries here... they all decode at exactly the
; same speed.  However the entry number must match the
; values in IL.inc.
;
ILTBL       dw	iXINIT	;0
            dw	iDONE	;1
            dw	iPRS	;2
            dw	iPRN	;3
            dw	iSPC	;4
            dw	iNLINE	;5
            dw	iNXT	;6
            dw	iXFER	;7
            dw	iSAV	;8
            dw	iRSTR	;9
            dw	iCMPR	 ;10
            dw	iINNUM	;11
            dw	iFIN	;12
            dw	iERR	;13
            dw	iADD	;14
            dw	iSUB	;15
            dw	iNEG	;16
            dw	iMUL	;17
            dw	iDIV	;18
            dw	iSTORE	;19
            dw	iIND	;20
            dw	iLST	;21
            dw	iINIT	;22
            dw	iGETLINE;23
            dw	iINSRT	;24
            dw	iRTN	;25
            dw	MONITOR	;26
            dw	iLIT	;27
            dw	iCALL	;28
            dw	iJMP	;29
            dw	iVINIT	;30
            dw	iERRGOTO;31
            dw	iTST	;32
            dw	iTSTV	;33
            dw	iTSTL	;34
            dw	iTSTN	;35
            dw	iFREE	;36
            dw	iRANDOM	;37
            dw	iABS	;38
;
; Disk functions.  There must be pointers
; to functions even if no disk is supported.
; Makes things easier in IL.inc.
;
	if	DISK_ACCESS
      dw    iOPENREAD    ;39
      dw    iOPENWRITE   ;40
      dw    iDCLOSE      ;41
      dw    iDGETLINE    ;42 Life, universe, everything(hitch hiker)
      dw    iDLIST       ;43 Did you remeber your towel?
      dw    iDDIR        ;44
      dw    iRMFILE      ;45
	else
      dw    NextIL	      ;39
      dw    NextIL	      ;40
      dw    NextIL	      ;41
      dw    NextIL	      ;42
      dw    NextIL	      ;43
      dw    NextIL	      ;44
      dw    NextIL	      ;45
	endif
;
	    dw  iCLEARSCREEN  ;46
	    dw  iPOKEMEMORY   ;47
	    dw  iPEEKMEMORY   ;48
	    dw  iTSTLET       ;49       Test if the let with no LET keyword
	    dw  iTSTDONE      ;50       Test if we are at the end of a line
	    dw  iGETCHAR      ;51       Get a character from the terminal
	    dw  iPUTCHAR      ;52       Put a char to the terminal
	    dw  iCallFunc     ;53       call a machine rtn accumulator
	    dw  iBranch       ;54       if value on stack is 0 then next line, else next instuction
	    dw  iTSTStr       ;55       Test Specifically for the start of a quoted string
	    dw  iSetIrq       ;56       sets the irq handler
	    dw  iTstIrq       ;57       test if irq is pending
	    dw  iRET          ;58       return from interupt
	    dw  iINSTR        ;59       read a string return first char on top of stack
      dw  iMOD          ;60       returns remainder of division
      dw  iTaskSet      ;61       sets a line number for the start of a task
      dw  iETask        ;62       Terminates a task
      dw  iNTask        ;63       goto next task
      dw  iArray        ;64       Allow Variable to have a subscript
      dw  iTaskKill     ;65       kill a running task
      dw  iTaskStat     ;66       return the state of a task PID
      dw  iHexOut       ;67       output the value on the stack as a hex string
      dw  iReadComplete ;68       Called after a background read completes
      dw  iReadStart    ;69       Called to start a background read request
      dw  iStartIO      ;70       Lock task until io complete
      dw  iEndIO        ;71       release task lock for io
      dw  iLogNot       ;72       Logical not
      dw  iLogOr        ;73       Logical Or
      dw  iLogAnd       ;74       Logical And
      dw  iLogXor       ;75       Logical Xor
      dw  iWTASK        ;76       Wait for a task or set of tasks to complete
      dw  iTASKPID      ;77

ILTBLend	equ	*
;
;=====================================================
;=====================================================
;=====================================================
; This marks the start of the handlers for IL opcodes.
;=====================================================
;=====================================================
;=====================================================
;
;
iINIT           lda     #0                      ;clear IL stack pointer,gosub stack
                sta     ILSTACKPTR
                sta     MATHSTACKPTR
                sta     GOSUBSTACKPTR
;
                lda     #ProgramStart&$ff        ;user prog
                sta     CURPTR
                sta     taskTable+1
                sta     PROGRAMEND
                lda     #ProgramStart>>8
                sta     CURPTR+1
                sta     taskTable+2
                sta     PROGRAMEND+1
                lda     #1
                sta     taskTable              ;Mark the first slot as active
                sta     taskCounter            ;there is always one task / Main task
                lda     #TASKCYCLESDEFAULT
                sta     taskResetValue
                sta     taskCurrentCycles      ; set up the task switch counts
;
; fall into XINIT...
;
;=====================================================
; This initializes for the start of the next line of
; BASIC text.
;
iXINIT          sei                          ;ensure interupts are off
                jsr       taskReset          ;Clear the task table
                sta       IRQPending         ; reset the irq pending
                sta       IRQStatus          ; Make sure irqs are off

goodExit        jmp       NextIL
;
;=====================================================
; This check if the escape key has been entered
; then changes out of run mode. z Set if esc found
BreakSet:
    jsr ISCHAR
    beq BreakNo
    jsr GETCH
    cmp #$1B
    rts
BreakNo:
    lda #1
    rts

;

;=====================================================
; Verify there is nothing else on this input line.
; If there is, generate an error.
;
iDONE           ldy     CUROFF
                jsr     SkipSpaces
                lda     (CURPTR),y
                beq     doneadv
                cmp     #COLON           ; is it a  ':' or eol
                bne     idoneErr
                sty     CUROFF
                jmp     NextIL           ; continue on this line

idoneErr
                ldx     #ERR_EXTRA_STUFF
                lda     #0
                jmp     iErr2
;
; Advance to the next line
;
doneadv
;               jsr     FindNext2
                jmp     NextIL
;
;=====================================================
; Print the string until a closing quote
;
iPRS		ldy	CUROFF
;
; Odd logic here.  The main loop skipped any leading
; whitespace inside the quoted text, so move back to
; the quote, then move forward again.
;
		jsr	PrtQuoted
		sty	CUROFF
		jmp	NextIL
;
;=====================================================
; Pop the top off the stack and print it as a signed
; decimal number.
;
iPRN		jsr	popR0
		jsr	PrintDecimal
		jmp	NextIL
;
;=====================================================
; Space to next zone.  Currently the code does not
; keep track of which column the output is on, so
; just print a tab.
;
iSPC		lda	#TAB
		jsr	OUTCH
		jmp	NextIL
;
;=====================================================
; If in immediate mode, jump to the address following
; the NXT instruction.  Else move to the next line of
; user code and continue.
;
iNXT            lda     RunMode
                bne     iNxtRun     ;in run mode
;
; Get address and jump to it.
;
                jmp     iJMP
;
iNxtRun
                ldy     CUROFF
                jsr     SkipSpaces
                lda     (CURPTR),y
                cmp     #COLON
                bne     iNxtRunGo
                iny
                jsr     SkipSpaces
                sty     CUROFF
                jmp     iNxtRun2

iNxtRunGo
                jsr     FindNextLine
                jsr     AtEnd
                bne     iNxtRun2          ;not at end
;
; At the end of the program.  Pretend an END statement
; was found.
;
iFINv           jmp     iFIN
;
iNxtRun2        jsr     getILWord     ;ignore next word
                jmp     NextIL
;
;=====================================================
; XFER takes the number on top of the stack and looks
; for that line in the program, or the next line
; higher.  Ie, if it's 1 but there is no line 1, then
; find the next one after that.
;
iXFER           jsr     popR0
                jsr     findLine
iXFER2          jsr     AtEnd           ;at end of user program?
                beq     iFINv
                ldy     #3              ;Change: 2->3 to skip length byte, point to start of text
                sty     CUROFF
                lda     #$ff
                sta     RunMode
;
; Transfer IL to STMT.  I don't like having this
; hard-coded; fix it.
;
                lda     #STMT&$ff
                sta     ILPC
                lda     #STMT>>8
                sta     ILPC+1
                jmp     NextIL
;
; Run
;
iXferok
                lda     #$ff
                sta     RunMode       ;we're running
;
; Need a more elegant way to do this
;
                lda     #STMT&$ff
                sta     ILPC
                lda     #STMT>>8
                sta     ILPC+1
                jmp     NextIL
;
;=====================================================
; Save the pointer to the next line to the call stack.
;
iSAV            jsr     pushLN
                bcs     iSAVErr
                jmp     NextIL

iSAVErr         ldx     #12
iSAVErr2        lda     #0
                jmp     iErr2

;
;=====================================================
; Pop the next line from the call stack.
;
iRET            jsr     popLN
                bcs     iSAVErr
                ldy     #3
                sty     CUROFF
                lda     #0
                sta     IRQPending
                cli
                jmp     NextIL
;
;=====================================================
; Return from IL program call
;
iRSTR           jsr     popLN
                bcs     iSAVErr
                jmp     NextIL

iRSTRErr        ldx     #11
                bne     iSAVErr2
;
;=====================================================
; Compare items on stack.  Okay, so on input there are
; three things on the stack
;
;    EXPR2 <- Top of stack
;    OP    <- relational operator, next on stack
;    EXPR1 <- last item on stack
;
; Comparison is: EXPR1 <operator> EXPR2
;
; Operator is one of...
;
;    2 is =
;    1 is <
;    3 is <=
;    5 is <>
;    4 is >
;    6 is >=
;
; Those are bit-mapped:
;
;    xxxxxGEL
;
;    G = Greater than
;    E = Equal
;    L = Less than
;
; If the comparison is false, do a NXT, ie, move to the
; next line and continue.  If true, continue executing
; on this line.
;
REL_LT		equ	%001
REL_EQUAL	equ	%010
REL_GT		equ	%100
;
iCMPR   jsr   popR1
        jsr   popMQ                      ;operator in MQ
        jsr   popR0
;
; See if they are equal or not
;
        lda   R0
        cmp   R1
        bne   iCMPRnoteq	                ;try not equal
        lda   R0+1
        cmp   R1+1
        bne   iCMPRnoteq
;
; Equal, set the flag in MQ+1
;
        lda   #REL_EQUAL                 ;They Are Equal
        bne   iCMPcom                    ;Exit it is equal
;
; See if EXPR1 (R0) < EXPR2 (R1)
; See www.6502.org/tutorials/compare_beyond.html
;
iCMPRnoteq
        lda   R0
        cmp   R1
        lda   R0+1
        sbc   R1+1
        bvc   iCMPR_2
        eor   #$80
iCMPR_2 bmi   iCMPlt
        lda   #REL_GT
        bne   iCMPcom
iCMPlt  lda   #REL_LT   ; R0 < R1

iCMPcom   ora  MQ+1         ; or with original mask
;
; Now compare the end result with what the caller
; was looking for.
;
        and    MQ
        beq    iCMPno     ; no match
        lda    #$FF       ; true is $ffff
        sta    R0
        sta    R0+1
        bne    iCMPDone
;
; R0 > R1
;
iCMPgt  lda    #REL_GT
        bne    iCMPcom
iCMPno:
        lda    #0
        sta    R0
        lda    #0
        sta    R0+1

iCMPDone:
        jsr    pushR0
        jmp    NextIL
;
; if Not a match, so jump to the next line of code.
; Branches based upon value on top of the stack
iBranch:
                jsr  popR0
                lda  R0
                ora  R0+1
                beq  iBranchFalse ; not true
                jmp  NextIL       ; It is true if any value not zero
;
iBranchFalse:
                jsr   FindNextLine
                jmp   iXFER2
;
;=====================================================
; Start a read of data in background
iReadStart
                lda    #'?          ; Prompt with question mark
                ldx    1            ; Indicate to start read in background
                jsr    GetLine      ; Call the getline to start read
                jmp    NextIL       ; next instruction
;
;=====================================================
; Complete the read and return the curptr, curoff pointing to data
iReadComplete
                jsr     pushLN
                jsr     ReadComplete
                jmp     NextIL
                jsr     popLN
                jmp     NextIL
;=====================================================
; Get a line of text from the user, convert to a
; number, leave on top of stack.
;
iINNUM
                jsr     pushLN
;
                lda     #'?
                ldx     #0              ;Wait for complete
                jsr     GetLine
                jsr     getDecimal
                jsr     pushR0          ;put onto stack
;
                jmp     ExitIn
;
;=====================================================
; Get a line of text from the user, convert to a
; character value , leave on top of stack. up to 2 characters
;
iINSTR
                jsr     pushLN
                lda     #'?
                ldx     #0            ;wait for read complete
                jsr     GetLine
                lda     (CURPTR),y
                sta     R0
                lda     #0
                sta     R0+1
                jsr     pushR0        ;put onto stack
ExitIn
                jsr     popLN
                jmp     NextIL
;
;
;=====================================================
; Stop the currently running program.  Actually very
; simple to do... clear the RunMode flag, then set the
; ILPC to the standard handler and continue running.
;
iFIN            lda     #0
                sta     RunMode
                jsr     taskReset
;
                lda     errGoto
                sta     ILPC
                lda     errGoto+1
                sta     ILPC+1
                jmp     NextIL
;
;=====================================================
; Handle the ERR opcode.  Following the instruction is
; a 16 bit error number.  Print an error message, and
; if we're in run mode, print the line number.  Stop
; program execution and return to the initial state.
;
iERR            jsr     getILWord       ;get err code
;
; Enter here with the error code in X (LSB) and A (MSB).
;
iErr2           stx     R0
                sta     R0+1
;
                jsr     puts
                db      CR,LF,"Error ",0
                jsr     PrintDecimal
;
                lda     RunMode         ;running?
                beq     iERR3           ;nope
                jsr     puts
                db      " at line ",0
                ldy     #1               ;Changed: Skip the leading length byte
iErr2a
                lda     (CURPTR),y
                sta     R0
                iny
                lda     (CURPTR),y
                sta     R0+1
                jsr     PrintDecimal
                jsr     puts
                db      ":",0
                lda     #0
                sta     R0+1
                lda     CUROFF
                clc
                sbc     #3
                sta     R0
                jsr     PrintDecimal
                jsr     puts
                db      ":",0
                lda     taskPtr
                sta     R0
                jsr     PrintDecimal
;
iERR3           jsr     CRLF
                lda     #0
                sta     RunMode     ;fall through...
;
;=====================================================
; Reset the IL to be back at the idle loop.  Does not
; clear variables so the user can see what state
; the program is in.
;
ResetIL         lda     #0
                sta     ILSTACKPTR
                lda     errGoto
                sta     ILPC
                lda     errGoto+1
                sta     ILPC+1
                jmp     NextIL

;
;=====================================================
; Pop two items off stack, add them, then place the
; result back onto the stack.
;
iADD		jsr	popR0
		jsr	popR1
		clc
		lda	R0
		adc	R1
		sta	R0
		lda	R0+1
		adc	R1+1
		sta	R0+1
		jmp	pushR0nextIl
;
;=====================================================
; Pop two items off the stack.  Subtract the top of
; stack from the lower entry.
;
iSUB		jsr	popR1
		jsr	popR0
		sec
		lda	R0
		sbc	R1
		sta	R0
		lda	R0+1
		sbc	R1+1
		sta	R0+1
		jmp	pushR0nextIl
;
;=====================================================
; Negate the top of stack.
;
iNEG		jsr	popR0
		lda	R0
		eor	#$ff
		sta	R0
		lda	R0+1
		eor	#$ff
		sta	R0+1
		inc	R0
		bne	iNEG2
		inc	R0+1
iNEG2		jmp	pushR0nextIl
;
;=====================================================
; Multiply top two items on the stack, put the results
; on top.  This uses the algorithm documented on page
; 115 of "Microprocessor Programming for Computer
; Hobbyists" by Neill Graham.
;
iMUL            jsr     iMultiply
                jmp     NextIL

iMultiply
    jsr	popR0	;AC
		jsr	popR1	;OP
;
		lda	R0
		sta	MQ
		lda	R0+1
		sta	MQ+1
		lda	#0	         ;clear result
		sta	R0
		sta	R0+1
;
		ldx	#16	        ;number of bits in value
multloop	asl	R0
		rol	R0+1
		asl	MQ
		rol	MQ+1
		bcc	multno	     ;skip add if no carry
;
; Add R1 back into R0
;
		clc
		lda	R0
		adc	R1
		sta	R0
		lda	R0+1
		adc	R1+1
		sta	R0+1
;
multno		dex		       ;did all bits yet?
		bne	multloop
		jsr	pushR0	     ;OP
		rts
;
pushR0nextIl
		jsr	pushR0	     ;OP
		jmp	NextIL
;
;=====================================================
; Divide the top of stack into the next to top item.
; Leave results on stack.  Taken from:
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
;
; R0 = R0 / R1
; Remainder is in MQ
;
iDIV  jsr iDoDiv
      jsr	RestoreSigns
		  jmp	pushR0nextIl

iMOD  jsr iDoDiv
      jsr RestoreSigns
      lda MQ
      sta R0
      lda MQ+1
      sta R0+1
      jmp pushR0nextIl

iDoDiv:
      jsr popR1
      jsr popR0
;
; Check for divide by zero
;

iDivNoPop
                lda     R1
                ora     R1+1
                beq     divby0
;
                jsr     SaveSigns
                lda     #0              ;preset remainder to 0
                sta     MQ
                sta     MQ+1
                ldx     #16             ;repeat for each bit: ...
divloop
		asl	R0              ;dividend lb & hb*2, msb -> Carry
		rol	R0+1
		rol MQ              ;remainder lb & hb * 2 + msb from carry
		rol	MQ+1
		lda	MQ
		sec
		sbc	R1              ;substract divisor to see if it fits in
		tay                 ;lb result -> Y, for we may need it later
		lda	MQ+1
		sbc	R1+1
		bcc	skip            ;if carry=0 then divisor didn't fit in yet

		sta	MQ+1            ;else save substraction result as new remainder,
		sty	MQ
		inc	R0              ;and INCrement result cause divisor fit in 1 times

skip		dex
		bne	divloop
		rts
;
; Indicate divide-by-zero error
;
divby0		ldx	#ERR_DIVIDE_ZERO
		lda	#0
		jmp	iErr2
;
;=====================================================
; This pops the top two items off the stack.  The top
; item is a data value and the other is an index into
; the variable table.  Save the value into that entry.
;
iSTORE          jsr     popR0       ;data
                jsr     popR1       ;index
                tya
                pha
                ldy     R1          ;get index
                lda     R0
                sta     (VARIABLES),y
                lda     R0+1
                iny
                sta     (VARIABLES),y
                pla
                tay
                jmp     NextIL
;
;=====================================================
; Replaces the top of stack with the variable whose
; index it represents.
;
iIND
                jsr     popR1
                tya
                pha
                ldy     R1              ;get index
                lda     (VARIABLES),y
                sta     R0
                iny
                lda     (VARIABLES),y
                sta     R0+1
                pla
                tay
                jmp     pushR0nextIl
;
;=====================================================
; Get the array index from top of stack get Current variable
; index from next on stack, add the offset
; push the result back onto the stack
iArray
                jsr     popR0           ; Get the array index
                jsr     popR1           ; Get the Variable number
                lda     R0              ; Verify that it is not zero
                ora     R0+1            
                beq     iArrayError     ; index must not be zero
                dec     R0              ; Basic array index starts at 1
                lda     R0              ; Get the Variable index
                clc
                rol
                adc     R1
                cmp     #(variablesEnd)
                bcs     iArrayError
                sta     R0
                lda     R0+1
                bne     iArrayError     ; Error if the Value is > than last Var index
                jmp     pushR0nextIl
; Get here if array index is out of range
iArrayError
                lda     #0
                ldx     #ERR_INDEX_OUT_OF_RANGE
                jmp     iErr2
;
;=====================================================
; List the current BASIC program in memory.  Uses R0,
; tempIly, and dpl.
;
iLST            jsr     SetOutConsole
iLST2           lda     #ProgramStart&$ff
                sta     dpl
                lda     #ProgramStart>>8
                sta     dpl+1
;
; dpl/dph point to the current line.  See if we're at
; the end of the program.
;
iLSTloop        lda      dpl
                cmp     PROGRAMEND
                bne     iLstNotEnd
                lda     dpl+1
                cmp     PROGRAMEND+1
                beq     iLstdone
;
iLstNotEnd      ldy     #1              ;Change:  Skip first byte length
                lda     (dpl),y         ;line number LSB
                sta     R0
                iny
                lda     (dpl),y	        ;line number MSB
                sta     R0+1
                iny
                sty     tempIlY
                jsr     PrintDecimal
                lda     #SPACE
                jsr     VOUTCH
                ldy     tempIlY
iLSTl2          lda     (dpl),y
                beq     iLST3           ;end of this line 0 value
                sty     tempIlY
                jsr     VOUTCH
                ldy     tempIlY
                iny
                bne     iLSTl2          ;do next char
;
; End of this line.  Print CR/LF, then move to the
; next line.
;
iLST3           iny                     ;Move to next line
                clc                     ;Clear the carry flag
                tya                     ;Current Offset
                adc     dpl             ;Add the offset to the pointer
                sta     dpl             ;Save the new value
                lda     dpl+1           ;Next byte
                adc     #0              ;ad in the carry if any
                sta     dpl+1           ;Save it
;
; Have to manually do CR/LF so it uses the vectored
; output function.
;
                lda     #CR
                jsr     VOUTCH
                lda     #LF
                jsr     VOUTCH
                jmp     iLSTloop        ;do next line
;
iLstdone        jsr     SetOutConsole
                jmp     NextIL
;
;=====================================================
; Get a line of text into LINBUF.  Terminate with a
; null byte.
;
iGETLINE
                lda     #'>            ;prompt character
                ldx     0              ;Wait for read to complete
                jsr     GetLine
;
                lda     #0
                sta     RunMode
                jmp     NextIL
;
;=====================================================
; This is called when the input buffer contains a line
; typed in by the user that starts with a line number.
; Insert the line into the program or delete the line
; if there is nothing after the line number,
;
iINSRT		ldy	#0
		jsr	getDecimal	;convert line #
		jsr	SkipSpaces      ;Ignore any spaces after the line number
		sty	offset		;Save the start of the program line text
;
; Now find the line OR the next higher line OR the
; end of the program.
;
		jsr	findLine        ; Look for the line number in the current program
					; Returns Z and curptr point to the line if found
					; Returns C and curptr at next higher line if not found and there is a higher line
					; Returns ZC clear and curptr to end of program if higher than all other lines
;
; If the line exists, it needs to be removed.
;
		bne	insert2		;jump if no line found higer or a higher line number found, at end of program curptr points to program end
;
; Get length of line to be removed, we fall thru to here if we find a matching line
;
;		jsr	getCURPTRLength	;results in Y , curptr is pointing to point we need to insert the line
                ldy	#0
		lda	(CURPTR),y	;Change the length is now at beginning of the line
		tay
					;If it is equal we delete the line and replace it, get length
					;then adjust all program line after up or down depending on len of line
					;If next higher then just move everythimg down by length bytes
					;This call will return how many bytes in the line we found
		sty	lineLength      ;Save the length of the line we found
;
; Compute the new end of the program first.
;
		sec                     ;Set the carry bit
		lda	PROGRAMEND      ;Get low byte of program end
		sbc	lineLength      ;Subtract the length of the current line
		sta	PROGRAMEND      ;save it
		lda	PROGRAMEND+1
		sbc	#0              ;Process the carry
		sta	PROGRAMEND+1    ;We now have the new end of program with the line removed
;
; Copy CURPTR into R1 for working
;
		lda	CURPTR          ;Save the current position to r1 copy destination
		sta	R1
		lda	CURPTR+1
		sta	R1+1
;
; See if we're at the end.
;
InsDelChk	lda	R1             ;Compare the copy dest to end of memory to check if we are finished copy
		cmp	PROGRAMEND
		bne	InsDelLoop
		lda	R1+1
		cmp	PROGRAMEND+1
		beq	insert2       ;Now the existing line was removed lets go insert the new line
;
; Move one byte, move to next location.
;
InsDelLoop  	ldy	lineLength    ;Move a byte up to remove the space
		beq	insert2       ;if this is zero it is a big oops
		lda	(R1),y
		ldy	#0
		sta	(R1),y
		inc	R1
		bne	InsDelChk
		inc	R1+1
		jmp	InsDelChk     ; Check if we have moved the last byte
;
; Deletion is done.
; If the new line is empty we're done.  Now we have to open a space for the line we are inserting
;
insert2		ldy	offset		;get back ptr  Get the current offset
		lda	LINBUF,y	;next byte     Get the next byte o be stored
		beq	mvUpFini	;empty line    if there is a null then we were deleting a line, no content
;
; CURPTR points to where the line will be inserted.
;
		jsr	getLineLength	;get bytes needed Reload the number of bytes required for the new line
;
		lda	PROGRAMEND      ;Load the start address for the copy
		                        ;At this point curptr still contains the location we will insert data
		sta	FROM
		lda	PROGRAMEND+1
		sta	FROM+1
;
mvup1		ldy	#0		;always zero from From copy position to use indirect addressing
		lda	(FROM),y
		ldy	lineLength      ;Now load y with new offset downward to store the byte
		sta	(FROM),y        ;Save the new byte
;
		lda	FROM            ;Check if we have copies the last byte
		cmp	CURPTR
		bne	mvUpMore
		lda	FROM+1
		cmp	CURPTR+1
		beq	mvUpDone        ; yes from now equals curptr where we insert the new line
;
; Not done yet
;
mvUpMore	lda	FROM	    	;decrement FROM to copy the next byte
		bne	mvUpMore2
		dec	FROM+1
mvUpMore2	dec	FROM
		jmp	mvup1           ;Loop until everything is moved
;
; All done with copy.
;
mvUpDone	clc                     ;Ok, We are now ready to copy the new line to the program
		lda	lineLength      ;Number of bytes to copy from line buff
		adc	PROGRAMEND      ;Now pdate the end of program address for space we just opened
		sta	PROGRAMEND
		lda	PROGRAMEND+1
		adc	#0
		sta	PROGRAMEND+1    ;Program end now points to the correct enpty space
;
;===================jlit use length before line newline

		ldy	#0		;Set offset of copy
                lda     lineLength      ;We will insert the actual length of the line first
                sta     (CURPTR),y      ;Store the length
                iny
		lda	R0              ;Store the line number next
		sta	(CURPTR),y
		iny
		lda	R0+1
		sta	(CURPTR),y
		iny
;
		ldx	offset         ;Load the offset into line buffer in page zero
mvUpLoop2	lda	LINBUF,x       ;get a byte
		sta	(CURPTR),y     ;Store into Space opened, copies the closing null as well
		beq	mvUpFini       ;hit the null at end of line then we are done
		inx
		iny
		bne	mvUpLoop2      ;in case y wraps past 256 bytes stop
;
mvUpFini	jmp	NextIL
;
;=====================================================
; Pops the top value of the ILPC stack and stores it
; in ILPC.  Ie, return from an IL subroutine.
;
iRTN		jsr	popILPC
		jmp	NextIL
;
;=====================================================
; NLINE print a newline
;
iNLINE		jsr	CRLF	;user supplied sub
		jmp	NextIL
;
;=====================================================
; This saves the current ILPC value on the stack, then
; jumps to the address specified by the next two bytes.
;
iCALL     jsr   pushILPC	        ;save ILPC
;
; Jmp to a specific location in the IL code.  The new
; address immediately follows the opcode.
;
iJMP		jsr	getILWord
		stx	ILPC
		sta	ILPC+1
		jmp	NextIL
;
;=====================================================
; Push the next two bytes onto the arithmetic stack.
;
iLIT		jsr	getILWord
		stx	R0
		sta	R0+1
		jsr	pushR0
		jmp	NextIL
;
;=====================================================
; Initialize all variables for a single task.  Ie, set to zero.
;
subVINIT        tya
                pha
                
                lda     #0
                ldy     #0
Vinit2          sta     (VARIABLES),y
                iny
                cpy     #variablesEnd
                bcc     Vinit2
                pla
                tay
                rts
                
iVINIT          jsr     subVINIT
                jmp     NextIL
;
;=====================================================
; Set the address of the error handler.  After any
; error, set to the ILPC to the specified location.
;
iERRGOTO	jsr	getILWord
		stx	errGoto
		sta	errGoto+1
		jmp	NextIL
;
;=====================================================
; TST is followed by an 8 bit signed offset, then a
; null terminated string.  Compare the string against
; the string starting at (CURPTR),CUROFF.  If the
; strings match, continue executing the next IL
; opcode.  Else, add the offset to ILPC.
;
iTST            jsr     getILByte
                sta     offset
;
                jsr     saveIL          ;in case of failure
                ldy     CUROFF
                sty     dpl             ;save for later
;
iTSTloop        jsr     getILByte       ;get next char
                beq     iTSTm           ;match!
                ldy     dpl
                cmp     (CURPTR),y
                beq     iTSTUpper       ; JLIT added 02/08/2022
                ora     #$20            ; lets allow lowercase as well
                cmp     (CURPTR),y
                bne     iTSTfail        ;mismatch
iTSTUpper       iny
                sty     dpl
                bne     iTSTloop
;
; It's a match!  Clean up a bit.
;
iTSTm           ldy     dpl
                sty     CUROFF
                jmp     NextIL
; Test for a single quote
iTSTStr         jsr     getILByte
                sta     offset
                jsr     saveIL
                ldy     CUROFF
                lda     #'"
                cmp     (CURPTR),y
                bne     iTSTfail
                iny
                sty     CUROFF
                jmp     NextILStr
;
; Not a match, reset ILPC and then move to the
; offset.
;
iTSTfail        jsr     restoreIL
                jmp     tstBranch
;
;=================================================JLIT=
; Test if we have a let statement without the let keyword
iTSTLET         jsr     getILByte     ; Get the relative offset byte
                sta     offset        ; Save the jump offset for fails
                jsr     saveIL        ; save to restore when done
                ldy     CUROFF        ; Get the current offset into the buffer
                jsr     SkipSpaces    ; move to first element on line
                sty     CUROFF        ; Just remember skipped spaces
                iny                   ; skip the Variable name
                jsr     SkipSpaces    ; skip any SkipSpaces
                lda     (CURPTR),y    ; Get what should be an equal sign
                cmp     #'=           ; check if equals
                beq     iTSTLETGOOD   ; Yes that is fine
                cmp     #'[           ; Can be a subscript as well
                bne     iTSTfail      ; return it failed
iTSTLETGOOD     jsr     restoreIL     ; restore the IL anyway
                jmp     NextIL        ; Then next instruction

;================================================jLIT=
;Test for end of line
;
iTSTDONE        jsr     getILByte
		    sta     offset
		    jsr	   saveIL
		    ldy	CUROFF
		    sty	dpl
		    jsr	SkipSpaces
		    lda	(CURPTR),y
		    beq	iTSTDONEtrue
		    cmp #COLON
		    beq iTSTDONEtrue
		    ldy	dpl
		    sty	CUROFF
		    jmp	iTSTfail
;
; Advance to the next line
;
iTSTDONEtrue
    jmp	NextIL
;
;=====================================================
; TSTV is followed by an 8 bit signed offset.  If the
; value at (CURPTR),CUROFF appears to be a variable
; name, move to the next IL statement.  Else, add the
; offset to ILPC.
;
iTSTV           jsr     getILByte   ;offset
                sta     offset
;
                ldy     CUROFF
                jsr     SkipSpaces
                lda     (CURPTR),y
;
                ora     #$20            ;make lower then upper
                eor     #$20            ;allow lower case here
                cmp     #'A
                bcc     tstBranch
                cmp     #'Z+1
                bcs     tstBranch
;
; The condition is true, so convert to an index, push
; it onto the stack and continue running.
;
		sec
		sbc	#'A	;index is zero based
		asl		;multiply by two
		sta	R0
		lda	#0
		sta	R0+1
		jsr	pushR0	;put index onto stack
		inc	CUROFF	;move to next input char
		jmp	NextIL
;
;=====================================================
; TSTL seems basically the same as TSTN, but leave the
; value in R0 instead of pushing onto stack.
; This tests for a valid line number
;
iTSTL		jsr	getILByte
		sta	offset
;
		ldy	CUROFF
		jsr	SkipSpaces
		lda	(CURPTR),y
;
		cmp	#'0
		bcc	tstBranch
		cmp	#'9+1
		bcs	tstBranch
;
; It's a digit, so convert to a number.
;
		jsr	getDecimal
		jmp	NextIL
;
;=====================================================
; TSTN checks for a number.  This is very simplistic;
; if the character is a digit, assume it's a number.
; Convert to a number and push it onto the stack.
;
iTSTN		jsr	getILByte
		sta	offset
;
		ldy	CUROFF
		jsr	SkipSpaces
		lda	(CURPTR),y
		cmp	#'-			;negative?
		beq	iTSTN_1
		cmp	#'0
		bcc	tstBranch
		cmp	#'9+1
		bcs	tstBranch
;
; It's a digit, so convert to a number.
;
iTSTN_1		jsr	getDecimal
		sty	CUROFF
		jsr	pushR0		;save onto stack
		jmp	NextIL

;
; Common jump point for all TSTx instructions that
; fail to meet the requirements.  This takes the
; offset and adds/subtracts to/from ILPC.
;
tstBranch       lda     offset          ;get signed offset
                bpl     tstPositive
;
; Do negative branch.  Do sign extension.
;
		clc
		adc	ILPC
		sta	ILPC
		lda	ILPC+1
		adc	#$ff
		sta	ILPC+1
		jmp	NextIL		;keep going
;
tstPositive	clc
		adc	ILPC
		sta	ILPC
		lda	ILPC+1
		adc	#0
		sta	ILPC+1
		jmp	NextIL

;
;====================================================
; Test for IRQ pending, and test if a break key pressed
; Yes I know but this handles all sorts of irq/break issues
;
iTstIrq         jsr     getILByte      ; get the offset to next instruction when not in irq
                sta     offset         ; Store the not true jump address offset
                jsr     BreakSet       ; Check if the escape key was pressed
                bne     irqNo          ; z not set of no break found
                jmp     iFIN           ; Exit out of run mode
irqNo           lda     IRQPending
                beq     tstBranch
                cmp     #1             ; only do this if set to first time
                bne     tstBranch
                sei                    ; disable the interupt until ireturn resets it
irqbrk          inc     IRQPending     ; Set the pending to 2, so this ignores it, iret sets it to 0
                jsr     pushLN         ; Push the next line to be executed
                bcs     irqErra        ; Check if there was an error
                lda     IRQEntry       ; Get the line number to branch to
                sta     CURPTR         ; put line number into r0
                lda     IRQEntry+1
                sta     CURPTR+1
                lda     #3             ; Point to first byte of program text
                sta     CUROFF
                jmp     NextIL         ; Execute the next instruction should jmp statement
irqErra         ldx     #12            ; Flag any error in line number
                lda     #0             ; stop the execution
                jmp     iErr2
;

;=====================================================
; This places the number of free bytes on top of the
; stack.
;
iFREE           jsr     GetSizes
                jsr     pushR0
                jmp     NextIL
;
;=====================================================
; Generate a random number from 0-FFFF and then MOD
; it with the value on top of stack.  Leaves number on
; stack
;
iRANDOM         jsr     popR1                   ;mod value
;
; If the value is zero, just return a one.
;
                lda     R1
                ora     R1+1
                beq     irandom1
;
                lda     random+1
                sta     rtemp1
                lda     random
                asl
                rol     rtemp1
                asl
                rol     rtemp1
                clc
                adc     random
                pha
                lda     rtemp1
                adc     random+1
                sta     random+1
                pla
                adc     #$11
                sta     random
                lda     random+1
                adc     #$36
                sta     random+1

                lda     random
                sta     R0
                lda     random+1
                and     #$7f                  ;make positive
                sta     R0+1
;
; R0 contains the number and R1 contains the max value.
;
                jsr     iDivNoPop
                jsr     RestoreSigns
                lda     MQ
                sta     R0
                lda     MQ+1
                sta     R0+1
                jmp     pushR0nextIl
irandom1
                lda     #0
                sta     R0+1
                lda     #1
                sta     R0
                jmp     pushR0nextIl

; The following replaced by call to division/modulo
;iRANDOM_2	lda	R0
;		cmp	R1
;		bne	iRANDOM_1
;		lda	R0+1
;		cmp	R1+1
;		bne	iRANDOM_1	;need to subtract
;
; Subtract R1 from R0
;
;iRANDOM_sub	sec
;		lda	R0
;		sbc	R1
;		sta	R0
;		lda	R0+1
;		sbc	R1+1
;		sta	R0+1
;		jmp	iRANDOM_2
;
; See if R1 > R0.  If so, branch to subtract.
;
;iRANDOM_1	lda	R0
;		cmp	R1
;		lda	R0+1
;		sbc	R1+1
;		bvc	iRANDOM_4
;		eor	#$80
;iRANDOM_4	bpl	iRANDOM_sub
;
; All done.  Almost.  Add one, then push the result.
;
;irandom1	inc	R0
;		bne	iRANDOM_3
;		inc	R0+1
;iRANDOM_3
;                jsr	pushR0	;return value
;		jmp	NextIL
;
; Poke a value into a memory location
iPOKEMEMORY     sty     tempy
                jsr     popR0
                jsr     popR1
                ldy     #0
                lda     R0
                sta     (R1),y
                ldy     tempy
                jmp     NextIL
;
; Get a value from a memory location
;
iPEEKMEMORY     sty     tempy
                jsr     popR0
                ldy     #0
                lda     (R0),y
                ldy     tempy
                sta     R0
                lda     #0
                sta     R0+1
                jmp     pushR0nextIl
;
; Call to address return what ever is in a to the stack
; func2 will load a value into a before the call
iCallFunc       jsr     popR1
                lda     R1
                jsr     iCallRtn
                sta     R0
                lda     #0
                sta     R0+1
                jsr     pushR0nextIl
iCallRtn
                jsr     popR0
                jmp     (R0)


;===========================================jlit======
;Get a character from the terminal convert to value
;leave the number on top of the stack
;
iGETCHAR:
;                jsr     pushLN      ;Save state before getline
                jsr     GETCH
 if   CTMON65
                pha
                jsr     cout        ;echo echo echo
                pla
 endif
                sta     R0
                lda     #0
                sta     R0+1
                jsr     pushR0
;
;                jsr     popLN
                jmp     NextIL
;===========================================jlit======
;Put a character to the terminal convert to
;
iPUTCHAR:       jsr     popR0
                lda     R0
                jsr     OUTCH
                jmp     NextIL
;=====================================================
; Put the number on the stack out as hex, suppress leading 0
iHexOut
                jsr     popR0
                lda     R0+1
                beq     iHexSecondByte
                jsr     OUTHEX
iHexSecondByte
                lda     R0
                jsr     OUTHEX
                jmp     NextIL
;
;=====================================================
; Replace TOS with its absolute value.
;
iABS            jsr     popR0
                lda     R0+1
                bpl     iABS_1        ;already positive
                eor     #$ff
                sta     R0+1
                lda     R0
                eor     #$ff
                sta     R0
                inc     R0
                bne     iABS_1
                inc     R0+1
iABS_1          jmp     pushR0nextIl

;
;================================================================
; The set of logical operators
iLogAnd
                jsr     popR0
                jsr     popR1
                lda     R0
                and     R1
                sta     R0
                lda     R0+1
                and     R1+1
                sta     R0+1
                jmp     pushR0nextIl
iLogOr
                jsr     popR0
                jsr     popR1
                lda     R0
                ora     R1
                sta     R0
                lda     R0+1
                ora     R1+1
                sta     R0+1
                jmp     pushR0nextIl
iLogXor
                jsr     popR0
                jsr     popR1
                lda     R0
                eor     R1
                sta     R0
                lda     R0+1
                eor     R1+1
                sta     R0+1
                jmp     pushR0nextIl
iLogNot
                jsr     popR0
                lda     R0
                eor     #$FF
                sta     R0
                lda     R0+1
                eor     #$FF
                sta     R0+1
                jmp     pushR0nextIl

iTruth
                lda     #$FF
                sta     R0
                sta     R0+1
                jmp     pushR0nextIl
iFalse
                lda     #$00
                sta     R0
                sta     R0+1
                jmp     pushR0nextIl

;================================================================
;Set the IRQ service rtn line number
;
iSetIrq         sei                 ; disable the interupts
                lda     #0          ; Zero the Status flag
                sta     IRQStatus
                jsr     popR0       ; get the line number
                lda     R0
                ora     R0+1
                beq     iSetExt     ; if it is zero disable all
                jsr     pushLN      ; Save the current line pointer
                jsr     findLine    ; Find the IRQ func Line Pointer
                bne     iSetIrqErr  ; Error if exact line not ound
                lda     CURPTR+1    ; Copy it to the Entry pointer
                sta     IRQEntry+1
                lda     CURPTR
                sta     IRQEntry
                lda     #1          ; Indicate there is an irq gosub
                sta     IRQStatus
                jsr     popLN       ; Restore the old line number
                cli                 ; Enable the interupts
iSetExt         jmp     NextIL

iSetIrqErr      jsr     popLN
                ldx     #ERR_BAD_LINE_NUMBER
                lda     #0
                jmp     iErr2
;


;=====================================================
; Define start of non page zero data
                seg.u     TBData
                org       PROGEND
;=================================================================
;
                include  "tasks.asm"
                include  "support.asm"

	if	DISK_ACCESS
            include       "storage.asm"
	endif
            include       "IL.inc"
;
	if FIXED
            org	$1000
	endif
            include       "basic.il"
PROGEND         equ       *


;=====================================================
; Define start of non page zero data
                seg.u     TBData

;=====================================================
; These are storage items not in page zero.
;
; IRQ BASIC Code Service RTN Support
SaveIrqReg      db      0         ; Store current setting
IRQStatus       db      0         ; 1 = enabled, 0 = dissabled
IRQPending      db      0         ; Irq recieved, Called at next Basic Line
IRQEntry        db      0,0       ; Basic code offset of IRQ Handler

;
;==================================================================================================
; Task Management information
; Tasks may be created by the Task <expr>,<expr>,[<expr>]   Slot number, Cycles per switch command
; Tasks are ended by the Endtask command   This with clear the entry from the task table
; Task switchs happen at the beginning of the next Basic command line
; It will not happen during an input or output operations
; Task switches otherwise are prememtive, The cycle count defaults to 100.
; Task Zero is always the root task, main line program
;
; Layout is repeated for each configured task
; Task Table Byte   use
;            0      Bit 0           Active task
;                   Bit 1           Task IO pending
;            1..CONTEXTLEN          All saved data for the task
taskPtr         ds      1                             ; Current offset into task table CONTEXTLEN modulo entry
taskTable       ds      TASKCOUNT*(CONTEXTLEN+1)      ; Task Table Offset and pointer to Basic code, active flag
TASKTABLEEND    equ     *                             ; End of task table
TASKTABLELEN    equ     TASKTABLEEND-taskTable        ; actual length of the task table

;Task Cycle Counter and reset count
taskCurrentCycles       ds      1
taskResetValue          ds      1
taskCounter             ds      1                     ; Count of active tasks

;
; Math stack and IL call and Gosub/For-next return stack definitions
;
STACKSTART      equ     *
mathStack       ds      MATHSTACKSIZE*2*TASKCOUNT    ;Stack used for math expressions
ilStack         ds      ILSTACKSIZE*2*TASKCOUNT      ;stack used by the IL for calls and returns
gosubStack      ds      GOSUBSTACKSIZE*4*TASKCOUNT   ;stack size for gosub stacks
variableStack   ds      26*2*TASKCOUNT               ;Stack of variables, 26 A-Z
STACKEND        equ     *
STACKLEN        equ     STACKEND-STACKSTART          ; total space used for stacks
;
;
LINBUF          ds      BUFFER_SIZE
getlinx         ds      1         ;temp for x during GetLine functions
printtx         ds      1         ;temp X for print funcs
inputNoWait     ds      1         ;Wait no wait for line buff input
promptChar      ds      1         ;the character to use for a prompt
diddigit        ds      1         ;for leading zero suppression
putsy           ds      1
errGoto         ds      2         ;where to set ILPC on err
sign            ds      1         ;0 = positive, else negative
rtemp1          ds      2         ;Temp for x and y
random          ds      2
BOutVec         ds      2
tempy           ds      1         ;temp y storage
        if      XKIM
buffer          ds      BUFFER_SIZE
        endif
;
; PROGRAMEND is the end of the user's BASIC program.
; More precisely, it is one byte past the end.  Or,
; it's where the next line added to the end will be
; placed.
;
PROGRAMEND      ds      2
HighMem         ds      2         ;highest location
UsedMem         ds      2         ;size of user program
FreeMem         ds      2         ;amount of free memory
;
;=====================================================
; This is the start of the user's BASIC program space.
;
; PERSONAL GOAL: This should be no larger than $0DFF.
;                0200-05FF = 1K
;                0200-09FF = 2K
;                0200-0DFF = 3K
;                0200-11FF = 4K
;                0200-13FF = 4.5K
;
	if FIXED
		org	$2000
	endif
ProgramStart	equ	*
/*
	if	CTMON65 || XKIM
		SEG Code
		org	AutoRun
		dw	TBasicCold
	endif
*/
		end

