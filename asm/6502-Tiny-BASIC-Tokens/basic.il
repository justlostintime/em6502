             seg  Code
; on goto table
; format :   first byte is value Base, subtracted from value found
;            second byte is the number of entries in the table max is 128... always a limit somewhere... lol
BasicStmts:
                db    kBeginKey,(kKeyCount+1)                ; Base address of table, length of table
                dw    ekLet
                dw    ekInc
                dw    ekDec
                dw    ekIreturn
                dw    ekIf
                dw    ekThen
                dw    ekGoto
                dw    ekGosub
                dw    ekReturn
                dw    ekRem
                dw    ekPrint                               ; should be entry for print
                dw    ekTaske
                dw    ekTaskn
                dw    ekTaskw
                dw    ekPoke
                dw    ekPutch
                dw    ekCls
                dw    ekInput
                dw    ekEnd
                dw    ekIrq
                dw    ekKill
                dw    ekList
                dw    ekRun
                dw    ekNew
                dw    ekSlice
                dw    ekTrace
                dw    ekExit
                dw    ekSave
                dw    ekLoad
                dw    ekErase
                dw    ekDir
                dw    ekSetTerm
                dw    ekSetMemB
                dw    ekSetMemW
                dw    ekCopyMem
;
; Logical operators
BasicLogical    db    kOr,3
                dw    ekOr
                dw    ekXor
                dw    ekAnd

BasicShift:     db    kShr,2
                dw    ekShiftRight
                dw    ekShiftLeft

;functions returning values

BasicFuncs:     db    kBeginFunc,kFuncCount
                dw    ekTrue
                dw    ekFalse
                dw    ekFree
                dw    ekGetch
                dw    ekPeek
                dw    ekTask
                dw    ekIpcc
                dw    ekIpcs
                dw    ekIpcr
                dw    ekRnd
                dw    ekStat
                dw    ekAbs
                dw    ekCall
                dw    ekGofn
                dw    ekPid
                dw    ekAddr
                dw    ekCmpMem
                dw    eKTimer
;====================================================
; file constants
OPEN_APPEND     equ    0             ; append to file
OPEN_CREATE     equ    1             ; truncate/create file

;=====================================================
; This is the IL of the BASIC (or whatever) language.
; Because of the way macros are implemented by as65,
; labels can't be on the same line as a macro
; invocation, so that's why labels are on separate
; lines.
;
IL		equ	*

;THE IL CONTROL SECTION

START:
	INIT                       ;INITIALIZE
	NLINE                      ;WRITE CRLF
	ERRGOTO    CO              ;where to go after an error
	VINIT                      ;clear all variables
;
; This is where we jump to get a line of commands or
; a program from the user.
;
CO:
	GETLINE                    ;WRITE PROMPT AND GET LINE
	TSTL       XEC             ;TEST FOR LINE NUMBER, if none then execute the line
	INSERT                     ;INSERT IT (MAY BE DELETE)
	IJMP       CO
XEC:
	XINIT                      ;INITIALIZE
;============================================================================
;STATEMENT EXECUTOR DO not change the NAME as task manager uses this
;
STMT:
        TSTIRQ      notirq        ;if it is an irq posted, this will cause transfer to irq handler
;==========================================================================================
; Process a let statement implied or explicit.
;
notirq:
        DEBUGBASIC                                ;Check if we are doing a debug for this session
        TSTLET      DoVector                      ; Test if Let keyword or a variable
ekLet:
        TSTV        DoVector                      ; Test for variable
        CALL        FactVarPtrNoTst               ; get a pointer to the variable

LETINDEX_ALL:
        TSTB       ERRMissingEquals,oEqual        ; (This line originally omitted)
LETBE:
        TSTBYTE    LETAssignByte,R2,tByte         ; Check for a byte conversion
        CALL       EXPR                           ; PLACE EXPR VALUE ON MathSTK
        DONE                                      ; REPORT ERROR IF NOT NEXT
        SETR2      tInteger                       ; Store an integer type
        STORE                                     ; STORE RESULT at variable address
        NXT        CO                             ; AND SEQUENCE TO NEXT
        IJMP       STMT                           ; Next statement

LETAssignByte
        CALL       EXPR                           ; PLACE EXPR VALUE ON MathSTK
        DONE                                      ; REPORT ERROR IF NOT NEXT
        SETR2      tByte                          ; Set to store a byte value
        STORE                                     ; STORE RESULT at variable address
        NXT        CO                             ; AND SEQUENCE TO NEXT
        IJMP       STMT                           ; Next statement

ERRMissingEquals
        ERRMSG     ERR_MISSINGEQUALSIGN           ; Missing asignment

ERRVEC2:
        IJMP       UNKNOWN
;=============================================================
;Branch on a valid statement start
DoVector:
        OnGoto     BasicStmts,UNKNOWN             ; use the table provided if not in table branch to unknown
        ;This will never return here!
;=============================================================
; Inc or dec a variable
ekInc:
         TSTV       ERRVEC2                        ; Verify we have a variable
         CALL       FactVarPtrNoTst                ; Get a pointer to the memory location to increment
;        TSTB       eDoInc,oBang                   ; Allow to inc or dec other tasks variables
;        IND                                       ; we just got a pid
;        TSTVT      ERRVEC2                        ; if it is not another variabe then error, Call test var. task
eDoInc:
         INCVAR                                    ; Do the increment of the variable
         DONE                                      ; Test for end of line or end of statement ":"
         NXT        CO                             ; Get the next statement, branch CO if end of program
         IJMP       STMT                           ; Process the next statement
;S1Dec:
;        TSTB       S1Iret,kDec                    ; Dec variable
ekDec:
         TSTV       ERRVEC2                        ; Must be followed by a variable
         CALL       FactVarPtrNoTst                ; Get a pointer to the variable to update
;        TSTB       eDoDec,oBang                   ; Allow to inc or dec other tasks variables
;        IND                                       ; we just got a pid
;        TSTVT      ERRVEC2                        ; if it is not another variabe then error, Call test var. task
eDoDec:
         DECVAR                                    ; Decrement the actual variable
         DONE                                      ; Test if end of line or : statement
         NXT        CO                             ; If at end of program then got the console
         IJMP       STMT                           ; Process the next statement of command line
;=============================================================================================================================
; iret or ireturn, Return from interupt process
;
;S1Iret:
;        TSTB       S1S1,kIreturn               ; test return from interupt
;S1Sa:
ekIreturn:
        DONE                                      ; Must be only thing on the line
        IRET                                      ; RESTORE LINE NUMBER OF CALL
        IJMP       STMT
;==============================================================================================================================
;Process if statement, if true then process all statements until end of line reached
;S1S1:
;        TSTB       S1Z,kIf                        ; IF STATEMENT
ekIf:
        CALL       EXPR                           ; GET EXPRESSION rel ops now valid expression 0 false, everything else true
        TSTB       S1W,kThen                      ; (This line originally omitted) not required
ekThen:
S1W:
        IBRANCH                                   ; PERFORM COMPARISON -- PERFORMS NXT IF FALSE calls iBranch
        IJMP       STMT
;===============================================================================================================================
; Test for GOTO
;S1Z:
;        TSTB       S2,kGoto                       ; YES...TO, OR...SUB
;
ekGoto:
        TSTBRANCH  ekGotoCompiled                 ; test the two byte vector following the goto if zero then normal line lookup
                                                  ; Else we have the address and just goto that address
        TSTB       ekGotoLine,oPeriod             ; If it is a period, then just go to start of this line
        REPEATLINE                                ; Repeat the same line again
        IJMP       STMT                           ; Go do the statement

ekGotoLine:
        CALL       EXPR                           ; GET LABEL
        lit        0                              ; Place indicator for line num on stack
        FASTXFER
ekGotoCompiled:
        lit        1                              ; tell it that mempointer on the stack
        FASTXFER                                  ; put top of stack into curptr


;==================================================================================================================================
; Process REM statement
;
;S2a:
;        TSTB       S3,kRem                        ; REMark.  Skip rest of line
ekRem:
        NXT        CO                             ; The rest of the line is ignored
        IJMP       STMT                           ; Process the next statement
;====================================================================================================================================
; Set the Task input output slot each slot is 16 byte starting at e000
; so 0,1,...
ekSetTerm
        CALL      EXPR
        TSTB      ERRVEC,oComma
        CALL      EXPR
        SETTERMINAL
        NXT        CO              ;Next statement to execute
        IJMP       STMT
;==================================================================================================================================
; Print statement
;
;S3:
;        TSTB       S8,kPrint                      ; ? or Print symonym for print
ekPrint:
        STARTIO                                   ; Lock task until io completes
S4:
        TSTDONE    S4a                            ; Test if we just want crlf printed
        IJMP       S6

S4a:
        TSTB       S7,tString                     ; TEST FOR QUOTED String
        PRS                                       ; PRINT STRING
S5:
        TSTB       S6A,oComma                     ; IS THERE MORE?
        SPC                                       ; SPACE TO NEXT ZONE
        TSTDONE    S4                             ; Not end of line jump back
        IJMP       S6Z                            ; YES JUMP BACK

;
; If a semicolon, don't do anything.
;
S6A:
        TSTB       S6,oSemiColon                  ; IF semicolon also check if end of line
        TSTDONE    S4                             ; Jump Back if not end of line
        IJMP       S6Z

S6:

        DONE                                      ; ERROR IF CR NOT NEXT
        NLINE
S6Z:
        ENDIO                                     ; release task io completed
        NXT        CO                             ; exit here if , or ; at end of print
        IJMP       STMT
;
; A jump for code too far away for relative branch
;
ERRVEC:
        IJMP       UNKNOWN
;
; Get here if there is an expression to print
S7:
        TSTB        S7AUnsigned,oDollar         ; Print the value in Hex format
        CALL       EXPR
        HEXPRT
        IJMP       S5

S7AUnsigned

        TSTB        S7A,oPercent                ; Print the value as an unsigned number
        CALL        EXPR
        SETR2       tUint
        PRN
        IJMP        S5

S7A:

        CALL       EXPR
        TSTB        S7B,oDollar                 ; Print the value as a single character
        PUTCHAR
        IJMP       S5

S7B
        SETR2      tInteger                     ; Print the value as a signed number
        PRN                                     ; PRINT IT
        IJMP       S5                           ; IS THERE MORE?
;
;===========================================================
; PROCESS ALL THE TASK STATEMENTS
;
;S8:
;        TSTB        S8G,kTaske                 ; End Task
ekTaske:
        TSTB        S8NoParm,oLeftBracket
        CALL        EXPR
        TSTB        UNKNOWNLnk,oRightBracket
        ETASK
        DONE
        IJMP       STMT
S8NoParm
        LIT        0
        ETASK
        DONE                       ; Must be last thing on a line
        IJMP       STMT
;
;===========================================================
; The task gives up the rest of the cycles
;S8G:
;        TSTB        S8a,kTaskn         ;Next task
ekTaskn:
        NTASK
        NXT        CO              ;Next statement to execute
        IJMP       STMT
;
;===========================================================
; Waits for a task or list of tasks to complete

ekTaskw:
       TSTB        UNKNOWNLnk,oLeftBracket
S8TSK:
       Call       EXPR            ;Gets the PID of task to wait for
S8LOOP:
       WTASK      S8LOOP          ;Chks for the task PID to finish in a loop, gives up time slice if not done
       TSTB       S8aa,oComma     ;Checks for more tasks
       IJMP       S8TSK           ;Go for the next task number
S8aa:
       TSTB       UNKNOWNLnk,oRightBracket     ;end of list
       DONE
       NXT        CO
       IJMP       STMT            ;Next Statement



UNKNOWNLnk
       iJMP       UNKNOWN

;
;===========================================================
; Update a memory location with a value
;  Use @[offset] to write a word value to memory
;
;S8a1:
;        TSTB        S8b,kPoke                           ; Poke a value into memory
ekPoke:
        TSTB        UNKNOWNV,oLeftBracket                ; opening bracket
        CALL        EXPR                                 ; Get address to write to
        TSTB        UNKNOWNV,oComma                      ; Must have a coma
        CALL        EXPR                                 ; Get the value to poke
        TSTB        UNKNOWNV,oRightBracket               ; closing bracket
        POKEMEM
        DONE
        NXT         CO                ;AND SEQUENCE TO NEXT
        IJMP        STMT
;
;================================================================
; Set a block of memory to a value word or byte
; SetMem(Value, Length, Destination)
ekSetMemB
        CALL       ekSetMem
        SETBLOCK   tByte                                ; set value as byte
        DONE
        NXT         CO
        IJMP        STMT

ekSetMemW
        CALL        ekSetMem
        SETBLOCK    tInteger                                ; set value as byte
        DONE
        NXT         CO
        IJMP        STMT

ekSetMem:
        TSTB        UNKNOWNV,oLeftBracket                ; opening bracket
        CALL        EXPR                                 ; Value to set
        TSTB        UNKNOWNV,oComma                      ; Must have a comma
        CALL        EXPR                                 ; Get The Length
        TSTB        UNKNOWNV,oComma                      ; Must have a comma
        CALL        EXPR                                 ; GET the address to start at
        TSTB        UNKNOWNV,oRightBracket               ; closing bracket
        RTN
;
;================================================================
; Copy a block of memory
; CopyMem(Length, Destination, Source)
ekCopyMem:
        TSTB        UNKNOWNV,oLeftBracket                ; opening bracket
        CALL        EXPR                                 ; Length
        TSTB        UNKNOWNV,oComma                      ; Must have a comma
        CALL        EXPR                                 ; Destination
        TSTB        UNKNOWNV,oComma                      ; Must have a comma
        CALL        EXPR                                 ; Source
        TSTB        UNKNOWNV,oRightBracket               ; closing bracket
        COPYBLOCK                                        ; Copy the memory
        DONE
        NXT         CO
        IJMP        STMT

;================================================================
; Write a single byte to the output device
;
ekPutch:
        CALL        EXPR
        PUTCHAR
        DONE
        NXT         CO                ;AND SEQUENCE TO NEXT
        IJMP        STMT
;================================================================
; Clear the screen lines
;  Uses the vt100 control seq, so must be connected to vt100 terminal
;
ekCls:
        CLEARSCREEN
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
;==================================================================
; Get input from the terminal
;   Reads from the currently active input device
;
ekInput:
S10:
        TSTB       S10A,tString             ;If there is a string print the prompt
        PRS
        TSTB        S10Z,oSemiColon         ;Must follow the prompt
S10A:
        TSTB       InNoString,oDollar       ; Check if we are reading characters
        TSTV       UNKNOWN 
        CALL       FactVarPtrNoTst
        INSTR                               ;Move character From tty to AESTK
        SETR2      tByte
        IJMP       S10A2

InNoString:
        TSTV       UNKNOWN                  ; GET VAR ADDRESS (Originally CALL VAR = nonexist)
        CALL       FactVarPtrNoTst
        INNUM                               ;MOVE NUMBER FROM TTY TO AESTK
        SETR2      tInteger

S10A2:
        STORE                               ;STORE IT
        TSTB        S11,oComma              ;IS THERE MORE?
        IJMP        S10                     ;YES
S10Z:
        iJMP       UNKNOWN
S11:
        DONE                                ;MUST BE CR
        NXT        CO                       ;SEQUENCE TO NEXT
        IJMP       STMT
;=====================================================================
; End of program, return to command line process
; Main Task may also use taske or return to stopped
;
ekEnd:
        FIN
UNKNOWNV:
        IJMP      UNKNOWN
;====================================================================
; IRQ <IRQ-HANDLER-Line expression>
;   Specify a line number subroutine to call when an interupt is processed
;   These subroutines must use iret to return.
;
ekIrq:
        CALL       EXPR            ;Get the LABEL .. line NUMBER
        DONE                       ;must be CR
        SETIRQ                     ;Set the line number now
        NXT        CO              ;SEQUENCE TO NEXT STATEMENT
        IJMP       STMT

;=========================================================================
; KILL PID-expression   kill a running task
;  ignored of task has already stopped
;
ekKill:
        CALL      EXPR
        DONE
        TASKKILL
        NXT       CO
        IJMP      STMT

;============================================================================
; List all program lines
;
ekList:
        DONE
        LST
        IJMP       CO
;=======================================================================
;RUN begin to executed the program in memory
;
ekRun:
        DONE
        VINIT                      ;clear variables compile the line numbers
        LIT        1               ;GOTO line 1
        XFER                       ;Bob's addition
; EXIT
        IJMP       STMT            ;and run!
;=========================================================================
;Clear the program memory, delete all proram lines
;
ekNew:
        DONE
        IJMP       START

;========================================================================
; Slice(slice legth expression)
;   set the length of time between task switches
;
ekSlice:
        CALL       EXPR
        SLICE
        DONE
        NXT         CO
        IJMP        STMT
;==========================================================================
; Turn off and on the thrace functions
; a debug terminal needs to be available
; Trace( Trace flag expression)
;      128 trace IL code, 64 trace basic code, 1 turn on interactive debug
;      for individual lines of basic code. These can be combined
ekTrace:
        TSTB        UNKNOWN,oLeftBracket     ;Are we going to trace
        CALL       EXPR
        TSTB        UNKNOWN,oRightBracket
        TRACEPROGRAM
        DONE
        NXT        CO
        IJMP       STMT
;=====================================================================
; Exit basic to machine monitor
;
ekExit:
        EXIT

;=======================================================================
; Commands related to saving/restoring programs
; to/from mass storage.
;=======================================================================
; Save a program file
;
      if      (XKIM || CTMON65) && DISK_ACCESS

ekSave:
        OPENWRITE  OPEN_CREATE
        DLIST
        DCLOSE
        IJMP       CO
;=========================================================================
; Load a program file
;
ekLoad:
        OPENREAD
S17CLP:
        DGETLINE                   ;get line from file
        TSTL       S17EOL          ;no line num means EOL
        INSERT                     ;put it into the program
        IJMP       S17CLP          ;keep going
S17EOL:
        DCLOSE                     ;close disk file
        IJMP       CO              ;back to start
;=========================================================================
; Display the directory content
;
ekDir:
        DDIR                      ;Display the directory content
        IJMP       CO
;=========================================================================
; Erase a file from disk
;
ekErase:
        RMFILE                    ;Erase the file from the disk
        IJMP       CO

      endif

;===========================================================================
; Else, unknown command.
;
UNKNOWN:
        ENDIO

NotKnownStatement:

        ERRMSG     ERR_SYNTAX      ;SYNTAX ERROR

;=======================================================
; Process Expresions, precidence is represented by the
; various call levels
;
EXPR:
        TSTB        EXPRLOGS,kNot
ekNot:
        Call       EXPR
        LOGNOT
        RTN

;=========================================================
;Look for logical operators, but does get first part of expression
EXPRLOGS:
        Call       EXPRCMP
;=========================================================
;Look for shift left and shift right level
EXPSHIFT:
       OnGoto     BasicShift,EXPRLOGICAL

ekShiftLeft:
        Call      EXPR
        SHIFT     0
        RTN

ekShiftRight:
        Call      EXPR
        SHIFT     1
        RTN

;======================================================================
EXPRLOGICAL
        OnGoto     BasicLogical,iLOGExit
ekAnd:
        Call       EXPR
        LOGAND
        RTN
ekOr:
        Call      EXPR
        LOGOR
        RTN
ekXor:
        Call    EXPR
        LOGXOR
iLOGExit:
        RTN
;========================================================================
; Process relational operators
;
EXPRCMP:
        Call       EXPR2           ; get the left first expression
        TSTRELOP   iRDone          ; Exit level if no rel ops found
;        TSTB       iR0,oEqual
;        LIT        2               ;=
;        IJMP       iRFound
;iR0:
;        TSTB        iR1,oLessEqual
;        LIT         3               ;<=
;        IJMP       iRFound
;iR1:
;        TSTB       iR3,oNotEqual
;        LIT        5                ;<>
;	      IJMP       iRFound
;iR3:
;        TSTB       iR4,oLess
;        LIT        1               ;<
;	      IJMP       iRFound
;iR4:
;        TST        iR5,oGreaterEqual
;        LIT        6               ;>=
;	      IJMP       iRFound
;iR5:
;        TSTB       iRDone,oGreater
;        LIT        4               ;>
iRFound:
        Call       EXPR               ; get the right side of the expression
        CMPR                          ; Push the value of the true false onto the stack
        RTN

iRDone:
        RTN

EXPR2:
        TSTB       E0,oMinus          ; Look for leading - to negate term
        CALL       TERM               ; Get value to negate FOR UNARY -.
        NEG                           ; Make value negated
        IJMP       E1                 ; We have Left term process operators next
E0:
        TSTB       E1A,oPlus          ; Look for a leading + for value and disgard it if found
E1A:
        CALL       TERM               ; Get the left term if it was not negated
E1:
        TST        E2,oPlus           ; Check if we are adding left term to something
        CALL       TERM               ; if adding then get the right side term
        ADD                           ; Add it to left term
        IJMP       E1                 ; look for next + or -
E2:
        TSTB       E3,oMinus          ; Check if we are subtractig something
        CALL       TERM               ; get right side to subtract Diffrence
        SUB                           ; Subtract the value
        IJMP       E1                 ; Look for next + or -
E3:                                   ; Finish processing the expression
        RTN                           ; We are finished processing the Expression
;
; Get one of the terms of an expression
;
TERM:
        CALL       FACT               ; Get a value
T0:                                   ; Check for higher precidence operators
        TSTB        T1,oMultiply      ; Check for *
        CALL       FACT               ; Get right side of term PRODUCT FACTOR.
        MUL                           ; Multiply factors
        IJMP       T0                 ; Check for * or /
T1:
        TSTB        T2,oDivide        ; Check for a division
        CALL        FACT              ; get right side QUOTIENT FACTOR.
        DIV                           ; do division
        IJMP       T0                 ; check for more * or /
T2:
        TSTB       T3,oModulo         ; Check for a division
        CALL       FACT               ; get right side QUOTIENT FACTOR.
        MODULO                        ; do division for remainder
        IJMP       T0                 ; check for more * or / or %
T3                                    ; Finish processing the Term
        RTN

UNKNOWNVEC:
        IJMP       UNKNOWN

;=============================================================================================
; Factor an expression.  Always test for functions
; first or else they'll be confused for variables.
;
FACT:
        OnGoto     BasicFuncs,FactNumber

;FACTCONTINUE:
;        TSTB       F1AA,kTrue
ekTrue:
        LIT       -1
        RTN
;F1AA:
;        TSTB       F1AB,kFalse
ekFalse:
        LIT       0
        RTN
;==================================================================================
; Returns the amount of free SPACE
ekFree:
        TSTB       UNKNOWNVEC,oLeftBracket
        TSTB       UNKNOWNVEC,oRightBracket
        FREE
        RTN
;===================================================================================
; getch() read a character from the input device
;
ekGetch:
        TSTB       UNKNOWNVEC,oLeftBracket
        TSTB       UNKNOWNVEC,oRightBracket
        GETCHAR
        RTN
;
;====================================================================================
; addr(variable definition)
; Returns the address of a variable on top of the stack
ekAddr
        TSTB        UNKNOWNVEC,oLeftBracket
        TSTV        ExpectedVariable
        CALL        FactVarPtrNoTst
        TSTB        UNKNOWNVEC,oRightBracket
        RTN

ExpectedVariable
       ERRMSG     ERR_SYNTAX      ;Expected Variable
;
;================================================================
; Compare a block of memory
; CmpMem(Length, Source 1, Source 2)
; returns on the stack 0 equal, 1 s1>s2, -1 s1<s2
ekCmpMem:
        TSTB        UNKNOWNVEC,oLeftBracket                ; opening bracket
        CALL        EXPR                                   ; Length
        TSTB        UNKNOWNVEC,oComma                      ; Must have a comma
        CALL        EXPR                                   ; Source 1
        TSTB        UNKNOWNVEC,oComma                      ; Must have a comma
        CALL        EXPR                                   ; Source 2
        TSTB        UNKNOWNVEC,oRightBracket               ; closing bracket
        CMPMEM                                             ; Compare the memory
        RTN
;
;===============================================================================
; Set/stop/return timer values
;value 9 = 1 second, 1-5 = value * 10ms 6 = 10ms, 7=250ms, 8=500ms
;cmd   0 =
eKTimer:
        TSTB        UNKNOWNVEC,oLeftBracket                ; opening bracket
        CALL        EXPR                                   ; purpose 0-4
        TSTB        eKTimerZero,oComma                     ; Must have a comma if more than one value
        CALL        EXPR                                   ; only used by set parameters
        iJMP        eKTimerOut                             ; get out
eKTimerZero:
        lit         0                                      ; no second parameter
eKTimerOut:
        TSTB        UNKNOWNVEC,oRightBracket               ; closing bracket
        Timer
        RTN

;====================================================================================
; peek(mem address) return the value of a byte in memory
; @[offset] return a word value from offset -- see tstv
;
;F1A2:
;        TSTB        F2AZ,kPeek                      ;Return a value from memory
ekPeek:
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                            ;Get the address to write to
        TSTB        UNKNOWNVEC,oRightBracket
        PEEKMEM
        RTN
;=======================================================================================
; TASK(line-num expr,[Parm1,....]) start a task with or without Parameters
;
;F2AZ:
;        TSTB        F2AZ1,kTask                     ;Check if we are setting a task start
ekTask:
        TSTBRANCH   ekTaskCompiled
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                            ;Get the LABEL .. line NUMBER
        lit         0
        iJMP        ekTaskLinenum

ekTaskCompiled:
        lit         1

ekTaskLinenum:
        TASKCREATE                                  ;Allocate the task and initialize it, Suspended
        TSTB        F2AZNoParms,oComma              ;Parameters to be passed to task
        SAVEMATHSTACK                               ;Push The mathstack
        TASKGETMATHSTACK                            ;Make the New Task Stack The current stack

F2AZLOOP:
        CALL        EXPR
        TSTB        F2AZEndParm,oComma              ;Parameters to be passed tp task
        IJMP        F2AZLOOP                        ;check for more

F2AZEndParm
        RESTOREMATHSTACK                            ;Back to normal stack
        TASKPUTMATHPTR                              ;Update the tasks stack pointer with parameter count

F2AZNoParms:
        TSTB        UNKNOWNVEC,oRightBracket        ;must be )

        TASKENABLE                                  ;Enable the task to execute

        RTN                                         ;Returns the Task number
;=========================================================================================
; Check for IPC interproccess instructions
;   IPCS  - Send a message
;
;F2AZ1:
;        TSTB        F2AZa,kIpcs                     ;Test if one of the IPC functions
ekIpcs:
        TSTB        UNKNOWNVEC,oLeftBracket         ;IPCS - send a message
        CALL        EXPR                            ;Get the message value
        TSTB        UNKNOWNVEC,oComma
        CALL        EXPR                            ;Get pid of task to send to
        TSTB        UNKNOWNVEC,oRightBracket
        IPCSEND                                     ;Send msg and clear pid msg pending
        RTN
;================================================================================
; IPCR() --- recieve a message, IPCR(VARIABLE) -- receive msg and return pid in var
;
ekIpcr:
        TSTB        UNKNOWNVEC,oLeftBracket         ;IPCS - send a message
        TSTB        F2AZa1,oRightBracket
        LIT         0                               ;We dont want the pid returned to us
        IJMP        ekIpcrComplete
F2AZa1
        TSTV        UNKNOWNVEC                      ;must be a variable to return pid of message to
        TSTB        UNKNOWNVEC,oRightBracket

ekIpcrComplete:
        IPCIO                                       ;Set the io bit and exit task till message
        IPCRECEIVE                                  ;Get the message
        RTN
;===============================================================================
; IPCC ---- check if a message is available
ekIpcc
        TSTB       UNKNOWNVEC,oLeftBracket
        TSTB       UNKNOWNVEC,oRightBracket
        IPCCHECK
        RTN

UNKNOWNID:
        IJMP    UNKNOWN
;============================================================
; RND() is supposed to have an argument but if none
; was provided, just assume a large value.
;
ekRnd:
        TSTB        UNKNOWNVEC,oLeftBracket
        TSTB        F2A1,oRightBracket
        LIT         32766
        RANDOM
        RTN

F2A1:
        CALL        EXPR                          ;GET RANGE
        TSTB        UNKNOWNVEC,oRightBracket
        RANDOM
        RTN
;==========================================================
;      Return absolute value of expresion
;
ekAbs:
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                           ;get value
        TSTB        UNKNOWNVEC,oRightBracket
        ABS
        RTN
;============================================================
;     Return the the status of a task, provide the PID
;
ekStat:
        TSTB        UNKNOWNVEC,oLeftBracket
        Call        EXPR
        TSTB        UNKNOWNVEC,oRightBracket
        TASKSTAT
        RTN
;==============================================================
; Return the current tasks pid
;
ekPid:
        TASKPID
        RTN
;===========================================================================
; Call a machine function, provide the address to call and optionally
; the value to be passed in reg A
ekCall:
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR
        TSTB         F2B2A,oComma
        CALL        EXPR
        TSTB        UNKNOWNVEC,oRightBracket
        CALLFUNC
        RTN
                                                    ; Run the gosub within this function
F2B2A:
        TSTB        UNKNOWNID,oRightBracket
        LIT       0                                 ; No parameter passed so just load zero to A
        CALLFUNC
        RTN


;=================================================================================
;Check for a number !
;
FactNumber:
        TSTN        FactVariable                 ;NUMBER, GET ITS VALUE.
        RTN

FactWithBracket:
        TSTB        F2RTN,oLeftBracket           ;PARENTHESIZED EXPR.
        CALL        EXPR
        TST         F2,oRightBracket
F2RTN:
        RTN

F2:
        ERRMSG     ERR_SYNTAX                     ;ERROR.
;
;=============================================================================
; See if this is just a simple variable
;  Allow a subscript for any variable
;
FactVariable:

        SETR2       tInteger
        TSTV        FactWithBracket
        TSTBYTE     FactFNParm,R2,GOSUB_RTN_VALUE
        CALL        FactVarPTR
;        CALL        FactVariableValue
        IND                                      ; get the variable value
        RTN

FactFNParm:
        TSTB         FactExpectedOpeningBracket,oLeftSQBracket
        CALL         EXPR
        TSTB         FactMissingRightBracket,oRightSQBracket
        SUBSCRIPT    GOSUB_RTN_VALUE
        SETR2        tInteger
        IND
        RTN

FactVarPTR:
FactVarPtrNoTst:
        TSTB        F2CLocalTask,oBang
        IND                                       ; we just got a pid
        TSTVT       UNKNOWNID                     ; if it is not another variable then error, Call test var. task

F2CLocalTask:
        SETR2       tInteger                      ; Sets the default type to integer
        TSTB        F2INTEGER,oDollar             ; Check if this will be a byte array @$[index] or a..z$[index] use a byte array
        SETR2       tByte
        TSTB        F2PTRLOADED,oLeftSQBracket
        CALL        EXPR
        TSTB        FactMissingRightBracket,oRightSQBracket
        SUBSCRIPT   tByte                         ; Byte type array
        SETR2       tByte                         ; Set byte data type
        RTN

F2INTEGER:
        TSTB        F2PTRLOADED,oLeftSQBracket
        CALL        EXPR
        TSTB        FactMissingRightBracket,oRightSQBracket
        SUBSCRIPT   tInteger                      ; Integer type array
        SETR2       tInteger                      ; Set word datatype
        TSTB        F2PTRLOADED,oBang             ; for a word it can be fillowed by a! as ref to other task variables
        IND                                       ; Get the value of the task context to access
        TSTVT       UNKNOWNID                     ; if it is not another variable name then it is a halt error
        IJMP        FactVarPTR                    ; Process the value on the other side
F2PTRLOADED:
        RTN

FactVariableValue
        IND                                       ; YES, GET THE VALUE as word
        RTN

FactMissingRightBracket:
        IJMP       ReturnExpectBracket
FactExpectedOpeningBracket:
        ERRMSG     ERR_EXPECTED_OPENING_BRACKET
;===============================================================================================================================
;========================= Gosub and function interface ========================================================================
; Process gosub 1000(Parm1, ...) no return value
;
ekGosub:
        CALL       GOSUBSTATEMENT                 ; Do the gosub
        DONE                                      ; ERROR IF CR NOT NEXT
        SAV        GOSUB_RTN                      ; SAVE RETURN LINE
        FASTXFER                                  ; AND JUMP to sub rtn
;
; End of gosub processing
;===========================================================================
; Same as gosub but expects a return value
;   FN 1000(parm1, ....)  Expects an integer to be returned
;
ekGofn:
        Call        GOSUBSTATEMENT
        SAV         GOSUB_RTN_VALUE               ;SAVE RETURN LINE
        FASTXFER

GOFNRet:
        RTN
;===============================================================================================================================
; Return from a gosub
;
ekReturn:
        TSTB      S2NoReturnValue,oLeftBracket         ; Check if we will return some value
        CALL      EXPR
        TSTB      ReturnExpectBracket,oRightBracket           ; Now a value is on the stack
        DONE
        RSTR       S2RetFunc,1                    ; decides if call was a func or statement, branch on func, return value
        NXT        CO                             ; SEQUENCE TO NEXT STATEMENT
        IJMP       STMT                           ; Process the new statement

S2NoReturnValue:
        DONE
        RSTR       S2RetFunc,0                    ; decides if call was a func or statement, branch on func, no return value
        NXT        CO                             ; SEQUENCE TO NEXT STATEMENT
        IJMP       STMT                           ; Process the new statement

S2RetFunc:
        IJMP       GOFNRet                        ; Back into the Function

ReturnExpectBracket:
        ENDIO
        ERRMSG      ERR_CLOSINGBRACKET
;
; End of return from gosub
;=============================================================
; Gosub can be both a Function and a Subroutine
GOSUBSTATEMENT
        TSTBRANCH  GOSUBCOMPILED                  ; if the two bytes after gosub are not zero then direct transfer
        CALL       EXPR                           ; GET DESTINATION
        CALL       GOSUBCONT
        lit        0                              ; mark as lookup on stack
        RTN

GOSUBCOMPILED:
        CALL       GOSUBCONT
        lit        1                              ;mark as compiled on stack
        RTN

GOSUBCONT:
        TSTB       GOSUBDONE,oLeftBracket         ;Check if any Parameters If not bracket get out
        STK2TMP                                   ;Transfer stack top to temp, temp gets line to goto
        PUSHMATHSTACK                             ;Record stack frame for return
GOSUBLOOP:
        CALL       EXPR                           ; Allows what ever fits onto stack
        INCPARMCOUNT
        TSTB       GOSUBParmDONE,oComma
        IJMP       GOSUBLOOP
GOSUBParmDONE
        TSTB        ReturnExpectBracket,oRightBracket
        TMP2STK                                   ; Restore line to goto
GOSUBDONE
        RTN

ILEND           equ     *
