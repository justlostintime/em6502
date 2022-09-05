;LET
;=====================================================
;=====================================================
                  seg  Code
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
	TSTL       XEC             ;TEST FOR LINE NUMBER
	INSERT                     ;INSERT IT (MAY BE DELETE)
	IJMP       CO
XEC:
	XINIT                      ;INITIALIZE
;============================================================================
;STATEMENT EXECUTOR DO not change the NAME as task manager uses this
;
STMT:
        DEBUGBASIC                ;Check if we are doing a debug for this session
        TSTIRQ      notirq        ;if it is an irq posted, this will cause transfer to irq handler
;==========================================================================================
; Process a let statement implied or explicit.
;
notirq:
        TSTLET      S1                            ; Test if Let keyword or a variable
DOLET:
        TSTV       ERRVEC                         ; YES, PLACE VAR ADDRESS ON AESTK
        TSTB       LETBE,oLeftSQBracket           ; [
        CALL       EXPR
        TSTB       ERRVEC,oRightSQBracket         ; ]
        SUBSCRIPT
LETBE:
        TSTB       ERRVEC,oEqual                  ; (This line originally omitted)
        CALL       EXPR                           ; PLACE EXPR VALUE ON MathSTK
        DONE                                      ; REPORT ERROR IF NOT NEXT
        STORE                                     ; STORE RESULT
        NXT        CO                             ; AND SEQUENCE TO NEXT
        IJMP       STMT
;=============================================================
; Inc or dec a variable
S1:
        TSTB       S1Dec,kInc                     ; Increment variable
        TSTV       ERRVEC                         ; Verify we have a variable
        INCVAR                                    ; Do the increment of the variable
        DONE                                      ; Test for end of line or end of statement ":"
        NXT        CO                             ; Get the next statement, branch CO if end of program
        IJMP       STMT                           ; Process the next statement
S1Dec:
        TSTB       S1Iret,kDec                    ; Dec variable
        TSTV       ERRVEC                         ; Must be followed by a variable
        DECVAR                                    ; Decrement the actual variable
        DONE                                      ; Test if end of line or : statement
        NXT        CO                             ; If at end of program then got the console
        IJMP       STMT                           ; Process the next statement of command line
;====================================================================
; iret or ireturn, Return from interupt process
;
S1Iret:
        TSTB       S1S1,kIreturn                  ; test return from interupt
        TSTB       S1S1,kIret                     ; Allow the short form as well
S1Sa:
        DONE                                      ; Must be only thing on the line
        IRET                                      ; RESTORE LINE NUMBER OF CALL
        IJMP       STMT
;=======================================================================
;Process if statement, if true then process all statements until end of line reached
S1S1:
        TSTB       S1Z,kIf                        ; IF STATEMENT
        CALL       EXPR                           ; GET EXPRESSION rel ops now valid expression 0 false, everything else true
        TSTB       S1W,kThen                      ; (This line originally omitted) not required
S1W:
        IBRANCH                                   ; PERFORM COMPARISON -- PERFORMS NXT IF FALSE calls iBranch
        IJMP       STMT
;===============================================================
; Test for GOTO
S1Z:
        TSTB       S2,kGoto                       ; YES...TO, OR...SUB
        CALL       EXPR                           ; GET LABEL
        XFER                                      ; SET UP AND JUMP

;===========================================================================
; Process gosub / function
;
S2:
        TSTB       S2b,kGosub                     ; ERROR IF NO MATCH
        CALL       GOSUBSTATEMENT                 ; Do the gosub
        DONE                                      ; ERROR IF CR NOT NEXT
        SAV        GOSUB_RTN                      ; SAVE RETURN LINE
        XFER                                      ; AND JUMP to sub rtn
;
; End of gosub processing
;==========================================================================
; Return from a gosub
S2b:
        TSTB       S2a,kReturn                    ; Speed up pocessing but more memory
        SETR2     0                               ; Default no return value
        TSTB       S2RetDone,oLeftBracket         ; Check if we will return some value
        SETR2     1                               ; Indicate a return value provided
        CALL      EXPR
        TSTB      ERRVEC,oRightBracket            ; Now a value is on the stack

S2RetDone:
        DONE                                      ; MUST BE CR or :
        RSTR                                      ; RESTORE LINE NUMBER OF CALL
        TSTBYTE    S2RetFunc,R2,1                 ; In This case jumps if equal
        NXT        CO                             ; SEQUENCE TO NEXT STATEMENT
        IJMP       STMT                           ; Process the new statement
S2RetFunc:
        IJMP       GOFNRet                        ; Back into the Function
;
; End of return from gosub
;============================================================================
; Process REM statement
;
S2a:
        TSTB       S3,kRem                        ; REMark.  Skip rest of line
        NXT        CO                             ; The rest of the line is ignored
        IJMP       STMT                           ; Process the next statement
;============================================================================
; Print statement
;
S3:
        TSTB       S8,kPrint                      ; ? or Print symonym for print
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
        ENDIO
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
        CALL       EXPR
        SETR2      1
        PRN
        IJMP       S5

S7A

        CALL       EXPR
        TSTB        S7B,oDollar                 ; Print the value as a single character
        PUTCHAR
        IJMP       S5

S7B
        SETR2      0                            ; Print the value as a signed number
        PRN                                     ; PRINT IT
        IJMP       S5                           ; IS THERE MORE?
;
;===========================================================
; PROCESS ALL THE TAST STATEMENTS
;
S8:
        TSTB        S8G,kTaske                 ; End Task
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
S8G:
        TSTB        S8a,kTaskn         ;Next task
        NTASK
        NXT        CO              ;Next statement to execute
        IJMP       STMT
;
;===========================================================
; Waits for a task or list of tasks to complete
S8a:
       TSTB        S8TASKNO,kTaskw                ;Wait for tasks
       TSTB        UNKNOWNLnk,oLeftBracket
S8TSK:
       Call       EXPR            ;Gets the PID of task to wait for
S8LOOP:
       WTASK      S8LOOP          ;Chks for the task PID to finish in a loop, gives up time slice if not done
       TST        S8aa,COMMA      ;Checks for more tasks
       IJMP       S8TSK           ;Go for the next task number
S8aa:
       TSTB       S8TASKNO,oRightBracket     ;end of list
       DONE
       NXT        CO
       IJMP       STMT            ;Next Statement
       
S8TASKNO:

UNKNOWNLnk
       iJMP       UNKNOWN

;
;===========================================================
; Update a memory location with a value
;  Use @[offset] to write a word value to memory
;
S8a1:
        TSTB        S8b,kPoke                           ; Poke a value into memory
        TSTB        UNKNOWNV,oLeftBracket               ; opening bracket
        CALL       EXPR                                 ; Get address to write to
        TSTB        UNKNOWNV,oComma                     ; Must have a coma
        CALL       EXPR                                 ; Get the value to poke
        TSTB        UNKNOWNV,oRightBracket              ; closing bracket
        POKEMEM
        DONE
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
;================================================================
; Write a single byte to the output device
;
S8b:
        TSTB        S8c,kPutch       ;Put a char to the terminal
        CALL       EXPR
        PUTCHAR
        DONE
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
;================================================================
; Clear the screen lines
;  Uses the vt100 control seq, so must be connected to vt100 terminal
;
S8c
        TSTB        S9,kCls          ;Clear the screen
        CLEARSCREEN
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
;==================================================================
; Get input from the terminal
;   Reads from the currently active input device
;
S9:
        TSTB       S13,kInput               ;INPUT STATEMENT
S10:
        TSTB       S10A,tString             ;If there is a string print the prompt
        PRS
        TSTB        S10Z,oSemiColon         ;Must follow the prompt
S10A:
        TSTV       UNKNOWN                  ;GET VAR ADDRESS (Originally CALL VAR = nonexist)
        TSTB       S10A1,oDollar
        INSTR                               ;Move character From tty to AESTK
        IJMP       S10A2
S10A1:
        INNUM                               ;MOVE NUMBER FROM TTY TO AESTK
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
S13:
        TSTB        S14,kEnd
        FIN

UNKNOWNV:
        IJMP      UNKNOWN
;====================================================================
; IRQ <IRQ-HANDLER-Line expression>
;   Specify a line number subroutine to call when an interupt is processed
;   These subroutines must use iret to return.
;
S14:
        TSTB        S14Z,kIrq      ;Check if we are setting IRQ HANDLER
        CALL       EXPR            ;Get the LABEL .. line NUMBER
        DONE                       ;must be CR
        SETIRQ                     ;Set the line number now
        NXT        CO              ;SEQUENCE TO NEXT STATEMENT
        IJMP       STMT

;=========================================================================
; KILL PID-expression   kill a running task
;  ignored of task has already stopped
;
S14Z:
        TSTB       S14S1,kKill     ; Kill A running Task
        CALL      EXPR
        DONE
        TASKKILL
        NXT       CO
        IJMP      STMT

;============================================================================
; List all program lines
;
S14S1:
        TSTB        S15,kList      ;LIST COMMAND
        DONE
        LST
        IJMP       CO
;=======================================================================
;RUN begin to executed the program in memory
;
S15:
        TSTB        S16,kRun       ;RUN COMMAND
        DONE
        VINIT                      ;clear variables
        LIT        1               ;GOTO line 1
        XFER                       ;Bob's addition
; EXIT
        IJMP       STMT            ;and run!
;=========================================================================
;Clear the program memory, delete all proram lines
;
S16:
        TSTB        S16A,kNew      ;clear program
        DONE
        IJMP       START

;========================================================================
; Slice(slice legth expression)
;   set the length of time between task switches
;
S16A:
        TSTB        S16Trace,kSlice
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
S16Trace:
        TSTB        S17A,kTrace
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
S17A:
        TSTB        S17B,kExit      ;allow them to exit BASIC
        EXIT

;=======================================================================
; Commands related to saving/restoring programs
; to/from mass storage.
;
S17B:
      if      (XKIM || CTMON65) && DISK_ACCESS

        TSTB        S17C,kSave
        OPENWRITE
        DLIST
        DCLOSE
        IJMP       CO

S17C:
        TSTB        S18,kLoad
        OPENREAD
S17CLP:
        DGETLINE                   ;get line from file
        TSTL       S17EOL          ;no line num means EOL
        INSERT                     ;put it into the program
        IJMP       S17CLP          ;keep going
S17EOL:
        DCLOSE                     ;close disk file
        IJMP       CO              ;back to start

S18:
        TSTB        S19,kDir
        DDIR                      ;Display the directory content
        IJMP       CO

S19:    TSTB        UNKNOWN,kErase
        RMFILE                    ;Erase the file from the disk
        IJMP       CO

      endif

;
; Else, unknown command.
;
UNKNOWN:
        ENDIO
        ERRMSG     ERR_SYNTAX      ;SYNTAX ERROR

;=======================================================
; Process Expresions, precidence is represented by the
; various call levels
;
EXPR:
        TSTB        EXPRLOGS,kNot
        Call       EXPR
        LOGNOT
        RTN

;=========================================================
;Look for logical operators
EXPRLOGS:
        Call       EXPRCMP
        TSTB        iLOG1,kAnd
        Call       EXPR
        LOGAND
        RTN
iLOG1:
        TSTB       iLOG2,kOr
        Call      EXPR
        LOGOR
        RTN
iLOG2:
        TST     iLOG3,"XOR"
        Call    EXPR
        LOGXOR
        RTN
iLOG3
        RTN

EXPRCMP:
        Call       EXPR2           ; get the first expression
        TSTB       iR0,oEqual
        LIT        2               ;=
        IJMP       iRFound
iR0:
        TSTB        iR1,oLessEqual
        LIT         3               ;<=
        IJMP       iRFound
iR1:
        TSTB       iR3,oNotEqual
        LIT        5                ;<>
	      IJMP       iRFound
iR3:
        TSTB       iR4,oLess
        LIT        1               ;<
	      IJMP       iRFound
iR4:
        TST        iR5,oGreaterEqual
        LIT        6               ;>=
	      IJMP       iRFound
iR5:
        TSTB       iRDone,oGreater
        LIT        4               ;>
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
        TSTB       F1AA,kTrue
        LIT       -1
        RTN
F1AA:
        TSTB       F1AB,kFalse
        LIT       0
        RTN
;==================================================================================
; Returns the amount of free SPACE
;
F1AB
        TSTB       F1A,kFree
        TSTB       UNKNOWNVEC,oLeftBracket
        TSTB       UNKNOWNVEC,oRightBracket
        FREE
        RTN
;===================================================================================
; getch() read a character from the input device
;
F1A:
        TSTB       F1A2,kGetch                      ; read char from the terminal
        TSTB       UNKNOWNVEC,oLeftBracket
        TSTB       UNKNOWNVEC,oRightBracket
        GETCHAR
        RTN
;====================================================================================
; peek(mem address) return the value of a byte in memory
; @[offset] return a word value from offset -- see tstv
;
F1A2:
        TSTB        F2AZ,kPeek                      ;Return a value from memory
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                            ;Get the address to write to
        TSTB        UNKNOWNVEC,oRightBracket
        PEEKMEM
        RTN
;=======================================================================================
; TASK(line-num expr,[Parm1,....]) start a task with or without Parameters
;
F2AZ:
        TSTB        F2AZ1,kTask                     ;Check if we are setting a task start
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                            ;Get the LABEL .. line NUMBER
        TASKCREATE                                  ;Allocate the task and initialize it, Suspended
        TSTB        F2AZNoParms,oComma              ;Parameters to be passed to task
        SAVEMATHSTACK                               ;Push The mathstack
        TASKGETMATHSTACK                            ;Make the New Task Stack The current stack
F2AZLOOP:
        CALL        EXPR                            ;do the expression leave answer on tasks stack
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
F2AZ1:
        TSTB        F2AZa,kIpcs                     ;Test if one of the IPC functions
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
F2AZa:
        TSTB        F2AZb,kIpcr                     ;IPCR recieve a message , wait if none
        TSTB        UNKNOWNVEC,oLeftBracket         ;IPCS - send a message
        TSTB        F2AZa1,oRightBracket
        LIT        0                                ;We dont want the pid returned to us
        IPCIO                                       ;Set the io bit and suspend the task till message
        IPCRECEIVE                                  ;Get the message
        RTN
F2AZa1
        TSTV       UNKNOWNVEC                       ;must be a variable to return pid of message to
        TSTB       UNKNOWNVEC,oRightBracket
        IPCIO                                       ;Set the io bit and exit task till message
        IPCRECEIVE                                  ;Get the message
        RTN
;===============================================================================
; IPCC ---- check if a message is available
F2AZb:
        TST        F2A,kIpcc                        ;Returns number of messages on the message queue
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
F2A:
        TSTB        F2B,kRnd
        TSTB        UNKNOWNVEC,oLeftBracket
        TSTB        F2A1,oRightBracket
        LIT         32766
        RANDOM
        RTN

F2A1:
        CALL        FACT                          ;GET RANGE
        TSTB        UNKNOWNVEC,oRightBracket
        RANDOM
        RTN
;==========================================================
;      Return absolute value of expresion
;
F2B:
        TSTB        F2B2,kAbs
        TSTB        UNKNOWNVEC,oLeftBracket
        CALL        EXPR                           ;get value
        TSTB        UNKNOWNVEC,oRightBracket
        ABS
        RTN
;============================================================
;     Return the the status of a task, provide the PID
;
F2B2:
        TSTB        F2Z,kStat
        TSTB        UNKNOWNVEC,oLeftBracket
        Call        EXPR
        TSTB        UNKNOWNVEC,oRightBracket
        TASKSTAT
        RTN
F2Z:
        TSTB        F2ZA,kPid
        TASKPID
        RTN
;===========================================================================
; Call a machine function, provide the address to call and optionally
; the value to be passed in reg A
F2ZA:
        TSTB         F2FUNC,kCall                    ;call machine function
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
;===========================================================================
; Same as gosub but expects a return value
;   GOFN 1000(parm1, ....)  Expects an integer to be returned
;
F2FUNC  TSTB        F2C,kGofn
        Call        GOSUBSTATEMENT
        SAV         GOSUB_RTN_VALUE               ;SAVE RETURN LINE
        XFER
GOFNRet
        RTN
;=============================================================================
; See if this is just a simple variable
;  Allow a subscript for any variable
;
F2C:
        TSTV        F0
        TSTB        F2CLocalTask,oBang
        IND                                       ; we just got a pid
        TSTVT       UNKNOWNID                      ; if it is not another variabe then error, Call test var. task

F2CLocalTask
        TSTB        F2C1,oLeftSQBracket
        CALL        EXPR
        TSTB        UNKNOWNID,oRightSQBracket
        SUBSCRIPT
F2C1:
        IND                                     ;YES, GET THE VALUE.
        RTN
;=================================================================================
;Finally check for a number !
;
F0:
        TSTN        F1                           ;NUMBER, GET ITS VALUE.
        RTN
F1:
        TSTB        F2RTN,oLeftBracket           ;PARENTHESIZED EXPR.
        CALL        EXPR
        TST         F2,oRightBracket
F2RTN:
        RTN

F2:
        ERRMSG     ERR_SYNTAX                     ;ERROR.
;
;=============================================================
; Gosub can be both a Function and a Subroutine
GOSUBSTATEMENT
        CALL       EXPR                           ;GET DESTINATION
        TSTB       GOSUBDONE,oLeftBracket         ;Check if any Parameters
        STK2TMP                                   ;Transfer stack top to temp
        PUSHMATHSTACK                             ;Record stack frame for return
GOSUBLOOP:
        CALL       EXPR                           ; Allows what ever fits onto stack
        INCPARMCOUNT
        TSTB       GOSUBParmDONE,oComma
        IJMP       GOSUBLOOP
GOSUBParmDONE
        TSTB        F2,oRightBracket
        TMP2STK                    ;Restore line to goto
GOSUBDONE
        RTN

ILEND           equ     *
