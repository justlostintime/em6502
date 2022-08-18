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

;STATEMENT EXECUTOR DO not change the NAME as task manager uses this
STMT:
        DEBUGBASIC                ;Check if we are doing a debug for this session
        TSTIRQ      notirq        ;if it is an irq posted, this will cause transfer to irq handler
notirq:
        TSTLET      LET           ;Test if second field is =
        IJMP        DOLET         ;allow the default to be let
LET:
        TST        S1,"LET"       ;IS STATEMENT A LET
DOLET:
        TSTV       ERRVEC         ;YES, PLACE VAR ADDRESS ON AESTK
        TST        LETBE,"["
        CALL       EXPR
        TST        ERRVEC,"]"
        SUBSCRIPT
LETBE:
        TST        ERRVEC,"="      ;(This line originally omitted)
        CALL       EXPR            ;PLACE EXPR VALUE ON AESTK
        DONE                       ;REPORT ERROR IF NOT NEXT
        STORE                      ;STORE RESULT
        NXT        CO              ;AND SEQUENCE TO NEXT
        IJMP       STMT
S1:
        TST        S1Dec,"INC"     ;Increment variable
        TSTV       ERRVEC
        INCVAR
        DONE
        NXT        CO
        IJMP       STMT
S1Dec:
        TST        S1Iret,"DEC"     ;Increment variable
        TSTV       ERRVEC
        DECVAR
        DONE
        NXT        CO
        IJMP       STMT
S1Iret:
        TST        S1S1,"IRET"     ;test return from interupt
        TST        S1Sa,"URN"
S1Sa:
        DONE                       ;Must be only thing on the line
        IRET                       ;RESTORE LINE NUMBER OF CALL
        IJMP       STMT

S1S1:
        TST        S1Z,"IF"        ;IF STATEMENT
        CALL       EXPR            ;GET EXPRESSION rel ops now valis expression 0 false, everything else true
        TST        S1W,"THEN"       ;(This line originally omitted) not required
S1W:
        IBRANCH                    ;PERFORM COMPARISON -- PERFORMS NXT IF FALSE calls iBranch
        IJMP       STMT

S1Z:
        TST        S2b,"GO"        ;GOTO OT GOSUB?
        TST        S2,"TO"         ;YES...TO, OR...SUB
        CALL       EXPR            ;GET LABEL
        XFER                       ;SET UP AND JUMP
S2:
;===========================================================================
; Process gosub / function
;
        TST        ERRVEC,"SUB"     ;ERROR IF NO MATCH
        CALL       GOSUBSTATEMENT   ;Do the gosub
        DONE                        ;ERROR IF CR NOT NEXT
        SAV        GOSUB_RTN        ;SAVE RETURN LINE
        XFER                        ;AND JUMP
;
; End of gosub processing
;==========================================================================
; Return from a gosub
S2b:
        TST        S3,"RE"         ;Speed up pocessing but more memory
        TST        S2a,"T"
        TST        S2aa,"URN"      ; RETURN STATEMENT
S2aa:
        SETR2     0                ; Default no return value
        TST       S2RetDone,"("    ; Check if we will return some value
        SETR2     1                ; Indicate a return value provided
        CALL      EXPR
        TST       ERRVEC,")"       ; Now a value is on the stack

S2RetDone:
        DONE                          ; MUST BE CR or :
        RSTR                          ; RESTORE LINE NUMBER OF CALL
        TSTBYTE    S2RetFunc,R2,1     ; In This case jumps if equal
        NXT        CO                 ; SEQUENCE TO NEXT STATEMENT
        IJMP       STMT
S2RetFunc:
        IJMP       GOFNRet              ; Back into the Function
;
; End of return from gosub
;============================================================================
; Process REM statement
S2a:
        TST        S3,"M"          ;REMark.  Skip rest of line
        NXT        CO
        IJMP       STMT

S3:
        TST        S3a,"?"         ; ? symonym for print
        IJMP       S4S
S3a:
        TST        S8,"PR"         ;allow short form of print
        TST        S4S,"INT"       ;PRINT
S4S:
        STARTIO                    ;Lock task until io completes
S4:
        TSTDONE    S4a             ;Test if we just want crlf printed
        IJMP       S6

S4a:
        TSTSTR     S7              ;TEST FOR QUOTED String
        PRS                        ;PRINT STRING
S5:
        TST        S6A,COMMA       ;IS THERE MORE?
        SPC                        ;SPACE TO NEXT ZONE
        TSTDONE    S4              ;Not end of line jump back
        IJMP       S6Z             ;YES JUMP BACK

;
; If a semicolon, don't do anything.
;
S6A:
        TST        S6,SEMICOLON    ;IF semicolon also check if end of line
        TSTDONE    S4              ;Jump Back if not end of line
        IJMP       S6Z
S6:
        DONE                       ;ERROR IF CR NOT NEXT
        NLINE
S6Z:
        ENDIO                      ;release task io completed
        NXT        CO              ;exit here if , or ; at end of print
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
        TST        S7A,"$"
        CALL       EXPR
        HEXPRT
        IJMP       S5
S7A
        CALL       EXPR
        TST        S7B,"$"
        PUTCHAR
        IJMP       S5
S7B
        PRN                        ;PRINT IT
        IJMP       S5              ;IS THERE MORE?
;
;===========================================================
; End of TASK process
S8:
        TST        S8a1,"TASK"     ; Any Task COmmand
        TST        S8G,"E"
        TST        S8NoParm,"("
        CALL       EXPR
        TST        UNKNOWNLnk,")"
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
        TST        S8a,"N"         ;Next task
        NTASK
        NXT        CO              ;Next statement to execute
        IJMP       STMT
;
;===========================================================
; Waits for a task or list of tasks to complete
S8a:
       TST        S8TASKNO,"W("   ;Wait for tasks
S8TSK:
       Call       EXPR            ;Gets the PID of task to wait for
S8LOOP:
       WTASK      S8LOOP          ;Chks for the task PID to finish in a loop, gives up time slice if not done
       TST        S8aa,COMMA      ;Checks for more tasks
       IJMP       S8TSK           ;Go for the next task number
S8aa:
       TST        S8TASKNO,")"     ;end of list
       DONE
       NXT        CO
       IJMP       STMT            ;Next Statement
S8TASKNO:

UNKNOWNLnk
       iJMP       UNKNOWN

;
;===========================================================
; Update a memory location with a value
S8a1:
        TST        S8b,"POKE("       ; Poke a value into memory
        CALL       EXPR              ; Get address to write to
        TST        UNKNOWNV,COMMA     ; Must have a coma
        CALL       EXPR              ; Get the value to poke
        TST        UNKNOWNV,")"       ; closing bracket
        POKEMEM
        DONE
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT

S8b:
        TST        S8c,"PUTCH"       ;Put a char to the terminal
        CALL       EXPR
        PUTCHAR
        DONE
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
S8c
        TST        S9,"CLS"          ;Clear the screen
        CLEARSCREEN
        NXT        CO                ;AND SEQUENCE TO NEXT
        IJMP       STMT
S9:
        TST        S13,"INPUT"     ;INPUT STATEMENT
S10:
        TSTSTR     S10A            ;If there is a string print the prompt
        PRS
        TST        S10Z,SEMICOLON  ;Must follow the prompt
S10A:
        TSTV       UNKNOWN         ;GET VAR ADDRESS (Originally CALL VAR = nonexist)
        TST        S10A1,DOLLAR
        INSTR                      ;Move character From tty to AESTK
        IJMP       S10A2
S10A1:
        INNUM                      ;MOVE NUMBER FROM TTY TO AESTK
S10A2:
        STORE                      ;STORE IT
        TST        S11,COMMA       ;IS THERE MORE?
        IJMP       S10             ;YES
S10Z:
        iJMP       UNKNOWN
S11:
        DONE                       ;MUST BE CR
        NXT        CO              ;SEQUENCE TO NEXT
        IJMP       STMT
S13:
        TST        S14,"END"
        FIN

UNKNOWNV:
        IJMP      UNKNOWN

S14:
        TST        S14Z,"IRQ"      ;Check if we are setting IRQ HANDLER
        CALL       EXPR            ;Get the LABEL .. line NUMBER
        DONE                       ;must be CR
        SETIRQ                     ;Set the line number now
        NXT        CO              ;SEQUENCE TO NEXT STATEMENT
        IJMP       STMT

S14Z:
        TST       S14S1,"KILL"     ; Kill A running Task
        CALL      EXPR
        DONE
        TASKKILL
        NXT       CO
        IJMP      STMT

S14S1:
        TST        S15,"LIST"      ;LIST COMMAND
        DONE
        LST
        IJMP       CO
S15:
        TST        S16,"RUN"       ;RUN COMMAND
        DONE
        VINIT                      ;clear variables
        LIT        1               ;GOTO line 1
        XFER                       ;Bob's addition
; EXIT
        IJMP       STMT            ;and run!
S16:
        TST        S16A,"NEW"      ;clear program
        DONE
        IJMP       START

S16A:
        TST        S16Trace,"SLICE"
        CALL       EXPR
        SLICE
        DONE
        NXT         CO
        IJMP        STMT

S16Trace:
        TST        S17A,"TRACE("     ;Are we going to trace
        CALL       EXPR
        TST        UNKNOWN,")"
        TRACEPROGRAM
        DONE
        NXT        CO
        IJMP       STMT

S17A:
        TST        S17B,"EXIT"      ;allow them to exit BASIC
        EXIT

;
; Commands related to saving/restoring programs
; to/from mass storage.
;
S17B:
      if      (XKIM || CTMON65) && DISK_ACCESS

        TST        S17C,"SAVE"
        OPENWRITE
        DLIST
        DCLOSE
        IJMP       CO

S17C:
        TST        S18,"LOAD"
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
        TST        S19,"DIR"
        DDIR                      ;Display the directory content
        IJMP       CO
      endif

S19:    TST        UNKNOWN,"ERASE"
        RMFILE                    ;Erase the file from the disk
        IJMP       CO
      endif

;
; Else, unknown command.
;
UNKNOWN:
        ENDIO
        ERRMSG     ERR_SYNTAX      ;SYNTAX ERROR

;-----------------------------------------------------
EXPR:
        TST        EXPRNOT,"NOT"
        Call       EXPR
        LOGNOT
        RTN
EXPRNOT:

EXPRLOGS:
        Call       EXPRCMP
        TST        iLOG1,"AND"
        Call       EXPR
        LOGAND
        RTN
iLOG1:
        TST       iLOG2,"OR"
  Call      EXPR
  LOGOR
  RTN
iLOG2:
  TST     iLOG3,"XOR"
  Call    EXPR
  LOGXOR
  RTN
iLOG3:
  RTN

EXPRCMP:
  Call       EXPR2           ; get the first expression
	TST        iR0,"="
	LIT        2               ;=
	IJMP       iRFound
iR0:
	TST        iR4,"<"
	TST        iR1,"="
	LIT        3               ;<=
	IJMP       iRFound
iR1:
	TST        iR3,">"
	LIT        5	              ;<>
	IJMP       iRFound
iR3:
	LIT        1               ;<
	IJMP       iRFound
iR4:
	TST        iRDone,">"
	TST        iR5,"="
	LIT        6               ;>=
	IJMP       iRFound
iR5:
	TST        iR6,"<"
	LIT        1
	IJMP       iRFound          ;(This line originally omitted)
iR6:
	LIT        4               ;>
iRFound:
  Call       EXPR2           ; get the right side of the expression
  CMPR                       ; Push the value of the true false onto the stack

iRDone:
  RTN

EXPR2:
	TST        E0,"-"          ; Look for leading - to negate term
	CALL       TERM            ; Get value to negate FOR UNARY -.
	NEG                        ; Make value negated
	IJMP       E1              ; We have Left term process operators next
E0:
	TST        E1A,"+"         ; Look for a leading + for value and disgard it if found
E1A:
	CALL       TERM            ; Get the left term if it was not negated
E1:
	TST        E2,"+"          ; Check if we are adding left term to something
	CALL       TERM            ; if adding then get the right side term
	ADD                        ; Add it to left term
	IJMP       E1              ; look for next + or -
E2:
	TST        E3,"-"          ; Check if we are subtractig something
	CALL       TERM            ; get right side to subtract Diffrence
	SUB                        ; Subtract the value
	IJMP       E1              ; Look for next + or -
E3:                          ; Finish processing the expression
	RTN                        ; We are finished processing the Expression
;
; Get one of the terms of an expression
;
TERM:
	CALL       FACT            ; Get a value
T0:                          ; Check for higher precidence operators
	TST        T1,"*"          ; Check for *
	CALL       FACT            ; Get right side of term PRODUCT FACTOR.
	MUL                        ; Multiply factors
	IJMP	  T0                   ; Check for * or /
T1:
	TST        T2,"/"          ; Check for a division
	CALL       FACT            ; get right side QUOTIENT FACTOR.
	DIV                        ; do division
	IJMP       T0              ; check for more * or /
T2:
  TST        T3,"%"          ; Check for a division
	CALL       FACT            ; get right side QUOTIENT FACTOR.
	MODULO                     ; do division for remainder
	IJMP       T0              ; check for more * or / or %
T3                           ; Finish processing the Term
  RTN

UNKNOWNVEC:
        IJMP       UNKNOWN

;
; Factor an expression.  Always test for functions
; first or else they'll be confused for variables.
;
FACT:
        TST       F1AA,"TRUE"
        LIT       -1
        RTN
F1AA:
        TST       F1AB,"FALSE"
        LIT       0
        RTN
F1AB
        TST       F1A,"FREE()"
        FREE
        RTN
F1A:
        TST       F1A2,"GETCH()"   ; read char from the terminal
        GETCHAR
        RTN
F1A2:
        TST       F2AZ,"PEEK("        ;Return a value from memory
        CALL      EXPR                ;Get the address to write to
        TST       UNKNOWNVEC,")"      ;Closing bracket
        PEEKMEM
        RTN
F2AZ:
        TST        F2AZ1,"TASK("      ;Check if we are setting a task start
        CALL       EXPR               ;Get the LABEL .. line NUMBER
        TASKCREATE                    ;Allocate the task and initialize it, Suspended
        TST        F2AZNoParms,COMMA  ;Parameters to be passed to task
        SAVEMATHSTACK                 ;Push The mathstack
        TASKGETMATHSTACK              ;Make the New Task Stack The current stack
F2AZLOOP:
        CALL       EXPR               ;do the expression leave answer on tasks stack
        TST        F2AZEndParm,COMMA  ;Parameters to be passed tp task
        IJMP       F2AZLOOP           ;check for more
F2AZEndParm
        RESTOREMATHSTACK              ;Back to normal stack
        TASKPUTMATHPTR                ;Update the tasks stack pointer with parameter count
F2AZNoParms:
        TST        UNKNOWNVEC,")"   ;must be )
        TASKENABLE                  ;Enable the task to execute
        RTN                         ;Returns the Task number
F2AZ1:
        TST        F2A,"IPC"        ;Test if one of the IPC functions
        TST        F2AZa,"S("       ;IPCS - send a message
        CALL       EXPR             ;Get the message value
        TST        UNKNOWNVEC,COMMA
        CALL       EXPR             ;Get pid of task to send to
        TST        UNKNOWNVEC,")"
        IPCSEND                     ;Send msg and clear pid msg pending
        RTN
F2AZa:
        TST        F2AZb,"R("       ;IPCR recieve a message , wait if none
        TST        F2AZa1,")"
        LIT        0               ;We dont want the pid returned to us
        IPCIO                      ;Set the io bit and suspend the task till message
        IPCRECEIVE                 ;Get the message
        RTN
F2AZa1
        TSTV       UNKNOWNVEC      ;must be a variable to return pid of message to
        TST        UNKNOWNVEC,")"
        IPCIO                      ;Set the io bit and exit task till message
        IPCRECEIVE                 ;Get the message
        RTN
F2AZb:
        TST        UNKNOWNVEC,"C()";Returns number of messages on the message queue
        IPCCHECK
        RTN

UNKNOWNID:
        IJMP    UNKNOWN
;
; RND() is supposed to have an argument but if none
; was provided, just assume a large value.
;
F2A:
        TST        F2B,"RND("
        TST        F2A1,")"
        LIT        32766
        RANDOM
        RTN

F2A1:
        CALL       FACT            ;GET RANGE
        TST        UNKNOWNVEC,")"
        RANDOM
        RTN

F2B:
        TST        F2B2,"ABS("
        CALL       EXPR            ;get value
        TST        UNKNOWNVEC,")"
        ABS
        RTN

F2B2:
        TST       F2Z,"STAT("
        Call      EXPR
        TST       UNKNOWNVEC,")"
        TASKSTAT
        RTN
F2Z:
        TST       F2ZA,"PID"
        TASKPID
        RTN
F2ZA:
        TST       F2FUNC,"CALL("                    ;call machine function
        CALL      EXPR
        TST       F2B2A,COMMA
        CALL      EXPR
        TST       UNKNOWNVEC,")"
        CALLFUNC
        RTN
                                  ; Run the gosub within this function
F2B2A:
        TST       UNKNOWNID,")"
        LIT       0
        CALLFUNC
        RTN

F2FUNC  TST        F2C,"GOFN"
        Call       GOSUBSTATEMENT
        SAV        GOSUB_RTN_VALUE               ;SAVE RETURN LINE
        XFER
GOFNRet
        RTN
F2C:
        TSTV       F0
        TST        F2CLocalTask,"!"
        IND                             ; we just got a pid
        TSTVT      UNKNOWNID            ; if it is not another variabe then error, Call test var. task

F2CLocalTask
        TST        F2C1,"["
        CALL       EXPR
        TST        UNKNOWNID,"]"
        SUBSCRIPT
F2C1:
        IND                            ;YES, GET THE VALUE.
        RTN
F0:
        TSTN       F1                  ;NUMBER, GET ITS VALUE.
        RTN
F1:
        TST        F2RTN,"("           ;PARENTHESIZED EXPR.
        CALL       EXPR
        TST        F2,")"
F2RTN:
        RTN

F2:
        ERRMSG     ERR_SYNTAX          ;ERROR.
;
;=============================================================
; Gosub can be both a Function and a Subroutine
GOSUBSTATEMENT
        CALL       EXPR            ;GET DESTINATION
        TST        GOSUBDONE,"("   ;Check if any Parameters
        STK2TMP                    ;Transfer stack top to temp
        PUSHMATHSTACK              ;Record stack frame for return
GOSUBLOOP:
        CALL       EXPR            ; Allows what ever fits onto stack
        INCPARMCOUNT
        TST        GOSUBParmDONE,COMMA
        IJMP       GOSUBLOOP
GOSUBParmDONE
        TST        F2,")"
        TMP2STK                    ;Restore line to goto
GOSUBDONE
        RTN

ILEND           equ     *
