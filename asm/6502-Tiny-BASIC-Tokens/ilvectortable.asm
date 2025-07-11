            seg Code
ILTBL1:
      dw    iXINIT        ;0  Initialize the IL
      dw    iDONE         ;1  Verify nothing left on statement line, return error if is
      dw    iPRS          ;2  Print string until closing quote
      dw    iPRN          ;3  Pop the top off the stack and print it as a signed decimal number.
      dw    iSPC          ;4  Space to next zone. Otherwise print a tab
      dw    iNLINE        ;5  Print a newline
      dw    iNXT          ;6  CMD MODE: Jump inst following NXT inst., Else move to next line of user code
      dw    iXFER         ;7  Get line number from top of stack, transfer to it or next higher if not found
      dw    iSAV          ;8  Save a pointer to the next basic code line to the CAll stack
      dw    iRSTR         ;9  Return from gosub or function, pops the line pointer from stack and sets pc
      dw    iCMPR         ;10 Compares top two entries on stach an pushes true or false onto stack, stack enries expected = val1, cmp type, val2
      dw    iINNUM        ;11 Read line of text, convert to number and puch onto stack
      dw    iFIN          ;12 Stop program, enter command mode
      dw    iERR          ;13 Print error code, stop program, enter command mode
      dw    iADD          ;14 Add the two value at top of stack push answer to top of stack
      dw    iSUB          ;15
      dw    iNEG          ;16
      dw    iMUL          ;17
      dw    iDIV          ;18
      dw    iSTORE        ;19 Pops two entries off stack, first is value, second is address where to store it
      dw    iIND          ;20 Indirect fetch, byte or word, pop address from stack, use address to get value and push onto stack
      dw    iLST          ;21 List the current user program in memory
      dw    iINIT         ;22 Initialize the IL virtual machine
      dw    iGETLINE      ;23 Read a line from the terminal, terminate with a null byte
      dw    iINSRT        ;24 Insert a line of user code into a program at correct location
      dw    iRTN          ;25 POP value from IL stack and place into ip program counter, then continue executing from there
      dw    MONITOR       ;26 Exit basic and return to system monitor
      dw    iLIT          ;27 Push a literal value onto the math stack
      dw    iCALL         ;28 Call and IL function, push nextil onto il stach and branch to provided address
      dw    iJMP          ;29 Immeadiate jump to provided address
      dw    iVINIT        ;30 Initialize all variables for a single task.  Ie, set to zero. And internal stack pointers
      dw    iERRGOTO      ;31 Sets the error HAndler Address
      dw    iTST          ;32 compare string, if match continue else jump to provided address
      dw    iTSTV         ;33 Test if next is a variable name, if it is then continue, else branch to provided address
      dw    iTSTL         ;34 Test for valid line number and leave it in r0, branch to provided address if not
      dw    iTSTN         ;35 Check for number, if it is then convert push onto math stack and continue, if not then branch to provided address
      dw    iFREE         ;36 Push the number of free bytes available for user programs into the math stack
      dw    iRANDOM       ;37 push a random number onto the math stack
      dw    iABS          ;38 pop math stack, push asbsolute value onto stack
;
; Disk functions.  There must be pointers
; to functions even if no disk is supported.
; Makes things easier in IL.inc.
; Life, universe, everything(hitch hiker)
; Did you remember your towel?
    if    DISK_ACCESS
      dw    iOPENREAD    ;39  OPen a file for reading
      dw    iOPENWRITE   ;40  Open a file for writing
      dw    iDCLOSE      ;41  Close an open file
      dw    iDGETLINE    ;42  Read a line from an open file
      dw    iDLIST       ;43  List the content of the user program to an open disk file... basically save
      dw    iDDIR        ;44  List the content of the directory on disk
      dw    iRMFILE      ;45  Delete as file from disk
    else
      dw    NextIL       ;39
      dw    NextIL       ;40
      dw    NextIL       ;41
      dw    NextIL       ;42
      dw    NextIL       ;43
      dw    NextIL       ;44
      dw    NextIL       ;45
    endif
;
      dw  iCLEARSCREEN  ;46       Clear the terminal of text
      dw  iPOKEMEMORY   ;47       Put a byte value into memory location, pop value from stack, pop memory address from stack.
      dw  iPEEKMEMORY   ;48       Get a byte from memory, pop memory address from stack
      dw  iTSTLET       ;49       Test if the let with no LET keyword
      dw  iTSTDONE      ;50       Test if we are at the end of a line
      dw  iGETCHAR      ;51       Get a character from the terminal
      dw  iPUTCHAR      ;52       Put a char to the terminal
      dw  iCallFunc     ;53       call a machine func rtn accumulator
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
ILTBL2:
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
      dw  iTASKPID      ;77       Returns the TASK PID
      dw  iTRACEPROG    ;78       Turn on and off il trace, bit 6 = basic trace on,  bit 7 = il trace on
      dw  idbgBasic     ;79       Interactive basic debugging
      dw  iIPCS         ;80       Sending a msg to a task
      dw  iIPCR         ;81       Recieve a message from a task
      dw  iIPCC         ;82       Check if any message available for task
      dw  iIPCIO        ;83       Check if ips queue is empty, suspend task if empty
      dw  iPushMathStack;84       Push the match stack frame pointer and create parameter count
      dw  iPopMathStack ;85       Restore the Math Stack frame after parameters have been passed
      dw  iSaveMathStack;86       Save all math info
      dw  iRestoreMathStack;87    Restore the math stack info
      dw  iIncParmCount ;88       Increment the parameter counter
      dw  iTaskGetMathStack ;89   get another tasks stack pointers
      dw  iTaskEnable   ;90       enable a suspended task
      dw  iTaskSuspend  ;91       Suspend a running task
      dw  iTaskPutMathPtr;92      updates the tasks stack pointer
      dw  iTSTVT        ;93       test for another tasks variable
      dw  iSetR2        ;94       Set the Working register R2 to a value
      dw  iStk2Tmp      ;95       Move top of stack to temp
      dw  iTmp2Stk      ;96       Move Temp to stack
      dw  iTSTBYTE      ;97       Test byte and branch if true
      dw  iINCVAR       ;98       Increment variable
      dw  iDECVAR       ;99       Decrement variable
      dw  iSLICE        ;100      set the time slice for tasks
      dw  iTSTB         ;101      Test if byte equals
      dw  iTSTW         ;102      Test If word equals
      dw  iOnGoto       ;103      Branch to table entry based upon buffer value
      dw  iTSTRELOP     ;104      Test relop, push mask onto stack if true, branch otherwise
      dw  iRepeatLine   ;105      Repeat the same line again, start execution from beginning of the same line
      dw  iTSTBRANCH    ;106      Test for compiled branch, take branch if is, skip two bytes and following integer value(line number) goto, gosub, gofn
      dw  iFastXfer     ;107      move top of stack to curptr
      dw  iSetTerminal  ;108      Set the Io Terminal to be used by print and input statements
      dw  iINDB         ;109      fetch a single byte from memory indirect
      dw  iSetBlock     ;110      Set a block or words or byte to a value 16 bit length
      dw  iCopyBlock    ;111      Copy a block or memory from one location to another 16 bit length
      dw  iCmpBlock     ;112      Compare to parts of memory
      dw  iShift        ;113      Shift left 0 or right 1 as parameters
      dw  iTimer        ;114      Start/Stop/Set timer and enable disable system irq

      dw  iJmpEnd       ;115      Point PC to end of Block if top of math stack is true(0)
      dw  iJmpStart     ;116      Point the PC to beginning of block in stack
      dw  iBeginBlock   ;117      Puts an entry onto the gosub stack for some type of block
      dw  iIfTrue       ;118      Pops the top off math stack and branches if true
      dw  iIfFalse      ;119      Pops the top off math stack and branches if false
      dw  iBadOP        ;120      Invalid IL op code
      dw  iBadOP        ;121      Invalid IL op code
      dw  iBadOP        ;122      Invalid IL op code
      dw  iBadOP        ;123      Invalid IL op code
      dw  iBadOP        ;124      Invalid IL op code
      dw  iBadOP        ;125      Invalid IL op code
      dw  iBadOP        ;126      Invalid IL op code
      dw  iBadOP        ;127      Invalid IL op code
