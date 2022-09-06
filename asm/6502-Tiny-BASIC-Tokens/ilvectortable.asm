            seg Code

            dw	iXINIT	;0
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
      dw  iSetR2        ;94
      dw  iStk2Tmp      ;95       Move top of stack to temp
      dw  iTmp2Stk      ;96       Move Temp to stack
      dw  iTSTBYTE      ;97       Test byte and branch if true
      dw  iINCVAR       ;98       Increment variable
      dw  iDECVAR       ;99       Decrement variable
      dw  iSLICE        ;100      set the time slice for tasks
      dw  iTSTB         ;101      Test if byte equals
      dw  iTSTW         ;102      Test If word equals
      dw  iOnGoto       ;103      Branch to table entry based upon buffer value

