
;=====================================================
; IL.inc
; These are macros for IL instructions
;
 mac dw
    .word {0}
 endm
 mac db
    .byte {0}
 endm
	macro XINIT		;reset the il to start clear all
		db	0
		endm
;
	macro DONE		;print an error if not end of line
		db	1
		endm
;
        macro PRS		;print a quoted string
		db	2
		endm
;
        macro PRN		;print a number
		db	3
		endm
;
        macro SPC		;print space til new tabstop
		db	4
		endm
;
	macro NLINE		;print a new line crlf
		db	5
	endm
;
; My NXT is a bit different in that it takes one
; parameter, which is an address.  If the BASIC
; program is currently running then move to the
; next line and continue execution.  However, if
; in direct mode, jump to the specified IL label.
;
    macro	NXT   ; addr
		db	6
		dw	{1}       ; addr
		endm
;
		macro XFER
		db	7
		endm
;
		macro SAV
		db	8
		db  {1}
		endm
;
;  Passed jump if function called, and true false value returned
		macro RSTR
		db	9
		db	({1}-*)-1            ;(addr-*)-1
		db  {2}
		endm
;
		macro CMPR
		db	10
		endm
;
		macro INNUM
		db	11
		endm
;
		macro FIN
		db	12
		endm
;
; ERR is followed by an error number.  The error
; code is printed along with the line number.
; Control is passed to the statement set with
; the ERRGOTO statement.
;
		macro	ERRMSG          ;ecode
		db	13
		dw	{1}              ;ecode
		endm
;
		macro ADD
		db	14
		endm
;
		macro SUB
		db	15
		endm
;
		macro NEG
		db	16
		endm
;
		macro MUL
		db	17
		endm
;
		macro DIV
		db	18
		endm
;
		macro STORE
		db	19
		endm
;
		macro IND
		db	20
		endm
;
		macro LST
		db	21
		endm
;
		macro INIT
		db	22
		endm
;
		macro GETLINE
		db	23
		endm
;
		macro INSERT
		db	24
		endm
;
		macro RTN
		db	25
		endm
;
		macro EXIT
		db	26
		endm
;
		macro	LIT             ; value LIT
		db	27
		dw	{1}                 ;value
		endm
;
		macro	CALL            ;addr
		db	28
		dw	{1}                 ;addr
		endm
;
; IJMP will set the IL PC to the specified value.
;
		macro	IJMP            ;addr
		db	29
		dw	{1}                 ;addr
		endm
;
		macro VINIT
		db	30
		endm
;
; ERRGOTO sets the point in the code where the IL
; interpreter will go after any error.
;
		macro	ERRGOTO  ;addr
		db	31
		dw	{1}      ;addr
		endm
;
		macro	TST                 ;addr,string
		db	32
		db	({1}-*)-1           ;(addr-*)-1
		db	{2},0               ;string,0
		endm
;
		macro	TSTV                ;addr
		db	33
		db	({1}-*)-1           ;(addr-*)-1
		endm
;
		macro	TSTL                 ;addr
		db	34
		db	({1}-*)-1            ;(addr-*)-1
		endm
;
		macro	TSTN                  ;addr
		db	35
		db	({1}-*)-1             ;(addr-*)-1
		endm
;
; FREE returns the amount of free RAM on top of
; the stack.  This is the amount of room the user
; program has available.
;
        macro FREE
		db	36
		endm
;
; RANDOM takes the top item off the stack and
; replaces it with a random number that is
; MOD the initial value.  Ie, if the TOS is
; 42 then RANDOM returns a value from 0 to 41.
;
		macro  RANDOM
		db	37
		endm
;
; ABS will replace the top of stack with the
; absolute value.
;
		macro   ABS
		db	38
		endm
;
; OPENREAD opens a file for reading, as in getting
; statements from it.
;
        macro OPENREAD
		db	39
		endm
;
; OPENWRITE opens a file for writing, as in saving
; the current program to it.
;
        macro OPENWRITE
		db	40
		db  {1}             ; 0= append 1 = create/truncate
		endm
;
; DCLOSE closes any open disk file.
;
		macro DCLOSE
		db	41
		endm
;
; DGETLINE gets one line from the disk file and puts it
; into LINBUFF.
;
	macro DGETLINE
		db	42
		endm
;
; DLIST saves the program to an open disk file.
;
        macro DLIST
		db	43
		endm
; DDIR list the current directory
;
	macro DDIR
		db	44
		endm

; RMFILE remove a fle from disk
	macro RMFILE
		db	45
		endm

; CLEARSCREEN clear the screen
	macro CLEARSCREEN
		db	46
		endm
; POKEMEM Poke value into memory
	macro	POKEMEM
		db	47
		endm
; PEEKMEM peek at value in memory
	macro	PEEKMEM
		db	48
		endm
; TSTLET Test if the statement is a let without the keyword let
	macro	TSTLET                      ;addr
		db	49
		db	({1}-*)-1           ;(addr-*)-1
		endm
; TSTDONE if we reach the end of a statement
	macro	TSTDONE                     ;addr
		db	50
		db	({1}-*)-1           ;(addr-*)-1
		endm
; GETCHAR	get a character from the input line leave it in RO
	macro	GETCHAR
		db 51
		endm
; PUTCHAR	Put a character to the terminal
	macro	PUTCHAR
		db	52
		endm
; Call		Call a machine function return a to stack
	macro	CALLFUNC
		db	53
		endm

; IBRANCH branch if value on stack = 0 false, nextil if value not = zero
  macro IBRANCH
    db 54
  endm

; TSTSTR        Tests for the open quote in a string
	macro	TSTSTR
                db      55
                db	({1}-*)-1           ;(addr-*)-1
                endm
; SETIRQ       Sets the line number to run when an irq happens irq 550
  macro SETIRQ
    db 56
    endm

; TSTIRQ       Test for irq pending,
;              if so push the IRQ LINE NUMBER into RO, onto stack
  macro TSTIRQ
    db 57
    db	({1}-*)-1           ;(addr-*)-1
    endm

; IRET    return from interupt service
  macro IRET
    db 58
    endm

; INSTR   read a string from the input
  macro INSTR
    db 59
    endm

; MODULO Returns the remainder of the division
  macro MODULO
    db 60
    endm
; Set a task line
  macro TASKCREATE
    db 61
  endm
; End a task
  macro ETASK
    db 62
  endm
; Skip to next task
  macro NTASK
    db 63
    endm
; Subscript
  macro SUBSCRIPT
    db 64
    db {1}
    endm
; KILL Task
  macro TASKKILL
    db 65
    endm
; STAT Task
  macro TASKSTAT
    db 66
    endm
;  output value as hex
    macro HEXPRT
    db 67
    endm
;  Read in background has completed
    macro READCOMPLETE
    db  68
    endm
;  ReadInput line
   macro READSTART
    db  69
  endm
; Startio request
   macro STARTIO
    db  70
    endm
; Endio
   macro ENDIO
    db  71
   endm
; Logical not
  macro LOGNOT
    db  72
    endm
; Logical OR
  macro LOGOR
    db 73
    endm
;Logical and
  macro LOGAND
    db 74
  endm
;Logical XOR
  macro LOGXOR
    db 75
  endm
;Wait for task to complete, or list of tasks
  macro WTASK
    db 76
    db	({1}-*)-1           ;(addr-*)-1
  endm
;Get the current task id
  MACRO TASKPID
    db 77
    endm
;Trace the basic execution
  Macro TRACEPROGRAM
    db 78
  endm
;Do a basic program Trace
  Macro DEBUGBASIC
    db 79
    endm

; Inter Process communications instructions
  Macro IPCSEND
    db 80
    endm
  Macro IPCRECEIVE
    db 81
    endm
  Macro IPCCHECK
    db 82
   endm
  Macro IPCIO
    db 83
   endm
  Macro PUSHMATHSTACK
    db 84
    endm
  Macro POPMATHSTACK
    db 85
    endm
  Macro SAVEMATHSTACK
    db 86
    endm
  Macro RESTOREMATHSTACK
    db 87
    endm
  Macro INCPARMCOUNT
    db 88
    endm
  Macro TASKGETMATHSTACK
    db 89
    endm
  Macro TASKENABLE
    db 90
    endm
  Macro TASKSUSPEND
    db 91
    endm
  Macro TASKPUTMATHPTR
    db 92
    endm
; Test for an extension type of variable that allows access to a tasks variables
; Using  PID!<Var name>
  Macro  TSTVT
    db 93
    db	({1}-*)-1           ;(addr-*)-1
    endm

; Provide access to R2 for the IL program
  Macro SETR2
    db 94
    db {1}               ; R2 is only one byte
    endm

;Move stack top to temp
  Macro STK2TMP
   db  95
   endm

  Macro TMP2STK
   db 96
   endm

  Macro TSTBYTE
    db  97
    db ({1}-*)-1          ; (addr-*)-1 goto if match
    dw {2}                ; address to check
    db {3}                ; Value to compare
  endm

  Macro INCVAR
   db 98
   endm
  Macro DECVAR
   db 99
   endm

  Macro SLICE
   db 100
   endm

  Macro TSTB
    db 101
    db ({1}-*)-1
    db {2}
  endm

  Macro TSTW
    db 102
    db ({1}-*)-1
    dw {2}
  endm

 Macro OnGoto
    db 103
    dw {1}
    dw {2}
  endm

 Macro TSTRELOP
    db 104
    db ({1}-*)-1
  endm

  Macro REPEATLINE
    db 105
   endm

; Check for a precompiled branch, will take branch if precompiled value present
  Macro TSTBRANCH
    db 106
    db ({1}-*)-1
  endm

  Macro FASTXFER
    db 107
    endm

; Set all io to to a terminal for input output
; SETTERMINAL outterm,interm    ; the numbers corrispond to the ct65 slot number each slot id 16 bytes,
; so 0 = 0, 1 = io address 16, 2 = io address 32 .... etc
  Macro SETTERMINAL
    db 108
    endm

;Fetch a single byte from memory
  Macro INDB
    db 109
    endm

; Set a block of memory to a fixed value, byte or word
; Stack must contain destiniation address, length to set in bytes, value {byte of word} to initialize to
  Macro SETBLOCK
    db 110
    db {1}              ; the data type to initialize  tByte or tInteger
  endm

; Copy a block of memory
; Stack must contain source address, destination address, length in bytes
  Macro COPYBLOCK
    db 111
  endm

; Compare one block of memory to another
; Stack contains Length, Source1 Source 2
; Place on stack 0 equal, 1 s1>s2, -1 s1 < s2
  Macro CMPMEM
    db 112
  endm

; Shift Left and Right
  Macro SHIFT
    db 113
    db {1}      ; direction  0 = left 1 = right
  endm
