
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
		endm
;
		macro RSTR
		db	9
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
