; Gosub stack definitions and structure information
; The gosub stack is used by, while, for, gosub, function parameters
; Task parameter passing as well, math stack frame info
;
;==================================================================
; gosub stack entry formats, always contains a typw identifier
; gosub: no return value, no parameters
; byte 0   : Math Stack pointer
; byte 1   : The number of parameters being passed on the math stack always 0
; byte 2   : Not used always 0
; byte 3   : type of entry #GOSUB_STACK_FRAME
;
; gosub with parameters, no return value
; byte 0   : Math Stack pointer
; byte 1   : The number of parameters being passed on the math stack 1-255
; byte 2   : Not used always 0
; byte 3   : type of entry #GOSUB_STACK_FRAME

; gosub with parameters, return value
; byte 0   : Math Stack pointer
; byte 1   : The number of parameters being passed on the math stack 1-255
; byte 2   : Not used always 0
; byte 3   : type of entry #GOSUB_STACK_FRAME
; Value is return in R0
;
; Task with parameters, return value always in variable array
;  For tasks just like gosub calls, the parameters are stored onto the match stack
;  and the parameter stack frame is stored onto the gosub stack

; For next step, continue, break
; byte: 0,1  pc counter to return to test
; byte: 2,3  pc counter to exit to
; byte: 4    type of entry #GOSUB_FOR

; while..wend, continue, break
; byte: 0,1  pc counter to return to test
; byte: 2.3  pc counter to exit to
; byte: 4    type of entry #GOSUB_WHILE

;
; Gosub entry types

GOSUB_RTN         equ    $01      ; This is a simple gosub return
GOSUB_RTN_VALUE   equ    $81      ; subroutine will return a value
GOSUB_FOR         equ     2       ; Return point for FOR..Next
GOSUB_While       equ     3       ; Return point for the While.. wend
GOSUB_MSG         equ     4       ; this identifies the entry as an IPC message
GOSUB_STACK_FRAME equ     5       ; used to contain the gosubs stackframe info when passing parameters
                                  ; 0 byte is the original stack offset, byte 1 is the parameter count
                                  ; 2 byte  is unused, 3 byte  is the type GOSUB_STACK_FRAME
GOSUB_STACK_SAVE  equ     6       ; SAVES THE FULL MATH STACK INFORMATION
GOSUB_SCRATCH_PAD equ     7       ; Used when a work space is needed in il code

