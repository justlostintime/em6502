                seg   Code

; Gosub and return related functions, While ..  Wend
;==========================================================
; Push the current math stack frame onto the gosub stack
;   the frame is really just the information about the area on the
;   math stack that contains the passed values as parameters
iPushMathStack:
                tya
                pha
                ldy     GOSUBSTACKPTR
                lda     MATHSTACKPTR 
                sta     (GOSUBSTACK),y        ; place the current Math stack ptr onto the stack
                lda     #0
                iny
                sta     (GOSUBSTACK),y        ; place a zero for the number of current parameters
                iny
                sta     (GOSUBSTACK),y        ; Place a zero for the next byte
                iny
                lda     #GOSUB_STACK_FRAME
                sta     (GOSUBSTACK),y        ; store the type of entry on the stack as the last byte
                iny
                sty     GOSUBSTACKPTR         ; save the pointer into the gosub stack
                pla
                tay
                jmp   NextIL
;
;==========================================================
; Increment parameter count. Assume Stack frame is top of stack
iIncParmCount:
                tya
                pha

                ldy     GOSUBSTACKPTR        ; get the pointer to update the stack entry
                dey                          ; point to the type of entry #GOSUB_STACK_FRAME
                dey                          ; point to the previous byte
                dey                          ; point to the count of parameters up to 255
                lda     (GOSUBSTACK),y       ; get the count
                clc
                adc     #1                   ; increment the count
                sta     (GOSUBSTACK),y       ; save the updated count

                pla
                tay
                jmp     NextIL
;
;==========================================================
;Restore the math stack frame, removing parameters from stop
;of the math stack
;
iPopMathStack:    jsr      PopMathStackNow
                  jmp      NextIL

PopMathStackNow:
                tya
                pha

                ldy       GOSUBSTACKPTR
                dey
                lda       (GOSUBSTACK),y
                cmp       #GOSUB_STACK_FRAME
                bne       iPopMathStackNoFrame
                dey
                dey
                dey
                lda       (GOSUBSTACK),y
                sta       MATHSTACKPTR
                sty       GOSUBSTACKPTR

iPopMathStackNoFrame:

                pla
                tay
                rts


;==========================================================
; Push the current math stack information onto the gosub stack
iSaveMathStack:
                tya
                pha

                ldy     GOSUBSTACKPTR
                lda     MATHSTACKPTR
                sta     (GOSUBSTACK),y
                lda     MATHSTACK
                iny

                sta     (GOSUBSTACK),y
                iny

                lda     MATHSTACK+1
                sta     (GOSUBSTACK),y
                iny

                lda     #GOSUB_STACK_SAVE
                sta     (GOSUBSTACK),y
                iny

                sty     GOSUBSTACKPTR

                pla
                tay
                jmp   NextIL
;
;==========================================================
;Restore the math stack information from the gosub stack
iRestoreMathStack:
                tya
                pha

                lda   MATHSTACKPTR
                sta   R2                              ; save the current offset for whatever task to R2

                ldy   GOSUBSTACKPTR
                dey
                lda   (GOSUBSTACK),y
                cmp   #GOSUB_STACK_SAVE
                bne   iPopMathStack_Err
                dey
                lda   (GOSUBSTACK),y
                sta   MATHSTACK+1
                dey
                lda   (GOSUBSTACK),y
                sta   MATHSTACK
                dey
                lda   (GOSUBSTACK),y
                sta   MATHSTACKPTR
                sty   GOSUBSTACKPTR

                pla
                tay
                jmp      NextIL

iPopMathStack_Err:
                pla
                tay
                lda     #0
                ldx     #ERR_INVALID_STK_FRAME
                jmp     iErr2
;===========================================================
; For functions and tasks the variable address of # means
; a passed parameter so #[0] is the first parameter etc
; will try for a better way later
;============================================================
; On entry il, branch to if function
;          il+1, value to be returned or not true or false
;
; Return from GOSUB  or function function
; format   RSTR 0   --- return form gosub
;          RSTR 1   --- return from Function
;
iRSTR:          jsr     getILByte             ; get where to go if 0 = gosub/1=function call
                sta     offset
                jsr     saveIL                ; for later jump if needed add extra entry to  the return stack

                jsr     popLN                 ; get the next item from the stack into curptr and curroff, returns call type func or stmt
                sta     R1                    ; keep the type of call returning from
                bcs     iRSTRErr              ; stack underflow error possible

                jsr     getILByte             ; get if a value is being returned

                pha                           ; save if a value was passed to be returned

                cmp     #0                    ; yes attemping to return a value
                beq     iRSTRPOP              ; no value to return
                jsr     popR0                 ; Get the value from the stack save if needed

iRSTRPOP:
                jsr     PopMathStackNow       ; adjust the stack frame from the call
                lda     R1                    ; called as a statement ?
                cmp     #GOSUB_RTN            ; Called as a statement
                beq     iRSTRExit

                pla                           ; get back if value returned or not
                cmp     #1                    ; we have a value to return
                beq     iRSTRVALUE

                ldx     #ERR_NO_RETURN_VALUE_PROVIDED         ; well no value provided and we need one
                jmp     iSAVErr2                              ; jump to general error reporting function

iRSTRVALUE:
                jsr     pushR0                ; return value back to top of stack
                jsr     restoreIL             ; get the correct il
                jmp     tstBranch             ; And called as a function

iRSTRExit:
                pla                           ; throw away gosub/func flag
                jmp     NextIL

iRSTRNORETURNVALUE:


iRSTRErr:       lda     taskPtr               ; Check if this is task zero
                beq     taskZeroEnd           ; this is task zero just stop with error
                lda     MQ
                bne     taskRet
                jsr     pushFalse             ; the result code by default is 0
taskRet:
                jmp     iETask                ; not task zero then do a task end instead
taskZeroEnd:
                ldx     #ERR_STACK_UNDER_FLOW
                jmp     iSAVErr2
;
;==========================================================================================
; Find the next gosub function parameter info  position on the stack
; Returns y = index and c set if found clear c otherwise
GosubFindParms:  ldy     GOSUBSTACKPTR          ;Get the Pointer to the top of stack
                 dey                            ;Point to stack entry type

;Veryify the stack size and position for the call
;Loops here until it finds a GOSUB with value entry or gosub-rtn entry

GosubFindLoop:  cpy     #0                     ;If we reach the top of the stack then no parametrs
                beq     GosubNotFunc
                cpy     #GOSUBSTACKSIZE        ;Tst if we are outside the stack size
                bcs     GosubNotFunc           ;Not valid

;Look for the   GOSUB_RTN_VALUE stack position
                lda     (GOSUBSTACK),y         ;Get the type of call - if it is not a fn call error
                cmp     #GOSUB_RTN             ;if we find this then this function had no parameters
                beq     GosubParmFnd           ;We can pass parameters to a function that returns nothing

                cmp     #GOSUB_RTN_VALUE       ;Parameters with the gosub call
                beq     GosubParmFnd           ;Skip any non Gosub related entries
                
                cmp     #GOSUB_STACK_FRAME     ;Stack frame pointer So should contain the start position of Variables
                beq     GosubParmSkip          ;We have a stackframe good
                
                dey
                dey
                dey
                dey
                jmp     GosubFindLoop

GosubParmFnd:   cpy     #3                     ; Check if we are outside the stack
                bcc     GosubNotFunc           ; if y < 3 then error not found
                cpy     #GOSUBSTACKSIZE        ; Largest value
                bcs     GosubNotFunc           ; no parameters passed

                dey                            ; Point to hopefully Math Stack frame information
                dey
                dey
                dey
GosubParmSkip:
                lda     (GOSUBSTACK),y         ;This should be a stack frame pointer
                cmp     #GOSUB_STACK_FRAME     ;Stack frame pointer So should contain the start position of Variables
                bne     GosubNotFunc           ;No parameters passed but expected
                sec
                rts

GosubNotFunc:   clc
                rts
                
;==========================================================================
; This section support while..wend, for x = <expr> to <expr> [ step <expr> ] ... next
;
;===========================================================================
;Jump end Block if top of math stack is false(0)
;
iJmpEndFalse:  jsr popR0                       ; get the top of the math stack
               lda R0
               ora R0+1                        ; check if they are zero
               beq  JmpEndBlock                 ; it is zero so we are done in the Loops
               jmp NextIL                      ; if true then
JmpEndBlock: 
               jmp NextIL                      ; ignore for now
               
               
;===========================================================================
; Jump back to the start of a block
; next or wend encountered
iJmpStart:      jsr popR0                       ; get the top of the math stack
                lda R0
                ora R0+1                        ; check if they are zero
                beq JmpStartBlock               ; it is zero so we are done in the Loops
                jmp NextIL                      ; if true then
JmpStartBlock: 
                jmp NextIL                      ; ignore for now
               
