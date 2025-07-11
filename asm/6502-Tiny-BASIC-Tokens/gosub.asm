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
; Begin a block of code, while, if endif, for next etc
; on entry x contains the type of block being created
; format WendPtr.wendptr,curptr,curptr+1,curoff,type
iBeginBlock:
                tya
                pha
                jsr     getILByte             ; get the type of block we are starting
                sta     R1
                jsr     getILByte             ; get the closing block marker
                sta     R1+1
                jsr     FindEndBlock          ; push the endblock onto the stack

                ldy     GOSUBSTACKPTR         ; get the top of stack
                lda     CURPTR                ; get the first byte of the program line
                sta     (GOSUBSTACK),y        ; place the current Math stack ptr onto the stack
                iny                           ; Next byte to save
                lda     CURPTR+1              ; get the second byte of the program line
                sta     (GOSUBSTACK),y        ; place the current Math stack ptr onto the stack
                iny
                lda     CUROFF                ; get the offset on the line
                sta     (GOSUBSTACK),y        ; place a zero for the number of current parameters
                iny
                lda     R1                    ; get the type of block to save
                sta     (GOSUBSTACK),y        ; store the type of entry on the stack as the last byte
                iny
                sty     GOSUBSTACKPTR         ; save the pointer into the gosub stack
                pla
                tay
                jmp   NextIL
;=================================================================================
;find the end block, account for nested begin types
;stores the address of the memory location to the next two byte on the gosub stack
;return cleared carry if good, sets the carry if failed
;R1 = begin block value, R1+1 = end block value, r2 is the balanced counter
;
FindEndBlock:   lda   CURPTR        ; preserve the current line number
                pha
                lda   CURPTR+1
                pha
                lda   CUROFF
                pha
                lda   #0
                sta   R2
FindEndBlkLoop:
                jsr   FindNextLine    ; CURPTR now points to the next line, CUROFF is location of first char
                jsr   AtEnd           ; At end of program
                beq   FindEof         ; Branch out if at end of program
                ldy   CUROFF          ; this is where the kwhend or kwhile will be stored for example
                lda   R1              ; get the start block value
                cmp   (CURPTR),y      ; test for a match
                beq   Findincr2       ; inc it and continue
                lda   R1+1            ; get the end of block value
                cmp   (CURPTR),y      ; is it end block
                bne   FindEndBlkLoop  ; check the next line
                lda   R2              ; check if we are at level 0
                cmp   #0
                beq   FindFound
                dec   R2              ; reduce it by one
                jmp   FindEndBlkLoop  ; do the next one
Findincr2:
                inc   R2
                jmp   FindEndBlkLoop
FindFound:
                ldy   GOSUBSTACKPTR   ; place the location of end block onto the gosub stack
                lda   CURPTR
                sta   (GOSUBSTACK),y
                iny
                lda   CURPTR+1
                sta   (GOSUBSTACK),y
                iny
                sty   GOSUBSTACKPTR
          
                pla
                sta   CUROFF
                pla                 ; restore the original line pointer
                sta   CURPTR+1
                pla
                sta   CURPTR

                rts
FindEof:                         ; the matching closing block id not found
                ldx     #ERR_NO_MATCHING_END_BLOCK
                jmp     iSAVErr2
;
;==================================================================================
;Find end of block and set the user pc to it
;
iJmpEnd:       jsr getILByte                   ; get the type of loop
               pha
               ldy GOSUBSTACKPTR               ; get the top of stack
               cpy #0                          ; empty stack?
               beq iJmpErrNoEntry
               dey
               pla
               cmp (GOSUBSTACK),y              ; check if it is the correct type of entry
               bne iJmpErrInvalid              ; Wrong type of entry
               dey                             ; remove the entry from the stack
               dey
               dey
               dey
               lda  (GOSUBSTACK),y             ; get the byte of curptr
               sta  CURPTR+1
               dey
               lda  (GOSUBSTACK),y             ; get the byte of curptr
               sta  CURPTR
               sty  GOSUBSTACKPTR
               lda  #3
               sta  CUROFF

               jmp NextIL                      ; if true then

;===========================================================================
; Jump back to the start of a block, look onto gosub stack for the while entry
; get the next il byte to determin which kind of block to process, while,for,if endif
iJmpStart:      tya
                pha
                jsr     getILByte               ; get the type of block we are looking for
                pha
                ldy     GOSUBSTACKPTR           ; the single byte offset to be used
                cpy     #0                      ; if it is zero bad juju
                beq     iJmpErrNoEntry          ; if it is zero then stack is empty get out
                dey                             ; point to entry type on the stack
                pla                             ; get type we are looking for
                cmp     (GOSUBSTACK),y          ; Check if it is the correct type of entry
                BNE     iJmpErrInvalid          ; not the expected block type
                dey
                lda     (GOSUBSTACK),y          ; get the correct offset of user program
                sta     CUROFF                  ; offset on text line
                dey
                lda     (GOSUBSTACK),y          ; get line start
                sta     CURPTR+1
                dey
                lda     (GOSUBSTACK),y          ; part of line start
                sta     CURPTR
                pla
                tay
                jmp     NextIL                  ; ignore for now

iJmpErrInvalid: ldx     #ERR_NO_MATCHING_BEGIN_BLOCK
                jmp     iSAVErr2

iJmpErrNoEntry: pla
                pla
                tay
                ldx     #ERR_STACK_UNDER_FLOW
                jmp     iSAVErr2
;
;=======================================================================================
;Branch types
iIfTrue:
          jsr       getILByte
          sta       offset
          jsr       popR0
          lda       R0
          ora       R0+1
          beq       iftestfailed
          jmp       tstBranch
iIfFalse:
          jsr       getILByte
          sta       offset
          jsr       popR0
          lda       R0
          ora       R0+1
          bne       iftestfailed
          jmp       tstBranch
iftestfailed:
          jmp       NextIL
