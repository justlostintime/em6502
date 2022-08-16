                seg   Code

; Gosub and return related functions
;==========================================================
; Push the current math stack frame onto the gosub stack
iPushMathStack:
                tya
                pha
                ldy     GOSUBSTACKPTR
                lda     MATHSTACKPTR
                sta     (GOSUBSTACK),y
                lda     #0
                iny
                sta     (GOSUBSTACK),y
                iny
                sta     (GOSUBSTACK),y
                iny
                lda     #GOSUB_STACK_FRAME
                sta     (GOSUBSTACK),y
                iny
                sty     GOSUBSTACKPTR
                pla
                tay
                jmp   NextIL
;
;==========================================================
; Increment parameter count. Assume Stack frame is top of stack
iIncParmCount:
                tya
                pha
                ldy     GOSUBSTACKPTR
                dey
                dey
                dey
                lda     (GOSUBSTACK),y
                tax
                inx
                txa
                sta     (GOSUBSTACK),y
                pla
                tay
                jmp     NextIL
;
;==========================================================
;Restore the math stack frame
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

iPopMathStackNoFrame

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

iPopMathStack_Err
                lda     #0
                ldx     #ERR_INVALID_STK_FRAME
                jmp     iErr2
;=========================================
; For functions and tasks the variable address of # means
; a passed parameter so #[0] is the first parameter etc
; will try for a better way later

