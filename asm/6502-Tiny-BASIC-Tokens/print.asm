                Seg   Code
;---------------------------
; Print 24-bit decimal number or  16bit unsigned
; ---------------------------
; On entry, R0=number to print
;           Defaults to pad=0 , y=21 default
;           R2 = 1 unsigned 16 bit
;           R2 = 0 Signed   16 bit

; On entry at PrintDecPadded:
;           X = padding, Y=(number of digits)*3-3, eg 21 for 8 digits

; On exit,  A,X,Y,num,pad corrupted
; Size      129 bytes, Table 24 bytes  --- total 153
; -----------------------------------------------------------------

PrintDecimal:
                TXA
                pha
                tya
                pha
                lda     #0
                sta     pad
                LDY     #21                                   ; Offset to powers of ten
                JMP     PrintDo

PrintDecPadded:
                stx     pad

PrintDo
                lda     #0
                sta     R1

                lda     R2
                bne     PrintPos

                lda     R0+1              ;MSB has sign
                bpl     PrintPos          ;it's a positive number;


; Negative numbers need more work.  Invert all the bits,
; then add one.

                lda     #'-
                jsr     VOUTCH        ;print the negative sign

                lda     #$FF
                sta     R1
                lda     R0            ;invert bits
                eor     #$ff
                sta     R0
                lda     R0+1
                eor     #$ff
                sta     R0+1
                lda     R1
                eor     #$ff
                sta     R1
                inc     R0            ;add one
                bne     PrintPos
                inc     R0+1
                bne     PrintPos
                inc     R1
PrintPos:

PrDec24Lp1:
                LDX #$FF
                SEC                                       ; Start with digit=-1
PrDec24Lp2:
                LDA R0+0
                SBC PrDec24Tens+0,Y
                STA R0+0                                  ; Subtract current tens
                LDA R0+1
                SBC PrDec24Tens+1,Y
                STA R0+1
                LDA R0+2
                SBC PrDec24Tens+2,Y
                STA R0+2
                INX
                BCS PrDec24Lp2                            ; Loop until <0
                LDA R0+0
                ADC PrDec24Tens+0,Y
                STA R0+0                                  ; Add current tens back in
                LDA R0+1
                ADC PrDec24Tens+1,Y
                STA R0+1
                LDA R0+2
                ADC PrDec24Tens+2,Y
                STA R0+2
                TXA
                BNE PrDec24Digit                          ; Not zero, print it
                LDA pad
                BNE PrDec24Print
                BEQ PrDec24Next                           ; pad<>0, use it
PrDec24Digit:
                LDX #'0
                STX pad                                   ; No more zero padding
                ORA #'0                                   ; Print this digit
PrDec24Print:
                JSR VOUTCH
PrDec24Next:
                DEY
                DEY
                DEY
                beq PrDec24LastDigit
                BPL PrDec24Lp1                             ; Loop for next digit
                pla
                tay
                pla
                tax
                RTS
PrDec24LastDigit
                LDX #'0
                STX pad                                    ; No more zero padding
                BNE  PrDec24Lp1                            ; Loop for last digit

pad             db    0

PrDec24Tens:
                dw 1
                db (1 / 65536)
                dw 10
                db (10 / 65536)
                dw 100
                db (100 / 65536)
                dw 1000
                db (1000 / 65536)
                dw 10000
                db (10000 / 65536)
                dw 100000
                db (100000 / 65536)
                dw 1000000
                db (1000000 / 65536)
                dw 10000000
                db (10000000 / 65536)
;=====================================================
; Print character in A as two hex digits to the Console

HexToOut    pha                               ;save return value
            pha
            lsr                               ;a  ;move top nibble to bottom
            lsr                               ;a
            lsr                               ;a
            lsr                               ;a
            jsr     hexta                     ;output nibble
            pla
            jsr     hexta
            pla                               ;restore
            rts
;
hexta       and     #%0001111
            cmp     #$0a
            clc
            bmi     hexta1
            adc     #7
hexta1      adc     #'0                       ;then fall into...
            jmp    VOUTCH
;
;=====================================================
; Print the string that immediately follows the JSR to
; this function.  Stops when a null byte is found,
; then returns to the instruction immediately
; following the null.
;
; Thanks to Ross Archer for this code.
; http://www.6502.org/source/io/primm.htm
;

tbputs          pla                       ;Get the low part of "return" address
                                          ;(data start address)
                sta       PrtFrom
                pla
                sta       PrtFrom+1       ;Get the high part of "return" address
                                          ;(data start address)
                                          ;Note: actually we're pointing one short
PSINB           ldy       #1
                lda       (PrtFrom),y     ;Get the next string character
                inc       PrtFrom         ;update the pointer
                bne       PSICHO          ;if not, we're pointing to next character
                inc       PrtFrom+1       ;account for page crossing
PSICHO          ora       #0              ;Set flags according to contents of
                                          ;   Accumulator
                beq       PSIX1           ;don't print the final NULL
                jsr       VOUTCH          ;write it out
                jmp       PSINB           ;back around
PSIX1           inc       PrtFrom
                bne       PSIX2
                inc       PrtFrom+1       ;account for page crossing
PSIX2           jmp       (PrtFrom)       ;return to byte following final NULL

;+
;====================================================
PrtTerm         equ     tempy

; on exit Print Y has the offset to use
; input y =     addr low
;       x =     addr high
;       a =     termination string


PrtQuoted                                     ; Print a quoted string from the current program space
                lda     #'"
                ldy     CUROFF
                cmp     (CURPTR),y            ; the opening quote, can to " or ' so long as they match
                bne     PrtNoInc
                iny
                sty     CUROFF
PrtNoInc
                sta     PrtTerm

PrtPrgString                                  ; Print a terminated string from the static program space
                ldy     CUROFF                
                lda     CURPTR
                sta     PrtFrom
                lda     CURPTR+1
                sta     PrtFrom+1
                jmp     PrtLoop

; Print a string pointed to by x= h, y=l terminated by a
; Return y as the length

PrtStr          stx      PrtFrom+1
                sty      PrtFrom
                sta      PrtTerm
                ldy      #0
;
; On entry here ptrfrom and prtterm point to area to print
;
PrtLoop         lda     (PrtFrom),y
                cmp     PrtTerm
                beq     PrtEnd
                cmp     #0                    ; always end if 0 is found
                beq     PrtEnd
                jsr     VOUTCH
                iny
                jmp     PrtLoop
PrtEnd          iny                           ;return byte after the copy
                rts

;
;=======================================================
; Print all Variables
PrintAllVars
                ldy     #0
                lda     #'A
PrintAllVarsLoop
                pha
                lda     (VARIABLES),y
                sta     R0
                iny
                lda     (VARIABLES),y
                sta     R0+1

                pla     ;get the current letter
                pha
                jsr     VOUTCH
                jsr     puts
                db      "=",0
                pla
                tax
                inx
                txa
                pha                           ;

                tya
                pha
                jsr     PrintDecimal
                jsr     puts
                db      " ",0
                pla
                tay
                iny
                cpy     #26<<1                  ; A-Z 2 bytes each
                bcc     PrintAllVarsLoop
                jsr     CRLF

                pla
                rts
;==========================================================================================================
;Debug   Print a Program Line from compile buffer
;
DebugPrintProgramLine:
                pha
                lda     #TOKENBUFFER&$FF
                sta     dpl
                lda     #TOKENBUFFER>>8
                sta     dpl+1
                pla

; Decode and print a line of program text
; on entry      dpl points to line of code to print
; on exit       no change in reg or dpl
;
PrintProgramLine:

                stx     printStorage
                sty     printStorage+1
                pha

                ldy     #1                      ; index into the token buffer
                sty     R2                      ; print unsigned decimal
                ldy     #0
                lda     (dpl),y                 ; get number of bytes
                tax                             ; place pointer into x
                iny
                dex                             ; Deduct the length byte
                jsr     DPL2R0                  ; Print the line number
                jsr     PrintDecimal
                lda     #$20
                jsr     VOUTCH

PrintProgLoop:
                lda     (dpl),y                 ; Get a character
                beq     PrintProgramComplete    ; If zero then at end of line
                and     #%10000000              ; check for Keyword or Variable/operator
                beq     PrintKeyword            ; It uses the index in a to find a keyword

PrintProgVars:
                lda    (dpl),y
                and    #$E0                     ; Check for operators and punctuation
                cmp    #$E0
                beq    PrintProgOperatorVect

                lda    (dpl),y                  ; Get char back again and check for var
                cmp    #$9D+1
                bcc    PrintProgVariableVec
                and    #$A0                     ; Check for a valid datatype
                cmp    #$A0
                beq    PrintDataType            ; if not just print the character
                lda    (dpl),y                  ; Get char back again and check for data type
                dex                             ; Ok we are processing it
                iny
                bne    PrintContinue            ; Print and do the next character

PrintDataType:
                lda    (dpl),y                  ; Get char back again and check for data type
                cmp    #tString
                beq    PrintStringVariable

PrintProgNumber:
                iny                             ; we have a numerical integer value
                dex
                pha
                lda    #0
                sta    R0+1
                sta    R2                       ; Set to print signed number
                lda    (dpl),y
                sta    R0
                pla
                cmp    #tInteger
                bne    PrintProgNumDone
                iny
                dex
                lda    (dpl),y
                sta    R0+1

PrintProgNumDone:
                iny
                dex
                jsr    PrintDecimal

PrintProgNext:
                lda    #$20
PrintContinue:
                jsr    VOUTCH
PrintProgSkipSpace:
                cpx    #0
                bne    PrintProgLoop
PrintProgramComplete:
                jsr    CRLF

                ldx    printStorage
                ldy    printStorage+1
                pla

                rts
;=================================================================================================================
; Print a string variable including the quotes
; On Input      y is offset into buffer
; On Exit       y is updated to new offset

PrintStringVariable:
                iny
                lda  #'"
                jsr  VOUTCH
                iny
                lda  dpl
                sta  PrtFrom
                lda  dpl+1
                sta  PrtFrom+1
                lda  #'"
                sta  PrtTerm
                jsr PrtLoop
                lda  #'"
                jsr  VOUTCH
                jmp PrintProgNext

PrintProgVariableVec
                jmp PrintProgVariable

PrintProgOperatorVect
                jmp  PrintProgOperator
;===============================================================================================================
; On entry dpl points to the buffer we are printing from
;          y   current offset into the dpl buffer
; all registers preserved
;
PrintKeyword:

                lda   (dpl),y                ; Get the Keyword token to lookup
                sta    R0                    ; The value we are looking for
                cmp    #kGoto                ; Test if we must skip an extra two bytes for branch type instructions
                beq    PrintKeyBranch
                cmp    #kGosub
                beq    PrintKeyBranch
                cmp    #kTask
                beq    PrintKeyBranch
                cmp    #kGofn
                bne    PrintKeySkipped
PrintKeyBranch:
                iny                          ; Skip the compiled memory address
                iny
                dex                          ; Change number of bytes to print
                dex                          ; Remove the bytes to print

PrintKeySkipped:
                iny                          ; Inc y to point to the next char to be printed
                dex                          ; Reduce number of bytes to print
                tya                          ; Save y and x for the return
                pha
                txa
                pha

                lda    #KeyWordTable&$FF     ; R1 to point to the entry in the keyword table
                sta    R1
                lda    #KeyWordTable>>8
                sta    R1+1


PrintKeyLoop
                ldy    #0                    ; Index into the keyword entry
                lda   (R1),y                 ; Get token value for this entry
                iny                          ; Point to first byte of key
                cmp   R0                     ; Compare to the token we are looking for
                Beq   PrintKeyFound          ; We have the correct Token, now print it

PrintKeyNext
                lda   (R1),y                 ; Get key letter
                iny                          ; Point to next byte always
                and   #%00100000             ; Check for last character in key work
                bne   PrintKeyNext           ; If it is not set then get next character

                tya                          ; Trabsfer y to a for the addition
                clc                          ; Table > 256 bytes
                adc   R1
                sta   R1
                lda   #0
                adc   R1+1
                sta   R1+1
                jmp   PrintKeyLoop

PrintKeyFound:
                lda   (R1),y                ; letter from key table
                pha                         ; Save it for later check
                ora   #%00100000            ; Force it to lower case
                jsr   VOUTCH                ; Print it out
                iny                         ; Point to next character
                pla                         ; Restore the value
                and   #%00100000            ; Check if it was last char in keyword
                bne   PrintKeyFound         ; Yes, then goto all done printing

                pla                         ; Restore the x and y values
                tax
                pla
                tay

PrintChkRem:
                lda   #kRem
                cmp   R0
                bne   PrintKeyDone
PrintKeyRem:
                lda  dpl                    ; if it is a rem then we must print the entire line
                sta  PrtFrom
                lda  dpl+1
                sta  PrtFrom+1
                lda  #0
                sta  PrtTerm
                jsr  PrtLoop
                dey                         ; point back to the terminating null value
PrintKeyDone:
                jmp  PrintProgNext
;==================================================================================================================
;Print Variable, number or operator
PrintProgOperator:
                lda   (dpl),y
                iny
                dex
                stx    printStorage+2
                ldx    #0
PrintOprLoop
                cmp    OperValues,x
                beq    PrintOprFound
                inx
                bne    PrintOprLoop
PrintOprFound
                txa
                asl
                tax
                lda     Operators,x
                jsr     VOUTCH
                inx
                lda     Operators,x
                beq     PrintOprDone
                jsr     VOUTCH
PrintOprDone
                ldx     printStorage+2
                jmp     PrintProgNext

;=================================================================================================================
;KeywordsMax       equ     128                    ; Allow to be range  1 to 127  key words, high order bit must be 0 for it to be a key word
;tVa               equ     128                    ; Variable A = 1, .... Z = 26   ^ = 27
;tVb               equ     130                    ; Variables 128 - 157  $80-$9D
;tVhat             equ     155                    ; Variable ^
;tVhash            equ     156                    ; Variable #
;tVat              equ     157                    ; Variable @ = 0
PrintProgVariable:
                lda    (dpl),y
                iny
                dex
                cmp    #tVhat
                bne    PrintProgChkHash
                lda    #'^
                bne    PrintTheVar
PrintProgChkHash:
                cmp   #tVhash
                bne   PrintProgChkAt
                lda   #'#
                bne   PrintTheVar
PrintProgChkAt:
                cmp   #tVat
                bne   PrintProgVarLetter
                lda   #'@
                bne   PrintTheVar
PrintProgVarLetter:
                and   #%01111111
                clc
                adc   #'A
PrintTheVar:
                jsr   VOUTCH
                jmp   PrintProgNext


;==================================================================================================
; Size of print functions
PrintFunctionsSize   equ  * - PrintDecimal          ; should use label of first fuction in file