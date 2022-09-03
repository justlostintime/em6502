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
                db 1 / 65536
                dw 10
                db 10 / 65536
                dw 100
                db 100 / 65536
                dw 1000
                db 1000 / 65536
                dw 10000
                db 10000 / 65536
                dw 100000
                db 100000 / 65536
                dw 1000000
                db 1000000 / 65536
                dw 10000000
                db 10000000 / 65536
;=====================================================
; Print character in A as two hex digits to the

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

PrtPrgLine
                lda    #0
                sta     PrtTerm
                beq     PrtPrgText

PrtQuoted       lda     #'"
                sta     PrtTerm

PrtPrgText      ldy     CUROFF
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

;==================================================================================================
; Size of print functions
PrintFunctionsSize   equ  * - PrintDecimal          ; should use label of first fuction in file