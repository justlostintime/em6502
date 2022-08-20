                Seg   Code
;---------------------------
; Print 24-bit decimal number, unsigned 16 bit integers as well
; ---------------------------
; On entry, R0=number to print
;           pad=0 or pad character (eg '0' or ' ')
; On entry at PrDec24Lp1, positive numbers only
;           Y=(number of digits)*3-3, eg 21 for 8 digits
; On exit,  A,X,Y,num,pad corrupted
; Size      129 bytes, Table 24 bytes  --- total 153
; -----------------------------------------------------------------

PrintDecimal: 
                lda     #0
                sta     R1
                sta     pad
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
                LDY #21                                   ; Offset to powers of ten
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
                BPL PrDec24Lp1                             ; Loop for next digit
                RTS
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
;==================================================================================================
; Size of print functions
PrintFunctionsSize   equ  * - PrintDecimal          ; should use label of first fuction in file