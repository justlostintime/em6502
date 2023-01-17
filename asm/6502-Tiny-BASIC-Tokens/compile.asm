               Seg       Code
;
;=====================================================================
; Scan the loaded program just before running and insert memory locations of each
; line number branched to. goto gosub, gofn
; These have the format  in memory  example 81{key word token} 0000{pointer to memory location} A1{number type} 92 00{byte or integer value}
Compile
                lda      #0
                sta      R0                           ; keep track of how many errors we find
                lda      RunMode
                pha
                inc      RunMode                      ; force run mode for error reporting
                lda      CURPTR
                pha
                lda      CURPTR+1
                pha
                lda      CUROFF
                pha
                lda      ProgramStart
                sta      dpl
                lda      ProgramStart+1
                sta      dpl+1

CompileLineStart:
                lda      dpl
                cmp     ProgramEnd
                bne     CompileContinue
                lda     dpl+1
                cmp     ProgramEnd+1
                beq     CompileComplete

CompileContinue:
                ldy      #3                             ; first real character in the line

CompileLoop:
                lda      (dpl),y                        ; get the byte
                beq      CompileEndOfLine               ; End of line, so goto next line for scan
                iny                                     ; Pass this byte
                cmp      #kGoto
                beq      CompileField                   ; Will update the memory address and move pointer to next value
                cmp      #kGosub
                beq      CompileField                   ; Will update the memory address and move pointer to next value
                cmp      #kGofn
                beq      CompileField                   ; Will update the memory address and move pointer to next value
                cmp      #kTask
                beq      CompileField
                cmp      #kRem
                beq      CompileRem                     ; Skip until end of line
                cmp      #tString
                beq      CompileString
                cmp      #tInteger
                beq      CompileInteger
                cmp      #tByte
                beq      CompileByte
                bne      CompileLoop                    ; Next character

CompileInteger:
                iny
CompileByte:
                iny
                bne     CompileLoop
CompileRem:
CompileEndOfLine:
                ldy     #0
                lda     (dpl),y
                clc
                adc     dpl
                sta     dpl
                lda     #0
                adc     dpl+1
                sta     dpl+1

                jmp     CompileLineStart

CompileComplete:
                pla
                sta     CUROFF
                pla
                sta     CURPTR+1
                pla
                sta     CURPTR
                pla
                sta     RunMode
                lda     R0                   ; returning the number of errors
                rts


CompileString:
                iny                           ; point past first "
CompileStringLoop:
                lda     (dpl),y
                beq     CompileStrDone2       ; end of line
                cmp     #'"                   ; end of string
                beq     CompileStrDone
                cmp     #'\                   ; escape character
                bne     CompileStrNext
                iny                           ; skip the escape character
CompileStrNext:
                iny                           ; Next character
                bne     CompileStringLoop     ; test for end
CompileStrDone:
                iny
CompileStrDone2:
                Jmp     CompileLoop
;
;===============================================================
; on entry y points to storage location y+2 points to line number
; on exit y points to line number type

CompileField:   sta     R0
                tya                            ; save the y pointer to store the memory value
                pha
                iny                            ; Skip over the memory vector
                iny
                lda      R0
                cmp      #kTask                ; for a task it is the next byte after a bracket
                bne      CompNoBracket

                lda     (dpl),y                ; Lets make sure it is a )
                cmp     #oLeftBracket
                bne     CompNoBracket          ; in case of error
                iny                            ; skip the bracket

CompNoBracket
                lda      #0                    ; In case the value is a byte
                sta      R0+1

                lda      (dpl),Y              ; get the type of the next byte t something or other
                cmp      #tByte
                beq      CompByteLoad
                cmp      #tInteger
                beq      CompIntLoad          ; If it is not a number then get out of here
                pla
                jmp      CompileLoop          ; Ignore the saved stack
CompIntLoad:
                iny
                lda      (dpl),y
                sta      R0
                iny
                lda      (dpl),y
                sta      R0+1
                jmp      CompFindLine
CompByteLoad:
                iny
                lda     (dpl),y
                sta     R0
CompFindLine:
                jsr     findLine
                beq     CompFoundLine
                inc     R0                      ; number of errors

                lda     dpl
                sta     CURPTR
                lda     dpl+1
                sta     CURPTR+1
                sty     CUROFF

                ldx     #ERR_LINE_NOT_FOUND
                lda     #0

                jsr     DisplayError
                jsr     PrintProgramLine

                pla
                tay
                iny
                iny
                jmp     CompileLoop

CompFoundLine:
                pla
                tay
                lda     CURPTR
                sta     (dpl),y
                iny
                lda     CURPTR+1
                sta     (dpl),y
                iny
                jmp     CompileLoop









































