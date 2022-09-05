        seg Code
DEBUGPARSER       equ     TRUE                   ; Print debugging information

; Define the types of tokens found, and identifiers
KeywordsMax       equ     $7F                    ; Allow to be range  1 to 127  key words, high order bit must be 0 for it to be a key word
tVa               equ     $80                    ; Variable A = 1, .... Z = 26   ^ = 27
tVb               equ     $81                    ; Variables 128 - 157  $80-$9D
tVz               equ     tVa+25                 ; Value of the last variable

tVhat             equ     $9B                    ; Variable ^
tVhash            equ     $9C                    ; Variable #
tVat              equ     $9D                    ; Variable @ = 0


tString           equ     $A0                    ; String all start with this byte and end with  byte value 0 strings can be accessed with array slicing
tInteger          equ     $A1                    ; all tokenized integers start with 251 as first byte
tByte             equ     $A2                    ; Unsigned byte value
tArray            equ     $A3                    ; Identifies Array Type, the byte following defines the length of each element
                                                 ; Arrays of string are arrays of pointers 2 bytes
tPointer          equ     $A4                    ; Pointer to another variable
tIndirect         equ     $A6                    ; Points to an address that points to the data

Operators: BYTE "<>","<=",">=",'+,0,'<,0,'=,0,">",0,"-",0,"/",0,"%",0,"*",0,"(",0,")",0,",",0,";",0,"[",0,"]",0,":",0,"$",0,"!",0
OperatorLen equ *-Operators

OperValues: BYTE  $F5,$F3,$F6,$F0,$F1,$F2,$F4,$F7,$F8,$F9,$FA,$E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8

oNotEqual         equ     $F5
oLessEqual        equ     $F3
oGreaterEqual     equ     $F6
oPlus             equ     $F0
oLess             equ     $F1
oEqual            equ     $F2
oGreater          equ     $F4
oMinus            equ     $F7
oDivide           equ     $F8
oModulo           equ     $F9
oPercent          equ     oModulo
oMultiply         equ     $FA
oLeftBracket      equ     $E0
oRightBracket     equ     $E1
oComma            equ     $E2
oSemiColon        equ     $E3
oLeftSQBracket    equ     $E4
oRightSQBracket   equ     $E5
oColon            equ     $E6
oDollar           equ     $E7
oBang             equ     $E8

OPCount           equ   * - OperValues

tOperatorX        equ     $F0 ;+ operator Value  ; stores the value used to do the relational operator compare

tError            equ      $FF                   ; Error should never happen
;
; Keyword table contains 49 keywords
KeyWordTable:
             db     "leT"                        ; 1, we only have 0 at end of program or line
kLet         equ     1
             db     "inC"
kInc         equ     2
             db     "deC"
kDec         equ     3
             db     "ireturN"
kIreturn     equ     4
             db     "iF"
kIf          equ     5
             db     "theN"
kThen        equ     6
             db     "gotO"
kGoto        equ     7
             db     "gosuB"
kGosub       equ     8
             db     "returN"
kReturn      equ     9
             db     "reM"
kRem         equ    10
             db     "prinT"
kPrint       equ    11                   ; should be entry for print
             db     "taskE"
kTaske       equ    12
             db     "taskN"
kTaskn       equ    13
             db     "taskW"
kTaskw       equ    14
             db     "pokE"
kPoke        equ    15
             db     "putcH"
kPutch       equ    16
             db     "clS"
kCls         equ    17
             db     "inpuT"
kInput       equ    18
             db     "enD"
kEnd         equ    19
             db     "irQ"
kIrq         equ    20
             db     "kilL"
kKill        equ    21
             db     "lisT"
kList        equ    22
             db     "ruN"
kRun         equ    23
             db     "neW"
kNew         equ    24
             db     "slicE"
kSlice       equ    25
             db     "tracE"
kTrace       equ    26
             db     "exiT"
kExit        equ    27
             db     "savE"
kSave        equ    28
             db     "loaD"
kLoad        equ    29
             db     "erasE"
kErase       equ    30
             db     "noT"
kNot         equ    31
             db     "oR"
kOr          equ    32
             db     "xoR"
kXor         equ    33
             db     "anD"
kAnd         equ    34
             db     "truE"
kTrue        equ    35
             db     "falsE"
kFalse       equ    36
             db     "diR"
kDir         equ    37
;functions returning values
             db     "freE"
kFree        equ    38
             db     "getcH"
kGetch       equ    39
             db     "peeK"
kPeek        equ    40
             db     "tasK"
kTask        equ    41
             db     "ipcc"
kIpcc        equ    42
             db     "ipcS"
kIpcs        equ    43
             db     "ipcR"
kIpcr        equ    44
             db     "rnD"
kRnd         equ    45
             db     "staT"
kStat        equ    46
             db     "abS"
kAbs         equ    47
             db     "calL"
kCall        equ    48
             db     "gofN"
kGofn        equ    49
             db     "ireT"
kIret        equ    50
             db     "piD"
kPid         equ    51
             db     0,0
KeyWordTableEnd      equ      *
KeyWordTableLength   equ      * - KeyWordTable
TOKENBUFFER  ds    256                    ; placed here as temp for testing the Code
printStorage ds    3
;==================================================================================================================
; Read accross the inputline and output to TOKENBUFFER
; Format   byte      Description
;           0        length of line 1-255
;          0-1       Line Number
;          Tokens and litteral values encoded into the line
;
;  First test for numbers    for numbers insert type byte plus value 1 or 2 byte, byte, integer, string(pointers)
;  if fails then test for keywords
;  if fails then test for variables and arrays
;  if fails check for operators/seperators  + - < > = % / * () [] , ; :
ParseInputLine:
 if DEBUGPARSER
                jsr     SetOutDebug
                jsr     DebugClearBuffer
 endif
                lda    CUROFF
                pha
                txa
                pha
                tya
                pha
                ldx     #1                 ; point to beginning of Token buffer + 1 reserve space for length byte
                jsr     getDecimal         ; Check for a line number, none is ok too
                sty     CUROFF
                jsr     R02TOKEN           ; Move R0 to token buffer

ParseInputLoop:
                ldy     CUROFF
                jsr     SkipSpaces         ; Skip any spaces
                sty     CUROFF             ; Even if it fails at least remove the spaces
                lda     LINBUF,y           ; Check for end of line
                beq     ParseComplete      ; Finish token buffer and return

ParseForNumber:
                jsr     ParseNumeric       ; Check for a numeric value
                bcc     ParseInputLoop     ; Go Back for next element

ParseForString:
                jsr     ParseString        ; Check for a string
                bcc     ParseInputLoop     ; It was a string

ParseForOp:
                jsr    ParseForOperator    ; Check for operator or punctuation
                bcc    ParseInputLoop      ; it was an operator/punctuation

ParseForKey:
                jsr     ParseLookupKey     ; Check for a keyword value
                bcc     ParseInputLoop     ; Go back for next token, we are not syntax checking

ParseForVar:
                jsr    ParseForVariable    ; Check for variable and convert to Index, as task centric
                bcc    ParseInputLoop

ParseKeepChar:                             ; if it does not parse just keep it safe
                lda    LINBUF,y
                sta    TOKENBUFFER,x
                inx
                iny
                sty    CUROFF
                bne    ParseInputLoop

ParseComplete:
                lda     #0
                sta     TOKENBUFFER,x      ; null terminate the line of tokens
                inx
                stx     TOKENBUFFER        ; Place size including null into buffer start

                pla
                tay
                pla
                tax
                pla
                sta     CUROFF
                
 if DEBUGPARSER

                jsr     printTokenBuffer
                jsr     DebugPrintProgramLine
                jsr     SetOutConsole

 endif
                rts

;==================================================================================================================
; Look at curptr, curpos and check for a valid KeyWord
; A contains the index value. c is clear
;                   not found c set  A undefined
; X is prerserved
;
ParseLookupKey:
                stx      R2
                ldy      CUROFF
                ldx      #0
                lda      #1
                sta      R0                    ; at the end this will contain the index of the keyword
  if DEBUGPARSER
              ;  jsr DebugKeyword
  endif
                lda      #'?                   ; check for fast form of print
                cmp      LINBUF,y
                bne      ParseLookupLoop       ; Skip to loop if not ?
                lda      #kPrint               ; Number for print
                bne      ParseKeySpecial       ; Get out with the special case

ParseLookupLoop:
                lda      KeyWordTable,x        ; Check both upper and lower characters
                and      #%11011111            ; Force Keyword to upper case
                cmp      LINBUF,y
                beq      ParseNextLetter
                ora      #%00100000            ; Force Keyword to lowercase
                cmp      LINBUF,y
                bne      ParseNextEntry

ParseNextLetter:
                lda      KeyWordTable,x       ; Check if we just processed the last letter
                and      #%00100000           ; if this bit not set then end of keyword, Last char is always uppercase
                beq      ParseKeyFound
                inx
                iny
                lda     #0                     ; Check if we are at the end of the input buffer
                cmp     LINBUF,y
                beq     ParseNextEntry         ; End of buffer but no keyword
                bne     ParseLookupLoop

ParseKeyFound:
                lda     R0                     ; get the keyword index
                cmp     #kRem                  ; remark statement
                beq     ParseMoveLine          ; Move everything until the end of line to the token buffer

ParseKeySpecial:
                iny                            ; point past the last character
                sty     CUROFF
                ldx     R2                     ; preserved the X pointer
                sta     TOKENBUFFER,x
                inx
                clc
                rts

; Move forward to the next entry in table
ParseNextEntry:
                lda   KeyWordTable,x
                and   #%00100000
                beq   ParseEndOfEntry
                inx
                bne   ParseNextEntry

ParseEndOfEntry:
                inx
  if DEBUGPARSER
            ;    jsr DebugKeyword
  endif
                inc   R0                        ; Point to next index
                ldy   CUROFF                    ; Restore Y to start of the parse
                lda   KeyWordTable,x
                beq   ParseNoneFound
                bne   ParseLookupLoop

ParseNoneFound:
                ldx   R2
                sec
                rts

;===============================================================================
; Move everything from current position until the end of line into the token buffer
;
ParseMoveLine: iny                                 ; next byte to parse
               ldx    R2                           ; where to place in the buffer
               lda    R0
               sta    TOKENBUFFER,x                ;Put the rem into the buffer
               inx                                 ;Skip to next byte after the reM
ParseMoveLoop:
               lda    LINBUF,y                     ; get the next byte
               beq    ParseMoveDone                ; if we load a zero then done
               sta    TOKENBUFFER,x                ; save the byte
               iny
               inx
               bne     ParseMoveLoop
ParseMoveDone:
               sty     CUROFF
               clc
               rts

;=========================================================================================================
;ParseString Parse a quotes string
; on input X = outbuf position
; y = inbuf position
; Copies string to output buffer, updates x and y
ParseString:
              ldy   CUROFF
              lda   #tString
              sta   TOKENBUFFER,X
              lda   LINBUF,y
              cmp   #'"
              bne   ParseStringInvalid
              inx
              sta   TOKENBUFFER,x
              inx
              iny

ParseStringLoop:
              lda   LINBUF,y
              sta   TOKENBUFFER,x
              cmp   #'"
              beq   ParseStringDone
              iny
              inx
              bne   ParseStringLoop

ParseStringDone:
              inx
              iny
              sty  CUROFF
              clc
              rts

ParseStringInvalid:
              sec
              rts

;=========================================================================================================
; Get numeric values and return value in RO and type in a
;
ParseNumeric:
               ldy    CUROFF
               lda    LINBUF,y
               cmp    #'0
               bcc    ParseNumInvalid
               cmp    #'9+1
               bcs    ParseNumInvalid
               stx    R2
               jsr    getDecimal
               ldx    R2
               sty    CUROFF
               lda    R0+1
               beq    ParseByteValue

ParseIntegerValue
               lda    #tInteger
               sta    TOKENBUFFER,x
               inx
               lda    R0
               sta    TOKENBUFFER,x
               inx
               lda    R0+1
               sta    TOKENBUFFER,X
               inx
               clc
               rts

ParseByteValue:
               lda    #tByte
               sta   TOKENBUFFER,x
               inx
               lda   R0
               sta   TOKENBUFFER,x
               inx
               clc
               rts

ParseNumInvalid                                         ;Not a valid Numeric
               sec
               rts

;=========================================================================================================
;Parse for operators
; +($F0), <($F1),=($F2),<=($F3), >($F4), <>($F5), >=($F6), -($F7), /($F8), %($F9), *($FA), (($FB), )($FC)
; on exit the A has the oper code, c is clear
;               not found then c is set
;      x is preserved
;
;Operators: BYTE "<>","<=",">=",'+,0,'<,0,'=,0,">",0,"-",0,"/",0,"%",0,"*",0,"(",0,")",0,",",0,";",0,"[",0,"]",0,":",0
;OperatorLen equ *-Operators
;
;OperValues BYTE  $F5,$F3,$F6,$F0,$F1,$F2,$F4,$F7,$F8,$F9,$FA,$E0,$E1,$E2,$E3,$E4,$E5,$E6
;OPCount    equ   * - OperValues

ParseForOperator:
                stx     R2
                ldy     CUROFF
                ldx     0
  if DEBUGPARSER
               ; jsr    DebugPrintOP
  endif
ParseOpLoop:
                lda     Operators,x
                cmp     LINBUF,y
                bne     ParseOpNext
                iny
                lda     Operators+1,x
                beq     ParseOpFoundSingle
                cmp     LINBUF,y
                bne     ParseOpNext

ParseOpFound:
                iny

ParseOpFoundSingle:
                sty    CUROFF

                txa
                lsr
                tax
                lda    OperValues,x
                ldx    R2
                sta    TOKENBUFFER,x
                inx
                clc
                rts

ParseOpNext:
                inx
                inx
                cpx    #OperatorLen
                bcs    ParseOpNotFound
  if DEBUGPARSER
             ;   jsr    DebugPrintOP
  endif
                ldy    CUROFF                ; reset the y pointer to beginning
                bne    ParseOpLoop

ParseOpNotFound
                ldx   R2
                sec
                rts
;=========================================================================================================
  if DEBUGPARSER
;Print the text of a keyword
;Input x = offset into table
DebugKeyword:
            pha
            txa
            pha
DebugKeyLoop
            lda     KeyWordTable,x
            jsr     VOUTCH
            and     #%00100000
            beq     DebugKeyDone
            inx
            bne     DebugKeyLoop

DebugKeyDone:
            jsr     CRLF
            pla
            tax
            pla
            rts
;========================================
DebugPrintOP:
            pha
            lda     Operators,x
            jsr     VOUTCH
            lda     Operators+1,x
            beq     DbgPrtOpDone
            jsr     VOUTCH

DbgPrtOpDone:
            jsr     CRLF
            pla
            rts
;=======================================
DebugClearBuffer:
            txa
            pha
            ldx     #$FF
            lda     #0
DebugClrLoop:
            sta    TOKENBUFFER,x
            dex
            bne    DebugClrLoop
            sta   TOKENBUFFER,x
            pla
            tax
            rts

;=====================================================
; Print the parser buffer as hex values
printTokenBuffer:
            stx   printStorage
            sty   printStorage+1
            sta   printStorage+2

            ldx   TOKENBUFFER               ; get the length of the buffer
            inx                             ; we want to show the last zero byte
            ldy   #0

printHexLoop:
            lda   TOKENBUFFER,y             ; get the character
            jsr   HexToOut                  ; print it
            lda   #$20
            jsr   VOUTCH
            iny
            dex
            cpx  #0
            bne   printHexLoop
            jsr   CRLF

            ldy   printStorage+1
            ldx   printStorage
            lda   printStorage+2
printHexDone:
            clc
            rts
  endif


;=========================================================================================================
; Parse for variables A-Z @, ^  x!x x[op]
ParseForVariable:
                ldy    CUROFF
                lda    LINBUF,y
                cmp    #'^                        ; is it an exit code
                bne    ParseVarMem
                lda    #tVhat                     ; Mark the index as 27th slot
                bne    ParseVarSpecial

ParseVarMem:
                cmp   #'@                         ; are we indirect through program end eg. @[0] ..
                bne   ParseVarStack
                lda   #tVat
                bne   ParseVarSpecial

ParseVarStack:
                cmp   #'#                         ; Indirect var through top of stack eg. #[0]
                bne   ParseVarLetters
                lda   #tVhash
                bne   ParseVarSpecial

ParseVarLetters:
                and    #%11011111                 ; Force upper case
                cmp    #'A
                bcc    ParseVarInvalid
                cmp    #'Z+1
                bcs    ParseVarInvalid
;
; The condition is true, so convert to an index, push
; it onto the stack and continue running.
;
                sec
                sbc     #'A                   ;index is zero based
                ora     #$80

ParseVarSpecial:
                sta     TOKENBUFFER,x
                inx
                iny
                sty     CUROFF
                clc
                rts

ParseVarInvalid:
                sec
                rts


;=========================================================================================================
; Transfer R0 to the TOKENBUFFER
;
R02TOKEN:
                lda     R0
                sta     TOKENBUFFER,x
                inx
                lda     R0+1
                sta     TOKENBUFFER,x
                inx
                clc
                rts
TOKEN2R0:
                lda     TOKENBUFFER,y
                sta     R0
                iny
                dex
                lda     TOKENBUFFER,y
                iny
                dex
                sta     R0+1
                rts
DPL2R0:
                lda     (dpl),y
                sta     R0
                iny
                dex
                lda     (dpl),y
                iny
                dex
                sta     R0+1
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
; on exit       no change in reg of dpl
;
PrintProgramLine:

                stx     printStorage
                sty     printStorage+1
                pha

                ldy     #1              ; index into the token buffer
                sty     R2              ; print unsigned decimal
                ldy     #0
                lda     (dpl),y         ; get number of bytes
                tax                     ; place pointer into x
                iny
                dex                     ; Deduct the length byte
                jsr     DPL2R0        ; Print the line number
                jsr     PrintDecimal
                lda     #$20
                jsr     VOUTCH

PrintProgLoop:
                lda     (dpl),y   ; Get a character
                beq     PrintProgramComplete
                and     #%10000000       ; check for Keyword or Variable/operator
                beq     PrintKeyword    ; It uses the index in a to find a keyword

PrintProgVars:
                lda    (dpl),y
                and    #$E0              ; Check for operators and punctuation
                cmp    #$E0
                beq    PrintProgOperatorVect
                lda    (dpl),y
                cmp    #$9D+1
                bcc    PrintProgVariableVec
                cmp    #tString
                beq    PrintStringVariable
                iny                     ; we have a numerical value
                dex
                pha
                lda    #0
                sta    R0+1
                sta    R2               ; Set to print signed number
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
                bcc PrintProgVariable
PrintProgOperatorVect
                jmp  PrintProgOperator
;===============================================================================================================
PrintKeyword:

                lda   (dpl),y
                dex
                iny
                sta    R0              ; the counter save area
                sta    R0+1            ; to refer to later if needed
                stx    printStorage+2

                ldx    #0
PrintKeyLoop
                dec   R0              ; Keyword indexes are 1 relative, adjust to zero relative
                lda   #0
                cmp   R0
                Beq   PrintKeyFound   ; We have the correct index, now print it
PrintKeyNext
                lda   KeyWordTable,x
                inx                   ; Point to next byte always
                and   #%00100000
                beq   PrintKeyLoop
                bne   PrintKeyNext

PrintKeyFound:
                lda   KeyWordTable,x
                pha
                ora   #%00100000
                jsr   VOUTCH
                inx
                pla
                and   #%00100000
                bne   PrintKeyFound
                ldx  printStorage+2
PrintChkRem:
                lda   #kRem
                cmp   R0+1
                bne   PrintKeyDone
PrintKeyRem:
                lda  dpl              ; if it is a rem then we must print the entire line
                sta  PrtFrom
                lda  dpl+1
                sta  PrtFrom+1
                lda  #0
                sta  PrtTerm
                jsr  PrtLoop
                dey                   ; point back to the terminating null value
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
                cmp    tVhat
                bne    PrintProgChkHash
                lda    #'^
                bne    PrintTheVar
PrintProgChkHash:
                cmp   tVhash
                bne   PrintProgChkAt
                lda   #'#
                bne   PrintTheVar
PrintProgChkAt:
                cmp   tVat
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























