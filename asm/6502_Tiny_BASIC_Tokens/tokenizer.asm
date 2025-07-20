        seg Code
DEBUGPARSER       equ     FALSE                  ; Print debugging information

; Define the types of tokens found, and identifiers
KeywordsMax       equ     $7F                    ; Allow to be range  1 to 127  key words, high order bit must be 0 for it to be a key word
tVa               equ     $80                    ; Variable A = 1, .... Z = 26   ^ = 27
tVb               equ     $81                    ; Variables 128 - 157  $80-$9D
tVz               equ     tVa+25                 ; Value of the last variable

tVhat             equ     $9B                    ; Variable ^
tVhash            equ     $9C                    ; Variable #
tVat              equ     $9D                    ; Variable @ = 0

; Base variable type supported by This basic
; Unsigned types always have the 0 bit set to 1
tString           equ     $A0                    ; Strings all start with this byte and end with  byte value 0 strings can be accessed with array slicing
tByte             equ     $A2                    ; Signed Byte value
tInteger          equ     $A4                    ; all tokenized integers start with 251 as first byte 16 bit signed number
tLong             equ     $A6                    ; Signed 32 bit integer

tArray            equ     $A1                    ; Identifies Array Type, the byte following defines the length of each element
                                                 ; Arrays of string are arrays of pointers 2 bytes each
tPointer          equ     $A3                    ; Pointer  unsigned 16 bit
tIndirect         equ     $A5                     ; Points to an address that points to the data 16 bits
tuByte            equ     $A7                    ; Unsigned byte value 8 bit unsigned value
tUint             equ     $A9                    ; unsigned integer type 16 bit
tUlong            equ     $AB                    ; Unsigned 32 bit integer

Operators: BYTE "<>"
           BYTE "<="
           BYTE ">="
           Byte "<<"
           Byte ">>"
           BYTE "<",0
           BYTE "=",0
           BYTE ">",0
           Byte "++"
           BYTE "+",0
           Byte "--"
           BYTE "-",0
           BYTE "/",0
           BYTE "%",0
           BYTE "*",0
           BYTE "(",0
           BYTE ")",0
           BYTE ",",0
           BYTE ";",0
           BYTE "[",0
           BYTE "]",0
           BYTE ":",0
           BYTE "$",0
           BYTE "!",0
           BYTE "?",0
           BYTE ".",0
           BYTE "&",0
           Byte "'",0
           Byte "|",0
           Byte "~",0
           BYTE 0,0

OperValues: BYTE  oNotEqual,oLessEqual,oGreaterEqual,oSHL,oSHR,oLess,oEqual,oGreater
            BYTE  oINC, oPlus, oDEC, oMinus, oDivide, oModulo, oMultiply
            BYTE  oLeftBracket, oRightBracket, oComma, oSemiColon, oLeftSQBracket, oRightSQBracket
            BYTE  oColon, oDollar, oBang, oQuestion, oPeriod, oAmphistan, oQuote, oBar,oTilde

oQuestion         equ     kPrint
;    2 is =
;    1 is <
;    3 is <=
;    5 is <>
;    4 is >
;    6 is >=
oLess             equ     $F1
oEqual            equ     $F2
oLessEqual        equ     $F3
oGreater          equ     $F4
oNotEqual         equ     $F5
oGreaterEqual     equ     $F6

oLeftBracket      equ     $E0
oRightBracket     equ     $E1
oComma            equ     $E2
oSemiColon        equ     $E3
oLeftSQBracket    equ     $E4
oRightSQBracket   equ     $E5
oColon            equ     $E6
oDollar           equ     $E7
oBang             equ     $E8
oPeriod           equ     $E9

oPlus             equ     $EA
oMinus            equ     $EB
oDivide           equ     $EC
oModulo           equ     $ED
oMultiply         equ     $EE

oPercent          equ     oModulo
oAmphistan        equ     kAnd
oBar              equ     kOr
oQuote            equ     kRem
oTilde            equ     kXor
oSHR              equ     kShr
oSHL              equ     kShl
oINC              equ     kInc
oDEC              equ     kDec

tOperatorX        equ     $F0   ;+ operator Value  ; stores the value used to do the relational operator compare

tError            equ      $FF                     ; Error should never happen
;============================================================================================
; Keyword and seperator values
'
kBeginKey    equ     kLet
;
kLet         equ     1
kInc         equ     kLet+1
kDec         equ     kInc+1
kIreturn     equ     kDec+1
kIf          equ     kIreturn+1
kThen        equ     kIf+1
kGoto        equ     kThen+1
kGosub       equ     kGoto+1
kReturn      equ     kGosub+1
kRem         equ     kReturn+1
kPrint       equ     kRem+1
kTaske       equ     kPrint+1
kTaskn       equ     kTaske+1
kTaskw       equ     kTaskn+1
kPoke        equ     kTaskw+1
kPutch       equ     kPoke+1
kCls         equ     kPutch+1
kInput       equ     kCls+1
kEnd         equ     kInput+1
kIrq         equ     kEnd+1
kKill        equ     kIrq+1
kList        equ     kKill+1
kRun         equ     kList+1
kNew         equ     kRun+1
kSlice       equ     kNew+1
kTrace       equ     kSlice+1
kExit        equ     kTrace+1
kSave        equ     kExit+1
kLoad        equ     kSave+1
kErase       equ     kLoad+1
kDir         equ     kErase+1
kSetTerm     equ     kDir+1
kSetMemB     equ     kSetTerm+1
kSetMemW     equ     kSetMemB+1
kCopyMem     equ     kSetMemW+1
kWhile       equ     kCopyMem+1
kWend        equ     kWhile+1
kFor         equ     kWend+1
kNext        equ     kFor+1
kStep        equ     kNext+1
;
; End of actual key words
;
kKeyCount    equ     kStep-kBeginKey
;
; Logical operators
;
kNot         equ     kStep+1
kOr          equ     kNot+1
kXor         equ     kOr+1
kAnd         equ     kXor+1
;
; Shift operators
;
kShr         equ     kAnd+1
kShl         equ     kShr+1

; numeric functions
;
kBeginFunc   equ     kTrue
;
; Truth operators
;
kTrue        equ     kShl+1
kFalse       equ     kTrue+1
; Functions
kFree        equ     kFalse+1
kGetch       equ     kFree+1
kPeek        equ     kGetch+1
kTask        equ     kPeek+1
kIpcc        equ     kTask+1
kIpcs        equ     kIpcc+1
kIpcr        equ     kIpcs+1
kRnd         equ     kIpcr+1
kStat        equ     kRnd+1
kAbs         equ     kStat+1
kCall        equ     kAbs+1
kGofn        equ     kCall+1
kPid         equ     kGofn+1
kAddr        equ     kPid+1
kCmpMem      equ     kAddr+1
kTimer       equ     kCmpMem+1
;
kFuncCount   equ     ((kTimer - kBeginFunc) + 1)

;
; Keyword table contains 54 keywords
KeyWordTable:
             db     kLet,"leT"                        ; 1, we only have 0 at end of program or line
             db     kInc,"inC"
             db     kDec,"deC"
             db     kIreturn,"ireturN"
             db     kIf,"iF"
             db     kThen,"theN"
             db     kGoto,"gotO"
             db     kGosub,"gosuB"
             db     kReturn,"returN"
             db     kRem,"reM"
             db     kPrint,"prinT"
             db     kTaske,"taskE"
             db     kTaskn,"taskN"
             db     kTaskw,"taskW"
             db     kPoke,"pokE"
             db     kPutch,"putcH"
             db     kCls,"clS"
             db     kInput,"inpuT"
             db     kEnd,"enD"
             db     kIrq,"irQ"
             db     kKill,"kilL"
             db     kList,"lisT"
             db     kRun,"ruN"
             db     kNew,"neW"
             db     kSlice,"slicE"
             db     kTrace,"tracE"
             db     kExit,"exiT"
             db     kSave,"savE"
             db     kLoad,"loaD"
             db     kErase,"erasE"
             db     kDir,"diR"
;Short form for statements:
            db      kIreturn,"ireT"
            db      kReturn,"reT"
            db      kPrint,"pR"                        ; some dialects of tiny basic use this for print
            db      kSetTerm, "setterM"
            db      kSetMemB, "setmemB"
            db      kSetMemW, "setmemW"
            db      kCopyMem, "copymeM"
            db      kWhile,   "whilE"
            db      kWend,    "wenD"

; Shift operators
             db     kShr,"shR"
             db     kShl,"shL"

;Logical and truth operators
             db     kNot,"noT"
             db     kOr,"oR"
             db     kXor,"xoR"
             db     kAnd,"anD"

; Truth values
             db     kTrue,"truE"
             db     kFalse,"falsE"

;functions returning values

             db     kFree,"freE"
             db     kGetch,"getcH"
             db     kPeek,"peeK"
             db     kTask,"tasK"
             db     kIpcc,"ipcC"
             db     kIpcs,"ipcS"
             db     kIpcr,"ipcR"
             db     kRnd,"rnD"
             db     kStat,"staT"
             db     kAbs,"abS"
             db     kCall,"calL"
             db     kGofn,"fN"
             db     kPid,"piD"
             db     kAddr,"addR"
             db     kCmpMem, "cmpmeM"
             db     kTimer,"timeR"
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
;  if fails check for operators/seperators  + - < > = % / * () [] , ; : >> <<

ParseInputLine:
 if DEBUGPARSER
                jsr     SetOutDebug
                jsr     DebugClearBuffer
 endif
                lda    CUROFF
                pha
                pushxy
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
                ;lda     #0
                ;sta     TOKENBUFFER,x      ; null terminate the line of tokens
                stz      TOKENBUFFER,x      ; null terminate the line of tokens
                inx
                stx     TOKENBUFFER        ; Place size including null into buffer start

                pullxy
                pla
                sta     CUROFF

 if DEBUGPARSER

                jsr     printTokenBuffer
                ;jsr     DebugPrintProgramLine
                jsr     SetOutDebugEnd

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
                ldy      #0
                lda      #KeyWordTable&$FF        ; Key Table longer than 256 bytes
                sta      R1
                lda      #KeyWordTable>>8
                sta      R1+1                     ; R1 points to first entry in keyword table
                lda      (R1),y                   ; Get the Key Token value for first keyword
                sta      R0                       ; Save until next keyword
                iny                               ; Point to first character of keyword
                ldx      CUROFF                   ; X points to the character in the input buffer

  if DEBUGPARSER
            ;    jsr DebugKeyword
  endif

ParseLookupLoop:
                lda      (R1),y                ; Get the first character of the keyword
                and      #%11011111            ; Force Keyword to upper case
                cmp      LINBUF,x              ; Check the input buffer
                beq      ParseNextLetter       ; If it equals then do next letter
                ora      #%00100000            ; Force Keyword to lowercase
                cmp      LINBUF,x              ; Compare value to upercase
                bne      ParseNextEntry        ; Not equal then move to next entry in the keyword table

ParseNextLetter:
                lda      (R1),y                ; Check if we just processed the last letter is upper
                and      #%00100000            ; if this bit not set then end of keyword, Last char is always uppercase
                beq      ParseKeyFound         ; If we are at end of keyword and all match then we found the key
                inx                            ; Point to next char in the input buffer
                iny                            ; Point to the next character in the Keyword table
                lda     #0                     ; Check if we are at the end of the input buffer
                cmp     LINBUF,x               ; Check if we are at the end of the input buffer
                beq     ParseNextEntry         ; End of buffer but no keyword, ext keyword entry
                bne     ParseLookupLoop        ; Go back and check the next characters

ParseKeyFound:
                lda     R0                     ; get the keyword index

ParseKeyDone:
                inx                            ; point past the last character
                stx     CUROFF                 ; update to point to next character in the input buffer
                ldx     R2                     ; Restore the original x pointer
                sta     TOKENBUFFER,x          ; store the Token into the compiled buffer
                inx                            ; Point to next position in the output buffer
                stx     R2                     ; Save next position in buffer
                cmp     #kRem                  ; remark statement
                beq     ParseMoveLine          ; Move everything until the end of line to the token buffer
                cmp     #kGoto
                beq     ParseHandleBranches    ; Jump allow space for memory address in token buffer
                cmp     #kGosub
                beq     ParseHandleBranches    ; Handle the gosub branch address
                cmp     #kGofn
                beq     ParseHandleBranches    ; Handle the gosub branch address
                cmp     #kTask
                beq     ParseHandleBranches    ; We may have the ability to also compile task vectors Bracket between the space and the value

                clc                            ; C flag clear, we found it
                rts

; Move forward to the next entry in table
ParseNextEntry:
                lda   (R1),y                   ; Get the next character in the token
                and   #%00100000               ; Is it the last character
                beq   ParseEndOfEntry          ; Yes then end of this entry found
                iny                            ; Point to next char in the entry
                bne   ParseNextEntry           ; loop until we find the end character

ParseEndOfEntry:
                iny                            ; Point to the byte after the last character
                tya                            ; Move into a as we must add this to the pointer in R1, more that 256 keyword characters in table
                clc                            ; table May be longer than 256 so increment r1 to next entry
                adc   R1
                sta   R1
                lda   R1+1
                adc   #0
                sta   R1+1                     ; Now pointing to start of next entry in the table
                ldy   #0                       ; Reset the index back to zero
                lda   (R1),y                   ; get keyword value
                beq   ParseNoneFound           ; Check for end of the table -> 0
                sta   R0                       ; save the next token value
                iny                            ; Inc past token value

  if DEBUGPARSER
            ;    jsr DebugKeyword
  endif
                ldx   CUROFF                    ; Restore x to last position in the input buffer
                jmp   ParseLookupLoop           ; branch back for next key word

ParseNoneFound:
                ldx   R2                        ; it did not find one, restore x to position in output buffer
                sec                             ; c clear, not found
                rts

;===============================================================================
; Move everything from current position until the end of line into the token buffer
;
ParseMoveLine: ldy    CUROFF                       ; next byte to parse
               ldx    R2                           ; where to place in the buffer
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
;================================================================================================
; Add two bytes after the gosub and goto to allow the "compiler" to place mem address, to directly
; transfer to a memory address
ParseHandleBranches:
              ldx     R2
              lda     #0
              sta     TOKENBUFFER,x
              inx
              sta     TOKENBUFFER,x
              inx
              stx     R2
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
;Parse for operators and seperators
; on exit the A has the oper code, c is clear
;               not found then c is set
;      x is preserved
;
ParseForOperator:
                stx     R2
                ldy     CUROFF
                ldx     #0
  if DEBUGPARSER
          ;      jsr    DebugPrintOP
  endif

ParseOpLoop:
                lda     Operators,x                 ; First byte of operator
                beq     ParseOpNotFound             ; Last entry os 0,0

                cmp     LINBUF,y                    ; Check the first byte
                bne     ParseOpNext

                iny

                lda     Operators+1,x
                beq     ParseOpFoundSingle          ; Single Character op

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

  if DEBUGPARSER
        ;       jsr    DebugPrintOP
  endif
                ldy    CUROFF                ; reset the y pointer to beginning
                jmp    ParseOpLoop

ParseOpNotFound
                ldx   R2
                sec
                rts
;=========================================================================================================
  if DEBUGPARSER
;Print the text of a keyword
;Input R1    = offset into table
DebugKeyword:
            phy
            ldy     #1
DebugKeyLoop
            lda     (R1),y
            jsr     VOUTCH
            and     #%00100000
            beq     DebugKeyDone
            iny
            bne     DebugKeyLoop

DebugKeyDone:
            jsr     CRLF
            ply
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
            phx
            ldx     #$FF
            lda     #0
DebugClrLoop:
            sta    TOKENBUFFER,x
            dex
            bne    DebugClrLoop
            sta   TOKENBUFFER,x
            plx
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
            cpx   #0
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
;=========================================================================
; Transfer word in Token Buffer to R0
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
;==========================================================================
; Transfer     Display Buffer position to R0
;
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

;=========================================================================
; Read an IL byte lookit up in the table, of words
; set the next ilpc to point to that address
; if not found then do ussual filter stuff
; ongoto ilvectortable, not found address
iOnGoto:        jsr     getILWord                             ; places the word into r0, pointer to table
                stx     R0
                sta     R0+1

                ldy     CUROFF
                lda     (CURPTR),y                            ; get the operation byte
                ldy     #0
                sec
                sbc     (R0),y                                ; Subract the base value
                iny
                cmp     (R0),y                                ; Check if we are in range
                bcs     iOnGotoInvalid
                inc     CUROFF                                ; Save the offset

                asl
                tay                                           ; Turn into vector
                iny                                           ; Inc must include the table base and entry count
                iny

                lda     (R0),y
                sta     ILPC
                iny
                lda     (R0),y
                sta     ILPC+1
                jmp     NextIL

iOnGotoInvalid:
                jsr     getILWord
                stx     ILPC
                sta     ILPC+1
                jmp     NextIL
;
;==========================================================================================
; Test the token for relop and push the value onto the stack if true
;
iTSTRELOP:
              jsr       getILByte
              sta       offset

              ldy       CUROFF
              lda       (CURPTR),y
              pha
              and       #$F0
              cmp       #$F0
              bne       iTSTRELOPNOT
              pla
              and       #$0F                  ; get the actual value
              sta       R0                    ; save it for later
              lda       #0
              sta       R0+1
              jsr       pushR0
              iny
              sty       CUROFF                ; save the y pointer
              jmp       NextIL

iTSTRELOPNOT:
              pla
              jmp       tstBranch

;
;===================================================================================================
; Test the token and following info for precompiled address information
; skip it if zero, transfer and skip next integer value if not zero
; used by both gosub, goto and gofN
;
iTSTBRANCH:   ; il format TSTBRANCH whereToGoIfFailed
              jsr       getILByte               ; Get jump address if vector is valid
              sta       offset                  ; Mark offset for later if vector found
              ldy       CUROFF                  ; get offset of first byte of compiled value
              dey                               ; point back to the type of branch
              lda       (CURPTR),y              ; get the actual instructions
              pha                               ; Save till needed
              iny                               ; back to memory vectors
ITSTBRANCHCont
              lda       (CURPTR),y              ; Get first byte of compiled value
              sta       R0                      ; R0 will contain mem pointer of present
              iny                               ; Point to next byte of mem vector
              lda       (CURPTR),y              ; It was compiled so get the hi byte value
              sta       R0+1                    ; Move it into R0, R0 now contains vector address
              iny                               ; Point to the byte past memory vector
              sty       CUROFF                  ; At least point past the memory vector built in

              ora       R0                      ; Get the second byte of the mem
              BEQ       iTSTBRANCHNoCompile     ; If both are zero then not compiled
              pla
              cmp       #kGoto                  ; Short cut lots if a goto stuff
              bne       NotGoto
              jmp       FastFastXfer
NotGoto:
              cmp       #kTask                  ; Task defined with Task() so bypass the first bracket
              bne       iTSTBRANCHCont
              lda       (CURPTR),y
              cmp       #oLeftBracket
              bne       iTSTBRANCHErr           ; Well in that case something is very wrong
              iny                               ; Increment past the bracket
iTSTBRANCHCont:
              lda       (CURPTR),y              ; We should get a datatype, if not memvector is invalid
              cmp       #tByte                  ; A byte value is valid
              beq       ITSTBRANCHBYTE          ; Skip the byte
              cmp       #tInteger               ; An integer value is valid
              bne       iTSTBRANCHErr           ; If not then we can not use the memory vector
              iny                               ; skip type indicator for
ITSTBRANCHBYTE:
              iny                               ; skip first byte of value line number
              iny                               ; Skip second byte of line number

iTSTBRANCHVALID:
              sty       CUROFF
              jsr       pushR0                  ; place transfer address on top of stack
              jmp       tstBranch

iTSTBRANCHNoCompile:
              pla
              cmp #kGoto                        ; check here if the destination is . then just do it
              bne iTSTBRANCHErr
              lda (CURPTR),y
              cmp  #oPeriod                     ; check if the goto is back to this line, then just do it
              bne  iTSTBRANCHErr
              jmp  iXFER3
iTSTBRANCHErr:
              jmp       NextIL
