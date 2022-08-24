        seg Code
DEBUGPARSER       equ     TRUE                   ; Print debugging information

; Define the types of tokens found, and identifiers
KeywordsMax       equ     128                    ; Allow to be range  1 to 127  key words, high order bit must be 0 for it to be a key word
tVa               equ     128                    ; Variable A = 1, .... Z = 26   ^ = 27  
tVb               equ     130                    ; Variables 128 - 157  $80-$9D
tVhat             equ     155                    ; Variable ^
tVhash            equ     156                    ; Variable #
tVat              equ     157                    ; Variable @ = 0

tInteger          equ     160                    ; all tokenized integers start with 251 as first byte
tString           equ     161                    ; String all start with this byte and end with  byte value 0 strings can be accessed with array slicing
tByte             equ     162                    ; Unsigned byte value
tArray            equ     163                    ; Identifies Array Type, the byte following defines the length of each element
                                                 ; Arrays of string are arrays of pointers 2 bytes
tPointer          equ     164                    ; Pointer to another variable
tVariable         equ     165                    ; Variable index  = A-Z and ^ variables
tIndirect         equ     167                    ; Points to an address that points to the data

tOperatorX        equ     $F0 ;+ operator Value  ; stores the value used to do the relational operator compare
                                                 ;  +($F0), <($F1),=($F2),<=($F3), >($F4), <>($F5), >=($F6), -($F7), /($F8), %($F9), *($FA), (($FB), )($FC)
                                                 ;  240 - 252
tError            equ      255                   ; Error should never happen
;
; Keyword table contains 48 keywords
KeyWordTable:
             db     "leT"                        ; 1, we only have 0 at end of program or line
             db     "inC"
             db     "deC"
             db     "ireturN"
             db     "iF"
             db     "theN"
             db     "gotO"
             db     "gosuB"
             db     "returN"
             db     "reM"
             db     "prinT"
kPrint       equ    11                   ; sould be entry for print
             db     "taskE"
             db     "taskN"
             db     "taskW"
             db     "pokE"
             db     "putcH"
             db     "clS"
             db     "inpuT"
             db     "enD"
             db     "irQ"
             db     "kilL"
             db     "lisT"
             db     "ruN"
             db     "neW"
             db     "slicE"
             db     "tracE"
             db     "exiT"
             db     "savE"
             db     "loaD"
             db     "erasE"
             db     "noT"
             db     "oR"
             db     "xoR"
             db     "anD"
             db     "truE"
             db     "falsE"
;functions returning values
             db     "freE"
             db     "getcH"
             db     "peeK"
             db     "tasK"
             db     "ipcc"
             db     "ipcS"
             db     "ipcR"
             db     "rnD"
             db     "staT"
             db     "abS"
             db     "calL"
             db     "gofN"
             db     0,0
KeyWordTableEnd      equ      *
KeyWordTableLength   equ      * - KeyWordTable
TOKENBUFFER  ds    256                    ; placed here as temp for testing the Code
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
;  if fails check for operators + - < > = % / * ()
ParseInputLine:
 if DEBUGPARSER
                jsr     SetOutDebug
                jsr     DebugClearBuffer
 endif
                ldx     #1                 ; point to beginning of Token buffer + 1 reserve space for length byte
                jsr     ParseNumeric       ; Check for a line number, none is ok too
                bcs     ParseInputDefLine  ; no numeric value
                jsr     R02TOKEN           ; Move R0 to token buffer
                bcc     ParseInputLoop     ; always clears carry
                
ParseInputDefLine:                         ; create a default line number of zero
                lda     #0
                sta     TOKENBUFFER,x
                inx
                sta     TOKENBUFFER,x
                inx

ParseInputLoop:
                ldy     CUROFF
                jsr     SkipSpaces         ; Skip any spaces
                sty     CUROFF             ; Even if it fails at least remove the spaces
                lda     LINBUF,y           ; Check for end of line
                beq     ParseComplete      ; Finish token buffer and return
                
ParseForString:
                jsr     ParseString        ; Check for a string
                bcc     ParseInputLoop     ; It was a string 
                
ParseForKey:
                jsr     ParseLookupKey     ; Check for a keyword value
                bcs     ParseForNumber     ; Go look for a number
                bcc     ParseInputLoop     ; Go back for next token, we are not syntax checking

ParseForNumber:
                jsr     ParseNumeric       ; Check for a numeric value
                bcs     ParseForOp         ; Check for some form of operator
                bcc     ParseInputLoop     ; Go Back for next element

ParseForOp:
                jsr    ParseForOperator
                bcs    ParseForVar         ; Check for variables
                bne    ParseInputLoop

ParseForVar:
                jsr    ParseForVariable    ; Check for variable and convert to Index, as task centric
                bcs    ParseKeepChar       ; If we can match nothing then error get output
                bcc    ParseInputLoop

ParseKeepChar:                             ; if it does not parse just keep it safe
                lda    LINBUF,y
                sta    TOKENBUFFER,x
                inx
                iny
                sty    CUROFF
                bne    ParseInputLoop

ParseComplete:
                stx     TOKENBUFFER        ; Place size into buffer start
                lda     #0
                sta     TOKENBUFFER,x      ; null terminate the line of tokens
 if DEBUGPARSER
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
                jsr DebugKeyword
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
                beq     ParseNextEntry        ; End of buffer but no keyword
                bne     ParseLookupLoop

ParseKeyFound:  
                lda     R0
                
ParseKeySpecial:
                iny                            ;point past the last character
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
                jsr DebugKeyword
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
Operators: BYTE "<>","<=",">=",'+,0,'<,0,'=,0,">",0,"-",0,"/",0,"%",0,"*",0,"(",0,")",0
OperatorLen equ *-Operators

OperValues BYTE  $F5,$F3,$F6,$F0,$F1,$F2,$F4,$F7,$F8,$F9,$FA,$FB,$FC
OPCount    equ   * - OperValues

ParseForOperator:
                stx     R2
                ldy     CUROFF
                ldx     0
  if DEBUGPARSER
                jsr    DebugPrintOP
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
                jsr    DebugPrintOP
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

;==========================================================================================================
; Decode and print a line of grogram text
; Prints line number from R0 upto line number in R1 if r1 is 0 then prints to end
; if R0 and R1 = 0 then print entire program.
;
PrintProgramLine:
                ldy     #1              ; index into the token buffer
                ldx     TOKENBUFFER     ; get number of bytes

                rts
























