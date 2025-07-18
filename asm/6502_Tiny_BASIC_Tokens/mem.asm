;===================================================================
;This file contains the memory allocation and free functions
; This is the management of free memory in the system
; the interface to these functions
; a,x returns or provides the low hi bytes of the managed addresses
; This uses the programend, to memory end as the area to manage
;===================================================================
          Seg.u  TBData
;
;=====================================================
;Pointers for memory Management
;Allocated block are not chained but can be followed for all memory by the associated length
; Mem block format is
;       0-1   pointer to next block for free blocks
;       0-1   for allocated blocks
;         0   type of block, blob | array bytes, ints ,string | single type byte or integer
;         1   refrence counter ... lol only up to 256 but it is something
;       2-3   length constant for exevy type of memory block
; Memory is recombined as it is released
; The memory manager is not interupted durring allocation
; or freeing of memory
;====================================================
MemFreeList            ds       2                 ; list of free blocks of memory
MemR0                  ds       2                 ; source for copy/move/Init
MemR1                  ds       2                 ; Destination for copy/move
;=====================================================
              Seg Code
;=====================================================
MemInit:
                lda     #FreeMemStart&$FF
                sta     ProgramStart
                sta     ProgramEnd
                lda     #FreeMemStart>>8
                sta     ProgramStart+1
                sta     ProgramEnd+1

                jsr     GetSizes
                jsr     MemFree
                jsr     MemUsed
MemInitEnd:
                rts


;
;=====================================================
; This function might go away eventually, but was
; added to provide data for other pieces of code.
; It has some ties to the operating environment that
; will need to be customized for the target system.
;
GetSizes:
;
; Here is machine specific code to get the highest
; memory location that can be used by BASIC.
;
          if ProgramStart < $2000
                lda     #$ff
                sta     HighMem               ;$13ff for KIM-1
                sta     MemFreeList
                lda     #$DE                  ;#$13
                sta     HighMem+1
                sta     MemFreeList+1
          else
                lda     #$ff
                sta     HighMem               ;$CFFF otherwise
                lda     #$cf
                sta     HighMem+1
          endif
          rts
;
; This computes the available memory remaining.
;
MemFree:
                sec
                lda     HighMem
                sbc     ProgramEnd
                sta     FreeMem
                sta     R0
                lda     HighMem+1
                sbc     ProgramEnd+1
                sta     FreeMem+1
                sta     R0+1
                rts
;
; This computes the size of the current user program.
;
MemUsed:
                sec
                lda     ProgramEnd
                sbc     ProgramStart
                sta     UsedMem
                sta     R0
                lda     ProgramEnd+1
                sbc     ProgramStart+1
                sta     UsedMem+1
                sta     R0+1
;
                rts
;
;=====================================================
; Set a block of memory to a value
iSetBlock:      txa
                pha
                tya
                pha
                jsr     popR0                 ; the address to write to
                lda     R0
                sta     dpl
                lda     R0+1
                sta     dpl+1
                jsr     popR1                 ; Number of bytes to write
                jsr     popR0                 ; Get the value to store into memory
                jsr     getILByte
                sta     R2                    ; store the data type into R2
                cmp     #tInteger
                beq     memset                ; skip this if we have an integer
                lda     R0                    ; Revers the order so they can be copied in correct order
                ldx     R0+1
                stx     R0
                sta     R0+1

memset:
                ldy     #0                    ; Set for length of block to copy
                ldx     #0                    ; set for number of block of 256 to copy

iSetBlockLoop:  lda     R2                    ; Get Datatype
                cmp     #tByte
                beq     iSetBlockB

iSetBlockW:     lda     R0
                sta     (dpl),y
                jsr     iSetBlockEnd
                beq     iSetBlockComplete

iSetBlockB:     lda     R0+1
                sta     (dpl),y
                jsr     iSetBlockEnd
                bne     iSetBlockLoop

iSetBlockComplete:
                pla
                tay
                pla
                tax
                jmp     NextIL
;
; Check if we have reached the end of the initialization/Copy
;
iSetBlockEnd:   iny
                bne     iSetBlockEndChk
                inx
                inc     dpl+1
iSetBlockEndChk:
                cpy     R1
                bne     iSetBlockEndExit
                cpx     R1+1
iSetBlockEndExit:
                rts
;
;================================================================
; Copy a block of memory from one location to another
;
iCopyBlock:     txa
                pha
                tya
                pha
                jsr     popR0           ; get the source address
                jsr     popR1           ; Destination address
                lda     R1
                sta     dpl
                lda     R1+1
                sta     dpl+1
                jsr     popR1           ; Number of bytes to copy
memcpy:
                ldx     #0
                ldy     #0
iCopyBlockLoop:
                lda     (R0),y          ;  Get the byte to copy
                sta     (dpl),y         ;  Store the byte
                iny
                bne     iCopyChkEnd
                inx
                inc     R0+1
                inc     dpl+1
iCopyChkEnd:    cpy     R1
                bne     iCopyBlockLoop
                cpx     R1+1
                bne     iCopyBlockLoop
iCopyBlockDone:
                pla
                tay
                pla
                tax
                jmp     NextIL
;
;=============================================================================
; Compare memory block location
; returns on the stack
; 0 - equals
; -1 - s1  <  s2
; 1   s1  >  s2
iCmpBlock:      txa
                pha
                tya
                pha
                jsr     popR1              ; Get the Source 2 pointer
                lda     R1
                sta     dpl                ; store the secon source in dpl
                lda     R1+1
                sta     dpl+1
                jsr     popR0              ; Get the Source 1 pointer
                jsr     popR1              ; Get the length of the compare to do
                ldy     #0
                ldx     #0
                jmp     iCmpCheckEnd

iCmpLoop:       lda     (dpl),y
                cmp     (R0),y
                bne     iCmpDone
                iny
                bne     iCmpCheckEnd
                inx
iCmpCheckEnd:
                cpy     R1
                bne     iCmpLoop
                cpx     R1+1
                bne     iCmpLoop
                lda     0
                sta     R0+1
                sta     R0
iCmpReturn:
                pla
                tay
                pla
                tax
                jmp     pushR0nextIl

iCmpDone:
                bcc     iCmpGreater
iCmpLess:
                lda     #0
                sta     R0+1
                lda     #1
                sta     R0
                bne     iCmpReturn
iCmpGreater:
                lda     #-1
                sta     R0
                sta     R0+1
                bne     iCmpReturn

