;===================================================================
;This file contains the memory allocation and free functions
;in herant in this is the management of free memory in the system
; the interface to these functions
; a,x returns or provides the low hi bytes of the managed addresses
; This uses the programend, to memory end as the area to manage
;===================================================================
              Seg Code
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
; Memory is allocated from the highest memory address towards
; the lowest memory address. meeting the Basic program end.
;====================================================
;MemFreeList            ds       2                 ; list of free blocks of memory, points to first block
;MemR0                  ds       2                 ; source for copy/move/Init
;MemR1                  ds       2                 ; Destination for copy/move
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
GetSizes
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