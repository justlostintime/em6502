USE6502 EQU FALSE
  if USE6502
  MACRO phy
    tya
    pha
  ENDM
  
  MACRO phx
    txa
    pha
  ENDM
  
  MACRO plx
    pla
    tax
  ENDM
  
  MACRO ply
    pla
    tay
  ENDM
  
  MACRO stz
    lda #0
    sta {1}
  ENDM
  
 endif
  
  MACRO pushxy
    phx
    phy
  ENDM
  
  MACRO pullxy
    ply
    plx
  ENDM
  
  MACRO dw
        .word {0}
  ENDM

  MACRO db
        .byte {0}
  ENDM
