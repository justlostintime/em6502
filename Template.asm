     processor 6502
     .org $1000
     ; program here
     
     
     
     .org $fffa
     .word $1000          ; NMI Non maskible interupt vector
     .word $1000          ; reset vector
     .word $1000          ; IRQ/BRK interupt vector