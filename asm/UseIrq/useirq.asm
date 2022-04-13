input            processor 6502
;=====================================================
;UseIrq is a sample program that enables the IRQ for the
;Emulator and then prints a star each time the
;irq arrives.
; 04/13/2022 https://github.com/justlostintime/em6502
;=====================================================
;   Need to define some macros for the dasm assembler
;
    MACRO dw
        .word {0}
    ENDM

    MACRO db
        .byte {0}
    ENDM
;
; Common ASCII constants
;
BEL			equ	$07
BS			equ	$08
TAB			equ	$09
LF			equ	$0A
CR			equ	$0D
QUOTE			equ	$22
SPACE			equ	$20
COMMA			equ	',
SEMICOLON		equ	';
;=====================================================
;
            SEG Code
            org	$0200
UseIrq      jmp CodeStart ; jump around the vectors

		include	"ctmon65.inc"
		SEG Code

OUTCH		jmp	cout
GETCH		jmp	cin
CRLF		jmp	crlf
OUTHEX		jmp	HexA
MONITOR		jmp	WARM
puts		equ	putsil

CodeStart jsr puts
          db CR,LF,CR,LF
          db "IRQ Test app Version 1.0.1"
          db CR,LF
          db "https://github.com/justlostintime/em6502"
          db CR,LF,0
		lda	#ServiceIrq&$ff	;Irq Service function
		sta	IRQvec
		lda	#ServiceIrq>>8
		sta	IRQvec+1
                lda	#ServiceNmi&$ff	;Nmi Service function
		sta	NMIvec
		lda	#ServiceNmi>>8
		sta	NMIvec+1
          cli
          jmp MONITOR

ServiceIrq  jsr puts
            db "IRQ Recieved"
            db CR,LF,0
            rti
            
ServiceNmi  jsr puts
            db "NMI Recieved"
            db CR,LF,0
            rti

