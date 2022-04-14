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
          
SaveReg     db 0,0,0

ServiceIrq  sta SaveReg
	    sty SaveReg+1
	    stx SaveReg+2
	    
	    lda SysTime                 ;Check if this created the IRQ
	    beq cycletimer
	    jsr puts                    ; the timer that uses a real system timer
	    db "IRQ Real Timer"
	    db CR,LF,0
	    lda #0                      ; Zero the pending and reset timer
	    sta SysTime_Ack             ; Write anything to reset the timer
	    beq ExitIrq

cycletimer  lda CycleTime               ; Check timer based upon clock cycles ticked
	    beq NotTimer
	    jsr puts                    ; the timer that uses a Cycle Clock as a timer source
	    db "IRQ Cycle Timer"
	    db CR,LF,0
	    lda #0                      ; Zero the pending and reset timer
	    sta CycleTime_Ack           ; Write anything to reset the timer
	    beq ExitIrq
	    
NotTimer    jsr puts                    ; we have some unknown irq happening
            db "IRQ UnKnown"
            db CR,LF,0
	    lda #0
            beq ExitIrq
            
ServiceNmi  sta SaveReg
	    sty SaveReg+1
	    stx SaveReg+2
	    
            jsr puts
            db "NMI Recieved"
            db CR,LF,0
            
ExitIrq	    lda SaveReg
	    ldy SaveReg+1
	    ldx SaveReg+2
            rti

