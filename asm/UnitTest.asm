;assum for this test that the default computer config is used
;assumes that the TTYSIM is loaded at $e000

  processor 6502
  .org $8000
portout   equ $e000
loop:
  cli
  lda #$12
  nop
  nop
  sta $2000
  lda #$24
  sta $3000
  lda $2000
  jsr subcall
  nop
  nop
  jsr sayhello
  nop
  nop
  jsr sayhello2
  lda #$00
  nop
  cmp #$00
  nop
  beq loop
  jmp $0200             ; jump to the tiny basic

subcall:
  lda $3000
  sta $2010
  lda #$00
  adc #$40
  adc #$af
  nop
  nop
  sta $2000,x
  nop
  nop
  lda #$31
  sta outport
  lda #$32
  sta outport
  rts

sayhello:
  ldx #0
hloop:
      lda  hello,x
 beq done
 sta outport
 inx
 bne hloop
done: rts

hello: .byte "HELLO, WORLD!", 13,10,0
 MAC print
	ldx #0
.loop:
       lda {1},x
       beq  .done
       sta outport               ; assume here running in the default computer
       inx
       bne .loop
.done:
 ENDM

 MAC  greet
	print hello1
	print {1}
	print hello2
 ENDM

sayhello2:
	lda #147
	sta outport
	greet target1
	greet target2
	greet target3
	greet target4
	greet target5
	greet target6
	greet target7
	greet target8
	greet target9
	greet target10
	rts

hello1: .byte "HELLO, ",0
hello2: .byte "!", 13, 10, 0

target1:  .byte "PROGRAMMER",0
target2:  .byte "ROOM",0
target3:  .byte "BUILDING", 0
target4:  .byte "NEIGHBORHOOD", 0
target5:  .byte "CITY",0
target6:  .byte "NATION", 0
target7:  .byte "WORLD", 0
target8:  .byte "SOLAR SYSTEM", 0
target9:  .byte "GALAXY", 0
target10: .byte "UNIVERSE",0
