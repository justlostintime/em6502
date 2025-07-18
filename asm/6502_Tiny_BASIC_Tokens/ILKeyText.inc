            seg Code
  if IL_DEBUG_TEXT
ILTEXTTABLE:
  db    0,23,"iXINIT              ",0
  db    1,23,"iDONE               ",0
  db    2,23,"iPRS                ",0
  db    3,23,"iPRN                ",0
  db    4,23,"iSPC                ",0
  db    5,23,"iNLINE              ",0
  db    6,23,"iNXT                ",0
  db    7,23,"iXFER               ",0
  db    8,23,"iSAV                ",0
  db    9,23,"iRSTR               ",0
  db    10,23,"iCMPR               ",0
  db    11,23,"iINNUM              ",0
  db    12,23,"iFIN                ",0
  db    13,23,"iERR                ",0
  db    14,23,"iADD                ",0
  db    15,23,"iSUB                ",0
  db    16,23,"iNEG                ",0
  db    17,23,"iMUL                ",0
  db    18,23,"iDIV                ",0
  db    19,23,"iSTORE              ",0
  db    20,23,"iIND                ",0
  db    21,23,"iLST                ",0
  db    22,23,"iINIT               ",0
  db    23,23,"iGETLINE            ",0
  db    24,23,"iINSRT              ",0
  db    25,23,"iRTN                ",0
  db    26,23,"MONITOR             ",0
  db    27,23,"iLIT                ",0
  db    28,23,"iCALL               ",0
  db    29,23,"iJMP                ",0
  db    30,23,"iVINIT              ",0
  db    31,23,"iERRGOTO            ",0
  db    32,23,"iTST                ",0
  db    33,23,"iTSTV               ",0
  db    34,23,"iTSTL               ",0
  db    35,23,"iTSTN               ",0
  db    36,23,"iFREE               ",0
  db    37,23,"iRANDOM             ",0
  db    38,23,"iABS                ",0
;
  if    DISK_ACCESS
  db    39,23,"iOPENREAD           ",0
  db    40,23,"iOPENWRITE          ",0
  db    41,23,"iDCLOSE             ",0
  db    42,23,"iDGETLINE           ",0
  db    43,23,"iDLIST              ",0
  db    44,23,"iDDIR               ",0
  db    45,23,"iRMFILE             ",0
  else
  db    39,23,"NextIL              ",0
  db    40,23,"NextIL              ",0
  db    41,23,"NextIL              ",0
  db    42,23,"NextIL              ",0
  db    43,23,"NextIL              ",0
  db    44,23,"NextIL              ",0
  db    45,23,"NextIL              ",0
  endif
;
  db    46,23,"iCLEARSCREEN        ",0
  db    47,23,"iPOKEMEMORY         ",0
  db    48,23,"iPEEKMEMORY         ",0
  db    49,23,"iTSTLET             ",0
  db    50,23,"iTSTDONE            ",0
  db    51,23,"iGETCHAR            ",0
  db    52,23,"iPUTCHAR            ",0
  db    53,23,"iCallFunc           ",0
  db    54,23,"iBranch             ",0
  db    55,23,"iTSTStr             ",0
  db    56,23,"iSetIrq             ",0
  db    57,23,"iTstIrq             ",0
  db    58,23,"iRET                ",0
  db    59,23,"iINSTR              ",0
  db    60,23,"iMOD                ",0
  db    61,23,"iTaskSet            ",0
  db    62,23,"iETask              ",0
  db    63,23,"iNTask              ",0
  db    64,23,"iArray              ",0
  db    65,23,"iTaskKill           ",0
  db    66,23,"iTaskStat           ",0
  db    67,23,"iHexOut             ",0
  db    68,23,"iReadComplete       ",0
  db    69,23,"iReadStart          ",0
  db    70,23,"iStartIO            ",0
  db    71,23,"iEndIO              ",0
  db    72,23,"iLogNot             ",0
  db    73,23,"iLogOr              ",0
  db    74,23,"iLogAnd             ",0
  db    75,23,"iLogXor             ",0
  db    76,23,"iWTASK              ",0
  db    77,23,"iTASKPID            ",0
  db    78,23,"iTRACEPROG          ",0
  db    79,23,"idbgBasic           ",0
  db    80,23,"iIPCS               ",0
  db    81,23,"iIPCR               ",0
  db    82,23,"iIPCC               ",0
  db    83,23,"iIPCIO              ",0
  db    84,23,"iPushMathStack      ",0
  db    85,23,"iPopMathStack       ",0
  db    86,23,"iSaveMathStack      ",0
  db    87,23,"iRestoreMathStack   ",0
  db    88,23,"iIncParmCount       ",0
  db    89,23,"iTaskGetMathStack   ",0
  db    90,23,"iTaskEnable         ",0
  db    91,23,"iTaskSuspend        ",0
  db    92,23,"iTaskPutMathPtr     ",0
  db    93,23,"iTSTVT              ",0
  db    94,23,"iSetR2              ",0
  db    95,23,"iStk2Tmp            ",0
  db    96,23,"iTmp2Stk            ",0
  db    97,23,"iTSTBYTE            ",0
  db    98,23,"iINCVAR             ",0
  db    99,23,"iDECVAR             ",0
  db    100,23,"iSLICE              ",0
  db    101,23,"iTSTB               ",0
  db    102,23,"iTSTW               ",0
  db    103,23,"iOnGoto             ",0
  db    104,23,"iTSTRELOP           ",0
  db    105,23,"iRepeatLine         ",0
  db    106,23,"iTSTBRANCH          ",0
  db    107,23,"iFastXfer           ",0
  db    108,23,"iSetTerminal        ",0
  db    109,23,"iINDB               ",0
  db    110,23,"iSetBlock           ",0
  db    111,23,"iCopyBlock          ",0
  db    112,23,"iCmpBlock           ",0
  db    113,23,"iShift              ",0
  db    114,23,"iTimer              ",0
  db    115,23,"iJmpEnd             ",0
  db    116,23,"iJmpStart           ",0
  db    117,23,"iBeginBlock         ",0
  db    118,23,"iIfTrue             ",0
  db    119,23,"iIfFalse            ",0
  db    $FF,23,"0                   ",0

 ; Search for the il instruction and print the correct text for it
 ; on entry a contains the ip instruction to print

                seg.u  TBData
R0TempIL        ds    2
ILSTA           ds    1

                Seg     Code

PrintILText:    sta     ILSTA
                tya
                pha
                txa
                pha
                lda     R0
                sta     R0TempIL
                lda     R0+1
                sta     R0TempIL
                lda     #ILTEXTTABLE&$FF
                sta     R0
                LDA     #ILTEXTTABLE>>8
                sta     R0+1
;Loop Here for each entry
PrintILLoop:    ldy     #0
                lda     (R0),y
                cmp     #$FF
                beq     PrintILNotFound
                cmp     ILSTA
                beq     PrintILFound
                iny
                lda     (R0),y
                clc
                adc     R0
                sta     R0
                lda     #0
                adc     R0+1
                sta     R0+1
                jmp     PrintILLoop
; The text was discovered
PrintILFound:   iny                    ; point to length of instruction
                iny                    ; Point to start of text
                tya
                clc
                adc     R0
                tay                    ; Low order byte of address
                lda     #0
                adc     R0+1
                tax                    ; High order byte of address
                lda     #0             ; Termination byte of string
                jsr     PrtStr         ; print the string
                jmp     PrintILDone

PrintILNotFound:lda     ILSTA
                jsr     HexToOut

PrintILDone:    lda     R0TempIL
                sta     R0
                lda     R0TempIL+1
                sta     R0
                pla
                tax
                pla
                tay
                rts
  endif
