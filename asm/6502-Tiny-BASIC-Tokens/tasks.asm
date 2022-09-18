;=====================================================
; Tiny Basic IL task management
; Data required by task management
; currently each context is about 30 bytes and is swapped
; into and out of page zero on each task switch....
; LOL yes it is slow, but works for this iteration.
;

                Seg Code
;=====================================================
; Sets the pointers to the math,IL and gosub stacks
; Creates the initial Context for each task slot
taskSetStacks
                lda     #mathStack&$FF
                sta     MATHSTACK
                lda     #mathStack>>8
                sta     MATHSTACK+1

                lda     #ilStack&$ff
                sta     ILSTACK
                lda     #ilStack>>8
                sta     ILSTACK+1

                lda     #gosubStack&$FF
                sta     GOSUBSTACK
                lda     #gosubStack>>8
                sta     GOSUBSTACK+1

                lda     #variableStack&$FF
                sta     VARIABLES
                lda     #variableStack>>8
                sta     VARIABLES+1
                ldx     #TASKCOUNT
                ldy     #0
                jsr     ContextSave

taskSetLoop     cpy     #TASKTABLELEN
                bcs     taskSetDone

                lda     GOSUBSTACK
                clc
                adc     #GOSUBSTACKSIZE*4   ; must be less than 256
                sta     GOSUBSTACK
                lda     GOSUBSTACK+1
                adc     #0
                sta     GOSUBSTACK+1

                lda     ILSTACK   ; must be less than 256
                clc
                adc     #ILSTACKSIZE*2
                sta     ILSTACK
                lda     ILSTACK+1
                adc     #0
                sta     ILSTACK+1

                lda     MATHSTACK   ; must be less than 256
                clc
                adc     #MATHSTACKSIZE*2
                sta     MATHSTACK
                lda     MATHSTACK+1
                adc     #0
                sta     MATHSTACK+1

                lda     VARIABLES   ; must be less than 256
                clc
                adc     #VARIABLESSIZE*2
                sta     VARIABLES
                lda     VARIABLES+1
                adc     #0
                sta     VARIABLES+1

                jsr     ContextSave
                jmp     taskSetLoop

taskSetDone
                ldy     #0                    ; reload the main loop context
                jsr     ContextLoad
                rts
;
;=====================================================
; In some error cases the math stacks may be left pointing to the wrong stack
; This function will reset those stack addresses but not the actual pointer
taskResetStacks:
                ldy     #0
                jsr     ContextLoad
                jmp     taskSetStacks
;
;=====================================================
; Clear all task entries and task stacks
taskReset       tya                        ; Save Y
                pha
                lda     #1
                sta     taskCounter         ; Set number of active tasks to 1
                ldy     taskPtr             ; Set the active task to 0 MAIN
                cpy     #0                  ; check if we are the main context
                beq     taskResetCont       ; if we are just continue

                ldy     #0                  ; else we need to switch to the main context
                sty     taskPtr
                jsr     ContextLoad         ; load the System Task context
taskResetCont
                ldy     #CONTEXTLEN+1       ; Start at the second task +1 account for task control byte

taskResetLoop
                lda     #TASKINACTIVE
                sta     taskTable,y         ; Ensure that the task is made inactive
                clc
                tya
                adc     #CONTEXTLEN+1
                tay
                cpy     #TASKTABLELEN       ; Are we at the end yet
                bcc     taskResetLoop       ; Go for more

taskResetComplete

                pla                         ; Restore y
                tay
                rts

;
;======================================================
; iTaskSwitch   switch to new task if not interrupt and
;               count is exceded for task time slice gets here
;               when time slice has reached zero
;
iTaskSwitch     tya
                pha

                lda     taskResetValue            ; Always reset the counter value
                sta     taskCurrentCycles         ; Update the counter with the new value
                dec     taskCurrentCycles+1       ; dec high order byte
                bne     iTaskSwitchDone           ; Exit if not zero

                lda     taskResetValue+1
                sta     taskCurrentCycles+1

                lda     IRQPending                ; Skip this if we are processing an irq
                ora     taskIOPending             ; If set then don't switch
                bne     iTaskSwitchDone           ; DO irq Higher priority than the Tasks

iTaskMain       lda     taskCounter               ; Number of tasks
                cmp     #1                        ; if there is only one task must be main
                bne     itasknext                 ; if it some other number continue to next

                ldy     taskPtr                   ; check if we have not just ended some other task
                bne     itasknext                 ; 0 = main task if so then do a next anyway
                beq     iTaskSwitchDone           ; Skip this if main is only task
;
; Save the current context this is moved from BASIC STMT LEVEL TO IL INSTRUCTION LEVEL
;
itasknext
                ldy     taskPtr
                jsr     ContextSave               ; Save the current context, y points to next context
itaskLoop
                cpy     #TASKTABLELEN             ; Are we at end of task table
                bcc     iTaskNextChk

iTaskResetTop   ldy     #0                        ; reset to top of taskTable
                beq     iTaskLoadEntry            ; Go Ahead and just start this As we Can back and it is always active

iTaskNextChk
                lda     taskTable,y               ; there is always at least one entry in table
                bne     iTaskLoadEntry            ; get next slot if this one empty
iTaskNext       clc
                tya
                adc     #CONTEXTLEN+1             ; Next Table entry
                tay
                jmp     itaskLoop                 ; Check for busy entry

iTaskLoadEntry  lda     #TASKACTIVE
                eor     taskTable,y               ; Check for anything waiting io
                bne     iTaskNext
                jsr     ContextLoad               ; load the next context
                sty     taskPtr                   ; update the task pointer

iTaskSwitchDone
                pla
                tay
                rts
;
;================================================================
; Task Set task number to line number to start
; on entry stack contains, type of line description and  memvector or linenumber 
; Task Table structure:
;    byte 0    -   Active inactive
;    byte 1-2  -   Basic code line pointer
;    byte 3    -   Offset on current line
iTaskSet:       tya                 ;preserve Y
                pha                                                                          ; push a
                jsr     popR1       ; Get if compiled or line number expression
                jsr     popR0       ; Get the line number to be saved
                
               

                ldy     taskPtr     ; find out where we are
                jsr     ContextSave ; Save the current context

;Find the pointer to the line we need to start at
                lda     R1
                beq     iTaskLineNum
                lda     R0
                sta     CURPTR
                lda     R0+1
                sta     CURPTR+1
                jmp     iTaskCont
                
iTaskLineNum:
                jsr     findLine    ; Get the offset of the line to start task at
                beq     iTaskCont

                ldy     taskPtr     ; Restore the original Context Error Exit
                jsr     ContextLoad

                pla                                                                         ; pop a - exit
                tay
                jmp     iSetIrqErr  ; Bad line number provided

iTaskCont
                jsr     TaskEmpty   ; Find an empty slot, y = new slot
                bcc     iTaskNoEmpty; There are no more empty slots

                lda     #TASKRUNPENDING+TASKACTIVE      ; Mark as enabled but suspended
                sta     taskTable,y                     ; new task as active

                lda     CURPTR
                pha                                                                        ; push a
                lda     CURPTR+1
                pha                                                                        ; push a

                jsr     ContextLoad  ; load the context of the new task

                pla                                                                        ; pop a
                sta     CURPTR+1
                pla                                                                        ; pop a
                sta     CURPTR
                lda     #3          ; Offset to first instruction
                sta     CUROFF

                lda     #0
                sta     ILSTACKPTR
                sta     MATHSTACKPTR
                sta     GOSUBSTACKPTR
                lda     #GOSUBSTACKSIZE*4
                sta     MESSAGEPTR

                jsr     subVINIT       ; Clear the variables

                lda     #STMT&$FF
                sta     ILPC
                lda     #STMT>>8       ; set ilpc to point to the STATEMENT processor
                sta     ILPC+1

                tya                    ; Save the new context offset to return to user
                pha                                                                            ; push a

itaskSetSave    jsr     ContextSave    ; save the updated context
                inc     taskCounter    ; Update the number of Tasks running

                ldy     taskPtr
                jsr     ContextLoad    ; restore the original context

                lda     #0             ; Set the R0 upper to zero
                sta     R0+1
                pla                    ; Get the task pid we stored                            ; pop a
                sta     R0             ; Get the table entry value

                pla                    ; Restore the y register we saved                       ; pop a   - exit
                tay

                jmp     pushR0nextIl   ; Push R0 and continue
iTaskNoEmpty
                ldy     taskPtr
                jsr     ContextLoad

                pla                                                                           ; pop a    -- exit
                tay

                ldx     #ERR_NO_EMPTY_TASK_SLOT
                lda     #0
                jmp     iErr2
;
;===============================================================
; Run the task whos PID is on the stack, preserve the stack
;
iTaskEnable
               tya
               pha
               jsr      popR1
               jsr      pushR1
               jsr      ipc_getcontext         ; get context pointer into mq
               ldy      #0
               lda      (MQ),y
               eor      #TASKRUNPENDING        ; Turn off the Suspend flags
               ora      #TASKACTIVE
               sta      (MQ),y
               pla
               tay
               jmp      NextIL

;
;===============================================================
; Suspend the task whos PID  is on the stack, preserve the stack
;
iTaskSuspend
               tya
               pha
               jsr      popR1
               jsr      pushR1
               jsr      ipc_getcontext         ; get context pointer into mq
               ldy      #0
               lda      (MQ),y
               ora      #TASKRUNPENDING        ; Turn off the Suspend flags
               ora      #TASKACTIVE
               pla
               tay
               jmp      NextIL

;================================================================
; Returns task Status
iTaskStat
                tya
                pha
                jsr     iTaskValid     ; returns pointer to task entry
                lda     taskTable,y
                beq     iTaskStatExit
                pla
                tay
                jmp     iTruth
iTaskStatExit
                pla
                tay
                jmp     iFalse

;
;================================================================
; Validate the task number on top of the stack
; on exit y points to the requested task entry
;
iTaskValid      jsr     popR0            ; get result of the multiply
                lda     R0+1
                bne     iTaskValidErr    ; high byte must be zero
                lda     R0
                cmp     #TASKTABLELEN
                bcc     iTaskIsValid

iTaskValidErr   pla     ;remove return address
                pla
                ldx     #ERR_INVALID_PID
                lda     #0
                jmp     iErr2

iTaskIsValid    tay
                rts
;
;================================================================
; Kill a running task, do nothing if already stopped
iTaskKill       jsr     iTaskValid
                lda     #0
                sta     taskTable,y     ; Fall thru to go to ntask - nexttask
;
;================================================================
;Skip to next task
iNTask
                lda     #1
                sta     taskCurrentCycles
                sta     taskCurrentCycles+1
                jmp     NextIL
;
;=======================================================
; Wait for a task to complete
iWTASK
                jsr     getILByte
                sta     offset
;
                jsr     saveIL          ;in case of failure

                jsr     iTaskValid      ; returns pointer to task entry from stack, y is offset
                lda     taskTable,y
                bne     iWTASKWAIT
iWTASKEXITED
                jmp     NextIL
iWTASKWAIT:
                jsr     pushR0                  ; Push R0 back onto the stack
                lda     #1
                sta     taskCurrentCycles       ; Give up the cycles
                sta     taskCurrentCycles+1
                jsr     restoreIL
                jmp     tstBranch
;
;=======================================================
; Set task io lock
iStartIO        inc    taskIOPending
                jmp    NextIL
;
;=======================================================
; Release the io lock
iEndIO          lda   taskIOPending
                beq   iEndIOExit
                dec   taskIOPending
iEndIOExit      jmp   NextIL
;
;===============================================================
; Return the task PID
iTASKPID
                lda     #0
                sta     R0+1
                lda     taskPtr
                sta     R0
                jmp     pushR0nextIl
;
;================================================================
; Terminate a task
iETask          ldy     taskPtr
                cpy     #0
                bne     iETaskCont
                jmp     iFIN                      ; if the main task does a ETASK then stop
iETaskCont
                lda     #TASKINACTIVE
                sta     taskTable,y               ; mark entry as free
                dec     taskCounter               ; reduce the number of active tasks
                lda     #1
                sta     taskCurrentCycles         ; Make it 1 as rtn will dec and check
                sta     taskCurrentCycles+1
                jsr     TaskSetExitCode
iETaskExit
                jmp     NextIL
;================================================================
; make the current tasks math stack equal another tasks stack
; The task to get is stored on the math stack

iTaskGetMathStack
                jsr     CopyStackR1             ; Get the top of stack to R1
                jsr     ipc_getcontext          ; MQ now has the context address
                ldy     #MATHSTACKPTRPOS
                lda     (MQ),y
                sta     MATHSTACKPTR
                ldy     #MATHSTACKPOS
                lda     (MQ),y
                sta     MATHSTACK
                iny
                lda     (MQ),y
                sta     MATHSTACK+1
                jmp     NextIL
;==================================================================
; Updates the tasks math stack pointer with contents of R2
; PID is on top of the stack
iTaskPutMathPtr
                jsr     CopyStackR1             ; Get the top of stack to R1
                jsr     ipc_getcontext          ; MQ now has the context address
                lda     R2
                ldy     #MATHSTACKPTRPOS
                sta     (MQ),y
                jmp     NextIL
;
;================================================================
; Set the time slice for each task
iSLICE
                jsr   popR0
                lda   R0
                sta   taskResetValue
                lda   R0+1
                sta   taskResetValue+1
                bne   iSliceSet
                inc   taskResetValue+1             ; must be at least 1 high counter
                lda   #1
                sta   taskCurrentCycles
                sta   taskCurrentCycles+1
iSliceSet
                jmp   NextIL
;================================================================
; Find an empty slot in the taskTable
; Return the index in y
; on exit   c set if an empty slot is found
;           c clear if not found
;================================================================
;
TaskEmpty       lda     taskCounter
                cmp     #TASKCOUNT
                bcs     TaskNoSlot
                ldy     #CONTEXTLEN+1                ;The first slot is always the main line SKIP
TaskLoop
                lda     taskTable,y
                beq     TaskEmptyFnd
                tya
                clc
                adc     #CONTEXTLEN+1
                tay
                cpy     #TASKTABLELEN
                bcc     TaskLoop          ; Y is never zero
TaskNoSlot
                clc
                rts
TaskEmptyFnd
                sec
                rts
;====================================================
; Set the task exit code called from the return command
; on entry stack top hold exit value
TaskSetExitCode
                tya
                pha
                jsr       popR0
                ldy       #TASKEXITCODE
                lda       R0
                sta       (VARIABLES),y
                iny
                lda       R0+1
                sta       (VARIABLES),y
                pla
                tya
                rts

;
;=====================================================
; Save Context Store the context to the TASK Table
; on entry y contains the task table entry to save to
; on exit y points to next task table entry
;         x contains the number of bytes copied
ContextSave     ldx   #0
                iny             ;inc past the task flags
ContextSvLoop   lda   CONTEXT,x
                sta   taskTable,y
                iny
                inx
                cpx   #CONTEXTLEN
                bcc   ContextSvLoop
                rts
;
; Load Context transfer context from task table to the Current Context
; on entry y contains the task table entry to transfer
; on exit y points to the original task table entry
;         x contains the number of byts copied
ContextLoad     tya
                pha
                ldx   #0
                iny                 ;inc past the task flags
ContextLDLoop   lda   taskTable,y
                sta   CONTEXT,x
                iny
                inx
                cpx   #CONTEXTLEN
                bcc   ContextLDLoop
                pla
                tay
                rts
