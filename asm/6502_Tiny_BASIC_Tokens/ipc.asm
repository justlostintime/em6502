;======================================================
; Inter process communications.
; Tasks may write/read integer messages among
; them selves.
; This uses each tasks gosub stack as a message queue
; Gosub calls start at the highest address and the
; msg queue starts at the highest address.
;
;======================================================
; ipcs   - Send msg to another task or many tasks
; on entry  math stack contains the  top PID
;                                    2ND Message value
; on exit   math stack contain top True-good or False-failed
;
; it may not be sent if queue is full
;
; a = ipcs(<message-expression>,<task PID-expression>)
;
iIPCS:
                phy
                jsr       ipc_enqueue
                bcs       iIPC_BAD
                jsr       pushTrue
                pla
                tay
                jmp       NextIL
iIPC_BAD:
                ply
                jsr       pushFalse
                jmp       NextIL

;======================================================
; ipcr   - Recieve msg from task
; on exit  the message value is returned from message queue
;          message -1  is reserved meaning no entry found
; The provided variable contains the pid of the sending
; task. This is optional. This always waits for a message
; before returning.
;
; a = ipcr(<variable name>)
;
iIPCR:
                phy
                jsr       ipc_dequeue
                bcs       iIPCR_Q_Empty
                pla
                tay
                jmp       NextIL
iIPCR_Q_Empty:
                ply
                jsr       pushTrue                  ; puts -1 on the stack
                jmp       NextIL

;=======================================================
; ipcc   - Check if message available
; on exit  Stack contains number of messages
;
; a = ipcc()
;
iIPCC:
         phy
         jsr      ipc_queue_count
         jsr      pushR0            ; return the count
         ply
         jmp      NextIL

;=======================================================
;ipcio    Turns on the tasks wait ips if nothing in queue
iIPCIO:
        phy
         jsr      ipc_queue_count
         lda      R0
         bne      iIPCIO_No_Halt
         lda      #1
         sta      taskCurrentCycles     ; force a task switch
         lda      #TASKWAITIPC
         ldy      taskPtr
         ora      taskTable,y
         sta      taskTable,y

iIPCIO_No_Halt:
         ply
         jmp      NextIL
;======================================================
;ipc_queue_count returns number of entries on the queue
; waiting to be recieved
;======================================================
ipc_queue_count:
         lda      MESSAGEPTR
         clc
         lsr                        ; divide by 4
         lsr
         sta      R0                ; store into R0
         lda      #GOSUBSTACKSIZE
         sec
         sbc      R0                ; Get how many entries on queue
         sta      R0
         lda      #0
         sta      R0+1
         rts
;=======================================================
; Support functions for messaging
;
; Enqueue message -> onto PID's MSG Q
; on entry top of stack contains the PID
;          second contains the Message of the task
; on exit contains c set if failed
;                  c cleared if success
;                  PID's MSG Q PTR points to the message
;===========================================================
ipc_enqueue:
              jsr     popR1                          ; Get the pid
              jsr     ipc_getcontext                 ; Get the PID's context into MQ

              ldy     #GOSUBPTRPOS                    ; pointer to required information
              lda     (MQ),Y                          ; Get the stk ptr gosub queue
              ldy     #MSGPTRPOS                      ; Get the offset to the msg q ptr
              cmp     (MQ),y                          ; Test if there is already the max messages on stack
              bcs    ipc_enq_full                     ; Exit with queue full message


              ; Get the PID'S stack address into R0
              ldy     #GOSUBSTKPOS
              lda     (MQ),y
              sta     R0
              iny
              lda     (MQ),y
              sta     R0+1                            ; R0 now points to Task gosub/msg stack

              ; Set y to point to the msg q entry
               ldy     #MSGPTRPOS                     ; Get the offset to the msg q ptr
               lda     (MQ),y                         ; Get the index
               tay                                    ; Set y to queue offset

              ; enqueue the message
              dey                                     ; First byte to save to
              lda     #GOSUB_MSG                      ; Get the Entry type
              sta     (R0),y                          ; Set the entry type

              dey
              lda     taskPtr                         ; Store the PID into queue
              sta     (R0),y
              jsr     popR1                           ; Get the actual message value
              jsr     ipc_pushR1                      ; Store Message value into queue

              tya                                     ; Save the new q ptr
              ldy     #MSGPTRPOS
              sta     (MQ),y                          ; Update the message stack pointer
              ldy     #0                              ; points to context root
              lda     #TASKWAITIPC                    ; Turn off the ipc wait flag
              eor     (MQ),y                          ; Turn off the bit
              sta     (MQ),y                          ; Clear the ipc wait flag
              clc
              rts
ipc_enq_full:
              sec
              rts
;=============================================================
; De-queue for message stack -> local tasks msg q
;  on entry  top of math stack contains the Variable to place, or 0 if not to save
;  message into
;  on exit   math stack contains value of message
;                                Variable if provided is pid
ipc_dequeue:
              jsr     popMQ                                  ; Variable address to put PID into

              ldy     MESSAGEPTR
              cpy     #[[GOSUBSTACKSIZE - 2] * 4]            ; see if anything to pop from stack
              bcs     ipc_deq_empty
              lda     (GOSUBSTACK),y                         ; get the message value
              sta     R0
              iny
              lda     (GOSUBSTACK),y
              sta     R0+1
              iny
              lda     (GOSUBSTACK),y                         ; get the pid value
              sta     R1
              iny
              lda    (GOSUBSTACK),y                          ; Get the type of message
              iny
              sty    MESSAGEPTR                              ; Save the message q ptr

              cmp    #GOSUB_MSG                              ; Should be a message
              bne    ipc_deq_empty

              jsr    pushR0                                  ; place value on stack

              lda    MQ
              ora    MQ+1
              beq    ipc_deq_done
              lda    R1
              ldy    #0
              sta    (MQ),y
              iny
              lda    #0
              sta    (MQ),y
ipc_deq_done:
              clc
              rts

ipc_deq_empty:
              sec
              rts

;=============================================
;  Get the context address into MQ from R1 with
;  context/index/pid
ipc_getcontext:
              clc                                    ; Get pointer to Task context
              lda     #taskTable&$FF                 ; change ptr to address
              adc     R1
              sta     MQ
              lda     #taskTable>>8
              adc     R1+1
              sta     MQ+1                            ; We now have a pointer into the context
              rts
;
;==============================================
; on entry R1 has a context value,
; on exit c is set if fails
;
ipc_CONTEXTVALUES:
              db     $00,CONTEXTLEN,[CONTEXTLEN*2],(CONTEXTLEN*3)
              db     (CONTEXTLEN*4),(CONTEXTLEN*5),(CONTEXTLEN*6),(CONTEXTLEN*7)
              db     (CONTEXTLEN*8),(CONTEXTLEN*9)

ipc_ValidateContext:
              pha
              phx
              lda      R1+1
              bne      ipc_Validate_Fail
              ldx      #0
              lda      R1
ipc_ValidateLoop:
              cmp      ipc_CONTEXTVALUES,x
              beq      ipc_Valid_Context
              inx
              cpx      #TASKCOUNT
              bcc      ipc_ValidateLoop

ipc_Validate_Fail:
              plx
              pla
              rts

ipc_Valid_Context:
              clc
              bcc      ipc_Validate_Fail
;
;==============================================
;Push R1 onto the stack
;on entry y = next entry
;R0 points to the stack space
;on exit y points to next free byte
ipc_pushR1:
              dey
              lda     R1+1                              ; PID first
              sta     (R0),y
              dey
              lda     R1
              sta     (R0),y
              rts







