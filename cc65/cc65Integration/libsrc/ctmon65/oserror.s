; /* Map a system specific error into a system independent code */
;
        .export         ___osmaperrno
        .include        "errno.inc"

.code

.proc   ___osmaperrno
        rts
.endproc