; /* Map a system specific error into a system independent code */
;
        .setcpu         "65C02"
        .export         ___osmaperrno
        .include        "errno.inc"

.code

.proc   ___osmaperrno
        rts
.endproc