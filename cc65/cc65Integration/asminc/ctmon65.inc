; ---------------------------------------------------------------------------
;
; CTMON65 definitions
;
; ---------------------------------------------------------------------------


RAMSTART := $0200     ; Entry point


; ---------------------------------------------------------------------------
; Monitor Functions
; ---------------------------------------------------------------------------
RESETMONITOR  := $F000     ; Reset everything and restart
WARMMONITOR   := $F003     ; Jump to a warm start
INTCHR        := $F009     ; Input character without case conversion
OUTCHR        := $F00C     ; Output character
CSTATUS       := $F00F     ; Get keyboard status
PUTSIL        := $F012     ; Write a string to console
GETLINE       := $F015     ; Read a line from console
CRLF          := $F018     ; Write  cr/lf to console
HEXA          := $F01b     ; Write the value in A as hex to console

;Low level sd card functions
XPARINITvev     := $f01e
XPARSETWRITEvec := $f021
XPARSETREADvec  := $f024
XPARWRITEvec    := $f027
XPARREADvec     := $f02a

DISKPING      := $F02D     ; Ping disk to see if it is working
DISKDIR       := $F030     ; REad content of disk directory
DISKDIRNEXT   := $F033     ; Get next entry from disk directory
DISKOPENREAD  := $F036     ; Open a file for reading
DISKOPENWRITE := $F039     ; Open a disk file for writing
DISKREAD      := $F03C     ; Read a block of data from disk
DISKWRITE     := $F03F     ; Write a buffer to disk file
DISKCLOSE     := $F042     ; Close the disk file
DISKRMFILE    := $F045     ; Delete a file


; ---------------------------------------------------------------------------
; System Memory
; ---------------------------------------------------------------------------
CTMON65VER    := $F006     ; CTMON Version
CTMON65REV    := $F007     ; CTMON Revision
IRQVEC        := $DF00     ; Irq jump vector
NMIVEC        := $DF02     ; NMI jump vector
AUTORUN       := $DF04     ; Set to FF at boot, after a load if set jump here
INPUTVECTOR   := $DF06     ; Get next byte from disk/console
OUTPUTVECTOR  := $DF08     ; PUT next byte to disk/console
MONITORBUFFER := $DF0A     ; Io buffer 132 bytes long

