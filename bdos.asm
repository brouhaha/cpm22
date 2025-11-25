; Reformatted and converted for cross-assembly by Macro Assembler AS
; Eric Smith <spacewar@gmail.com> 2018-01-24
; ------------------------------------------------------------
;   Clean BDOS Header for fixed-address CP/M system
; ------------------------------------------------------------
; This version removes Digital Research’s conditional ORG logic.
; BDOS is always located at an explicit address.
;
; Captain’s architecture:
;     BDOS @ 0xE000
;
; Override when building:
;     sjasmplus --define=BDOS_ORG=0xE000 bdos.asm
; ------------------------------------------------------------

patch1  equ     1

; BDOS Interface, BDOS, Version 2.2

; ------------------------------------------------------------
; CP/M fixed memory placement
; ------------------------------------------------------------

    ifndef  BDOS_ORG
BDOS_ORG    equ     0E000h ; default BDOS location
    endif

    ORG     BDOS_ORG

; BIOS entry point address will be defined later in this module
; (Digital Research placed BIOS just above BDOS; our BIOS is at C000h)


ssize	equ	24 ;24 level stack

;	low memory locations
reboot	equ	0000h ;reboot system
ioloc	equ	0003h ;i/o byte location
bdosa	equ	0006h ;address field of jmp BDOS

;	bios access constants
bootf	equ	bios+3*0 ;cold boot function
wbootf	equ	bios+3*1 ;warm boot function
constf	equ	bios+3*2 ;console status function
coninf	equ	bios+3*3 ;console input function
conoutf	equ	bios+3*4 ;console output function
listf	equ	bios+3*5 ;list output function
punchf	equ	bios+3*6 ;punch output function
readerf	equ	bios+3*7 ;reader input function
homef	equ	bios+3*8 ;disk home function
seldskf	equ	bios+3*9 ;select disk function
settrkf	equ	bios+3*10 ;EQU track function
setsecf	equ	bios+3*11 ;EQU sector function
setdmaf	equ	bios+3*12 ;EQU dma function
readf	equ	bios+3*13 ;read disk function
writef	equ	bios+3*14 ;write disk function
liststf	equ	bios+3*15 ;list status function
sectran	equ	bios+3*16 ;sector translate

;	equates for non graphic characters
ctlc	equ	03h ;control c
ctle	equ	05h ;physical eol
ctlh	equ	08h ;backspace
ctlp	equ	10h ;prnt toggle
ctlr	equ	12h ;repeat line
ctls	equ	13h ;stop/start screen
ctlu	equ	15h ;line delete
ctlx	equ	18h ;=ctl-u
ctlz	equ	1ah ;end of file
rubout	equ	7fh ;char delete
tab	equ	09h ;tab char
cr	equ	0dh ;carriage return
lf	equ	0ah ;line feed
ctl	equ	5eh ;up arrow

; serial number (not documented in original DRI source file)
	db	0	; OEM number, low byte
	db	0	; CP/M version, 16h = 2.2
	db	0	; OEM number, high byte
	db	0,0,0	; serial number, big-endian

;	enter here from the user's program with function number in c,
;	and information address in d,e
		jp bdose ;past parameter block

;	************************************************
;	*** relative locations 0009 - 000e           ***
;	************************************************
pererr:	dw	persub	;permanent error subroutine
selerr:	dw	selsub	;select error subroutine
roderr:	dw	rodsub	;ro disk error subroutine
roferr:	dw	rofsub	;ro file error subroutine


bdose:				;arrive here from user programs
		ex de,hl ;info=DE, DE=info
		ld (info),hl
		ex de,hl
		ld a,e ;linfo = low(info) - don't equ
		ld (linfo),a
		ld hl,0 ;return value defaults to 0000
		ld (aret),hl
	;save user's stack pointer, EQU to local stack
		add hl,sp ;entsp = stackptr
		ld (entsp),hl
		ld sp,lstack ;local stack setup
		xor a ;fcbdsk,resel=false
		ld (fcbdsk),a
		ld (resel),a
		ld hl,goback ;return here after all functions
	push	hl ;jmp goback equivalent to ret
		ld a,c ;skip if invalid #
		cp nfuncs
		ret nc
		ld c,e ;possible output character to C
		ld hl,functab ;DE=func, HL=.ciotab
		ld e,a
		ld d,0
		add hl,de ;DE=functab(func)
		add hl,de
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,(info) ;info in DE for later xchg	
		ex de,hl ;dispatched
		jp (hl)

;	dispatch table for functions
functab:
	dw	wbootf, func1, func2, func3
	dw	punchf, listf, func6, func7
	dw	func8, func9, func10,func11
diskf	equ	($-functab)/2 ;disk funcs
	dw	func12,func13,func14,func15
	dw	func16,func17,func18,func19
	dw	func20,func21,func22,func23
	dw	func24,func25,func26,func27
	dw	func28,func29,func30,func31
	dw	func32,func33,func34,func35
	dw	func36,func37,func38,func39
	dw	func40
nfuncs	equ	($-functab)/2


;	error subroutines
persub:	;report permanent error
		ld hl,permsg ;to report the error
	call	errflg
		cp ctlc ;reboot if response is ctlc
		jp z,reboot
	ret ;and ignore the error

selsub:	;report select error
		ld hl,selmsg ;wait console before boot
		jp wait_err
;
rodsub:	;report write to read/only disk
		ld hl,rodmsg ;wait console
		jp wait_err
;
rofsub:	;report read/only file
		ld hl,rofmsg ;drop through to wait for console
;
wait_err:
	;wait for response before boot
	call	errflg
		jp reboot

;	error messages
dskmsg:	db	"Bdos Err On "
dskerr:	db	" : $"	;filled in by errflg
permsg:	db	"Bad Sector$"
selmsg:	db	"Select$"
rofmsg:	db	"File "
rodmsg:	db	"R/O$"


errflg:
	;report error to console, message address in HL
	push	hl ;stack mssg address, new line
	call	crlf
		ld a,(curdsk) ;current disk name
		add a,'A'
		ld (dskerr),a
		ld bc,dskmsg ;the error message
	call	print
	pop	bc ;error mssage tail
	call	print
	;jmp	conin		;to get the input character			
	;(drop through to conin)
	;ret


;	console handlers
conin:
	;read console character to A
		ld hl,kbchar
		ld a,(hl)
		ld (hl),0
		or a
		ret nz
	;no previous keyboard character ready
		jp coninf ;get character externally
	;ret

conech:
	;read character with echo
	call	conin ;echo character?
	call	echoc
		ret c
        ;character must be echoed before return
		push	af
		ld c,a
	call	tabout
		pop	af
	ret ;with character in A

echoc:
	;echo character if graphic
	;cr, lf, tab, or backspace
		cp cr ;carriage return?
		ret z
		cp lf ;line feed?
		ret z
		cp tab ;tab?
		ret z
		cp ctlh ;backspace?
		ret z
		cp ' ' ;carry EQU if not graphic
	ret

conbrk:	;check for character ready
		ld a,(kbchar) ;skip if active kbchar
		or a
		jp nz,conb1
	;no active kbchar, check external break
	call	constf ;return if no char ready
		and 1
		ret z
	;character ready, read it
	call	coninf ;to A
		cp ctls ;check stop screen function
		jp nz,conb0
	;found ctls, read next character
	call	coninf ;to A
		cp ctlc ;ctlc implies re-boot
		jp z,reboot
	;not a reboot, act as if nothing has happened
		xor a ;with zero in accumulator
	ret
conb0:	;character in accum, save it
		ld (kbchar),a
conb1:	;return with true EQU in accumulator
		ld a,1
	ret

conout:
	;compute character position/write console char from C
	;compcol = true if computing column position
		ld a,(compcol)
		or a
		jp nz,compout
	;write the character, then compute the column
	;write console character from C
	push	bc ;check for screen stop function
	call	conbrk
	pop	bc ;recall/save character
	push	bc
	call	conoutf ;externally, to console
	pop	bc ;recall/save character
	push	bc
	;may be copying to the list device
		ld a,(listcp) ;to printer, if so
		or a
		call nz,listf
	pop	bc ;recall the character
compout:
		ld a,c ;recall the character
	;and compute column position
		ld hl,column ;A = char, HL = .column
		cp rubout ;no column change if nulls
		ret z
		inc (hl) ;column = column + 1
		cp ' ' ;return if graphic
		ret nc
	;not graphic, reEQU column position
		dec (hl) ;column = column - 1
		ld a,(hl) ;return if at zero
		or a
		ret z
	;not at zero, may be backspace or end line
		ld a,c ;character back to A
		cp ctlh
		jp nz,notbacksp
	;backspace character
		dec (hl) ;column = column - 1
	ret
notbacksp:
	;not a backspace character, eol?
		cp lf ;return if not
		ret nz
	;end of line, column = 0
		ld (hl),0 ;column = 0
	ret

ctlout:
	;send C character with possible preceding up-arrow
		ld a,c ;cy if not graphic (or special case)
	call	echoc
		jp nc,tabout ;skip if graphic, tab, cr, lf, or ctlh
	;send preceding up arrow
		push	af ;up arrow
		ld c,ctl
	call	conout
		pop	af ;becomes graphic letter
		or 40h
		ld c,a ;ready to print
	;(drop through to tabout)

tabout:
	;expand tabs to console
		ld a,c ;direct to conout if not
		cp tab
		jp nz,conout
	;tab encountered, move to next tab position
tab0:
		ld c,' ' ;another blank
	call	conout
		ld a,(column) ;column mod 8 = 0 ?
		and 111b
		jp nz,tab0 ;back for another if not
	ret


backup:
	;back-up one screen position
	call	pctlh
		ld c,' '
	call	conoutf
;	(drop through to pctlh)
pctlh:
	;send ctlh to console without affecting column count
		ld c,ctlh
		jp conoutf
	;ret

crlfp:
	;print #, cr, lf for ctlx, ctlu, ctlr functions
	;then move to strtcol (starting column)
		ld c,'#'
	call	conout
	call	crlf
	;column = 0, move to position strtcol
crlfp0:
		ld a,(column)
		ld hl,strtcol
		cp (hl) ;stop when column reaches strtcol
		ret nc
		ld c,' ' ;print blank
	call	conout
		jp crlfp0


crlf:
	;carriage return line feed sequence
		ld c,cr
	call	conout
		ld c,lf
		jp conout
	;ret

print:
	;print message until M(BC) = '$'
	ld	a,(bc) ;stop on $
		cp '$'
		ret z
	;more to print
		inc bc ;char to C
	push	bc
		ld c,a
	call	tabout ;another character printed
	pop	bc
		jp print

read:	;read to info address (max length, current length, buffer)
		ld a,(column) ;save start for ctl-x, ctl-h
		ld (strtcol),a
		ld hl,(info)
		ld c,(hl)
		inc hl
	push	hl
		ld b,0
	;B = current buffer length,
	;C = maximum buffer length,
	;HL= next to fill - 1
readnx:
	;read next character, BC, HL active
	push	bc ;blen, cmax, HL saved
	push	hl
readn0:
	call	conin ;next char in A
		and 7fh ;mask parity bit
	pop	hl ;reactivate counters
	pop	bc
		cp cr ;end of line?
		jp z,readen
		cp lf ;also end of line
		jp z,readen
		cp ctlh ;backspace?
		jp nz,noth
	;do we have any characters to back over?
		ld a,b
		or a
		jp z,readnx
	;characters remain in buffer, backup one
		dec b ;remove one character
		ld a,(column) ;col > 0
		ld (compcol),a
	;compcol > 0 marks repeat as length compute
		jp linelen ;uses same code as repeat
noth:
	;not a backspace
		cp rubout ;rubout char?
		jp nz,notrub
	;rubout encountered, rubout if possible
		ld a,b ;skip if len=0
		or a
		jp z,readnx
	;buffer has characters, resend last char
		ld a,(hl) ;A = last char
		dec b
		dec hl
	;blen=blen-1, next to fill - 1 decremented
		jp rdech1 ;act like this is an echo

notrub:
	;not a rubout character, check end line
		cp ctle ;physical end line?
		jp nz,note
	;yes, save active counters and force eol
	push	bc
	push	hl
	call	crlf
		xor a ;start position = 00
		ld (strtcol),a
		jp readn0 ;for another character
note:
	;not end of line, list toggle?
		cp ctlp ;skip if not ctlp
		jp nz,notp
	;list toggle - change parity
	push	hl ;save next to fill - 1
		ld hl,listcp ;HL=.listcp flag
		ld a,1 ;True-listcp
	sub	(hl)
		ld (hl),a ;listcp = not listcp
	pop	hl ;for another char
		jp readnx
notp:
	;not a ctlp, line delete?
		cp ctlx
		jp nz,notx
	pop	hl ;discard start position
	;loop while column > strtcol
backx:
		ld a,(strtcol)
		ld hl,column
		cp (hl) ;start again
		jp nc,read
		dec (hl) ;column = column - 1
	call	backup ;one position
		jp backx
notx:
	;not a control x, control u?
	;not control-X, control-U?
		cp ctlu ;skip if not
		jp nz,notu
	;delete line (ctlu)
	call	crlfp ;physical eol
	pop	hl ;discard starting position
		jp read ;to start all over
notu:
	;not line delete, repeat line?
		cp ctlr
		jp nz,notr
linelen:
	;repeat line, or compute line len (ctlh)
	;if compcol > 0
	push	bc ;save line length
	call	crlfp
	pop	bc
	pop	hl
	push	hl
	push	bc
	;bcur, cmax active, beginning buff at HL
rep0:
		ld a,b ;count len to 00
		or a
		jp z,rep1
		inc hl ;next to print
		ld c,(hl)
		dec b ;count length down
	push	bc
	push	hl
	call	ctlout ;character echoed
	pop	hl ;recall remaining count
	pop	bc
		jp rep0 ;for the next character
rep1:
	;end of repeat, recall lengths
	;original BC still remains pushed
	push	hl ;save next to fill
		ld a,(compcol) ;>0 if computing length
		or a
		jp z,readn0 ;for another char if so
	;column position computed for ctlh
		ld hl,column ;diff > 0
	sub	(hl)
		ld (compcol),a ;count down below
	;move back compcol-column spaces
backsp:
	;move back one more space
	call	backup ;one space
		ld hl,compcol
		dec (hl)
		jp nz,backsp
		jp readn0 ;for next character
notr:
	;not a ctlr, place into buffer
rdecho:
		inc hl ;character filled to mem
		ld (hl),a
		inc b ;blen = blen + 1
rdech1:
	;look for a random control character
	push	bc ;active values saved
	push	hl
		ld c,a ;ready to print
	call	ctlout ;may be up-arrow C
	pop	hl ;recall char
	pop	bc
		ld a,(hl)
		cp ctlc ;EQU flags for reboot test
		ld a,b ;move length to A
		jp nz,notc ;skip if not a control c
		cp 1 ;control C, must be length 1
		jp z,reboot ;reboot if blen = 1
	;length not one, so skip reboot
notc:
	;not reboot, are we at end of buffer?
		cp c ;go for another if not
		jp c,readnx
readen:
	;end of read operation, store blen
	pop	hl ;M(current len) = B
		ld (hl),b
		ld c,cr ;return carriage
		jp conout
	;ret
func1:
	;return console character with echo
	call	conech
		jp sta_ret
;
func2	equ	tabout
	;write console character with tab expansion
;
func3:
	;return reader character
	call	readerf
		jp sta_ret

;func4:	equated to punchf
	;write punch character

;func5:	equated to listf
	;write list character
	;write to list device

func6:
	;direct console i/o - read if 0ffh
		ld a,c ;0ffh => 00h, means input mode
		inc a
		jp z,dirinp
		inc a ;0feH in C for status
		jp z,constf
	;direct output function
		jp conoutf
dirinp:
	call	constf ;status check
		or a ;skip, return 00 if not ready
		jp z,retmon
	;character is ready, get it
	call	coninf ;to A
		jp sta_ret

func7:
	;return io byte
		ld a,(ioloc)
		jp sta_ret

func8:
	;EQU i/o byte
		ld hl,ioloc
		ld (hl),c
	ret ;jmp goback

func9:
	;write line until $ encountered
		ex de,hl ;was lhld info	
		ld c,l ;BC=string address
		ld b,h
		jp print ;out to console	

func10	equ	read
	;read a buffered console line

func11:
	;check console status
	call	conbrk
	;(drop through to sta_ret)
sta_ret:
	;store the A register to aret
		ld (aret),a
func_ret:						;
	ret ;jmp goback (pop stack for non cp/m functions)

setlret1:
	;EQU lret = 1
		ld a,1
		jp sta_ret



;	data areas

compcol:db	0	;true if computing column position
strtcol:db	0	;starting column position after read
column:	db	0	;column position
listcp:	db	0	;listing toggle
kbchar:	db	0	;initial key char = 00
entsp:	ds	2	;entry stack pointer
	ds	ssize*2	;stack size
lstack:
;	end of Basic I/O System

;*****************************************************************
;*****************************************************************

;	common values shared between bdosi and bdos
usrcode:db	0	;current user number
curdsk:	db	0	;current disk number
info:	ds	2	;information address
aret:	ds	2	;address value to return
lret	equ	aret ;low(aret)

;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**                                                             **
;*****************************************************************
;*****************************************************************

dvers	equ	22h ;version 2.2
;	module addresses

;	literal constants
_true	equ	0ffh	;constant true
_false	equ	000h	;constant false
enddir	equ	0ffffh ;end of directory
byte	equ	1 ;number of bytes for "byte" type
word	equ	2 ;number of bytes for "word" type

;	fixed addresses in low memory
tfcb	equ	005ch ;default fcb location
tbuff	equ	0080h ;default buffer location

;	fixed addresses referenced in bios module are
;	pererr (0009), selerr (000c), roderr (000f)

;	error message handlers

;per_error:	 
	;report permanent error to user	
;	lxi h,pererr  jmp goerr		

;rod_error:
	;report read/only disk error
;	lxi h,roderr  jmp goerr

;rof_error:
	;report read/only file error
;	lxi h,roferr	;jmp goerr	

sel_error:
	;report select error
		ld hl,selerr


goerr:
	;HL = .errorhandler, call subroutine
		ld e,(hl) ;address of routine in DE
		inc hl
		ld d,(hl)
		ex de,hl ;to subroutine
		jp (hl)



;	local subroutines for bios interface

move:
	;move data length of length C from source DE to
	;destination given by HL
		inc c ;in case it is zero
move0:
		dec c ;more to move
		ret z
	ld	a,(de) ;one byte moved
		ld (hl),a
		inc de ;to next byte
		inc hl
		jp move0

selectdisk:
	;select the disk drive given by curdsk, and fill
	;the base addresses curtrka - alloca, then fill
	;the values of the disk parameter block
		ld a,(curdsk) ;current disk# to c
		ld c,a
	;lsb of e = 0 if not yet logged - in
	call	seldskf ;HL filled by call
	;HL = 0000 if error, otherwise disk headers
		ld a,h ;return with 0000 in HL and z flag
		or l
		ret z
	;disk header block address in hl
		ld e,(hl) ;DE=.tran
		inc hl
		ld d,(hl)
		inc hl
		ld (cdrmaxa),hl ;.cdrmax
		inc hl
		inc hl
		ld (curtrka),hl ;HL=.currec
		inc hl
		inc hl
		ld (curreca),hl ;HL=.buffa
		inc hl
		inc hl
	;DE still contains .tran
		ex de,hl ;.tran vector
		ld (tranv),hl
		ld hl,buffa ;DE= source for move, HL=dest
		ld c,addlist ;addlist filled
	call	move
	;now fill the disk parameter block
		ld hl,(dpbaddr) ;DE is source
		ex de,hl
		ld hl,sectpt ;HL is destination
		ld c,dpblist ;data filled
	call	move
	;now EQU single/double map mode
		ld hl,(maxall) ;largest allocation number
		ld a,h ;00 indicates < 255
		ld hl,single ;assume a=00
		ld (hl),_true
		or a
		jp z,retselect
	;high order of maxall not zero, use double dm
		ld (hl),_false
retselect:
		ld a,_true ;select disk function ok
		or a
	ret

home:
	;move to home position, then offEQU to start of dir
	call	homef ;move to track 00, sector 00 reference
	;lxi h,offEQU ;mov c,m ;inx h ;mov b,m ;call settrkf
	;first directory position selected
		xor a ;constant zero to accumulator
		ld hl,(curtrka) ;curtrk=0000
		ld (hl),a
		inc hl
		ld (hl),a
		ld hl,(curreca) ;currec=0000
		ld (hl),a
		inc hl
		ld (hl),a
	;curtrk, currec both EQU to 0000
	ret

rdbuff:
	;read buffer and check condition
	call	readf ;current drive, track, sector, dma
		jp diocomp ;check for i/o errors

wrbuff:
	;write buffer and check condition
	;write type (wrtype) is in register C
	;wrtype = 0 => normal write operation
	;wrtype = 1 => directory write operation
	;wrtype = 2 => start of new block
	call	writef ;current drive, track, sector, dma
diocomp:	;check for disk errors
		or a
		ret z
		ld hl,pererr
		jp goerr

seek_dir:
	;seek the record containing the current dir entry
		ld hl,(dcnt) ;directory counter to HL
		ld c,dskshf ;value to HL
	call	hlrotr
		ld (arecord),hl ;ready for seek
		ld (drec),hl
	;  jmp seek
	;ret


seek:
	;seek the track given by arecord (actual record)
	;local equates for registers
;arech	equ	b		;arecord = BC
;arecl	equ	c
;crech	equ	d		;currec  = DE
;crecl	equ	e
;ctrkh	equ	h		;curtrk  = HL
;ctrkl	equ	l
;tcrech	equ	h		;tcurrec = HL
;tcrecl	equ	l
	;load the registers from memory
		ld hl,arecord
		ld c,(hl) ; % c = arecl
		inc hl
		ld b,(hl) ; % b = arech
		ld hl,(curreca)
		ld e,(hl) ; % e = crecl
		inc hl
		ld d,(hl) ; % d = crech
		ld hl,(curtrka)
		ld a,(hl)
		inc hl
		ld h,(hl) ; % h = ctrkh
		ld l,a ; % l = ctrkl
	;loop while arecord < currec
seek0:
		ld a,c ; % c = arecl
	sub	e ; % e = crecl
		ld a,b ; % b = arech
		sbc a,d ; % d = crech
		jp nc,seek1 ;skip if arecord >= currec
	;currec = currec - sectpt
	push	hl ; % h = ctrkh
		ld hl,(sectpt)
		ld a,e ; % e = crecl
	sub	l
		ld e,a ; % e = crecl
		ld a,d ; % d = crech
		sbc a,h
		ld d,a ; % d = crech
	pop	hl ; % h = ctrkh
	;curtrk = curtrk - 1
		dec hl ; % h = ctrkh
		jp seek0 ;for another try
seek1:
	;look while arecord >= (t:=currec + sectpt)
	push	hl ; % h = ctrkh
		ld hl,(sectpt) ;HL = currec+sectpt
		add hl,de ; % d = crech
		jp c,seek2 ;can be > FFFFH	
		ld a,c ; % c = arecl
	sub	l ; % l = tcrecl
		ld a,b ; % b = arech
		sbc a,h ; % h = tcrech
		jp c,seek2 ;skip if t > arecord
	;currec = t
		ex de,hl
	;curtrk = curtrk + 1
	pop	hl ; % h = ctrkh
		inc hl ; % h = ctrkh
		jp seek1 ;for another try
seek2:	pop	hl ; % h = ctrkh
	;arrive here with updated values in each register
	push	bc ;to stack for later ; % b = arech
	push	de ; % d = crech
	push	hl ; % h = ctrkh
	;stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
		ex de,hl ;HL = curtrk+offset
		ld hl,(offset)
		add hl,de
		ld b,h ;track EQU up
		ld c,l
	call	settrkf
	;note that BC - curtrk is difference to move in bios
	pop	de ;recall curtrk
		ld hl,(curtrka) ;curtrk updated
		ld (hl),e
		inc hl
		ld (hl),d
	;now compute sector as arecord-currec
	pop	de ;recall currec ; % d = crech
		ld hl,(curreca)
		ld (hl),e ; % e = crecl
		inc hl
		ld (hl),d ; % d = crech
	pop	bc ;BC=arecord, DE=currec ; % b = arech
		ld a,c ; % c = arecl
	sub	e ; % e = crecl
		ld c,a ; % c = arecl
		ld a,b ; % b = arech
		sbc a,d ; % d = crech
		ld b,a ; % b = arech
		ld hl,(tranv) ;BC=sector#, DE=.tran
		ex de,hl
	call	sectran ;HL = tran(sector)
		ld c,l ;BC = tran(sector)
		ld b,h
		jp setsecf ;sector selected
	;ret

;	file control block (fcb) constants
empty	equ	0e5h ;empty directory entry
lstrec	equ	127 ;last record# in extent
recsiz	equ	128 ;record size
fcblen	equ	32 ;file control block size
dirrec	equ	recsiz/fcblen ;directory elts / record
dskshf	equ	2 ;log2(dirrec)
dskmsk	equ	dirrec-1
fcbshf	equ	5 ;log2(fcblen)

extnum	equ	12 ;extent number field
maxext	equ	31 ;largest extent number
ubytes	equ	13 ;unfilled bytes field
modnum	equ	14 ;data module number
maxmod	equ	15 ;largest module number
fwfmsk	equ	80h ;file write flag is high order modnum
namlen	equ	15 ;name length
reccnt	equ	15 ;record count field
dskmap	equ	16 ;disk map field
lstfcb	equ	fcblen-1
nxtrec	equ	fcblen
ranrec	equ	nxtrec+1 ;random record field (2 bytes)

;	reserved file indicators
rofile	equ	9 ;high order of first type char
invis	equ	10 ;invisible file in dir command
;	equ	11	;reserved

;	utility functions for file access

dm_position:
	;compute disk map position for vrecord to HL
		ld hl,blkshf ;shift count to C
		ld c,(hl)
		ld a,(vrecord) ;current virtual record to A
dmpos0:
		or a
		rr a
		dec c
		jp nz,dmpos0
	;A = shr(vrecord,blkshf) = vrecord/2**(sect/block)
		ld b,a ;save it for later addition
		ld a,8 ;8-blkshf to accumulator
	sub	(hl)
		ld c,a ;extent shift count in register c
		ld a,(extval) ;extent value ani extmsk
dmpos1:
	;blkshf = 3,4,5,6,7, C=5,4,3,2,1
	;shift is 4,3,2,1,0
		dec c
		jp z,dmpos2
		or a
		rl a
		jp dmpos1
dmpos2:
	;arrive here with A = shl(ext and extmsk,7-blkshf)
		add a,b ;add the previous shr(vrecord,blkshf) value
	;A is one of the following values, depending upon alloc
	;bks blkshf
	;1k   3     v/8 + extval * 16
	;2k   4     v/16+ extval * 8
	;4k   5     v/32+ extval * 4
	;8k   6     v/64+ extval * 2
	;16k  7     v/128+extval * 1
	ret ;with dm_position in A

getdm:
	;return disk map value from position given by BC
		ld hl,(info) ;base address of file control block
		ld de,dskmap ;HL =.diskmap
		add hl,de
		add hl,bc ;index by a single byte value
		ld a,(single) ;single byte/map entry?
		or a ;get disk map single byte
		jp z,getdmd
		ld l,(hl) ;with HL=00bb
		ld h,0
	ret
getdmd:
		add hl,bc ;HL=.fcb(dm+i*2)
	;double precision value returned
		ld e,(hl)
		inc hl
		ld d,(hl)
		ex de,hl
	ret

index:
	;compute disk block number from current fcb
	call	dm_position ;0...15 in register A
		ld c,a ;value to HL
		ld b,0
	call	getdm
		ld (arecord),hl
	ret

allocated:
	;called following index to see if block allocated
		ld hl,(arecord)
		ld a,l
		or h
	ret

atran:
	;compute actual record address, assuming index called
		ld a,(blkshf) ;shift count to reg A
		ld hl,(arecord)
atran0:
		add hl,hl ;shl(arecord,blkshf)
		dec a
		jp nz,atran0
		ld (arecord1),hl ;save shifted block #  
		ld a,(blkmsk) ;mask value to C
		ld c,a
		ld a,(vrecord) ;masked value in A
		and c
		or l ;to HL
		ld l,a
		ld (arecord),hl ;arecord=HL or (vrecord and blkmsk)
	ret

getexta:
	;get current extent field address to A
		ld hl,(info) ;HL=.fcb(extnum)
		ld de,extnum
		add hl,de
	ret

getfcba:
	;compute reccnt and nxtrec addresses for get/setfcb
		ld hl,(info) ;DE=.fcb(reccnt)
		ld de,reccnt
		add hl,de
		ex de,hl
		ld hl,(nxtrec-reccnt) ;HL=.fcb(nxtrec)
		add hl,de
	ret

getfcb:
	;EQU variables from currently addressed fcb
	call	getfcba ;addresses in DE, HL
		ld a,(hl) ;vrecord=fcb(nxtrec)
		ld (vrecord),a
		ex de,hl ;rcount=fcb(reccnt)
		ld a,(hl)
		ld (rcount),a
	call	getexta ;HL=.fcb(extnum)
		ld a,(extmsk) ;extent mask to a
		and (hl) ;fcb(extnum) and extmsk
		ld (extval),a
	ret

setfcb:
	;place values back into current fcb
	call	getfcba ;addresses to DE, HL
		ld a,(seqio)
		cp 02 ;check ranfill
		jp nz,setfcb1
		xor a
setfcb1:
 		ld c,a ;=1 if sequential i/o
		ld a,(vrecord) ;fcb(nxtrec)=vrecord+seqio
		add a,c
		ld (hl),a
		ex de,hl ;fcb(reccnt)=rcount
		ld a,(rcount)
		ld (hl),a
	ret

hlrotr:
	;hl rotate right by amount C
		inc c ;in case zero
hlrotr0:
		dec c ;return when zero
		ret z
		ld a,h ;high byte
		or a
		rr a
		ld h,a
		ld a,l ;low byte
		rr a
		ld l,a
		jp hlrotr0


compute_cs:
	;compute checksum for current directory buffer
		ld c,recsiz ;size of directory buffer
		ld hl,(buffa) ;current directory buffer
		xor a ;clear checksum value
computecs0:
		add a,(hl) ;cs=cs+buff(recsiz-C)
		inc hl
		dec c
		jp nz,computecs0
	ret ;with checksum in A

hlrotl:
	;rotate the mask in HL by amount in C
		inc c ;may be zero
hlrotl0:
		dec c ;return if zero
		ret z
		add hl,hl
		jp hlrotl0

set_cdisk:
	;EQU a "1" value in curdsk position of BC
	push	bc ;save input parameter
		ld a,(curdsk) ;ready parameter for shift
		ld c,a
		ld hl,1 ;number to shift
	call	hlrotl ;HL = mask to integrate
	pop	bc ;original mask
		ld a,c
		or l
		ld l,a
		ld a,b ;HL = mask or rol(1,curdsk)
		or h
		ld h,a
	ret

nowrite:
	;return true if dir checksum difference occurred
		ld hl,(rodsk)
		ld a,(curdsk)
		ld c,a
	call	hlrotr
		ld a,l ;non zero if nowrite
		and 1b
	ret

set_ro:
	;EQU current disk to read only
		ld hl,rodsk
		ld c,(hl)
		inc hl
		ld b,(hl)
	call	set_cdisk ;sets bit to 1
		ld (rodsk),hl
	;high water mark in directory goes to max
		ld hl,(dirmax) ;DE = directory max
		inc hl
		ex de,hl
		ld hl,(cdrmaxa) ;HL = .cdrmax
		ld (hl),e ;cdrmax = dirmax
		inc hl
		ld (hl),d
	ret

check_rodir:
	;check current directory element for read/only status
	call	getdptra ;address of element
						
check_rofile:
	;check current buff(dptr) or fcb(0) for r/o status
		ld de,rofile ;offEQU to ro bit
		add hl,de
		ld a,(hl) ;return if not set
		rl a
		ret nc
		ld hl,roferr
		jp goerr
;	jmp rof_error ;exit to read only disk message


check_write:
	;check for write protected disk
	call	nowrite ;ok to write if not rodsk
		ret z
		ld hl,roderr
		jp goerr
;	jmp rod_error ;read only disk error

getdptra:
	;compute the address of a directory element at
	;positon dptr in the buffer
		ld hl,(buffa)
		ld a,(dptr)
addh:
	;HL = HL + A
		add a,l
		ld l,a
		ret nc
	;overflow to H
		inc h
	ret


getmodnum:
	;compute the address of the module number 
	;bring module number to accumulator
	;(high order bit is fwf (file write flag)
		ld hl,(info) ;HL=.fcb(modnum)
		ld de,modnum
		add hl,de
		ld a,(hl) ;A=fcb(modnum)
	ret

clrmodnum:
	;clear the module number field for user open/make
	call	getmodnum ;fcb(modnum)=0
		ld (hl),0
	ret

setfwf:
	call	getmodnum ;HL=.fcb(modnum), A=fcb(modnum)
	;EQU fwf (file write flag) to "1"
		or fwfmsk ;fcb(modnum)=fcb(modnum) or 80h
		ld (hl),a
	;also returns non zero in accumulator
	ret


compcdr:
	;return cy if cdrmax > dcnt
		ld hl,(dcnt) ;DE = directory counter
		ex de,hl
		ld hl,(cdrmaxa) ;HL=.cdrmax
		ld a,e ;low(dcnt) - low(cdrmax)
	sub	(hl)
		inc hl ;HL = .cdrmax+1
		ld a,d ;hig(dcnt) - hig(cdrmax)
		sbc a,(hl)
	;condition dcnt - cdrmax  produces cy if cdrmax>dcnt
	ret

setcdr:
	;if not (cdrmax > dcnt) then cdrmax = dcnt+1
	call	compcdr
		ret c ;return if cdrmax > dcnt
	;otherwise, HL = .cdrmax+1, DE = dcnt
		inc de
		ld (hl),d
		dec hl
		ld (hl),e
	ret

subdh:
	;compute HL = DE - HL
		ld a,e
	sub	l
		ld l,a
		ld a,d
		sbc a,h
		ld h,a
	ret

newchecksum:
		ld c,_true ;drop through to compute new checksum
checksum:
	;compute current checksum record and update the
	;directory element if C=true, or check for = if not
	;drec < chksiz?
		ld hl,(drec) ;DE-HL
		ex de,hl
		ld hl,(chksiz)
	call	subdh
		ret nc ;skip checksum if past checksum vector size
	;drec < chksiz, so continue
	push	bc ;save init flag
	call	compute_cs ;check sum value to A
		ld hl,(checka) ;address of check sum vector
		ex de,hl
		ld hl,(drec) ;value of drec
		add hl,de ;HL = .check(drec)
	pop	bc ;recall true=0ffh or false=00 to C
		inc c ;0ffh produces zero flag
		jp z,initial_cs
	;not initializing, compare
		cp (hl) ;compute_cs=check(drec)?
		ret z ;no message if ok
	;checksum error, are we beyond
	;the end of the disk?
	call	compcdr
		ret nc ;no message if so
	call	set_ro ;read/only disk set
	ret
initial_cs:
	;initializing the checksum
		ld (hl),a
	ret


wrdir:
	;write the current directory entry, EQU checksum
	call	newchecksum ;initialize entry
	call	setdir ;directory dma
		ld c,1 ;indicates a write directory operation
	call	wrbuff ;write the buffer
        	jp setdata ;to data dma address
	;ret

rd_dir:
	;read a directory entry into the directory buffer
	call	setdir ;directory dma
	call	rdbuff ;directory record loaded
        ; jmp setdata to data dma address    
	;ret

setdata:
	;EQU data dma address
		ld hl,dmaad ;to complete the call
		jp setdma

setdir:
	;EQU directory dma address
		ld hl,buffa ;jmp setdma to complete call     

setdma:
	;HL=.dma address to EQU (i.e., buffa or dmaad)
		ld c,(hl) ;parameter ready
		inc hl
		ld b,(hl)
		jp setdmaf


dir_to_user:
	;copy the directory entry to the user buffer
	;after call to search or searchn by user code
		ld hl,(buffa) ;source is directory buffer
		ex de,hl
		ld hl,(dmaad) ;destination is user dma address
		ld c,recsiz ;copy entire record
		jp move
	;ret

end_of_dir:
	;return zero flag if at end of directory, non zero
	;if not at end (end of dir if dcnt = 0ffffh)
		ld hl,dcnt ;may be 0ffh
		ld a,(hl)
		inc hl ;low(dcnt) = high(dcnt)?
		cp (hl)
		ret nz ;non zero returned if different
	;high and low the same, = 0ffh?
		inc a ;0ffh becomes 00 if so
	ret

set_end_dir:
	;EQU dcnt to the end of the directory
		ld hl,enddir
		ld (dcnt),hl
	ret

read_dir:
	;read next directory entry, with C=true if initializing
		ld hl,(dirmax) ;in preparation for subtract
		ex de,hl
		ld hl,(dcnt) ;dcnt=dcnt+1
		inc hl
		ld (dcnt),hl
	;continue while dirmax >= dcnt (dirmax-dcnt no cy)
	call	subdh ;DE-HL
		jp nc,read_dir0
	;yes, EQU dcnt to end of directory
		jp set_end_dir
;		ret
read_dir0:
	;not at end of directory, seek next element
	;initialization flag is in C
		ld a,(dcnt) ;low(dcnt) and dskmsk
		and dskmsk
		ld b,fcbshf ;to multiply by fcb size
read_dir1:
		add a,a
		dec b
		jp nz,read_dir1
	;A = (low(dcnt) and dskmsk) shl fcbshf
		ld (dptr),a ;ready for next dir operation
		or a ;return if not a new record
		ret nz
	push	bc ;save initialization flag C
	call	seek_dir ;seek proper record
	call	rd_dir ;read the directory record
	pop	bc ;recall initialization flag
		jp checksum ;checksum the directory elt
	;ret


getallocbit:
	;given allocation vector position BC, return with byte
	;containing BC shifted so that the least significant
	;bit is in the low order accumulator position.  HL is
	;the address of the byte for possible replacement in
	;memory upon return, and D contains the number of shifts
	;required to place the returned value back into position
		ld a,c
		and 111b
		inc a
		ld e,a
		ld d,a
	;d and e both contain the number of bit positions to shift
		ld a,c ;C shr 3 to C
		rrca
		rrca
		rrca
		and 11111b
		ld c,a
		ld a,b ;B shl 5
		add a,a
		add a,a
		add a,a
		add a,a
		add a,a
		or c ;bbbccccc to C
		ld c,a
		ld a,b ;BC shr 3 to BC
		rrca
		rrca
		rrca
		and 11111b
		ld b,a
		ld hl,(alloca) ;base address of allocation vector
		add hl,bc ;byte to A, hl = .alloc(BC shr 3)
		ld a,(hl)
	;now move the bit to the low order position of A
rotl:	rlca
		dec e
		jp nz,rotl
	ret


set_alloc_bit:
	;BC is the bit position of ALLOC to EQU or reset.  The
	;value of the bit is in register E.
	push	de ;shifted val A, count in D
	call	getallocbit
		and 11111110b ;mask low bit to zero (may be set)
	pop	bc ;low bit of C is masked into A
		or c
;	jmp rotr ;to rotate back into proper position	
	;ret
rotr:
	;byte value from ALLOC is in register A, with shift count
	;in register C (to place bit back into position), and
	;target ALLOC position in registers HL, rotate and replace
		rrca ;back into position
		dec d
		jp nz,rotr
		ld (hl),a ;back to ALLOC
	ret

scandm:
	;scan the disk map addressed by dptr for non-zero
	;entries, the allocation vector entry corresponding
	;to a non-zero entry is EQU to the value of C (0,1)
	call	getdptra ;HL = buffa + dptr
	;HL addresses the beginning of the directory entry
		ld de,dskmap ;hl now addresses the disk map
		add hl,de
	push	bc ;save the 0/1 bit to set
		ld c,fcblen-dskmap+1 ;size of single byte disk map + 1
scandm0:
	;loop once for each disk map entry
	pop	de ;recall bit parity
		dec c ;all done scanning?
		ret z
	;no, get next entry for scan
	push	de ;replace bit parity
		ld a,(single)
		or a
		jp z,scandm1
	;single byte scan operation
	push	bc ;save counter
	push	hl ;save map address
		ld c,(hl) ;BC=block#
		ld b,0
		jp scandm2
scandm1:
	;double byte scan operation
		dec c ;count for double byte
	push	bc ;save counter
		ld c,(hl) ;BC=block#
		inc hl
		ld b,(hl)
	push	hl ;save map address
scandm2:
	;arrive here with BC=block#, E=0/1
		ld a,c ;skip if = 0000
		or b
		jp z,scanm3
		ld hl,(maxall) ;check invalid index
		ld a,l ;maxall - block#
	sub	c
		ld a,h
		sbc a,b
		call nc,set_alloc_bit
	;bit EQU to 0/1
scanm3:
	pop	hl ;to next bit position
		inc hl
	pop	bc ;recall counter
		jp scandm0 ;for another item

initialize:
	;initialize the current disk
	;lret = false ;EQU to true if $ file exists
	;compute the length of the allocation vector - 2
		ld hl,(maxall) ;perform maxall/8
		ld c,3
	;number of bytes in alloc vector is (maxall/8)+1
	call	hlrotr ;HL = maxall/8+1
		inc hl
		ld b,h ;count down BC til zero
		ld c,l
		ld hl,(alloca) ;base of allocation vector
	;fill the allocation vector with zeros
initial0:
		ld (hl),0 ;alloc(i)=0
		inc hl
		dec bc ;count length down
		ld a,b
		or c
		jp nz,initial0
	;EQU the reserved space for the directory
		ld hl,(dirblk)
		ex de,hl
		ld hl,(alloca) ;HL=.alloc()
		ld (hl),e ;sets reserved directory blks
		inc hl
		ld (hl),d
	;allocation vector initialized, home disk
	call	home
        ;cdrmax = 3 (scans at least one directory record)
		ld hl,(cdrmaxa)
		ld (hl),3
		inc hl
		ld (hl),0
	;cdrmax = 0000
	call	set_end_dir ;dcnt = enddir
	;read directory entries and check for allocated storage
initial2:
		ld c,_true
	call	read_dir
	call	end_of_dir ;return if end of directory
		ret z
	;not end of directory, valid entry?
	call	getdptra ;HL = buffa + dptr
		ld a,empty
		cp (hl)
		jp z,initial2 ;go get another item
	;not empty, user code the same?
		ld a,(usrcode)
		cp (hl)
		jp nz,pdollar
	;same user code, check for '$' submit
		inc hl ;first character
		ld a,(hl)
		sub '$' ;dollar file?
		jp nz,pdollar
	;dollar file found, mark in lret
		dec a ;lret = 255
		ld (lret),a
pdollar:
	;now scan the disk map for allocated blocks
		ld c,1 ;EQU to allocated
	call	scandm
	call	setcdr ;EQU cdrmax to dcnt
		jp initial2 ;for another entry

copy_dirloc:
	;copy directory location to lret following
	;delete, rename, ... ops
		ld a,(dirloc)
		jp sta_ret
;	ret

compext:
	;compare extent# in A with that in C, return nonzero
	;if they do not match
	push	bc ;save C's original value
		push	af
		ld a,(extmsk)
		cpl
		ld b,a
	;B has negated form of extent mask
		ld a,c ;low bits removed from C
		and b
		ld c,a
		pop	af ;low bits removed from A
		and b
	sub	c ;EQU flags
		and maxext
	pop	bc ;restore original values
	ret

search:
	;search for directory element of length C at info
		ld a,0ffh ;changed if actually found
		ld (dirloc),a
		ld hl,searchl ;searchl = C
		ld (hl),c
		ld hl,(info) ;searcha = info
		ld (searcha),hl
	call	set_end_dir ;dcnt = enddir
	call	home ;to start at the beginning
	;(drop through to searchn)

searchn:
	;search for the next directory element, assuming
	;a previous call on search which sets searcha and
	;searchl
		ld c,_false ;read next dir element
	call	read_dir
	call	end_of_dir ;skip to end if so
		jp z,search_fin
	;not end of directory, scan for match
		ld hl,(searcha) ;DE=beginning of user fcb
		ex de,hl
	ld	a,(de) ;first character
		cp empty ;keep scanning if empty
		jp z,searchnext
	;not empty, may be end of logical directory
	push	de ;save search address
	call	compcdr ;past logical end?
	pop	de ;recall address
		jp nc,search_fin ;artificial stop
searchnext:
	call	getdptra ;HL = buffa+dptr
		ld a,(searchl) ;length of search to c
		ld c,a
		ld b,0 ;b counts up, c counts down
searchloop:
		ld a,c
		or a
		jp z,endsearch
	ld	a,(de) ;? matches all
		cp '?'
		jp z,searchok
	;scan next character if not ubytes
		ld a,b
		cp ubytes
		jp z,searchok
	;not the ubytes field, extent field?
		cp extnum ;may be extent field
	ld	a,(de) ;fcb character
		jp z,searchext ;skip to search extent
	sub	(hl) ;mask-out flags/extent modulus
		and 7fh
		jp nz,searchn ;skip if not matched
		jp searchok ;matched character
searchext:
	;A has fcb character
	;attempt an extent # match
	push	bc ;save counters
		ld c,(hl) ;directory character to c
	call	compext ;compare user/dir char
	pop	bc ;recall counters
		jp nz,searchn ;skip if no match
searchok:
	;current character matches
		inc de
		inc hl
		inc b
		dec c
		jp searchloop
endsearch:
	;entire name matches, return dir position
		ld a,(dcnt)
		and dskmsk
		ld (lret),a
	;lret = low(dcnt) and 11b
		ld hl,dirloc ;dirloc=0ffh?
		ld a,(hl)
		rl a
		ret nc
	;yes, change it to 0 to mark as found
		xor a ;dirloc=0
		ld (hl),a
	ret
search_fin:
	;end of directory, or empty name
	call	set_end_dir ;may be artifical end
		ld a,255
		jp sta_ret


delete:
	;delete the currently addressed file
	call	check_write ;write protected?
		ld c,extnum ;search through file type
	call	search
delete0:
	;loop while directory matches
	call	end_of_dir ;stop if end
		ret z
	;EQU each non zero disk map entry to 0
	;in the allocation vector
	;may be r/o file
	call	check_rodir ;ro disk error if found
	call	getdptra ;HL=.buff(dptr)
		ld (hl),empty
		ld c,0 ;alloc elts EQU to 0
	call	scandm
	call	wrdir ;write the directory
	call	searchn ;to next element
		jp delete0 ;for another record

get_block:
	;given allocation vector position BC, find the zero bit
	;closest to this position by searching left and right.
	;if found, EQU the bit to one and return the bit position
	;in hl.  if not found (i.e., we pass 0 on the left, or
	;maxall on the right), return 0000 in hl
		ld d,b ;copy of starting position to de
		ld e,c
lefttst:
		ld a,c ;skip if left=0000
		or b
		jp z,righttst
	;left not at position zero, bit zero?
		dec bc ;left,right pushed
	push	de
	push	bc
	call	getallocbit
		rr a ;return block number if zero
		jp nc,retblock
	;bit is one, so try the right
	pop	bc ;left, right restored
	pop	de
righttst:
		ld hl,(maxall) ;value of maximum allocation#
		ld a,e ;right=maxall?
	sub	l
		ld a,d
		sbc a,h
		jp nc,retblock0 ;return block 0000 if so
		inc de ;left, right pushed
	push	bc
	push	de
		ld b,d ;ready right for call
		ld c,e
	call	getallocbit
		rr a ;return block number if zero
		jp nc,retblock
	pop	de ;restore left and right pointers
	pop	bc
		jp lefttst ;for another attempt
retblock:
		rl a ;bit back into position and EQU to 1
		inc a
	;d contains the number of shifts required to reposition
	call	rotr ;move bit back to position and store
	pop	hl ;HL returned value, DE discarded
	pop	de
	ret
retblock0:
	;cannot find an available bit, return 0000
		ld a,c ;
		or b ;also at beginning
		jp nz,lefttst
		ld hl,0000h
	ret

copy_fcb:
	;copy the entire file control block
		ld c,0 ;start at 0, to fcblen-1
		ld e,fcblen
	;	jmp copy_dir

copy_dir:
	;copy fcb information starting at C for E bytes
	;into the currently addressed directory entry
	push	de ;save length for later
		ld b,0 ;double index to BC
		ld hl,(info) ;HL = source for data
		add hl,bc ;DE=.fcb(C), source for copy
		ex de,hl
	call	getdptra ;HL=.buff(dptr), destination
	pop	bc ;DE=source, HL=dest, C=length
	call	move ;data moved
seek_copy:
	;enter from close to seek and copy current element
	call	seek_dir ;to the directory element
		jp wrdir ;write the directory element
	;ret


rename:
	;rename the file described by the first half of
	;the currently addressed file control block. the
	;new name is contained in the last half of the
	;currently addressed file conrol block.  the file
	;name and type are changed, but the reel number
	;is ignored.  the user number is identical
	call	check_write ;may be write protected
	;search up to the extent field
		ld c,extnum
	call	search
	;copy position 0
		ld hl,(info) ;HL=.fcb(0), A=fcb(0)
		ld a,(hl)
		ld de,dskmap ;HL=.fcb(dskmap)
		add hl,de
		ld (hl),a ;fcb(dskmap)=fcb(0)
	;assume the same disk drive for new named file
rename0:
	call	end_of_dir ;stop at end of dir
		ret z
	;not end of directory, rename next element
	call	check_rodir ;may be read-only file
		ld c,dskmap
		ld e,extnum
	call	copy_dir
	;element renamed, move to next
	call	searchn
		jp rename0

indicators:
	;EQU file indicators for current fcb
		ld c,extnum ;through file type
	call	search
indic0:
	call	end_of_dir ;stop at end of dir
		ret z
	;not end of directory, continue to change
		ld c,0 ;copy name
		ld e,extnum
	call	copy_dir
	call	searchn
		jp indic0

open:
	;search for the directory entry, copy to fcb
		ld c,namlen
	call	search
	call	end_of_dir ;return with lret=255 if end
		ret z
	;not end of directory, copy fcb information
open_copy:
	;(referenced below to copy fcb info)
	call	getexta ;save extent#
		ld a,(hl)
		push	af
	push	hl
	call	getdptra ;DE = .buff(dptr)
		ex de,hl
		ld hl,(info) ;HL=.fcb(0)
		ld c,nxtrec ;length of move operation
	push	de ;save .buff(dptr)
	call	move ;from .buff(dptr) to .fcb(0)
	;note that entire fcb is copied, including indicators
	call	setfwf ;sets file write flag
	pop	de ;HL=.buff(dptr+extnum)
		ld hl,extnum
		add hl,de
		ld c,(hl) ;C = directory extent number
		ld hl,reccnt ;HL=.buff(dptr+reccnt)
		add hl,de
		ld b,(hl) ;B holds directory record count
	pop	hl ;restore extent number
		pop	af
		ld (hl),a
	;HL = .user extent#, B = dir rec cnt, C = dir extent#
	;if user ext < dir ext then user := 128 records
	;if user ext = dir ext then user := dir records
	;if user ext > dir ext then user := 0 records
		ld a,c ;ready dir reccnt
		cp (hl)
		ld a,b
		jp z,open_rcnt ;if same, user gets dir reccnt
		ld a,0 ;user is larger
		jp c,open_rcnt
		ld a,128 ;directory is larger
open_rcnt: ;A has record count to fill
		ld hl,(info)
		ld de,reccnt
		add hl,de
		ld (hl),a
	ret

mergezero:
	;HL = .fcb1(i), DE = .fcb2(i),
	;if fcb1(i) = 0 then fcb1(i) := fcb2(i)
		ld a,(hl) ;return if = 0000
		inc hl
		or (hl)
		dec hl
		ret nz
	ld	a,(de) ;low byte copied
		ld (hl),a
		inc de
		inc hl
	ld	a,(de) ;back to input form
		ld (hl),a
		dec de
		dec hl
	ret

close:
	;locate the directory element and re-write it
		xor a
		ld (lret),a
		ld (dcnt),a
		ld (dcnt+1),a
	call	nowrite ;skip close if r/o disk
		ret nz
	;check file write flag - 0 indicates written
	call	getmodnum ;fcb(modnum) in A
		and fwfmsk ;return if bit remains set
		ret nz
		ld c,namlen ;locate file
	call	search
	call	end_of_dir ;return if not found
		ret z
	;merge the disk map at info with that at buff(dptr)
		ld bc,dskmap
	call	getdptra
		add hl,bc ;DE is .buff(dptr+16)
		ex de,hl
		ld hl,(info) ;DE=.buff(dptr+16), HL=.fcb(16)
		add hl,bc
		ld c,(fcblen-dskmap) ;length of single byte dm
merge0:
		ld a,(single) ;skip to double
		or a
		jp z,merged
	;this is a single byte map
	;if fcb(i) = 0 then fcb(i) = buff(i)
	;if buff(i) = 0 then buff(i) = fcb(i)
	;if fcb(i) <> buff(i) then error
		ld a,(hl)
		or a
	ld	a,(de)
		jp nz,fcbnzero
	;fcb(i) = 0
		ld (hl),a ;fcb(i) = buff(i)
fcbnzero:
		or a
		jp nz,buffnzero
	;buff(i) = 0
		ld a,(hl) ;buff(i)=fcb(i)
	ld	(de),a
buffnzero:
		cp (hl) ;fcb(i) = buff(i)?
		jp nz,mergerr
		jp dmset ;if merge ok
merged:
	;this is a double byte merge operation
	call	mergezero ;buff = fcb if buff 0000
		ex de,hl ;fcb = buff if fcb 0000
	call	mergezero
		ex de,hl
	;they should be identical at this point
	ld	a,(de) ;low same?
		cp (hl)
		jp nz,mergerr
		inc de ;to high byte
		inc hl
	ld	a,(de) ;high same?
		cp (hl)
		jp nz,mergerr
	;merge operation ok for this pair
		dec c ;extra count for double byte
dmset:
		inc de ;to next byte position
		inc hl
		dec c ;for more
		jp nz,merge0
	;end of disk map merge, check record count
	;DE = .buff(dptr)+32, HL = .fcb(32)
		ld bc,-(fcblen-extnum)
		add hl,bc
		ex de,hl
		add hl,bc
	;DE = .fcb(extnum), HL = .buff(dptr+extnum)
	ld	a,(de) ;current user extent number
	;if fcb(ext) >= buff(fcb) then
	;buff(ext) := fcb(ext), buff(rec) := fcb(rec)
		cp (hl)
		jp c,endmerge
	;fcb extent number >= dir extent number
		ld (hl),a ;buff(ext) = fcb(ext)
	;update directory record count field
		ld bc,(reccnt-extnum)
		add hl,bc
		ex de,hl
		add hl,bc
	;DE=.buff(reccnt), HL=.fcb(reccnt)
		ld a,(hl) ;buff(reccnt)=fcb(reccnt)
	ld	(de),a
endmerge:
		ld a,_true ;mark as copied
		ld (fcb_copied),a
		jp seek_copy ;ok to "wrdir" here - 1.4 compat
	;		ret
mergerr:
	;elements did not merge correctly
		ld hl,lret ;=255 non zero flag set
		dec (hl)
	ret

make:
	;create a new file by creating a directory entry
	;then opening the file
	call	check_write ;may be write protected
		ld hl,(info) ;save fcb address, look for e5
	push	hl
		ld hl,efcb ;info = .empty
		ld (info),hl
		ld c,1 ;length 1 match on empty entry
	call	search
	call	end_of_dir ;zero flag EQU if no space
	pop	hl ;recall info address
		ld (info),hl ;in case we return here
		ret z ;return with error condition 255 if not found
		ex de,hl ;DE = info address
	;clear the remainder of the fcb
		ld hl,namlen ;HL=.fcb(namlen)
		add hl,de
		ld c,fcblen-namlen ;number of bytes to fill
		xor a ;clear accumulator to 00 for fill
make0:
		ld (hl),a
		inc hl
		dec c
		jp nz,make0
		ld hl,ubytes ;HL = .fcb(ubytes)
		add hl,de
		ld (hl),a ;fcb(ubytes) = 0
	call	setcdr ;may have extended the directory
	;now copy entry to the directory
	call	copy_fcb
	;and EQU the file write flag to "1"
		jp setfwf
	;ret

open_reel:
	;close the current extent, and open the next one
	;if possible.  RMF is true if in read mode
		xor a ;EQU true if actually copied
		ld (fcb_copied),a
	call	close ;close current extent
	;lret remains at enddir if we cannot open the next ext
	call	end_of_dir ;return if end
		ret z
	;increment extent number
		ld hl,(info) ;HL=.fcb(extnum)
		ld bc,extnum
		add hl,bc
		ld a,(hl) ;fcb(extnum)=++1
		inc a
		and maxext
		ld (hl),a
		jp z,open_mod ;move to next module if zero
	;may be in the same extent group
		ld b,a
		ld a,(extmsk)
		and b
	;if result is zero, then not in the same group
		ld hl,fcb_copied ;true if the fcb was copied to directory
		and (hl) ;produces a 00 in accumulator if not written
		jp z,open_reel0 ;go to next physical extent
	;result is non zero, so we must be in same logical ext
		jp open_reel1 ;to copy fcb information
open_mod:
	;extent number overflow, go to next module
		ld bc,(modnum-extnum) ;HL=.fcb(modnum)
		add hl,bc
		inc (hl) ;fcb(modnum)=++1
	;module number incremented, check for overflow
		ld a,(hl) ;mask high order bits
		and maxmod
		jp z,open_r_err ;cannot overflow to zero
	;otherwise, ok to continue with new module
open_reel0:
		ld c,namlen ;next extent found?
	call	search
	call	end_of_dir
		jp nz,open_reel1
	;end of file encountered
		ld a,(rmf) ;0ffh becomes 00 if read
		inc a
		jp z,open_r_err ;sets lret = 1
	;try to extend the current file
	call	make
	;cannot be end of directory
	call	end_of_dir
		jp z,open_r_err ;with lret = 1
		jp open_reel2
open_reel1:
	;not end of file, open
	call	open_copy
open_reel2:
	call	getfcb ;EQU parameters
		xor a ;lret = 0
		jp sta_ret
;	ret ;with lret = 0
open_r_err:
	;cannot move to next extent of this file
	call	setlret1 ;lret = 1
		jp setfwf ;ensure that it will not be closed
	;ret

seqdiskread:
	;sequential disk read operation
		ld a,1
		ld (seqio),a
	;drop through to diskread

diskread:	;(may enter from seqdiskread)
		ld a,_true ;read mode flag = true (open_reel)
		ld (rmf),a
	;read the next record from the current fcb
	call	getfcb ;sets parameters for the read
		ld a,(vrecord) ;vrecord-rcount
		ld hl,rcount
		cp (hl)
	;skip if rcount > vrecord
		jp c,recordok
	;not enough records in the extent
	;record count must be 128 to continue
		cp 128 ;vrecord = 128?
		jp nz,diskeof ;skip if vrecord<>128
	call	open_reel ;go to next extent if so
		xor a ;vrecord=00
		ld (vrecord),a
	;now check for open ok
		ld a,(lret) ;stop at eof
		or a
		jp nz,diskeof
recordok:
	;arrive with fcb addressing a record to read
	call	index
	;error 2 if reading unwritten data
	;(returns 1 to be compatible with 1.4)
	call	allocated ;arecord=0000?
		jp z,diskeof
	;record has been allocated, read it
	call	atran ;arecord now a disk address
	call	seek ;to proper track,sector
	call	rdbuff ;to dma address
		jp setfcb ;replace parameter	
;		ret
diskeof:
		jp setlret1 ;lret = 1
	;ret

seqdiskwrite:
	;sequential disk write
		ld a,1
		ld (seqio),a
	;drop through to diskwrite

diskwrite:	;(may enter here from seqdiskwrite above)
		ld a,_false ;read mode flag
		ld (rmf),a
	;write record to currently selected file
	call	check_write ;in case write protected
		ld hl,(info) ;HL = .fcb(0)
	call	check_rofile ;may be a read-only file
	call	getfcb ;to EQU local parameters
		ld a,(vrecord) ;vrecord-128
		cp lstrec+1
	;skip if vrecord > lstrec
	;vrecord = 128, cannot open next extent
		jp nc,setlret1 ;lret=1
diskwr0:
	;can write the next record, so continue
	call	index
	call	allocated
		ld c,0 ;marked as normal write operation for wrbuff
		jp nz,diskwr1
	;not allocated
	;the argument to getblock is the starting
	;position for the disk search, and should be
	;the last allocated block for this file, or
	;the value 0 if no space has been allocated
	call	dm_position
		ld (dminx),a ;save for later
		ld bc,0000h ;may use block zero
		or a ;skip if no previous block
		jp z,nopblock
	;previous block exists at A
		ld c,a ;previous block # in BC
		dec bc
	call	getdm ;previous block # to HL
		ld b,h ;BC=prev block#
		ld c,l
nopblock:
	;BC = 0000, or previous block #
	call	get_block ;block # to HL
	;arrive here with block# or zero
		ld a,l
		or h
		jp nz,blockok
	;cannot find a block to allocate
		ld a,2 ;lret=2
		jp sta_ret
blockok:
	;allocated block number is in HL
		ld (arecord),hl
		ex de,hl ;block number to DE
		ld hl,(info) ;HL=.fcb(dskmap)
		ld bc,dskmap
		add hl,bc
		ld a,(single) ;EQU flags for single byte dm
		or a
		ld a,(dminx) ;recall dm index
		jp z,allocwd ;skip if allocating word
	;allocating a byte value
	call	addh ;single byte alloc
		ld (hl),e
		jp diskwru ;to continue
allocwd:
	;allocate a word value
		ld c,a ;double(dminx)
		ld b,0
		add hl,bc ;HL=.fcb(dminx*2)
		add hl,bc
		ld (hl),e ;double wd
		inc hl
		ld (hl),d
diskwru:
	;disk write to previously unallocated block
		ld c,2 ;marked as unallocated write
diskwr1:
	;continue the write operation of no allocation error
	;C = 0 if normal write, 2 if to prev unalloc block
		ld a,(lret) ;stop if non zero returned value
		or a
		ret nz
	push	bc ;save write flag
	call	atran ;arecord set
		ld a,(seqio)
		dec a
		dec a
		jp nz,diskwr11
	pop	bc
	push	bc
		ld a,c
		dec a
		dec a
		jp nz,diskwr11 ;old allocation  
	push	hl ;arecord in hl ret from atran
		ld hl,(buffa) ;zero buffa & fill
		ld d,a
fill0:	ld (hl),a
		inc hl
		inc d
		jp p,fill0
	call	setdir
		ld hl,(arecord1)
		ld c,2
fill1:	ld (arecord),hl
	push	bc
	call	seek
	pop	bc
	call	wrbuff ;write fill record
		ld hl,(arecord) ;restore last record     
		ld c,0 ;change  allocate flag   
		ld a,(blkmsk)
		ld b,a
		and l
		cp b
		inc hl
		jp nz,fill1 ;cont until cluster is zeroed
	pop	hl
		ld (arecord),hl
	call	setdata
diskwr11:
	call seek ;to proper file position
	pop	bc ;restore/save write flag (C=2 if new block)
	push	bc
	call	wrbuff ;written to disk
	pop	bc ;C = 2 if a new block was allocated, 0 if not
	;increment record count if rcount<=vrecord
		ld a,(vrecord) ;vrecord-rcount
		ld hl,rcount
		cp (hl)
		jp c,diskwr2
	;rcount <= vrecord
		ld (hl),a ;rcount = vrecord+1
		inc (hl)
		ld c,2 ;mark as record count incremented
diskwr2:
	if	patch1
	; CP/M V2.2 patch 1 for use of optional blocking/deblocking
	nop
	nop
		ld hl,0
	else
	; original code
	;A has vrecord, C=2 if new block or new record#
		dec c
		dec c
		jp nz,noupdate
	endif
		push	af ;save vrecord value
	call	getmodnum ;HL=.fcb(modnum), A=fcb(modnum)
	;reEQU the file write flag to mark as written fcb
		and (~fwfmsk)&0ffh ;bit reset
		ld (hl),a ;fcb(modnum) = fcb(modnum) and 7fh
		pop	af ;restore vrecord
noupdate:
	;check for end of extent, if found attempt to open
	;next extent in preparation for next write
		cp lstrec ;vrecord=lstrec?
		jp nz,diskwr3 ;skip if not
	;may be random access write, if so we are done
	;change next     
		ld a,(seqio) ;skip next extent open op
		cp 1
		jp nz,diskwr3
	;update current fcb before going to next extent
	call	setfcb
	call	open_reel ;rmf=false
	;vrecord remains at lstrec causing eof if
	;no more directory space is available
		ld hl,lret
		ld a,(hl)
		or a
		jp nz,nospace
	;space available, EQU vrecord=255
		dec a ;goes to 00 next time
		ld (vrecord),a
nospace:
		ld (hl),0 ;lret = 00 for returned value
diskwr3:
		jp setfcb ;replace parameters
	;ret

rseek:
	;random access seek operation, C=0ffh if read mode
	;fcb is assumed to address an active file control block
	;(modnum has been EQU to 1100_0000b if previous bad seek)
		xor a ;marked as random access operation
		ld (seqio),a
rseek1:
	push	bc ;save r/w flag
		ld hl,(info) ;DE will hold base of fcb
		ex de,hl
		ld hl,ranrec ;HL=.fcb(ranrec)
		add hl,de
		ld a,(hl) ;record number
		and 7fh
		push	af
		ld a,(hl) ;cy=lsb of extent#
		rl a
		inc hl ;A=ext#
		ld a,(hl)
		rl a
		and 11111b
		ld c,a ;C holds extent number, record stacked
		ld a,(hl) ;mod#
		rr a
		rr a
		rr a
		rr a
		and 1111b
		ld b,a ;B holds module#, C holds ext#
		pop	af ;recall sought record #
	;check to insure that high byte of ran rec = 00
		inc hl ;l=high byte (must be 00)
		ld l,(hl)
		inc l ;zero flag, l=6
		dec l
		ld l,6
	;produce error 6, seek past physical eod
		jp nz,seekerr
	;otherwise, high byte = 0, A = sought record
		ld hl,nxtrec ;HL = .fcb(nxtrec)
		add hl,de
		ld (hl),a ;sought rec# stored away
	;arrive here with B=mod#, C=ext#, DE=.fcb, rec stored
	;the r/w flag is still stacked.  compare fcb values
		ld hl,extnum ;A=seek ext#
		add hl,de
		ld a,c
	sub	(hl) ;tests for = extents
		jp nz,ranclose
	;extents match, check mod#
		ld hl,modnum ;B=seek mod#
		add hl,de
		ld a,b
	;could be overflow at eof, producing module#
	;of 90H or 10H, so compare all but fwf
	sub	(hl) ;same?
		and 7fh
		jp z,seekok
ranclose:
	push	bc ;save seek mod#,ext#, .fcb
	push	de
	call	close ;current extent closed
	pop	de ;recall parameters and fill
	pop	bc
		ld l,3 ;cannot close error #3
		ld a,(lret)
		inc a
		jp z,badseek
		ld hl,extnum ;fcb(extnum)=ext#
		add hl,de
		ld (hl),c
		ld hl,modnum ;fcb(modnum)=mod#
		add hl,de
		ld (hl),b
	call	open ;is the file present?
		ld a,(lret) ;open successful?
		inc a
		jp nz,seekok
	;cannot open the file, read mode?
	pop	bc ;r/w flag to c (=0ffh if read)
	push	bc ;everyone expects this item stacked
		ld l,4 ;seek to unwritten extent #4
		inc c ;becomes 00 if read operation
		jp z,badseek ;skip to error if read operation
	;write operation, make new extent
	call	make
		ld l,5 ;cannot create new extent #5
		ld a,(lret) ;no dir space
		inc a
		jp z,badseek
	;file make operation successful
seekok:
	pop	bc ;discard r/w flag
		xor a ;with zero set
		jp sta_ret
badseek:
	;fcb no longer contains a valid fcb, mark
	;with 1100_000b in modnum field so that it
	;appears as overflow with file write flag set
	push	hl ;save error flag
	call	getmodnum ;HL = .modnum
		ld (hl),11000000b
	pop	hl ;and drop through
seekerr:
	pop	bc ;discard r/w flag
		ld a,l ;lret=#, nonzero
		ld (lret),a
	;setfwf returns non-zero accumulator for err
		jp setfwf ;flag set, so subsequent close ok
	;ret

randiskread:
	;random disk read operation
		ld c,_true ;marked as read operation
	call	rseek
	call	z,diskread ;if seek successful
	ret

randiskwrite:
	;random disk write operation
		ld c,_false ;marked as write operation
	call	rseek
	call	z,diskwrite ;if seek successful
	ret

compute_rr:
	;compute random record position for getfilesize/setrandom
		ex de,hl
		add hl,de
	;DE=.buf(dptr) or .fcb(0), HL = .f(nxtrec/reccnt)
		ld c,(hl) ;BC = 0000 0000 ?rrr rrrr
		ld b,0
		ld hl,extnum ;A=e000 0000
		add hl,de
		ld a,(hl)
		rrca
		and 80h
		add a,c
		ld c,a
		ld a,0
		adc a,b
		ld b,a
	;BC = 0000 000? errrr rrrr
		ld a,(hl)
		rrca
		and 0fh
		add a,b
		ld b,a
	;BC = 000? eeee errrr rrrr
		ld hl,modnum ;A=XXX? mmmm
		add hl,de
		ld a,(hl)
		add a,a ;cy=? A=mmmm 0000
		add a,a
		add a,a
		add a,a
		push	af
		add a,b
		ld b,a
	;cy=?, BC = mmmm eeee errr rrrr
		push	af ;possible second carry
	pop	hl ;cy = lsb of L
		ld a,l ;cy = lsb of A
	pop	hl ;cy = lsb of L
		or l ;cy/cy = lsb of A
		and 1 ;A = 0000 000? possible carry-out
	ret

getfilesize:
	;compute logical file size for current fcb
		ld c,extnum
	call	search
	;zero the receiving ranrec field
		ld hl,(info) ;save position
		ld de,ranrec
		add hl,de
	push	hl
		ld (hl),d ;=00 00 00
		inc hl
		ld (hl),d
		inc hl
		ld (hl),d
getsize:
	call	end_of_dir
		jp z,setsize
	;current fcb addressed by dptr
	call	getdptra ;ready for compute size
		ld de,reccnt
	call	compute_rr
	;A=0000 000? BC = mmmm eeee errr rrrr
	;compare with memory, larger?
	pop	hl ;recall, replace .fcb(ranrec)
	push	hl
		ld e,a ;save cy
		ld a,c ;ls byte
	sub	(hl)
		inc hl
		ld a,b ;middle byte
		sbc a,(hl)
		inc hl
		ld a,e ;carry if .fcb(ranrec) > directory
		sbc a,(hl)
		jp c,getnextsize ;for another try
	;fcb is less or equal, fill from directory
		ld (hl),e
		dec hl
		ld (hl),b
		dec hl
		ld (hl),c
getnextsize:
	call	searchn
		jp getsize
setsize:
	pop	hl ;discard .fcb(ranrec)
	ret

setrandom:
	;EQU random record from the current file control block
		ld hl,(info) ;ready params for computesize
		ld de,nxtrec
	call	compute_rr ;DE=info, A=cy, BC=mmmm eeee errr rrrr
		ld hl,ranrec ;HL = .fcb(ranrec)
		add hl,de
		ld (hl),c ;to ranrec
		inc hl
		ld (hl),b
		inc hl
		ld (hl),a
	ret

select:
	;select disk info for subsequent input or output ops
		ld hl,(dlog)
		ld a,(curdsk)
		ld c,a
	call	hlrotr
	push	hl ;save it for test below, send to seldsk
		ex de,hl
	call	selectdisk ;recall dlog vector
	pop	hl
	call	z,sel_error ;returns true if select ok
	;is the disk logged in?
		ld a,l ;return if bit is set
		rr a
		ret c
	;disk not logged in, EQU bit and initialize
		ld hl,(dlog) ;call ready
		ld c,l
		ld b,h
	call	set_cdisk ;dlog=set_cdisk(dlog)
		ld (dlog),hl
		jp initialize
	;ret

curselect:
		ld a,(linfo) ;skip if linfo=curdsk
		ld hl,curdsk
		cp (hl)
		ret z
		ld (hl),a ;curdsk=info
		jp select
	;ret

reselect:
	;check current fcb to see if reselection necessary
		ld a,_true ;mark possible reselect
		ld (resel),a
		ld hl,(info) ;drive select code
		ld a,(hl)
		and 11111b ;non zero is auto drive select
		dec a ;drive code normalized to 0..30, or 255
		ld (linfo),a ;save drive code
		cp 30
		jp nc,noselect
	;auto select function, save curdsk
		ld a,(curdsk) ;olddsk=curdsk
		ld (olddsk),a
		ld a,(hl) ;save drive code
		ld (fcbdsk),a
		and 11100000b ;preserve hi bits
		ld (hl),a
	call	curselect
noselect:
	;EQU user code
		ld a,(usrcode) ;0...31
		ld hl,(info)
		or (hl)
		ld (hl),a
	ret

;	individual function handlers
func12:
	;return version number
		ld a,dvers ;lret = dvers (high = 00)
		jp sta_ret
;	ret ;jmp goback

func13:
	;reEQU disk system - initialize to disk 0
		ld hl,0
		ld (rodsk),hl
		ld (dlog),hl
		xor a ;note that usrcode remains unchanged
		ld (curdsk),a
		ld hl,tbuff ;dmaad = tbuff
		ld (dmaad),hl
        call	setdata ;to data dma address
		jp select
	;ret ;jmp goback

func14	equ	curselect
	;select disk info
	;ret ;jmp goback

func15:
	;open file
	call	clrmodnum ;clear the module number
	call	reselect
		jp open
	;ret ;jmp goback

func16:
	;close file
	call	reselect
		jp close
	;ret ;jmp goback

func17:
	;search for first occurrence of a file
		ld c,0 ;length assuming '?' true
		ex de,hl ;was lhld info		
		ld a,(hl) ;no reselect if ?
		cp '?'
		jp z,qselect ;skip reselect if so
	;normal search
	call	getexta
		ld a,(hl)
		cp '?'
		call nz,clrmodnum ;module number zeroed
	call	reselect
		ld c,namlen
qselect:
	call	search
		jp dir_to_user ;copy directory entry to user
	;ret ;jmp goback

func18:
	;search for next occurrence of a file name
		ld hl,(searcha)
		ld (info),hl
	call	reselect
	call	searchn
		jp dir_to_user ;copy directory entry to user
	;ret ;jmp goback

func19:
	;delete a file
	call	reselect
	call	delete
		jp copy_dirloc
	;ret ;jmp goback

func20:
	;read a file
	call	reselect
		jp seqdiskread ;
	 ;jmp goback

func21:
	;write a file
	call	reselect
		jp seqdiskwrite ;
	 ;jmp goback

func22:
	;make a file
	call	clrmodnum
	call	reselect
		jp make
	;ret ;jmp goback

func23:
	;rename a file
	call	reselect
	call	rename
		jp copy_dirloc
	;ret ;jmp goback

func24:
	;return the login vector
		ld hl,(dlog)
		jp sthl_ret ;
;	ret ;jmp goback

func25:
	;return selected disk number
		ld a,(curdsk)
		jp sta_ret
;	ret ;jmp goback

func26:
	;EQU the subsequent dma address to info
		ex de,hl ;was lhld info	
		ld (dmaad),hl ;dmaad = info
        	jp setdata ;to data dma address
	;ret ;jmp goback

func27:
	;return the login vector address
		ld hl,(alloca)
		jp sthl_ret
;	ret ;jmp goback

func28	equ	set_ro
	;write protect current disk
	;ret ;jmp goback

func29:
	;return r/o bit vector
		ld hl,(rodsk)
		jp sthl_ret
;	ret ;jmp goback

func30:
	;EQU file indicators
	call	reselect
	call	indicators
		jp copy_dirloc ;lret=dirloc
	;ret ;jmp goback

func31:
	;return address of disk parameter block
		ld hl,(dpbaddr)
sthl_ret:
 		ld (aret),hl
	ret ;jmp goback
func32:
	;EQU user code
        	ld a,(linfo)
		cp 0ffh
		jp nz,setusrcode
	;interrogate user code instead
		ld a,(usrcode) ;lret=usrcode
		jp sta_ret
;		ret ;jmp goback
setusrcode:
		and 1fh
		ld (usrcode),a
	ret ;jmp goback
;
func33:
	;random disk read operation
	call	reselect
		jp randiskread ;to perform the disk read
	;ret ;jmp goback
;
func34:
	;random disk write operation
	call	 reselect
		jp randiskwrite ;to perform the disk write
	;ret ;jmp goback
;
func35:
	;return file size (0-65536)
	call	reselect
		jp getfilesize
	;ret ;jmp goback
;
func36	equ	setrandom ;
	;EQU random record
	;ret ;jmp goback
func37:
;
		ld hl,(info)
		ld a,l
		cpl
		ld e,a
		ld a,h
		cpl
		ld hl,(dlog)
		and h
		ld d,a
		ld a,l
		and e
		ld e,a
		ld hl,(rodsk)
		ex de,hl
		ld (dlog),hl
		ld a,l
		and e
		ld l,a
		ld a,h
		and d
		ld h,a
		ld (rodsk),hl
	ret
;
;
goback:
	;arrive here at end of processing to return to user
		ld a,(resel)
		or a
		jp z,retmon
	;reselection may have taken place
		ld hl,(info) ;fcb(0)=0
		ld (hl),0
		ld a,(fcbdsk)
		or a
		jp z,retmon
	;restore disk number
		ld (hl),a ;fcb(0)=fcbdsk
		ld a,(olddsk)
		ld (linfo),a
	call	curselect
;
;	return from the disk monitor
retmon:
		ld hl,(entsp) ;user stack restored
		ld sp,hl
		ld hl,(aret) ;BA = HL = aret
		ld a,l
		ld b,h
	ret

func38	equ	func_ret
func39	equ	func_ret

func40:
	;random disk write with zero fill of unallocated block
	call	reselect
		ld a,2
		ld (seqio),a
		ld c,_false
	call	rseek1
	call	z,diskwrite ;if seek successful
	ret


;	data areas

;	initialized data
efcb:	db	empty	;0e5=available dir entry
rodsk:	dw	0	;read only disk vector
dlog:	dw	0	;logged-in disks
dmaad:	dw	tbuff	;initial dma address

;	curtrka - alloca are EQU upon disk select
;	(data must be adjacent, do not insert variables)
;	(address of translate vector, not used)
cdrmaxa:ds	word	;pointer to cur dir max value
curtrka:ds	word	;current track address
curreca:ds	word	;current record address
buffa:	ds	word	;pointer to directory dma address
dpbaddr:ds	word	;current disk parameter block address
checka:	ds	word	;current checksum vector address
alloca:	ds	word	;current allocation vector address
addlist	equ	$-buffa ;address list size

;	sectpt - offEQU obtained from disk parm block at dpbaddr
;	(data must be adjacent, do not insert variables)
sectpt:	ds	word	;sectors per track
blkshf:	ds	byte	;block shift factor
blkmsk:	ds	byte	;block mask
extmsk:	ds	byte	;extent mask
maxall:	ds	word	;maximum allocation number
dirmax:	ds	word	;largest directory number
dirblk:	ds	word	;reserved allocation bits for directory
chksiz:	ds	word	;size of checksum vector
offset:	ds	word	;offEQU tracks at beginning
dpblist	equ	$-sectpt ;size of area

;	local variables
tranv:	ds	word	;address of translate vector
fcb_copied:
	ds	byte	;EQU true if copy_fcb called
rmf:	ds	byte	;read mode flag for open_reel
dirloc:	ds	byte	;directory flag in rename, etc.
seqio:	ds	byte	;1 if sequential i/o
linfo:	ds	byte	;low(info)
dminx:	ds	byte	;local for diskwrite
searchl:ds	byte	;search length
searcha:ds	word	;search address
tinfo:	ds	word	;temp for info in "make"
single:	ds	byte	;EQU true if single byte allocation map
resel:	ds	byte	;reselection flag
olddsk:	ds	byte	;disk on entry to bdos
fcbdsk:	ds	byte	;disk named in fcb
rcount:	ds	byte	;record count in current fcb
extval:	ds	byte	;extent number and extmsk
vrecord:ds	word	;current virtual record
arecord:ds	word	;current actual record
arecord1:	ds	word	;current actual block# * blkmsk

;	local variables for directory access
dptr:	ds	byte	;directory pointer 0,1,2,3
dcnt:	ds	word	;directory counter 0,1,...,dirmax
drec:	ds	word	;directory record 0,1,...,dirmax/4

bios	equ 0C000h;next module
	end



