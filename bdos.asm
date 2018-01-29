; Reformatted and converted for cross-assembly by Macro Assembler AS
; Eric Smith <spacewar@gmail.com> 2018-01-24
; from original source os3bdos.asm from
;   http://www.cpm.z80.de/download/cpm2-plm.zip
; includes Digital Research CP/M V2.2 Patch 01 (cpm22pat.01) from
;   http://www.cpm.z80.de/download/cpm22pat.zip

; Changes:
;   multiple instructions per line split to separate lines
;   dollar sign in labels replaced by underscore
;   dollar sign (as digit separator) in binary constants removed
;   no colons for labels for equates
;   single quotes around strings replaced with double quotes
;   true and false replaced with _true and _false
;   eliminated equates for 8080 registers, added comments introduced with %
;   replaced "not", "and" operators with "~", "&"
;   removed empty comments
;   added ifdef origin to allow origin to be specified from command line
;   added commments about serial number

	.cpu	8080

patch1	equ	1


	title	"Bdos Interface, Bdos, Version 2.2 Feb, 1980"
;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**            I n t e r f a c e   M o d u l e                   **
;**                                                             **
;*****************************************************************
;*****************************************************************
;
;	Copyright (c) 1978, 1979, 1980
;	Digital Research
;	Box 579, Pacific Grove
;	California
;
;
;      20 january 1980
;
;
on	equ	0ffffh
off	equ	00000h
test	equ	off

	ifdef	origin
	org	origin
	else
	if	test
	org	0dc00h
	else
	org	0800h
	endif
	endif
;	bios value defined at end of module

ssize	equ	24		;24 level stack

;	low memory locations
reboot	equ	0000h		;reboot system
ioloc	equ	0003h		;i/o byte location
bdosa	equ	0006h		;address field of jmp BDOS

;	bios access constants
bootf	set	bios+3*0	;cold boot function
wbootf	set	bios+3*1	;warm boot function
constf	set	bios+3*2	;console status function
coninf	set	bios+3*3	;console input function
conoutf	set	bios+3*4	;console output function
listf	set	bios+3*5	;list output function
punchf	set	bios+3*6	;punch output function
readerf	set	bios+3*7	;reader input function
homef	set	bios+3*8	;disk home function
seldskf	set	bios+3*9	;select disk function
settrkf	set	bios+3*10	;set track function
setsecf	set	bios+3*11	;set sector function
setdmaf	set	bios+3*12	;set dma function
readf	set	bios+3*13	;read disk function
writef	set	bios+3*14	;write disk function
liststf	set	bios+3*15	;list status function
sectran	set	bios+3*16	;sector translate

;	equates for non graphic characters
ctlc	equ	03h	;control c
ctle	equ	05h	;physical eol
ctlh	equ	08h	;backspace
ctlp	equ	10h	;prnt toggle
ctlr	equ	12h	;repeat line
ctls	equ	13h	;stop/start screen
ctlu	equ	15h	;line delete
ctlx	equ	18h	;=ctl-u
ctlz	equ	1ah	;end of file
rubout	equ	7fh	;char delete
tab	equ	09h	;tab char
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
ctl	equ	5eh	;up arrow

; serial number (not documented in original DRI source file)
	db	0	; OEM number, low byte
	db	0	; CP/M version, 16h = 2.2
	db	0	; OEM number, high byte
	db	0,0,0	; serial number, big-endian

;	enter here from the user's program with function number in c,
;	and information address in d,e
	jmp	bdose	;past parameter block

;	************************************************
;	*** relative locations 0009 - 000e           ***
;	************************************************
pererr:	dw	persub	;permanent error subroutine
selerr:	dw	selsub	;select error subroutine
roderr:	dw	rodsub	;ro disk error subroutine
roferr:	dw	rofsub	;ro file error subroutine


bdose:				;arrive here from user programs
	xchg			;info=DE, DE=info
	shld	info
	xchg
	mov	a,e		;linfo = low(info) - don't equ
	sta	linfo
	lxi	h,0		;return value defaults to 0000
	shld	aret
	;save user's stack pointer, set to local stack
	dad	sp		;entsp = stackptr
	shld	entsp
	lxi	sp,lstack	;local stack setup
	xra	a		;fcbdsk,resel=false
	sta	fcbdsk
	sta	resel
	lxi	h,goback	;return here after all functions
	push	h		;jmp goback equivalent to ret
	mov	a,c		;skip if invalid #
	cpi	nfuncs
	rnc
	mov	c,e		;possible output character to C
	lxi	h,functab	;DE=func, HL=.ciotab
	mov	e,a
	mvi	d,0
	dad	d		;DE=functab(func)
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	lhld	info		;info in DE for later xchg	
	xchg			;dispatched
	pchl

;	dispatch table for functions
functab:
	dw	wbootf, func1, func2, func3
	dw	punchf, listf, func6, func7
	dw	func8, func9, func10,func11
diskf	equ	($-functab)/2	;disk funcs
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
	lxi	h,permsg	;to report the error
	call	errflg
	cpi	ctlc		;reboot if response is ctlc
	jz	reboot
	ret			;and ignore the error

selsub:	;report select error
	lxi	h,selmsg	;wait console before boot
	jmp	wait_err
;
rodsub:	;report write to read/only disk
	lxi	h,rodmsg	;wait console
	jmp	wait_err
;
rofsub:	;report read/only file
	lxi	h,rofmsg	;drop through to wait for console
;
wait_err:
	;wait for response before boot
	call	errflg
	jmp	reboot

;	error messages
dskmsg:	db	"Bdos Err On "
dskerr:	db	" : $"	;filled in by errflg
permsg:	db	"Bad Sector$"
selmsg:	db	"Select$"
rofmsg:	db	"File "
rodmsg:	db	"R/O$"


errflg:
	;report error to console, message address in HL
	push	h		;stack mssg address, new line
	call	crlf
	lda	curdsk		;current disk name
	adi	'A'
	sta	dskerr
	lxi	b,dskmsg	;the error message
	call	print
	pop	b		;error mssage tail
	call	print
	;jmp	conin		;to get the input character			
	;(drop through to conin)
	;ret


;	console handlers
conin:
	;read console character to A
	lxi	h,kbchar
	mov	a,m
	mvi	m,0
	ora	a
	rnz
	;no previous keyboard character ready
	jmp	coninf		;get character externally
	;ret

conech:
	;read character with echo
	call	conin		;echo character?
	call	echoc
	rc 
        ;character must be echoed before return
	push	psw
	mov	c,a
	call	tabout
	pop	psw
	ret			;with character in A

echoc:
	;echo character if graphic
	;cr, lf, tab, or backspace
	cpi	cr		;carriage return?
	rz
	cpi	lf		;line feed?
	rz
	cpi	tab		;tab?
	rz
	cpi	ctlh		;backspace?
	rz
	cpi	' '		;carry set if not graphic
	ret 

conbrk:	;check for character ready
	lda	kbchar		;skip if active kbchar
	ora	a
	jnz	conb1
	;no active kbchar, check external break
	call	constf		;return if no char ready
	ani	1
	rz
	;character ready, read it
	call	coninf		;to A
	cpi	ctls		;check stop screen function
	jnz	conb0
	;found ctls, read next character
	call	coninf		;to A
	cpi	ctlc		;ctlc implies re-boot
	jz	reboot
	;not a reboot, act as if nothing has happened
	xra a			;with zero in accumulator
	ret
conb0:	;character in accum, save it
	sta	kbchar
conb1:	;return with true set in accumulator
	mvi	a,1
	ret

conout:
	;compute character position/write console char from C
	;compcol = true if computing column position
	lda	compcol
	ora	a
	jnz	compout
	;write the character, then compute the column
	;write console character from C
	push	b		;check for screen stop function
	call	conbrk
	pop	b		;recall/save character
	push	b
	call	conoutf		;externally, to console
	pop	b		;recall/save character
	push	b
	;may be copying to the list device
	lda	listcp		;to printer, if so
	ora	a
	cnz	listf
	pop	b		;recall the character
compout:
	mov	a,c		;recall the character
	;and compute column position
	lxi	h,column	;A = char, HL = .column
	cpi	rubout		;no column change if nulls
	rz
	inr	m		;column = column + 1
	cpi	' '		;return if graphic
	rnc
	;not graphic, reset column position
	dcr	m		;column = column - 1
	mov	a,m		;return if at zero
	ora	a
	rz
	;not at zero, may be backspace or end line
	mov	a,c		;character back to A
	cpi	ctlh
	jnz	notbacksp
	;backspace character
	dcr	m		;column = column - 1
	ret
notbacksp:
	;not a backspace character, eol?
	cpi	lf		;return if not
	rnz
	;end of line, column = 0
	mvi	m,0		;column = 0
	ret

ctlout:
	;send C character with possible preceding up-arrow
	mov	a,c		;cy if not graphic (or special case)
	call	echoc
	jnc	tabout		;skip if graphic, tab, cr, lf, or ctlh
	;send preceding up arrow
	push	psw		;up arrow
	mvi	c,ctl
	call	conout
	pop	psw		;becomes graphic letter
	ori	40h
	mov	c,a		;ready to print
	;(drop through to tabout)

tabout:
	;expand tabs to console
	mov	a,c		;direct to conout if not
	cpi	tab
	jnz	conout
	;tab encountered, move to next tab position
tab0:
	mvi	c,' '		;another blank
	call	conout
	lda	column		;column mod 8 = 0 ?
	ani	111b
	jnz	tab0		;back for another if not
	ret


backup:
	;back-up one screen position
	call	pctlh
	mvi	c,' '
	call	conoutf
;	(drop through to pctlh)
pctlh:
	;send ctlh to console without affecting column count
	mvi	c,ctlh
	jmp	conoutf
	;ret

crlfp:
	;print #, cr, lf for ctlx, ctlu, ctlr functions
	;then move to strtcol (starting column)
	mvi	c,'#'
	call	conout
	call	crlf
	;column = 0, move to position strtcol
crlfp0:
	lda	column
	lxi	h,strtcol
	cmp	m		;stop when column reaches strtcol
	rnc
	mvi	c,' '		;print blank
	call	conout 
	jmp	crlfp0


crlf:
	;carriage return line feed sequence
	mvi	c,cr
	call	conout
	mvi	c,lf
	jmp	conout
	;ret

print:
	;print message until M(BC) = '$'
	ldax	b		;stop on $
	cpi	'$'
	rz
	;more to print
	inx	b		;char to C
	push	b
	mov	c,a
	call	tabout		;another character printed
	pop	b
	jmp	print

read:	;read to info address (max length, current length, buffer)
	lda	column		;save start for ctl-x, ctl-h
	sta	strtcol 
	lhld	info
	mov	c,m
	inx	h
	push	h
	mvi	b,0
	;B = current buffer length,
	;C = maximum buffer length,
	;HL= next to fill - 1
readnx:
	;read next character, BC, HL active
	push	b		;blen, cmax, HL saved
	push	h
readn0:
	call	conin		;next char in A
	ani	7fh		;mask parity bit
	pop	h		;reactivate counters
	pop	b
	cpi	cr		;end of line?
	jz	readen
	cpi	lf		;also end of line
	jz	readen
	cpi	ctlh		;backspace?
	jnz	noth
	;do we have any characters to back over?
	mov	a,b
	ora	a
	jz	readnx
	;characters remain in buffer, backup one
	dcr	b		;remove one character
	lda	column		;col > 0
	sta	compcol
	;compcol > 0 marks repeat as length compute
	jmp	linelen		;uses same code as repeat
noth:
	;not a backspace
	cpi	rubout		;rubout char?
	jnz	notrub
	;rubout encountered, rubout if possible
	mov	a,b		;skip if len=0
	ora	a
	jz	readnx 
	;buffer has characters, resend last char
	mov	a,m		;A = last char
	dcr	b
	dcx	h
	;blen=blen-1, next to fill - 1 decremented
	jmp	rdech1		;act like this is an echo

notrub:
	;not a rubout character, check end line
	cpi	ctle		;physical end line?
	jnz	note
	;yes, save active counters and force eol
	push	b
	push	h
	call	crlf
	xra	a		;start position = 00
	sta	strtcol
	jmp	readn0		;for another character
note:
	;not end of line, list toggle?
	cpi	ctlp		;skip if not ctlp
	jnz	notp
	;list toggle - change parity
	push	h		;save next to fill - 1
	lxi	h,listcp	;HL=.listcp flag
	mvi	a,1		;True-listcp
	sub	m
	mov	m,a		;listcp = not listcp
	pop	h		;for another char
	jmp	readnx
notp:
	;not a ctlp, line delete?
	cpi	ctlx
	jnz	notx
	pop	h		;discard start position
	;loop while column > strtcol
backx:
	lda	strtcol
	lxi	h,column
	cmp	m		;start again
	jnc	read
	dcr	m		;column = column - 1
	call	backup		;one position
	jmp	backx
notx:
	;not a control x, control u?
	;not control-X, control-U?
	cpi	ctlu		;skip if not
	jnz	notu
	;delete line (ctlu)
	call	crlfp		;physical eol
	pop	h		;discard starting position
	jmp	read		;to start all over
notu:
	;not line delete, repeat line?
	cpi	ctlr
	jnz	notr
linelen:
	;repeat line, or compute line len (ctlh)
	;if compcol > 0
	push	b		;save line length
	call	crlfp 
	pop	b
	pop	h
	push	h
	push	b
	;bcur, cmax active, beginning buff at HL
rep0:
	mov	a,b		;count len to 00
	ora	a
	jz	rep1 
	inx	h		;next to print
	mov	c,m
	dcr	b		;count length down
	push	b
	push	h
	call	ctlout		;character echoed
	pop	h		;recall remaining count
	pop	b
	jmp	rep0		;for the next character
rep1:
	;end of repeat, recall lengths
	;original BC still remains pushed
	push	h		;save next to fill
	lda	compcol		;>0 if computing length
	ora	a
	jz	readn0		;for another char if so
	;column position computed for ctlh
	lxi	h,column	;diff > 0
	sub	m
	sta	compcol		;count down below
	;move back compcol-column spaces
backsp:
	;move back one more space
	call	backup		;one space
	lxi	h,compcol
	dcr	m
	jnz	backsp
	jmp	readn0		;for next character
notr:
	;not a ctlr, place into buffer
rdecho:
	inx	h		;character filled to mem
	mov	m,a
	inr	b		;blen = blen + 1
rdech1:
	;look for a random control character
	push	b		;active values saved
	push	h 
	mov	c,a		;ready to print
	call	ctlout		;may be up-arrow C
	pop	h		;recall char
	pop	b
	mov	a,m
	cpi	ctlc		;set flags for reboot test
	mov	a,b		;move length to A
	jnz	notc		;skip if not a control c
	cpi	1		;control C, must be length 1
	jz	reboot		;reboot if blen = 1
	;length not one, so skip reboot
notc:
	;not reboot, are we at end of buffer?
	cmp	c		;go for another if not
	jc	readnx
readen:
	;end of read operation, store blen
	pop	h		;M(current len) = B
	mov	m,b
	mvi	c,cr		;return carriage
	jmp	conout
	;ret
func1:
	;return console character with echo
	call	conech
	jmp	sta_ret
;
func2	equ	tabout
	;write console character with tab expansion
;
func3:
	;return reader character
	call	readerf
	jmp	sta_ret

;func4:	equated to punchf
	;write punch character

;func5:	equated to listf
	;write list character
	;write to list device

func6:
	;direct console i/o - read if 0ffh
	mov	a,c		;0ffh => 00h, means input mode
	inr	a
	jz	dirinp
	inr	a		;0feH in C for status
	jz	constf
	;direct output function
	jmp	conoutf
dirinp:
	call	constf		;status check
	ora	a		;skip, return 00 if not ready
	jz	retmon
	;character is ready, get it
	call	coninf		;to A
	jmp	sta_ret

func7:
	;return io byte
	lda	ioloc
	jmp	sta_ret

func8:
	;set i/o byte
	lxi	h,ioloc
	mov	m,c
	ret			;jmp goback

func9:
	;write line until $ encountered
	xchg			;was lhld info	
	mov	c,l		;BC=string address
	mov	b,h
	jmp	print		;out to console	

func10	equ	read
	;read a buffered console line

func11:
	;check console status
	call	conbrk
	;(drop through to sta_ret)
sta_ret:
	;store the A register to aret
	sta	aret
func_ret:						;
	ret			;jmp goback (pop stack for non cp/m functions)

setlret1:
	;set lret = 1
	mvi	a,1
	jmp	sta_ret



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
lret	equ	aret	;low(aret)

;*****************************************************************
;*****************************************************************
;**                                                             **
;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
;**                                                             **
;*****************************************************************
;*****************************************************************

dvers	equ	22h	;version 2.2
;	module addresses

;	literal constants
_true	equ	0ffh	;constant true
_false	equ	000h	;constant false
enddir	equ	0ffffh	;end of directory
byte	equ	1	;number of bytes for "byte" type
word	equ	2	;number of bytes for "word" type

;	fixed addresses in low memory
tfcb	equ	005ch	;default fcb location
tbuff	equ	0080h	;default buffer location

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
	lxi	h,selerr


goerr:
	;HL = .errorhandler, call subroutine
	mov	e,m		;address of routine in DE
	inx	h
	mov	d,m
	xchg			;to subroutine
	pchl



;	local subroutines for bios interface

move:
	;move data length of length C from source DE to
	;destination given by HL
	inr	c		;in case it is zero
move0:
	dcr	c		;more to move
	rz
	ldax	d		;one byte moved
	mov	m,a
	inx	d		;to next byte
	inx	h
	jmp	move0

selectdisk:
	;select the disk drive given by curdsk, and fill
	;the base addresses curtrka - alloca, then fill
	;the values of the disk parameter block
	lda	curdsk		;current disk# to c
	mov	c,a
	;lsb of e = 0 if not yet logged - in
	call	seldskf		;HL filled by call
	;HL = 0000 if error, otherwise disk headers
	mov	a,h		;return with 0000 in HL and z flag
	ora	l
	rz
	;disk header block address in hl
	mov	e,m		;DE=.tran
	inx	h
	mov	d,m
	inx	h
	shld	cdrmaxa		;.cdrmax
	inx	h
	inx	h
	shld	curtrka		;HL=.currec
	inx	h
	inx	h
	shld	curreca		;HL=.buffa
	inx	h
	inx	h
	;DE still contains .tran
	xchg			;.tran vector
	shld	tranv
	lxi	h,buffa		;DE= source for move, HL=dest
	mvi	c,addlist	;addlist filled
	call	move
	;now fill the disk parameter block
	lhld	dpbaddr		;DE is source
	xchg
	lxi	h,sectpt	;HL is destination
	mvi	c,dpblist	;data filled
	call	move
	;now set single/double map mode
	lhld	maxall		;largest allocation number
	mov	a,h		;00 indicates < 255
	lxi	h,single	;assume a=00
	mvi	m,_true
	ora	a
	jz	retselect
	;high order of maxall not zero, use double dm
	mvi	m,_false
retselect:
	mvi	a,_true		;select disk function ok
	ora	a
	ret

home:
	;move to home position, then offset to start of dir
	call	homef		;move to track 00, sector 00 reference
	;lxi h,offset ;mov c,m ;inx h ;mov b,m ;call settrkf
	;first directory position selected
	xra	a		;constant zero to accumulator
	lhld	curtrka		;curtrk=0000
	mov	m,a
	inx	h
	mov	m,a
	lhld	curreca		;currec=0000
	mov	m,a
	inx	h
	mov	m,a
	;curtrk, currec both set to 0000
	ret

rdbuff:
	;read buffer and check condition
	call	readf		;current drive, track, sector, dma
	jmp	diocomp		;check for i/o errors

wrbuff:
	;write buffer and check condition
	;write type (wrtype) is in register C
	;wrtype = 0 => normal write operation
	;wrtype = 1 => directory write operation
	;wrtype = 2 => start of new block
	call	writef		;current drive, track, sector, dma
diocomp:	;check for disk errors
	ora	a
	rz
	lxi	h,pererr
	jmp	goerr

seek_dir:
	;seek the record containing the current dir entry
	lhld	dcnt		;directory counter to HL
	mvi	c,dskshf	;value to HL
	call	hlrotr
	shld	arecord		;ready for seek
	shld	drec
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
	lxi	h,arecord
	mov	c,m		; % c = arecl
	inx	h
	mov	b,m		; % b = arech
	lhld	curreca
	mov	e,m		; % e = crecl
	inx	h
	mov	d,m		; % d = crech
	lhld	curtrka
	mov	a,m
	inx	h
	mov	h,m		; % h = ctrkh
	mov	l,a		; % l = ctrkl
	;loop while arecord < currec
seek0:
	mov	a,c		; % c = arecl
	sub	e		; % e = crecl
	mov	a,b		; % b = arech
	sbb	d		; % d = crech
	jnc	seek1		;skip if arecord >= currec
	;currec = currec - sectpt
	push	h		; % h = ctrkh
	lhld	sectpt
	mov	a,e		; % e = crecl
	sub	l
	mov	e,a		; % e = crecl
	mov	a,d		; % d = crech
	sbb	h
	mov	d,a		; % d = crech
	pop	h		; % h = ctrkh
	;curtrk = curtrk - 1
	dcx	h		; % h = ctrkh
	jmp	seek0 ;for another try
seek1:
	;look while arecord >= (t:=currec + sectpt)
	push	h		; % h = ctrkh
	lhld	sectpt		;HL = currec+sectpt
	dad	d		; % d = crech
	jc	seek2		;can be > FFFFH	
	mov	a,c		; % c = arecl
	sub	l		; % l = tcrecl
	mov	a,b		; % b = arech
	sbb	h		; % h = tcrech
	jc	seek2		;skip if t > arecord
	;currec = t
	xchg
	;curtrk = curtrk + 1
	pop	h		; % h = ctrkh
	inx	h		; % h = ctrkh
	jmp	seek1		;for another try
seek2:	pop	h		; % h = ctrkh
	;arrive here with updated values in each register
	push	b		;to stack for later ; % b = arech
	push	d		; % d = crech
	push	h		; % h = ctrkh
	;stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
	xchg			;HL = curtrk+offset
	lhld	offset
	dad	d
	mov	b,h		;track set up
	mov	c,l
	call	settrkf
	;note that BC - curtrk is difference to move in bios
	pop	d		;recall curtrk
	lhld	curtrka		;curtrk updated
	mov	m,e
	inx	h
	mov	m,d
	;now compute sector as arecord-currec
	pop	d		;recall currec ; % d = crech
	lhld	curreca
	mov	m,e		; % e = crecl
	inx	h
	mov	m,d		; % d = crech
	pop	b		;BC=arecord, DE=currec ; % b = arech
	mov	a,c		; % c = arecl
	sub	e		; % e = crecl
	mov	c,a		; % c = arecl
	mov	a,b		; % b = arech
	sbb	d		; % d = crech
	mov	b,a		; % b = arech
	lhld	tranv		;BC=sector#, DE=.tran
	xchg
	call	sectran		;HL = tran(sector)
	mov	c,l		;BC = tran(sector)
	mov	b,h
	jmp	setsecf		;sector selected
	;ret

;	file control block (fcb) constants
empty	equ	0e5h	;empty directory entry
lstrec	equ	127	;last record# in extent
recsiz	equ	128	;record size
fcblen	equ	32	;file control block size
dirrec	equ	recsiz/fcblen	;directory elts / record
dskshf	equ	2	;log2(dirrec)
dskmsk	equ	dirrec-1
fcbshf	equ	5	;log2(fcblen)

extnum	equ	12	;extent number field
maxext	equ	31	;largest extent number
ubytes	equ	13	;unfilled bytes field
modnum	equ	14	;data module number
maxmod	equ	15	;largest module number
fwfmsk	equ	80h	;file write flag is high order modnum
namlen	equ	15	;name length
reccnt	equ	15	;record count field
dskmap	equ	16	;disk map field
lstfcb	equ	fcblen-1
nxtrec	equ	fcblen
ranrec	equ	nxtrec+1;random record field (2 bytes)

;	reserved file indicators
rofile	equ	9	;high order of first type char
invis	equ	10	;invisible file in dir command
;	equ	11	;reserved

;	utility functions for file access

dm_position:
	;compute disk map position for vrecord to HL
	lxi	h,blkshf	;shift count to C
	mov	c,m
	lda	vrecord		;current virtual record to A
dmpos0:
	ora	a
	rar
	dcr	c
	jnz	dmpos0
	;A = shr(vrecord,blkshf) = vrecord/2**(sect/block)
	mov	b,a		;save it for later addition
	mvi	a,8		;8-blkshf to accumulator
	sub	m
	mov	c,a		;extent shift count in register c
	lda	extval		;extent value ani extmsk
dmpos1:
	;blkshf = 3,4,5,6,7, C=5,4,3,2,1
	;shift is 4,3,2,1,0
	dcr	c
	jz	dmpos2
	ora	a
	ral
	jmp	dmpos1
dmpos2:
	;arrive here with A = shl(ext and extmsk,7-blkshf)
	add	b		;add the previous shr(vrecord,blkshf) value
	;A is one of the following values, depending upon alloc
	;bks blkshf
	;1k   3     v/8 + extval * 16
	;2k   4     v/16+ extval * 8
	;4k   5     v/32+ extval * 4
	;8k   6     v/64+ extval * 2
	;16k  7     v/128+extval * 1
	ret		;with dm_position in A

getdm:
	;return disk map value from position given by BC
	lhld	info		;base address of file control block
	lxi	d,dskmap	;HL =.diskmap
	dad	d
	dad	b		;index by a single byte value
	lda	single		;single byte/map entry?
	ora	a		;get disk map single byte
	jz	getdmd
	mov	l,m		;with HL=00bb
	mvi	h,0
	ret
getdmd:
	dad	b		;HL=.fcb(dm+i*2)
	;double precision value returned
	mov	e,m
	inx	h
	mov	d,m
	xchg
	ret

index:
	;compute disk block number from current fcb
	call	dm_position	;0...15 in register A
	mov	c,a		;value to HL
	mvi	b,0
	call	getdm
	shld	arecord
	ret

allocated:
	;called following index to see if block allocated
	lhld	arecord
	mov	a,l
	ora	h
	ret

atran:
	;compute actual record address, assuming index called
	lda	blkshf		;shift count to reg A
	lhld	arecord
atran0:
	dad	h		;shl(arecord,blkshf)
	dcr	a
	jnz	atran0
	shld	arecord1	;save shifted block #  
	lda	blkmsk		;mask value to C
	mov	c,a
	lda	vrecord		;masked value in A
	ana	c
	ora	l		;to HL
	mov	l,a
	shld	arecord		;arecord=HL or (vrecord and blkmsk)
	ret

getexta:
	;get current extent field address to A
	lhld	info		;HL=.fcb(extnum)
	lxi	d,extnum
	dad	d
	ret

getfcba:
	;compute reccnt and nxtrec addresses for get/setfcb
	lhld	info		;DE=.fcb(reccnt)
	lxi	d,reccnt
	dad	d
	xchg
	lxi	h,(nxtrec-reccnt)	;HL=.fcb(nxtrec)
	dad	d
	ret

getfcb:
	;set variables from currently addressed fcb
	call	getfcba		;addresses in DE, HL
	mov	a,m		;vrecord=fcb(nxtrec)
	sta	vrecord
	xchg			;rcount=fcb(reccnt)
	mov	a,m
	sta	rcount
	call	getexta		;HL=.fcb(extnum)
	lda	extmsk		;extent mask to a
	ana	m		;fcb(extnum) and extmsk
	sta	extval
	ret

setfcb:
	;place values back into current fcb
	call	getfcba		;addresses to DE, HL
	lda	seqio
	cpi	02		;check ranfill
	jnz	setfcb1
	xra	a
setfcb1:
 	mov	c,a		;=1 if sequential i/o
	lda	vrecord		;fcb(nxtrec)=vrecord+seqio
	add	c
	mov	m,a
	xchg			;fcb(reccnt)=rcount
	lda	rcount
	mov	m,a
	ret

hlrotr:
	;hl rotate right by amount C
	inr	c		;in case zero
hlrotr0:
	dcr	c		;return when zero
	rz
	mov	a,h		;high byte
	ora	a
	rar
	mov	h,a
	mov	a,l		;low byte
	rar
	mov	l,a
	jmp	hlrotr0


compute_cs:
	;compute checksum for current directory buffer
	mvi	c,recsiz	;size of directory buffer
	lhld	buffa		;current directory buffer
	xra	a		;clear checksum value
computecs0:
	add	m		;cs=cs+buff(recsiz-C)
	inx	h
	dcr	c
	jnz	computecs0
	ret			;with checksum in A

hlrotl:
	;rotate the mask in HL by amount in C
	inr	c		;may be zero
hlrotl0:
	dcr	c		;return if zero
	rz
	dad	h
	jmp	hlrotl0

set_cdisk:
	;set a "1" value in curdsk position of BC
	push	b		;save input parameter
	lda	curdsk		;ready parameter for shift
	mov	c,a
	lxi	h,1		;number to shift
	call	hlrotl		;HL = mask to integrate
	pop	b		;original mask
	mov	a,c
	ora	l
	mov	l,a
	mov	a,b		;HL = mask or rol(1,curdsk)
	ora	h
	mov	h,a
	ret

nowrite:
	;return true if dir checksum difference occurred
	lhld	rodsk
	lda	curdsk
	mov	c,a
	call	hlrotr
	mov	a,l		;non zero if nowrite
	ani	1b
	ret

set_ro:
	;set current disk to read only
	lxi	h,rodsk
	mov	c,m
	inx	h
	mov	b,m
	call	set_cdisk	;sets bit to 1
	shld	rodsk
	;high water mark in directory goes to max
	lhld	dirmax		;DE = directory max
	inx	h
	xchg
	lhld	cdrmaxa		;HL = .cdrmax
	mov	m,e		;cdrmax = dirmax
	inx	h
	mov	m,d
	ret

check_rodir:
	;check current directory element for read/only status
	call	getdptra	;address of element
						
check_rofile:
	;check current buff(dptr) or fcb(0) for r/o status
	lxi	d,rofile	;offset to ro bit
	dad	d
	mov	a,m		;return if not set
	ral
	rnc
	lxi	h,roferr
	jmp	goerr
;	jmp rof_error ;exit to read only disk message


check_write:
	;check for write protected disk
	call	nowrite		;ok to write if not rodsk
	rz
	lxi	h,roderr
	jmp	goerr
;	jmp rod_error ;read only disk error

getdptra:
	;compute the address of a directory element at
	;positon dptr in the buffer
	lhld	buffa
	lda	dptr
addh:
	;HL = HL + A
	add	l
	mov	l,a
	rnc
	;overflow to H
	inr	h
	ret


getmodnum:
	;compute the address of the module number 
	;bring module number to accumulator
	;(high order bit is fwf (file write flag)
	lhld	info		;HL=.fcb(modnum)
	lxi	d,modnum
	dad	d
	mov	a,m		;A=fcb(modnum)
	ret

clrmodnum:
	;clear the module number field for user open/make
	call	getmodnum	;fcb(modnum)=0
	mvi	m,0
	ret

setfwf:
	call	getmodnum	;HL=.fcb(modnum), A=fcb(modnum)
	;set fwf (file write flag) to "1"
	ori	fwfmsk		;fcb(modnum)=fcb(modnum) or 80h
	mov	m,a
	;also returns non zero in accumulator
	ret


compcdr:
	;return cy if cdrmax > dcnt
	lhld	dcnt		;DE = directory counter
	xchg
	lhld	cdrmaxa		;HL=.cdrmax
	mov	a,e		;low(dcnt) - low(cdrmax)
	sub	m
	inx	h		;HL = .cdrmax+1
	mov	a,d		;hig(dcnt) - hig(cdrmax)
	sbb	m
	;condition dcnt - cdrmax  produces cy if cdrmax>dcnt
	ret

setcdr:
	;if not (cdrmax > dcnt) then cdrmax = dcnt+1
	call	compcdr
	rc			;return if cdrmax > dcnt
	;otherwise, HL = .cdrmax+1, DE = dcnt
	inx	d
	mov	m,d
	dcx	h
	mov	m,e
	ret

subdh:
	;compute HL = DE - HL
	mov	a,e
	sub	l
	mov	l,a
	mov	a,d
	sbb	h
	mov	h,a
	ret

newchecksum:
	mvi	c,_true		;drop through to compute new checksum
checksum:
	;compute current checksum record and update the
	;directory element if C=true, or check for = if not
	;drec < chksiz?
	lhld	drec		;DE-HL
	xchg
	lhld	chksiz
	call	subdh
	rnc			;skip checksum if past checksum vector size
	;drec < chksiz, so continue
	push	b		;save init flag
	call	compute_cs	;check sum value to A
	lhld	checka		;address of check sum vector
	xchg
	lhld	drec		;value of drec
	dad	d		;HL = .check(drec)
	pop	b		;recall true=0ffh or false=00 to C
	inr	c		;0ffh produces zero flag
	jz	initial_cs
	;not initializing, compare
	cmp	m		;compute_cs=check(drec)?
	rz			;no message if ok
	;checksum error, are we beyond
	;the end of the disk?
	call	compcdr
	rnc			;no message if so
	call	set_ro		;read/only disk set
	ret
initial_cs:
	;initializing the checksum
	mov	m,a
	ret


wrdir:
	;write the current directory entry, set checksum
	call	newchecksum	;initialize entry
	call	setdir		;directory dma
	mvi	c,1		;indicates a write directory operation
	call	wrbuff		;write the buffer
        jmp	setdata		;to data dma address
	;ret

rd_dir:
	;read a directory entry into the directory buffer
	call	setdir		;directory dma
	call	rdbuff		;directory record loaded
        ; jmp setdata to data dma address    
	;ret

setdata:
	;set data dma address
	lxi	h,dmaad		;to complete the call
	jmp	setdma

setdir:
	;set directory dma address
	lxi	h,buffa		;jmp setdma to complete call     

setdma:
	;HL=.dma address to set (i.e., buffa or dmaad)
	mov	c,m		;parameter ready
	inx	h
	mov	b,m
	jmp	setdmaf


dir_to_user:
	;copy the directory entry to the user buffer
	;after call to search or searchn by user code
	lhld	buffa		;source is directory buffer
	xchg
	lhld	dmaad		;destination is user dma address
	mvi	c,recsiz	;copy entire record
	jmp	move
	;ret

end_of_dir:
	;return zero flag if at end of directory, non zero
	;if not at end (end of dir if dcnt = 0ffffh)
	lxi	h,dcnt		;may be 0ffh
	mov	a,m
	inx	h		;low(dcnt) = high(dcnt)?
	cmp	m
	rnz			;non zero returned if different
	;high and low the same, = 0ffh?
	inr	a		;0ffh becomes 00 if so
	ret

set_end_dir:
	;set dcnt to the end of the directory
	lxi	h,enddir
	shld	dcnt
	ret

read_dir:
	;read next directory entry, with C=true if initializing
	lhld	dirmax		;in preparation for subtract
	xchg
	lhld	dcnt		;dcnt=dcnt+1
	inx	h
	shld	dcnt
	;continue while dirmax >= dcnt (dirmax-dcnt no cy)
	call	subdh		;DE-HL
	jnc	read_dir0
	;yes, set dcnt to end of directory
	jmp	set_end_dir
;		ret
read_dir0:
	;not at end of directory, seek next element
	;initialization flag is in C
	lda	dcnt		;low(dcnt) and dskmsk
	ani	dskmsk
	mvi	b,fcbshf	;to multiply by fcb size
read_dir1:
	add	a
	dcr	b
	jnz	read_dir1
	;A = (low(dcnt) and dskmsk) shl fcbshf
	sta	dptr		;ready for next dir operation
	ora	a		;return if not a new record
	rnz
	push	b		;save initialization flag C
	call	seek_dir	;seek proper record
	call	rd_dir		;read the directory record
	pop	b		;recall initialization flag
	jmp	checksum	;checksum the directory elt
	;ret


getallocbit:
	;given allocation vector position BC, return with byte
	;containing BC shifted so that the least significant
	;bit is in the low order accumulator position.  HL is
	;the address of the byte for possible replacement in
	;memory upon return, and D contains the number of shifts
	;required to place the returned value back into position
	mov	a,c
	ani	111b
	inr	a
	mov	e,a
	mov	d,a
	;d and e both contain the number of bit positions to shift
	mov	a,c		;C shr 3 to C
	rrc
	rrc
	rrc
	ani	11111b
	mov	c,a
	mov	a,b		;B shl 5
	add	a
	add	a
	add	a
	add	a
	add	a
	ora	c		;bbbccccc to C
	mov	c,a
	mov	a,b		;BC shr 3 to BC
	rrc
	rrc
	rrc
	ani	11111b
	mov	b,a
	lhld	alloca		;base address of allocation vector
	dad	b		;byte to A, hl = .alloc(BC shr 3)
	mov	a,m
	;now move the bit to the low order position of A
rotl:	rlc
	dcr	e
	jnz	rotl
	ret


set_alloc_bit:
	;BC is the bit position of ALLOC to set or reset.  The
	;value of the bit is in register E.
	push	d		;shifted val A, count in D
	call	getallocbit
	ani	11111110b	;mask low bit to zero (may be set)
	pop	b		;low bit of C is masked into A
	ora	c
;	jmp rotr ;to rotate back into proper position	
	;ret
rotr:
	;byte value from ALLOC is in register A, with shift count
	;in register C (to place bit back into position), and
	;target ALLOC position in registers HL, rotate and replace
	rrc			;back into position
	dcr	d
	jnz	rotr 
	mov	m,a		;back to ALLOC
	ret

scandm:
	;scan the disk map addressed by dptr for non-zero
	;entries, the allocation vector entry corresponding
	;to a non-zero entry is set to the value of C (0,1)
	call	getdptra	;HL = buffa + dptr
	;HL addresses the beginning of the directory entry
	lxi	d,dskmap	;hl now addresses the disk map
	dad	d
	push	b		;save the 0/1 bit to set
	mvi	c,fcblen-dskmap+1	;size of single byte disk map + 1
scandm0:
	;loop once for each disk map entry
	pop	d		;recall bit parity
	dcr	c		;all done scanning?
	rz
	;no, get next entry for scan
	push	d		;replace bit parity
	lda	single
	ora	a
	jz	scandm1
	;single byte scan operation
	push	b		;save counter
	push	h		;save map address
	mov	c,m		;BC=block#
	mvi	b,0
	jmp	scandm2
scandm1:
	;double byte scan operation
	dcr	c		;count for double byte
	push	b		;save counter
	mov	c,m		;BC=block#
	inx	h
	mov	b,m
	push	h		;save map address
scandm2:
	;arrive here with BC=block#, E=0/1
	mov	a,c		;skip if = 0000
	ora	b
	jz	scanm3
	lhld	maxall		;check invalid index
	mov	a,l		;maxall - block#
	sub	c
	mov	a,h
	sbb	b
	cnc	set_alloc_bit
	;bit set to 0/1
scanm3:
	pop	h		;to next bit position
	inx	h
	pop	b		;recall counter
	jmp	scandm0		;for another item

initialize:
	;initialize the current disk
	;lret = false ;set to true if $ file exists
	;compute the length of the allocation vector - 2
	lhld	maxall		;perform maxall/8
	mvi	c,3
	;number of bytes in alloc vector is (maxall/8)+1
	call	hlrotr		;HL = maxall/8+1
	inx	h
	mov	b,h		;count down BC til zero
	mov	c,l
	lhld	alloca		;base of allocation vector
	;fill the allocation vector with zeros
initial0:
	mvi	m,0		;alloc(i)=0
	inx	h
	dcx	b		;count length down
	mov	a,b
	ora	c
	jnz	initial0
	;set the reserved space for the directory
	lhld	dirblk
	xchg
	lhld	alloca		;HL=.alloc()
	mov	m,e		;sets reserved directory blks
	inx	h
	mov	m,d
	;allocation vector initialized, home disk
	call	home
        ;cdrmax = 3 (scans at least one directory record)
	lhld	cdrmaxa
	mvi	m,3
	inx	h
	mvi	m,0
	;cdrmax = 0000
	call	set_end_dir	;dcnt = enddir
	;read directory entries and check for allocated storage
initial2:
	mvi	c,_true
	call	read_dir
	call	end_of_dir	;return if end of directory
	rz
	;not end of directory, valid entry?
	call	getdptra	;HL = buffa + dptr
	mvi	a,empty
	cmp	m
	jz	initial2	;go get another item
	;not empty, user code the same?
	lda	usrcode
	cmp	m
	jnz	pdollar
	;same user code, check for '$' submit
	inx	h		;first character
	mov	a,m
	sui	'$'		;dollar file?
	jnz	pdollar
	;dollar file found, mark in lret
	dcr	a		;lret = 255
	sta	lret
pdollar:
	;now scan the disk map for allocated blocks
	mvi	c,1		;set to allocated
	call	scandm
	call	setcdr		;set cdrmax to dcnt
	jmp	initial2	;for another entry

copy_dirloc:
	;copy directory location to lret following
	;delete, rename, ... ops
	lda	dirloc
	jmp	sta_ret
;	ret

compext:
	;compare extent# in A with that in C, return nonzero
	;if they do not match
	push	b		;save C's original value
	push	psw
	lda	extmsk
	cma
	mov	b,a
	;B has negated form of extent mask
	mov	a,c		;low bits removed from C
	ana	b
	mov	c,a
	pop	psw		;low bits removed from A
	ana	b
	sub	c		;set flags
	ani	maxext
	pop	b		;restore original values
	ret

search:
	;search for directory element of length C at info
	mvi	a,0ffh		;changed if actually found
	sta	dirloc
	lxi	h,searchl	;searchl = C
	mov	m,c
	lhld	info		;searcha = info
	shld	searcha
	call	set_end_dir	;dcnt = enddir
	call	home		;to start at the beginning
	;(drop through to searchn)

searchn:
	;search for the next directory element, assuming
	;a previous call on search which sets searcha and
	;searchl
	mvi	c,_false	;read next dir element
	call	read_dir
	call	end_of_dir	;skip to end if so
	jz	search_fin
	;not end of directory, scan for match
	lhld	searcha		;DE=beginning of user fcb
	xchg
	ldax	d		;first character
	cpi	empty		;keep scanning if empty
	jz	searchnext
	;not empty, may be end of logical directory
	push	d		;save search address
	call	compcdr		;past logical end?
	pop	d		;recall address
	jnc	search_fin	;artificial stop
searchnext:
	call	getdptra	;HL = buffa+dptr
	lda	searchl		;length of search to c
	mov	c,a
	mvi	b,0		;b counts up, c counts down
searchloop:
	mov	a,c
	ora	a
	jz	endsearch
	ldax	d		;? matches all
	cpi	'?'
	jz	searchok
	;scan next character if not ubytes
	mov	a,b
	cpi	ubytes
	jz	searchok
	;not the ubytes field, extent field?
	cpi	extnum		;may be extent field
	ldax	d		;fcb character
	jz	searchext	;skip to search extent
	sub	m		;mask-out flags/extent modulus
	ani	7fh
	jnz	searchn		;skip if not matched
	jmp	searchok	;matched character
searchext:
	;A has fcb character
	;attempt an extent # match
	push	b		;save counters
	mov	c,m		;directory character to c
	call	compext		;compare user/dir char
	pop	b		;recall counters
	jnz	searchn		;skip if no match
searchok:
	;current character matches
	inx	d
	inx	h
	inr	b
	dcr	c
	jmp	searchloop
endsearch:
	;entire name matches, return dir position
	lda	dcnt
	ani	dskmsk
	sta	lret
	;lret = low(dcnt) and 11b
	lxi	h,dirloc	;dirloc=0ffh?
	mov	a,m
	ral
	rnc
	;yes, change it to 0 to mark as found
	xra	a		;dirloc=0
	mov	m,a
	ret
search_fin:
	;end of directory, or empty name
	call	set_end_dir	;may be artifical end
	mvi	a,255
	jmp	sta_ret


delete:
	;delete the currently addressed file
	call	check_write	;write protected?
	mvi	c,extnum	;search through file type
	call	search
delete0:
	;loop while directory matches
	call	end_of_dir	;stop if end
	rz
	;set each non zero disk map entry to 0
	;in the allocation vector
	;may be r/o file
	call	check_rodir	;ro disk error if found
	call	getdptra	;HL=.buff(dptr)
	mvi	m,empty
	mvi	c,0		;alloc elts set to 0
	call	scandm
	call	wrdir		;write the directory
	call	searchn		;to next element
	jmp	delete0		;for another record

get_block:
	;given allocation vector position BC, find the zero bit
	;closest to this position by searching left and right.
	;if found, set the bit to one and return the bit position
	;in hl.  if not found (i.e., we pass 0 on the left, or
	;maxall on the right), return 0000 in hl
	mov	d,b		;copy of starting position to de
	mov	e,c
lefttst:
	mov	a,c		;skip if left=0000
	ora	b
	jz	righttst
	;left not at position zero, bit zero?
	dcx	b		;left,right pushed
	push	d
	push	b
	call	getallocbit
	rar			;return block number if zero
	jnc	retblock
	;bit is one, so try the right
	pop	b		;left, right restored
	pop	d
righttst:
	lhld	maxall		;value of maximum allocation#
	mov	a,e		;right=maxall?
	sub	l
	mov	a,d
	sbb	h
	jnc	retblock0	;return block 0000 if so
	inx	d		;left, right pushed
	push	b
	push	d
	mov	b,d		;ready right for call
	mov	c,e
	call	getallocbit
	rar			;return block number if zero
	jnc	retblock
	pop	d		;restore left and right pointers
	pop	b
	jmp	lefttst ;for another attempt
retblock:
	ral			;bit back into position and set to 1
	inr	a
	;d contains the number of shifts required to reposition
	call	rotr		;move bit back to position and store
	pop	h		;HL returned value, DE discarded
	pop	d
	ret
retblock0:
	;cannot find an available bit, return 0000
	mov	a,c		;
	ora	b		;also at beginning
	jnz	lefttst
	lxi	h,0000h
	ret

copy_fcb:
	;copy the entire file control block
	mvi	c,0		;start at 0, to fcblen-1
	mvi	e,fcblen
	;	jmp copy_dir

copy_dir:
	;copy fcb information starting at C for E bytes
	;into the currently addressed directory entry
	push	d		;save length for later
	mvi	b,0		;double index to BC
	lhld	info		;HL = source for data
	dad	b		;DE=.fcb(C), source for copy
	xchg
	call	getdptra	;HL=.buff(dptr), destination
	pop	b		;DE=source, HL=dest, C=length
	call	move		;data moved
seek_copy:
	;enter from close to seek and copy current element
	call	seek_dir	;to the directory element
	jmp	wrdir		;write the directory element
	;ret


rename:
	;rename the file described by the first half of
	;the currently addressed file control block. the
	;new name is contained in the last half of the
	;currently addressed file conrol block.  the file
	;name and type are changed, but the reel number
	;is ignored.  the user number is identical
	call	check_write	;may be write protected
	;search up to the extent field
	mvi	c,extnum
	call	search
	;copy position 0
	lhld	info		;HL=.fcb(0), A=fcb(0)
	mov	a,m
	lxi	d,dskmap	;HL=.fcb(dskmap)
	dad	d
	mov	m,a		;fcb(dskmap)=fcb(0)
	;assume the same disk drive for new named file
rename0:
	call	end_of_dir	;stop at end of dir
	rz
	;not end of directory, rename next element
	call	check_rodir	;may be read-only file
	mvi	c,dskmap
	mvi	e,extnum
	call	copy_dir
	;element renamed, move to next
	call	searchn
	jmp	rename0

indicators:
	;set file indicators for current fcb
	mvi	c,extnum	;through file type
	call	search
indic0:
	call	end_of_dir	;stop at end of dir
	rz
	;not end of directory, continue to change
	mvi	c,0		;copy name
	mvi	e,extnum
	call	copy_dir
	call	searchn
	jmp	indic0

open:
	;search for the directory entry, copy to fcb
	mvi	c,namlen
	call	search
	call	end_of_dir	;return with lret=255 if end
	rz
	;not end of directory, copy fcb information
open_copy:
	;(referenced below to copy fcb info)
	call	getexta		;save extent#
	mov	a,m
	push	psw
	push	h
	call	getdptra	;DE = .buff(dptr)
	xchg
	lhld	info		;HL=.fcb(0)
	mvi	c,nxtrec	;length of move operation
	push	d		;save .buff(dptr)
	call	move		;from .buff(dptr) to .fcb(0)
	;note that entire fcb is copied, including indicators
	call	setfwf		;sets file write flag
	pop	d		;HL=.buff(dptr+extnum)
	lxi	h,extnum
	dad	d
	mov	c,m		;C = directory extent number
	lxi	h,reccnt	;HL=.buff(dptr+reccnt)
	dad	d
	mov	b,m		;B holds directory record count
	pop	h		;restore extent number
	pop	psw
	mov	m,a
	;HL = .user extent#, B = dir rec cnt, C = dir extent#
	;if user ext < dir ext then user := 128 records
	;if user ext = dir ext then user := dir records
	;if user ext > dir ext then user := 0 records
	mov	a,c		;ready dir reccnt
	cmp	m
	mov	a,b
	jz	open_rcnt	;if same, user gets dir reccnt
	mvi	a,0		;user is larger
	jc	open_rcnt
	mvi	a,128		;directory is larger
open_rcnt: ;A has record count to fill
	lhld	info
	lxi	d,reccnt
	dad	d
	mov	m,a
	ret

mergezero:
	;HL = .fcb1(i), DE = .fcb2(i),
	;if fcb1(i) = 0 then fcb1(i) := fcb2(i)
	mov	a,m		;return if = 0000
	inx	h
	ora	m
	dcx	h
	rnz
	ldax	d		;low byte copied
	mov	m,a
	inx	d
	inx	h
	ldax	d		;back to input form
	mov	m,a
	dcx	d
	dcx	h
	ret

close:
	;locate the directory element and re-write it
	xra	a
	sta	lret
	sta	dcnt
	sta	dcnt+1
	call	nowrite		;skip close if r/o disk
	rnz
	;check file write flag - 0 indicates written
	call	getmodnum	;fcb(modnum) in A
	ani	fwfmsk		;return if bit remains set
	rnz
	mvi	c,namlen	;locate file
	call	search
	call	end_of_dir	;return if not found
	rz
	;merge the disk map at info with that at buff(dptr)
	lxi	b,dskmap
	call	getdptra
	dad	b		;DE is .buff(dptr+16)
	xchg
	lhld	info		;DE=.buff(dptr+16), HL=.fcb(16)
	dad	b
	mvi	c,(fcblen-dskmap)	;length of single byte dm
merge0:
	lda	single		;skip to double
	ora	a
	jz	merged
	;this is a single byte map
	;if fcb(i) = 0 then fcb(i) = buff(i)
	;if buff(i) = 0 then buff(i) = fcb(i)
	;if fcb(i) <> buff(i) then error
	mov	a,m
	ora	a
	ldax	d
	jnz	fcbnzero
	;fcb(i) = 0
	mov	m,a		;fcb(i) = buff(i)
fcbnzero:
	ora	a
	jnz	buffnzero
	;buff(i) = 0
	mov	a,m		;buff(i)=fcb(i)
	stax	d
buffnzero:
	cmp	m		;fcb(i) = buff(i)?
	jnz	mergerr
	jmp	dmset		;if merge ok
merged:
	;this is a double byte merge operation
	call	mergezero	;buff = fcb if buff 0000
	xchg			;fcb = buff if fcb 0000
	call	mergezero
	xchg
	;they should be identical at this point
	ldax	d		;low same?
	cmp	m
	jnz	mergerr
	inx	d		;to high byte
	inx	h
	ldax	d		;high same?
	cmp	m
	jnz	mergerr
	;merge operation ok for this pair
	dcr	c		;extra count for double byte
dmset:
	inx	d		;to next byte position
	inx	h
	dcr	c		;for more
	jnz	merge0
	;end of disk map merge, check record count
	;DE = .buff(dptr)+32, HL = .fcb(32)
	lxi	b,-(fcblen-extnum)
	dad	b
	xchg
	dad	b
	;DE = .fcb(extnum), HL = .buff(dptr+extnum)
	ldax	d		;current user extent number
	;if fcb(ext) >= buff(fcb) then
	;buff(ext) := fcb(ext), buff(rec) := fcb(rec)
	cmp	m
	jc	endmerge
	;fcb extent number >= dir extent number
	mov	m,a		;buff(ext) = fcb(ext)
	;update directory record count field
	lxi	b,(reccnt-extnum)
	dad	b
	xchg
	dad	b
	;DE=.buff(reccnt), HL=.fcb(reccnt)
	mov	a,m		;buff(reccnt)=fcb(reccnt)
	stax	d
endmerge:
	mvi	a,_true		;mark as copied
	sta	fcb_copied
	jmp	seek_copy	;ok to "wrdir" here - 1.4 compat
	;		ret
mergerr:
	;elements did not merge correctly
	lxi	h,lret		;=255 non zero flag set
	dcr	m
	ret

make:
	;create a new file by creating a directory entry
	;then opening the file
	call	check_write	;may be write protected
	lhld	info		;save fcb address, look for e5
	push	h
	lxi	h,efcb		;info = .empty
	shld	info
	mvi	c,1		;length 1 match on empty entry
	call	search
	call	end_of_dir	;zero flag set if no space
	pop	h		;recall info address
	shld	info		;in case we return here
	rz			;return with error condition 255 if not found
	xchg			;DE = info address
	;clear the remainder of the fcb
	lxi	h,namlen	;HL=.fcb(namlen)
	dad	d
	mvi	c,fcblen-namlen	;number of bytes to fill
	xra	a		;clear accumulator to 00 for fill
make0:
	mov	m,a
	inx	h
	dcr	c
	jnz	make0
	lxi	h,ubytes	;HL = .fcb(ubytes)
	dad	d
	mov	m,a		;fcb(ubytes) = 0
	call	setcdr		;may have extended the directory
	;now copy entry to the directory
	call	copy_fcb
	;and set the file write flag to "1"
	jmp	setfwf
	;ret

open_reel:
	;close the current extent, and open the next one
	;if possible.  RMF is true if in read mode
	xra	a		;set true if actually copied
	sta	fcb_copied
	call	close		;close current extent
	;lret remains at enddir if we cannot open the next ext
	call	end_of_dir	;return if end
	rz
	;increment extent number
	lhld	info		;HL=.fcb(extnum)
	lxi	b,extnum
	dad	b
	mov	a,m		;fcb(extnum)=++1
	inr	a
	ani	maxext
	mov	m,a
	jz	open_mod	;move to next module if zero
	;may be in the same extent group
	mov	b,a
	lda	extmsk
	ana	b
	;if result is zero, then not in the same group
	lxi	h,fcb_copied	;true if the fcb was copied to directory
	ana	m		;produces a 00 in accumulator if not written
	jz	open_reel0	;go to next physical extent
	;result is non zero, so we must be in same logical ext
	jmp	open_reel1	;to copy fcb information
open_mod:
	;extent number overflow, go to next module
	lxi	b,(modnum-extnum)	;HL=.fcb(modnum)
	dad	b
	inr	m		;fcb(modnum)=++1
	;module number incremented, check for overflow
	mov	a,m		;mask high order bits
	ani	maxmod
	jz	open_r_err	;cannot overflow to zero
	;otherwise, ok to continue with new module
open_reel0:
	mvi	c,namlen	;next extent found?
	call	search
	call	end_of_dir
	jnz	open_reel1
	;end of file encountered
	lda	rmf		;0ffh becomes 00 if read
	inr	a
	jz	open_r_err	;sets lret = 1
	;try to extend the current file
	call	make
	;cannot be end of directory
	call	end_of_dir
	jz	open_r_err	;with lret = 1
	jmp	open_reel2
open_reel1:
	;not end of file, open
	call	open_copy
open_reel2:
	call	getfcb		;set parameters
	xra	a		;lret = 0
	jmp	sta_ret
;	ret ;with lret = 0
open_r_err:
	;cannot move to next extent of this file
	call	setlret1	;lret = 1
	jmp	setfwf		;ensure that it will not be closed
	;ret

seqdiskread:
	;sequential disk read operation
	mvi	a,1
	sta	seqio
	;drop through to diskread

diskread:	;(may enter from seqdiskread)
	mvi	a,_true		;read mode flag = true (open_reel)
	sta	rmf
	;read the next record from the current fcb
	call	getfcb		;sets parameters for the read
	lda	vrecord		;vrecord-rcount
	lxi	h,rcount
	cmp	m
	;skip if rcount > vrecord
	jc	recordok
	;not enough records in the extent
	;record count must be 128 to continue
	cpi	128		;vrecord = 128?
	jnz	diskeof		;skip if vrecord<>128
	call	open_reel	;go to next extent if so
	xra	a		;vrecord=00
	sta	vrecord
	;now check for open ok
	lda	lret		;stop at eof
	ora	a
	jnz	diskeof
recordok:
	;arrive with fcb addressing a record to read
	call	index
	;error 2 if reading unwritten data
	;(returns 1 to be compatible with 1.4)
	call	allocated	;arecord=0000?
	jz	diskeof
	;record has been allocated, read it
	call	atran		;arecord now a disk address
	call	seek		;to proper track,sector
	call	rdbuff		;to dma address
	jmp	setfcb		;replace parameter	
;		ret
diskeof:
	jmp	setlret1	;lret = 1
	;ret

seqdiskwrite:
	;sequential disk write
	mvi	a,1
	sta	seqio
	;drop through to diskwrite

diskwrite:	;(may enter here from seqdiskwrite above)
	mvi	a,_false	;read mode flag
	sta	rmf
	;write record to currently selected file
	call	check_write	;in case write protected
	lhld	info		;HL = .fcb(0)
	call	check_rofile	;may be a read-only file
	call	getfcb		;to set local parameters
	lda	vrecord		;vrecord-128
	cpi	lstrec+1
	;skip if vrecord > lstrec
	;vrecord = 128, cannot open next extent
	jnc	setlret1	;lret=1
diskwr0:
	;can write the next record, so continue
	call	index
	call	allocated
	mvi	c,0		;marked as normal write operation for wrbuff
	jnz	diskwr1
	;not allocated
	;the argument to getblock is the starting
	;position for the disk search, and should be
	;the last allocated block for this file, or
	;the value 0 if no space has been allocated
	call	dm_position
	sta	dminx		;save for later
	lxi	b,0000h		;may use block zero
	ora	a		;skip if no previous block
	jz	nopblock
	;previous block exists at A
	mov	c,a		;previous block # in BC
	dcx	b
	call	getdm		;previous block # to HL
	mov	b,h		;BC=prev block#
	mov	c,l
nopblock:
	;BC = 0000, or previous block #
	call	get_block	;block # to HL
	;arrive here with block# or zero
	mov	a,l
	ora	h
	jnz	blockok
	;cannot find a block to allocate
	mvi	a,2	 	;lret=2
	jmp	sta_ret
blockok:
	;allocated block number is in HL
	shld	arecord
	xchg			;block number to DE
	lhld	info		;HL=.fcb(dskmap)
	lxi	b,dskmap
	dad	b
	lda	single		;set flags for single byte dm
	ora	a
	lda	dminx		;recall dm index
	jz	allocwd		;skip if allocating word
	;allocating a byte value
	call	addh		;single byte alloc
	mov	m,e
	jmp	diskwru		;to continue
allocwd:
	;allocate a word value
	mov	c,a		;double(dminx)
	mvi	b,0
	dad	b		;HL=.fcb(dminx*2)
	dad	b
	mov	m,e		;double wd
	inx	h
	mov	m,d
diskwru:
	;disk write to previously unallocated block
	mvi	c,2		;marked as unallocated write
diskwr1:
	;continue the write operation of no allocation error
	;C = 0 if normal write, 2 if to prev unalloc block
	lda	lret		;stop if non zero returned value
	ora	a
	rnz
	push	b		;save write flag
	call	atran		;arecord set
	lda	seqio
	dcr	a
	dcr	a
	jnz	diskwr11
	pop	b
	push	b
	mov	a,c
	dcr	a
	dcr	a
	jnz	diskwr11	;old allocation  
	push	h		;arecord in hl ret from atran
	lhld	buffa		;zero buffa & fill
	mov	d,a
fill0:  mov	m,a
	inx	h
	inr	d
	jp	fill0
	call	setdir
	lhld	arecord1
	mvi	c,2
fill1:  shld	arecord
	push	b
	call	seek
	pop	b
	call	wrbuff		;write fill record
	lhld	arecord		;restore last record     
	mvi	c,0		;change  allocate flag   
	lda	blkmsk
	mov	b,a
	ana	l
	cmp	b
	inx	h
	jnz	fill1		;cont until cluster is zeroed
	pop	h
	shld	arecord
	call	setdata
diskwr11:
	call seek ;to proper file position
	pop	b		;restore/save write flag (C=2 if new block)
	push	b
	call	wrbuff		;written to disk
	pop	b		;C = 2 if a new block was allocated, 0 if not
	;increment record count if rcount<=vrecord
	lda	vrecord		;vrecord-rcount
	lxi	h,rcount
	cmp	m
	jc	diskwr2
	;rcount <= vrecord
	mov	m,a		;rcount = vrecord+1
	inr	m
	mvi	c,2		;mark as record count incremented
diskwr2:
	if	patch1
	; CP/M V2.2 patch 1 for use of optional blocking/deblocking
	nop
	nop
	lxi	h,0
	else
	; original code
	;A has vrecord, C=2 if new block or new record#
	dcr	c
	dcr	c
	jnz	noupdate
	endif
	push	psw		;save vrecord value
	call	getmodnum	;HL=.fcb(modnum), A=fcb(modnum)
	;reset the file write flag to mark as written fcb
	ani	(~fwfmsk)&0ffh	;bit reset
	mov	m,a		;fcb(modnum) = fcb(modnum) and 7fh
	pop	psw		;restore vrecord
noupdate:
	;check for end of extent, if found attempt to open
	;next extent in preparation for next write
	cpi	lstrec		;vrecord=lstrec?
	jnz	diskwr3		;skip if not
	;may be random access write, if so we are done
	;change next     
	lda	seqio		;skip next extent open op
	cpi	1
	jnz	diskwr3
	;update current fcb before going to next extent
	call	setfcb
	call	open_reel	;rmf=false
	;vrecord remains at lstrec causing eof if
	;no more directory space is available
	lxi	h,lret
	mov	a,m
	ora	a
	jnz	nospace
	;space available, set vrecord=255
	dcr	a		;goes to 00 next time
	sta	vrecord
nospace:
	mvi	m,0		;lret = 00 for returned value
diskwr3:
	jmp	setfcb ;replace parameters
	;ret

rseek:
	;random access seek operation, C=0ffh if read mode
	;fcb is assumed to address an active file control block
	;(modnum has been set to 1100_0000b if previous bad seek)
	xra	a		;marked as random access operation
	sta	seqio
rseek1:
	push	b		;save r/w flag
	lhld	info		;DE will hold base of fcb
	xchg
	lxi	h,ranrec	;HL=.fcb(ranrec)
	dad	d
	mov	a,m		;record number
	ani	7fh
	push	psw
	mov	a,m		;cy=lsb of extent#
	ral
	inx	h		;A=ext#
	mov	a,m
	ral
	ani	11111b
	mov	c,a		;C holds extent number, record stacked
	mov	a,m		;mod#
	rar
	rar
	rar
	rar
	ani	1111b
	mov	b,a		;B holds module#, C holds ext#
	pop	psw		;recall sought record #
	;check to insure that high byte of ran rec = 00
	inx	h		;l=high byte (must be 00)
	mov	l,m
	inr	l		;zero flag, l=6
	dcr	l
	mvi	l,6
	;produce error 6, seek past physical eod
	jnz	seekerr
	;otherwise, high byte = 0, A = sought record
	lxi	h,nxtrec	;HL = .fcb(nxtrec)
	dad	d
	mov	m,a		;sought rec# stored away
	;arrive here with B=mod#, C=ext#, DE=.fcb, rec stored
	;the r/w flag is still stacked.  compare fcb values
	lxi	h,extnum	;A=seek ext#
	dad	d
	mov	a,c
	sub	m		;tests for = extents
	jnz	ranclose
	;extents match, check mod#
	lxi	h,modnum	;B=seek mod#
	dad	d
	mov	a,b
	;could be overflow at eof, producing module#
	;of 90H or 10H, so compare all but fwf
	sub	m		;same?
	ani	7fh
	jz	seekok
ranclose:
	push	b		;save seek mod#,ext#, .fcb
	push	d
	call	close		;current extent closed
	pop	d		;recall parameters and fill
	pop	b
	mvi	l,3		;cannot close error #3
	lda	lret
	inr	a
	jz	badseek
	lxi	h,extnum	;fcb(extnum)=ext#
	dad	d
	mov	m,c
	lxi	h,modnum	;fcb(modnum)=mod#
	dad	d
	mov	m,b
	call	open		;is the file present?
	lda	lret		;open successful?
	inr	a
	jnz	seekok
	;cannot open the file, read mode?
	pop	b		;r/w flag to c (=0ffh if read)
	push	b		;everyone expects this item stacked
	mvi	l,4		;seek to unwritten extent #4
	inr	c		;becomes 00 if read operation
	jz	badseek		;skip to error if read operation
	;write operation, make new extent
	call	make
	mvi	l,5		 ;cannot create new extent #5
	lda	lret		;no dir space
	inr	a
	jz	badseek
	;file make operation successful
seekok:
	pop	b		;discard r/w flag
	xra	a		;with zero set
	jmp	sta_ret
badseek:
	;fcb no longer contains a valid fcb, mark
	;with 1100_000b in modnum field so that it
	;appears as overflow with file write flag set
	push	h		;save error flag
	call	getmodnum	;HL = .modnum
	mvi	m,11000000b
	pop	h		;and drop through
seekerr:
	pop	b		;discard r/w flag
	mov	a,l		;lret=#, nonzero
	sta	lret
	;setfwf returns non-zero accumulator for err
	jmp	setfwf		;flag set, so subsequent close ok
	;ret

randiskread:
	;random disk read operation
	mvi	c,_true		;marked as read operation
	call	rseek
	cz	diskread	;if seek successful
	ret

randiskwrite:
	;random disk write operation
	mvi	c,_false	;marked as write operation
	call	rseek
	cz	diskwrite	;if seek successful
	ret

compute_rr:
	;compute random record position for getfilesize/setrandom
	xchg
	dad	d
	;DE=.buf(dptr) or .fcb(0), HL = .f(nxtrec/reccnt)
	mov	c,m		;BC = 0000 0000 ?rrr rrrr
	mvi	b,0
	lxi	h,extnum	;A=e000 0000
	dad	d
	mov	a,m
	rrc
	ani	80h
	add	c
	mov	c,a
	mvi	a,0
	adc	b
	mov	b,a
	;BC = 0000 000? errrr rrrr
	mov	a,m
	rrc
	ani	0fh
	add	b
	mov	b,a
	;BC = 000? eeee errrr rrrr
	lxi	h,modnum	;A=XXX? mmmm
	dad	d
	mov	a,m
	add	a		;cy=? A=mmmm 0000
	add	a
	add	a
	add	a
	push	psw
	add	b
	mov	b,a
	;cy=?, BC = mmmm eeee errr rrrr
	push	psw		;possible second carry
	pop	h		;cy = lsb of L
	mov	a,l		;cy = lsb of A
	pop	h		;cy = lsb of L
	ora	l		;cy/cy = lsb of A
	ani	1		;A = 0000 000? possible carry-out
	ret

getfilesize:
	;compute logical file size for current fcb
	mvi	c,extnum
	call	search
	;zero the receiving ranrec field
	lhld	info		;save position
	lxi	d,ranrec
	dad	d
	push	h
	mov	m,d		;=00 00 00
	inx	h
	mov	m,d
	inx	h
	mov	m,d
getsize:
	call	end_of_dir
	jz	setsize
	;current fcb addressed by dptr
	call	getdptra	;ready for compute size
	lxi	d,reccnt
	call	compute_rr
	;A=0000 000? BC = mmmm eeee errr rrrr
	;compare with memory, larger?
	pop	h		;recall, replace .fcb(ranrec)
	push	h
	mov	e,a		;save cy
	mov	a,c		;ls byte
	sub	m
	inx	h
	mov	a,b		;middle byte
	sbb	m
	inx	h
	mov	a,e		;carry if .fcb(ranrec) > directory
	sbb	m
	jc	getnextsize	;for another try
	;fcb is less or equal, fill from directory
	mov	m,e
	dcx	h
	mov	m,b
	dcx	h
	mov	m,c
getnextsize:
	call	searchn
	jmp	getsize
setsize:
	pop	h		;discard .fcb(ranrec)
	ret

setrandom:
	;set random record from the current file control block
	lhld	info		;ready params for computesize
	lxi	d,nxtrec
	call	compute_rr	;DE=info, A=cy, BC=mmmm eeee errr rrrr
	lxi	h,ranrec	;HL = .fcb(ranrec)
	dad	d
	mov	m,c		;to ranrec
	inx	h
	mov	m,b
	inx	h
	mov	m,a
	ret

select:
	;select disk info for subsequent input or output ops
	lhld	dlog
	lda	curdsk
	mov	c,a
	call	hlrotr
	push	h		;save it for test below, send to seldsk
	xchg
	call	selectdisk	;recall dlog vector
	pop	h
	cz	sel_error	;returns true if select ok
	;is the disk logged in?
	mov	a,l		;return if bit is set
	rar
	rc
	;disk not logged in, set bit and initialize
	lhld	dlog		;call ready
	mov	c,l
	mov	b,h
	call	set_cdisk	;dlog=set_cdisk(dlog)
	shld	dlog
	jmp	initialize
	;ret

curselect:
	lda	linfo		;skip if linfo=curdsk
	lxi	h,curdsk
	cmp	m
	rz
	mov	m,a		;curdsk=info
	jmp	select
	;ret

reselect:
	;check current fcb to see if reselection necessary
	mvi	a,_true		;mark possible reselect
	sta	resel
	lhld	info		;drive select code
	mov	a,m
	ani	11111b		;non zero is auto drive select
	dcr	a		;drive code normalized to 0..30, or 255
	sta	linfo		;save drive code
	cpi	30
	jnc	noselect
	;auto select function, save curdsk
	lda	curdsk		;olddsk=curdsk
	sta	olddsk
	mov	a,m		;save drive code
	sta	fcbdsk
	ani	11100000b	;preserve hi bits
	mov	m,a
	call	curselect
noselect:
	;set user code
	lda	usrcode		;0...31
	lhld	info
	ora	m
	mov	m,a
	ret

;	individual function handlers
func12:
	;return version number
	mvi	a,dvers		;lret = dvers (high = 00)
	jmp	sta_ret
;	ret ;jmp goback

func13:
	;reset disk system - initialize to disk 0
	lxi	h,0
	shld	rodsk
	shld	dlog
	xra	a		;note that usrcode remains unchanged
	sta	curdsk
	lxi	h,tbuff		;dmaad = tbuff
	shld	dmaad
        call	setdata		;to data dma address
	jmp	select
	;ret ;jmp goback

func14	equ	curselect
	;select disk info
	;ret ;jmp goback

func15:
	;open file
	call	clrmodnum	;clear the module number
	call	reselect
	jmp	open
	;ret ;jmp goback

func16:
	;close file
	call	reselect
	jmp	close
	;ret ;jmp goback

func17:
	;search for first occurrence of a file
	mvi	c,0		;length assuming '?' true
	xchg			;was lhld info		
	mov	a,m		;no reselect if ?
	cpi	'?'
	jz	qselect		;skip reselect if so
	;normal search
	call	getexta
	mov	a,m
	cpi	'?'
	cnz	clrmodnum	;module number zeroed
	call	reselect
	mvi	c,namlen
qselect:
	call	search
	jmp	dir_to_user	;copy directory entry to user
	;ret ;jmp goback

func18:
	;search for next occurrence of a file name
	lhld	searcha
	shld	info
	call	reselect
	call	searchn
	jmp	dir_to_user	;copy directory entry to user
	;ret ;jmp goback

func19:
	;delete a file
	call	reselect
	call	delete
	jmp	copy_dirloc
	;ret ;jmp goback

func20:
	;read a file
	call	reselect
	jmp	seqdiskread				;
	 ;jmp goback

func21:
	;write a file
	call	reselect
	jmp	seqdiskwrite			;
	 ;jmp goback

func22:
	;make a file
	call	clrmodnum
	call	reselect
	jmp	make
	;ret ;jmp goback

func23:
	;rename a file
	call	reselect
	call	rename
	jmp	copy_dirloc
	;ret ;jmp goback

func24:
	;return the login vector
	lhld	dlog
	jmp	sthl_ret			;
;	ret ;jmp goback

func25:
	;return selected disk number
	lda	curdsk
	jmp	sta_ret
;	ret ;jmp goback

func26:
	;set the subsequent dma address to info
	xchg			;was lhld info	
	shld	dmaad		;dmaad = info
        jmp	setdata		;to data dma address
	;ret ;jmp goback

func27:
	;return the login vector address
	lhld	alloca
	jmp	sthl_ret
;	ret ;jmp goback

func28	equ	set_ro
	;write protect current disk
	;ret ;jmp goback

func29:
	;return r/o bit vector
	lhld	rodsk
	jmp	sthl_ret
;	ret ;jmp goback

func30:
	;set file indicators
	call	reselect
	call	indicators
	jmp	copy_dirloc		;lret=dirloc
	;ret ;jmp goback

func31:
	;return address of disk parameter block
	lhld	dpbaddr
sthl_ret:
 	shld	aret
	ret			;jmp goback
func32:
	;set user code
        lda	linfo
	cpi	0ffh
	jnz	setusrcode
	;interrogate user code instead
	lda	usrcode		;lret=usrcode
	jmp	sta_ret
;		ret ;jmp goback
setusrcode:
	ani	1fh
	sta	usrcode
	ret			;jmp goback
;
func33:
	;random disk read operation
	call	reselect
	jmp	randiskread	;to perform the disk read
	;ret ;jmp goback
;
func34:
	;random disk write operation
	call	 reselect
	jmp	randiskwrite	;to perform the disk write
	;ret ;jmp goback
;
func35:
	;return file size (0-65536)
	call	reselect
	jmp	getfilesize
	;ret ;jmp goback
;
func36	equ	setrandom			;
	;set random record
	;ret ;jmp goback
func37:
;
	lhld	info
	mov	a,l
	cma
	mov	e,a
	mov	a,h
	cma
	lhld	dlog
	ana	h
	mov	d,a
	mov	a,l
	ana	e
	mov	e,a
	lhld	rodsk
	xchg
	shld	dlog
	mov	a,l
	ana	e
	mov	l,a
	mov	a,h
	ana	d
	mov	h,a
	shld	rodsk
	ret
;
;
goback:
	;arrive here at end of processing to return to user
	lda	resel
	ora	a
	jz	retmon
	;reselection may have taken place
	lhld	info		;fcb(0)=0
	mvi	m,0
	lda	fcbdsk
	ora	a
	jz	retmon
	;restore disk number
	mov	m,a		;fcb(0)=fcbdsk
	lda	olddsk
	sta	linfo
	call	curselect
;
;	return from the disk monitor
retmon:
	lhld	entsp		;user stack restored
	sphl
	lhld	aret		;BA = HL = aret
	mov	a,l
	mov	b,h
	ret

func38	equ	func_ret
func39	equ	func_ret

func40:
	;random disk write with zero fill of unallocated block
	call	reselect
	mvi	a,2
	sta	seqio
	mvi	c,_false
	call	rseek1
	cz	diskwrite	;if seek successful
	ret


;	data areas

;	initialized data
efcb:	db	empty	;0e5=available dir entry
rodsk:	dw	0	;read only disk vector
dlog:	dw	0	;logged-in disks
dmaad:	dw	tbuff	;initial dma address

;	curtrka - alloca are set upon disk select
;	(data must be adjacent, do not insert variables)
;	(address of translate vector, not used)
cdrmaxa:ds	word	;pointer to cur dir max value
curtrka:ds	word	;current track address
curreca:ds	word	;current record address
buffa:	ds	word	;pointer to directory dma address
dpbaddr:ds	word	;current disk parameter block address
checka:	ds	word	;current checksum vector address
alloca:	ds	word	;current allocation vector address
addlist	equ	$-buffa	;address list size

;	sectpt - offset obtained from disk parm block at dpbaddr
;	(data must be adjacent, do not insert variables)
sectpt:	ds	word	;sectors per track
blkshf:	ds	byte	;block shift factor
blkmsk:	ds	byte	;block mask
extmsk:	ds	byte	;extent mask
maxall:	ds	word	;maximum allocation number
dirmax:	ds	word	;largest directory number
dirblk:	ds	word	;reserved allocation bits for directory
chksiz:	ds	word	;size of checksum vector
offset:	ds	word	;offset tracks at beginning
dpblist	equ	$-sectpt	;size of area

;	local variables
tranv:	ds	word	;address of translate vector
fcb_copied:
	ds	byte	;set true if copy_fcb called
rmf:	ds	byte	;read mode flag for open_reel
dirloc:	ds	byte	;directory flag in rename, etc.
seqio:	ds	byte	;1 if sequential i/o
linfo:	ds	byte	;low(info)
dminx:	ds	byte	;local for diskwrite
searchl:ds	byte	;search length
searcha:ds	word	;search address
tinfo:	ds	word	;temp for info in "make"
single:	ds	byte	;set true if single byte allocation map
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

bios	equ	($ & 0ff00h)+100h	;next module
	end
