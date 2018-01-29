; Reformatted and converted for cross-assembly by Macro Assembler AS
; Eric Smith <spacewar@gmail.com> 2018-01-24
; from original source os2ccp.asm from
;   http://www.cpm.z80.de/download/cpm2-plm.zip

; Changes:
;   multiple instructions per line split to separate lines
;   dollar sign in labels replaced by underscore
;   dollar sign (as digit separator) in binary constants removed
;   single quotes around strings replaced with double quotes
;   true and false replaced with _true and _false
;   replaced "not" operator with "~"
;   removed empty comments
;   added ifdef origin to allow origin to be specified from command line
;   added commments about serial number
;   added ifdefs on "noserial" to omit serialization check


	.cpu	8080

	title	"console command processor (CCP), ver 2.0"
;	assembly language version of the CP/M console command processor

;	version 2.2 February, 1980

;	Copyright (c) 1976, 1977, 1978, 1979, 1980
;	Digital Research
;	Box 579, Pacific Grove,
;	California, 93950

_false	equ	0000h
_true	equ	~_false
testing	equ	_false	;true if debugging


	ifdef	origin
	org	origin
bdosl	equ	$+800h		;bdos location
	else
	if	testing
	org	3400h
bdosl	equ	$+800h		;bdos location
	else
	org	000h
bdosl	equ	$+800h		;bdos location
	endif
	endif
tran	equ	100h
tranm	equ	$
ccploc	equ	$

;	********************************************************
;	*	Base of CCP contains the following code/data   *
;	*	ccp:	jmp ccpstart	(start with command)   *
;	*		jmp ccpclear    (start, clear command) *
;	*	ccp+6	127		(max command length)   *
;	*	ccp+7	comlen		(command length = 00)  *
;	*	ccp+8	' ... '		(16 blanks)	       *
;	********************************************************
;	* Normal entry is at ccp, where the command line given *
;	* at ccp+8 is executed automatically (normally a null  *
;	* command with comlen = 00).  An initializing program  *
;	* can be automatically loaded by storing the command   *
;	* at ccp+8, with the command length at ccp+7.  In this *
;	* case, the ccp executes the command before prompting  *
;	* the console for input.  Note that the command is exe-*
;	* cuted on both warm and cold starts.  When the command*
;	* line is initialized, a jump to "jmp ccpclear" dis-   *
;	* ables the automatic command execution.               *
;	********************************************************

	jmp	ccpstart	;start ccp with possible initial command
	jmp	ccpclear	;clear the command buffer
maxlen:	db	127	;max buffer length
comlen:	db	0	;command length (filled in by dos)
;	(command executed initially if comlen non zero)
combuf:
	db	"        "	;8 character fill
	db	"        "	;8 character fill
	db	"COPYRIGHT (C) 1979, DIGITAL RESEARCH  "; 38
	ds	128-($-combuf)
;	total buffer length is 128 characters
comaddr:dw	combuf	;address of next to char to scan
staddr:	ds	2	;starting address of current fillfcb request

diska	equ	0004h	;disk address for current disk
bdos	equ	0005h	;primary bdos entry point
buff	equ	0080h	;default buffer
fcb	equ	005ch	;default file control block

rcharf	equ	1	;read character function
pcharf	equ	2	;print character function
pbuff	equ	9	;print buffer function
rbuff	equ	10	;read buffer function
breakf	equ	11	;break key function
liftf	equ	12	;lift head function (no operation)
initf	equ	13	;initialize bdos function
self	equ	14	;select disk function
openf	equ	15	;open file function
closef	equ	16	;close file function
searf	equ	17	;search for file function
searnf	equ	18	;search for next file function
delf	equ	19	;delete file function
dreadf	equ	20	;disk read function
dwritf	equ	21	;disk write function
makef	equ	22	;file make function
renf	equ	23	;rename file function
logf	equ	24	;return login vector
cself	equ	25	;return currently selected drive number
dmaf	equ	26	;set dma address
userf	equ	32	;set user number

;	special fcb flags
rofile	equ	9	;read only file
sysfile	equ	10	;system file flag

;	special characters
cr	equ	13	;carriage return
lf	equ	10	;line feed
la	equ	5fh	;left arrow
eofile	equ	1ah	;end of file

;	utility procedures
printchar:
	mov	e,a
	mvi	c,pcharf
	jmp	bdos

printbc:
	;print character, but save b,c registers
	push	b
	call	printchar
	pop	b
	ret

crlf:
	mvi	a,cr
	call	printbc
	mvi	a,lf
	jmp	printbc

blank:
	mvi	a,' '
	jmp	printbc

print:	;print string starting at b,c until next 00 entry
	push	b		;now print the string
	call	crlf
	pop	h
prin0:	mov	a,m		;stop on 00
	ora	a
	rz
	inx	h		;ready for next
	push	h
	call	printchar	;character printed
	pop	h
	jmp	prin0		;for another character

initialize:
	mvi	c,initf
	jmp	bdos

select:
	mov	e,a
	mvi	c,self
	jmp	bdos

bdos_inr:
	call	bdos
	sta	dcnt
	inr	a
	ret

open:	;open the file given by d,e
	mvi	c,openf
	jmp	bdos_inr

openc:	;open comfcb
	xra	a		;clear next record to read
	sta	comrec
	lxi	d,comfcb
	jmp	open

close:	;close the file given by d,e
	mvi	c,closef
	jmp	bdos_inr

search:	;search for the file given by d,e
	mvi	c,searf
	jmp	bdos_inr

searchn:
	;search for the next occurrence of the file given by d,e
	mvi	c,searnf
	jmp	bdos_inr

searchcom:
	;search for comfcb file
	lxi	d,comfcb
	jmp	search

delete:	;delete the file given by d,e
	mvi	c,delf
	jmp	bdos

bdos_cond:
	call	bdos
	ora	a
	ret

diskread:
	;read the next record from the file given by d,e
	mvi	c,dreadf
	jmp	bdos_cond

diskreadc:
	;read the comfcb file
	lxi	d,comfcb
	jmp	diskread

diskwrite:
	;write the next record to the file given by d,e
	mvi	c,dwritf
	jmp	bdos_cond

make:	;create the file given by d,e
	mvi	c,makef
	jmp	bdos_inr

renam:	;rename the file given by d,e
	mvi	c,renf
	jmp	bdos

getuser:
	;return current user code in a
	mvi	e,0ffh		;drop through to setuser

setuser:
        mvi	c,userf		;sets user number
	jmp	bdos

saveuser:
	;save user#/disk# before possible ^c or transient
	call	getuser		;code to a
	add	a		;rot left
	add	a
	add	a
	add	a
	lxi	h,cdisk		;4b=user, 4b=disk
	ora	m
	sta	diska		;stored away in memory for later
	ret

setdiska:
	lda	cdisk		;user/disk
	sta	diska
	ret

translate:
	;translate character in register A to upper case
	cpi	61h		;return if below lower case a
	rc
	cpi	7bh		;return if above lower case z
	rnc
	ani	5fh		;translated to upper case
	ret

readcom:
	;read the next command into the command buffer
	;check for submit file
	lda	submit
	ora	a
	jz	nosub
	;scanning a submit file
	;change drives to open and read the file
	lda	cdisk
	ora	a
	mvi	a,0
	cnz	select
	;have to open again in case xsub present
	lxi	d,subfcb	;skip if no sub
	call	open
	jz	nosub
	lda	subrc		;read last record(s) first
	dcr	a
	sta	subcr		;current record to read
	lxi	d,subfcb	;end of file if last record
	call	diskread
	jnz	nosub
	;disk read is ok, transfer to combuf
	lxi	d,comlen
	lxi	h,buff
	mvi	b,128
	call	move0
	;line is transferred, close the file with a
	;deleted record
	lxi	h,submod	;clear fwflag
	mvi	m,0
	inx	h		;one less record
	dcr	m
	lxi	d,subfcb
	call	close
	jz	nosub
	;close went ok, return to original drive
	lda	cdisk
	ora	a
	cnz	select
	;print to the 00
	lxi	h,combuf
	call	prin0
	call	break_key
	jz	noread
	call	del_sub		;break key depressed
	jmp	ccp

nosub:	;no submit file
	call	del_sub
	;translate to upper case, store zero at end
	call	saveuser	;user # save in case control c
	mvi	c,rbuff
	lxi	d,maxlen
	call	bdos
	call	setdiska	;no control c, so restore diska
noread:	;enter here from submit file
	;set the last character to zero for later scans
	lxi	h,comlen	;length is in b
	mov	b,m
readcom0:
	inx	h		;end of scan?
	mov	a,b
	ora	a
	jz	readcom1	;get character and translate
	mov	a,m
	call	translate
	mov	m,a
	dcr	b
	jmp	readcom0

readcom1: ;end of scan, h,l address end of command
	mov	m,a		;store a zero
	lxi	h,combuf	;ready to scan to zero
	shld	comaddr
	ret

break_key:
	;check for a character ready at the console
	mvi	c,breakf
	call	bdos
	ora	a
	rz
	mvi	c,rcharf	;character cleared
	call	bdos
	ora	a
	ret

cselect:
	;get the currently selected drive number to reg-A
	mvi	c,cself
	jmp	bdos

setdmabuff:
	;set default buffer dma address
	lxi	d,buff		;(drop through)

setdma:
	;set dma address to d,e
	mvi	c,dmaf
	jmp	bdos

del_sub:
	;delete the submit file, and set submit flag to false
	lxi	h,submit	;return if no sub file
	mov	a,m
	ora	a
	rz
	mvi	m,0		;submit flag is set to false
	xra	a		;on drive a to erase file
	call	select
	lxi	d,subfcb
	call	delete
	lda	cdisk		;back to original drive
	jmp	select

	ifndef	noserial
serialize:
	;check serialization
	lxi	d,serial	;check six bytes
	lxi	h,bdosl
	mvi	b,6
ser0:	ldax	d
	cmp	m
	jnz	badserial
	inx	d
	inx	h
	dcr	b
	jnz	ser0
	ret			;serial number is ok
	endif

comerr:
	;error in command string starting at position
	;'staddr' and ending with first delimiter
	call	crlf		;space to next line
	lhld	staddr		;h,l address first to print
comerr0: ;print characters until blank or zero
	mov	a,m		; not blank
	cpi	' '
	jz	comerr1
	ora	a		; not zero, so print it
	jz	comerr1
	push	h
	call	printchar
	pop	h
	inx	h
	jmp	comerr0;	for another character
comerr1: ;print question mark,and delete sub file
	mvi	a,'?'
	call	printchar
	call	crlf
	call	del_sub
	jmp	ccp ;restart with next command

; fcb scan and fill subroutine (entry is at fillfcb below)
	;fill the comfcb, indexed by A (0 or 16)
	;subroutines
delim:	;look for a delimiter
	ldax	d		;not the last element
	ora	a
	rz
	cpi	' '		;non graphic
	jc	comerr
	rz			;treat blank as delimiter
	cpi	'='
	rz
	cpi	la		;left arrow
	rz
	cpi	'.'
	rz
	cpi	':'
	rz
	cpi	';'
	rz
	cpi	'<'
	rz
	cpi	'>'
	rz
	ret			;delimiter not found

deblank: ;deblank the input line
	ldax	d		;treat end of line as blank
	ora	a
	rz
	cpi	' '
	rnz
	inx	d
	jmp	deblank

addh: ;add a to h,l
	add	l
	mov	l,a
	rnc
	inr	h
	ret

fillfcb0:
	;equivalent to fillfcb(0)
	mvi	a,0

fillfcb:
	lxi	h,comfcb	;fcb rescanned at end
	call	addh
	push	h
	push	h
	xra	a		;clear selected disk (in case A:...)
	sta	sdisk
	lhld	comaddr		;command address in d,e
	xchg
	call	deblank		;to first non-blank character
	xchg			;in case of errors
	shld	staddr
	xchg			;d,e has command, h,l has fcb address
	pop	h
	;look for preceding file name A: B: ...
	ldax	d		;use current disk if empty command
	ora	a
	jz	setcur0
	sbi	'A'-1		;disk name held in b if : follows
	mov	b,a
	inx	d		;set disk name if :
	ldax	d
	cpi	':'
	jz	setdsk

setcur: ;set current disk
	dcx	d		;back to first character of command
setcur0:
	lda	cdisk
	mov	m,a
	jmp	setname

setdsk: ;set disk to name in register b
	mov	a,b		;mark as disk selected
	sta	sdisk
	mov	m,b		;past the :
	inx	d

setname: ;set the file name field
	mvi	b,8		;file name length (max)
setnam0:
	call	delim		;not a delimiter
	jz	padname
	inx	h		;must be ?'s
	cpi	'*'
	jnz	setnam1
	mvi	m,'?'		;to dec count
	jmp	setnam2

setnam1:
	mov	m,a		;store character to fcb
	inx	d
setnam2:
	dcr	b		;count down length
	jnz	setnam0

	;end of name, truncate remainder
trname:	call	delim		;set type field if delimiter
	jz	setty
	inx	d
	jmp	trname

padname:
	inx	h
	mvi	m,' '
	dcr	b
	jnz	padname

setty: ;set the type field
	mvi	b,3		;skip the type field if no .
	cpi	'.'
	jnz	padty
	inx	d		;past the ., to the file type field
setty0: ;set the field from the command buffer
	call	delim
	jz	padty
	inx	h
	cpi	'*'
	jnz	setty1
	mvi	m,'?'		;since * specified
	jmp	setty2

setty1: ;not a *, so copy to type field
	mov	m,a
	inx	d
setty2: ;decrement count and go again
	dcr	b
	jnz	setty0

	;end of type field, truncate
trtyp: ;truncate type field
	call	delim
	jz	efill
	inx	d
	jmp	trtyp

padty:	;pad the type field with blanks
	inx	h
	mvi	m,' '
	dcr	b
	jnz	padty

efill: ;end of the filename/filetype fill, save command address
	;fill the remaining fields for the fcb
	mvi	b,3
efill0:	inx h
	mvi	m,0
	dcr	b
	jnz	efill0
	xchg			;set new starting point
	shld	comaddr

	;recover the start address of the fcb and count ?'s
	pop	h		;b=0, c=8+3
	lxi	b,11
scnq:	inx	h
	mov	a,m
	cpi	'?'
	jnz	scnq0
	;? found, count it in b
	inr b
scnq0:	dcr	c
	jnz	scnq

	;number of ?'s in c, move to a and return with flags set
	mov	a,b
	ora	a
	ret

intvec:
	;intrinsic function names (all are four characters)
	db	"DIR "
	db	"ERA "
	db	"TYPE"
	db	"SAVE"
	db	"REN "
        db      "USER"
intlen	equ ($-intvec)/4 ;intrinsic function length
	
; serial number (details not documented in original DRI source file)
serial:	db	0		; OEM number, low byte
	db	0		; CP/M version, 16h = 2.2
	db	0		; OEM number, high byte
	db	0,0,0		; serial number, big-endian


intrinsic:
	;look for intrinsic functions (comfcb has been filled)
	lxi	h,intvec	;c counts intrinsics as scanned
	mvi	c,0
intrin0:
	mov	a,c		;done with scan?
	cpi	intlen
	rnc
	;no, more to scan
	lxi	d,comfcb+1	;beginning of name
	mvi	b,4		;length of match is in b
intrin1:
	ldax	d		;match?
	cmp	m
	jnz	intrin2		;skip if no match
	inx	d
	inx	h
	dcr	b
	jnz	intrin1		;loop while matching

	;complete match on name, check for blank in fcb
	ldax	d		;otherwise matched
	cpi	' '
	jnz	intrin3
	mov	a,c		;with intrinsic number in a
	ret

intrin2: ;mismatch, move to end of intrinsic
	inx	h
	dcr	b
	jnz	intrin2

intrin3: ;try next intrinsic
	inr	c		;to next intrinsic number
	jmp	intrin0		;for another round

ccpclear:
	;clear the command buffer
	xra	a
	sta	comlen
	;drop through to start ccp
ccpstart:
	;enter here from boot loader
	lxi	sp,stack	;save initial disk number
	push	b
        ;(high order 4bits=user code, low 4bits=disk#)
	mov	a,c		;user code
	rar
	rar
	rar
	rar
	ani	0fh
	
	mov	e,a		;user code selected
	call	setuser
	;initialize for this user, get $ flag
        call	initialize	;0ffh in accum if $ file present
        sta	submit		;submit flag set if $ file present
        pop	b		;recall user code and disk number
	mov	a,c		;disk number in accumulator
	ani	0fh
        sta	cdisk		;clears user code nibble
	call	select		;proper disk is selected, now check sub files
	;check for initial command
	lda	comlen		;assume typed already
	ora	a
	jnz	ccp0

ccp:
	;enter here on each command or error condition
	lxi	sp,stack
	call	crlf		;print d> prompt, where d is disk name
	call	cselect		;get current disk number
	adi	'A'
	call	printchar
	mvi	a,'>'
	call	printchar
	call	readcom		;command buffer filled
ccp0:	;(enter here from initialization with command full)
	lxi	d,buff		;default dma address at buff
	call	setdma
	call	cselect		;current disk number saved
	sta	cdisk
	call	fillfcb0	;command fcb filled
	cnz	comerr		;the name cannot be an ambiguous reference
	lda	sdisk
	ora	a
	jnz	userfunc
	;check for an intrinsic function
	call	intrinsic
	lxi	h,jmptab	;index is in the accumulator
	mov	e,a		;index in d,e
	mvi	d,0
	dad	d
	dad	d
	mov	a,m
	inx	h
	mov	h,m
	mov	l,a
	pchl
	;pc changes to the proper intrinsic or user function

jmptab:
	dw	direct	;directory search
	dw	erase	;file erase
	dw	type	;type file
	dw	save	;save memory image
	dw	rename	;file rename
	dw	user	;user number
	dw	userfunc;user-defined function

	ifndef	noserial
badserial:
	LXI	H,76F3H	;'DI HLT' instructions.	
	;typo "lxi h,di or (hlt shl 8)" here originally,
	;corrected by comparing to disassembly of Clark Calkins.
	shld	ccploc
	lxi	h,ccploc
	pchl
	endif

;utility subroutines for intrinsic handlers
readerr:
	;print the read error message
	lxi	b,rdmsg
	jmp	print
rdmsg: db "READ ERROR",0

nofile:
	;print no file message
	lxi	b,nofmsg
	jmp	print
nofmsg: db "NO FILE",0

getnumber: ;read a number from the command line
	call	fillfcb0	;should be number
	lda	sdisk		;cannot be prefixed
	ora	a
	jnz	comerr
	;convert the byte value in comfcb to binary
	lxi	h,comfcb+1	;(b=0, c=11)
	lxi	b,11
	;value accumulated in b, c counts name length to zero
conv0:	mov	a,m
	cpi ' '
	jz	conv1
	;more to scan, convert char to binary and add
	inx	h		;valid?
	sui	'0'
	cpi	10
	jnc	comerr
	mov	d,a		;save value
	mov	a,b		;mult by 10
	ani	11100000b
	jnz	comerr
	mov	a,b		;recover value
	rlc			;*8
	rlc
	rlc
	add	b
	jc	comerr
	add	b		;*8+*2 = *10
	jc	comerr
	add	d		;+digit
	jc	comerr
	mov	b,a		;for another digit
	dcr	c
	jnz	conv0
	ret
conv1:	;end of digits, check for all blanks
	mov	a,m		;blanks?
	cpi	' '
	jnz	comerr
	inx	h
	dcr	c
	jnz	conv1
	mov	a,b		;recover value
	ret

movename:
	;move 3 characters from h,l to d,e addresses
	mvi	b,3
move0:	mov	a,m
	stax	d
	inx	h
	inx	d
	dcr	b
	jnz	move0
	ret

addhcf:	;buff + a + c to h,l followed by fetch
	lxi	h,buff
	add	c
	call	addh
	mov	a,m
	ret

setdisk:
	;change disks for this command, if requested
	xra	a		;clear disk name from fcb
	sta	comfcb
	lda	sdisk		;no action if not specified
	ora	a
	rz
	dcr	a		;already selected
	lxi	h,cdisk
	cmp	m
	rz
	jmp	select

resetdisk:
	;return to original disk after command
	lda	sdisk		;no action if not selected
	ora	a
	rz
	dcr	a		;same disk
	lxi	h,cdisk
	cmp	m
	rz
	lda	cdisk
	jmp	select

;individual intrinsics follow
direct:
	;directory search
	call	fillfcb0	;comfcb gets file name
	call	setdisk		;change disk drives if requested
	lxi	h,comfcb+1	;may be empty request
	mov	a,m
	cpi	' '		;skip fill of ??? if not blank
	jnz	dir1
	;set comfcb to all ??? for current disk
	mvi	b,11		;length of fill ????????.???
dir0:	mvi	m,'?'
	inx	h
	dcr	b
	jnz	dir0
	;not a blank request, must be in comfcb
dir1:	mvi	e,0		;E counts directory entries
	push	d
	call	searchcom	;first one has been found
	cz	nofile		;not found message
dir2:	jz	endir
	;found, but may be system file
	lda	dcnt		;get the location of the element
	rrc
	rrc
	rrc
	ani	1100000b
	mov	c,a
	;c contains base index into buff for dir entry
	mvi	a,sysfile	;value to A
	call	addhcf
	ral			;skip if system file
	jc	dir6
	;c holds index into buffer
	;another fcb found, new line?
	pop	d
	mov	a,e
	inr	e
	push	d
	;e=0,1,2,3,...new line if mod 4 = 0
	ani	11b		;and save the test
	push	psw
	jnz	dirhdr0		;header on current line
	call	crlf
	push	b
	call	cselect
	pop	b
	;current disk in A
	adi	'A'
	call	printbc
	mvi	a,':'
	call	printbc
	jmp	dirhdr1		;skip current line hdr
dirhdr0:
	call	blank		;after last one
	mvi	a,':'
	call	printbc
dirhdr1:
	call	blank
	;compute position of name in buffer
	mvi	b,1		;start with first character of name
dir3:	mov	a,b		;buff+a+c fetched
	call	addhcf
	ani	7fh		;mask flags
	;may delete trailing blanks
	cpi	' '		;check for blank type
	jnz	dir4
	pop	psw		;may be 3rd item
	push	psw
	cpi	3		;place blank at end if not
	jnz	dirb
	mvi	a,9		;first char of type
	call	addhcf
	ani	7fh
	cpi	' '
	jz	dir5
	;not a blank in the file type field
dirb:	mvi	a,' '		;restore trailing filename chr
dir4:
	call	printbc		;char printed
	inr	b
	mov	a,b
	cpi	12
	jnc	dir5
	;check for break between names
	cpi	9		;for another char
	jnz	dir3
	;print a blank between names
	call	blank
	jmp	dir3

dir5:	;end of current entry
	pop	psw		;discard the directory counter (mod 4)
dir6:	call	break_key	;check for interrupt at keyboard
	jnz	endir		;abort directory search
	call	searchn		;for another entry
	jmp	dir2
endir:	;end of directory scan
	pop	d		;discard directory counter
	jmp	retcom


erase:
	call	fillfcb0	;cannot be all ???'s
	cpi	11
	jnz	erasefile
	;erasing all of the disk
	lxi	b,ermsg
	call	print
	call	readcom
	lxi	h,comlen	;bad input
	dcr	m
	jnz	ccp
	inx	h
	mov	a,m
	cpi	'Y'
	jnz	ccp
	;ok, erase the entire diskette
	inx	h		;otherwise error at retcom
	shld	comaddr
erasefile:
	call	setdisk
	lxi	d,comfcb
	call	delete
	inr	a		;255 returned if not found
	cz	nofile		;no file message if so
	jmp	retcom

ermsg:	db	"ALL (Y/N)?",0

type:
	call	fillfcb0	;don't allow ?'s in file name
	jnz	comerr
	call	setdisk		;open the file
	call	openc
	jz	typerr		;zero flag indicates not found
	;file opened, read 'til eof
	call	crlf		;read first buffer
	lxi	h,bptr
	mvi	m,255
type0:	;loop on bptr
	lxi	h,bptr		;end buffer
	mov	a,m
	cpi	128
	jc	type1		;carry if 0,1,...,127
	push	h
	;read another buffer full
	call	diskreadc
	pop	h		;recover address of bptr
	jnz	typeof		;hard end of file
	xra	a		;bptr = 0
	mov	m,a
type1:	;read character at bptr and print
	inr	m		;bptr = bptr + 1
	lxi	h,buff		;h,l addresses char
	call	addh
	mov	a,m
	cpi	eofile
	jz	retcom
	call	printchar
	call	break_key	;abort if break
	jnz	retcom
	jmp	type0 ;for another character

typeof:	;end of file, check for errors
	dcr	a
	jz	retcom
	call	readerr
typerr:	call	resetdisk
	jmp	comerr

save:
	call	getnumber	; value to register a
	push	psw		;save it for later

	;should be followed by a file to save the memory image
	call	fillfcb0
	jnz	comerr		;cannot be ambiguous
	call	setdisk		;may be a disk change
	lxi	d,comfcb	;existing file removed
	push	d
	call	delete
	pop	d
	call	make		;create a new file on disk
	jz	saverr		;no directory space
	xra	a		;clear next record field
	sta	comrec
	pop	psw		;#pages to write is in a, change to #sectors
	mov	l,a
	mvi	h,0
	dad	h 
	lxi	d,tran		;h,l is sector count, d,e is load address
save0:	;check for sector count zero
	mov	a,h		;may be completed
	ora	l
	jz	save1
	dcx	h		;sector count = sector count - 1
	push	h		;save it for next time around
	lxi	h,128		;next dma address saved
	dad	d
	push	h
	call	setdma		;current dma address set
	lxi	d,comfcb
	call	diskwrite
	pop	d		;dma address, sector count
	pop	h
	jnz	saverr		;may be disk full case
	jmp	save0		;for another sector

save1:	;end of dump, close the file
	lxi	d,comfcb
	call	close
	inr	a		;255 becomes 00 if error
	jnz	retsave		;for another command
saverr:	;must be full or read only disk
	lxi	b,fullmsg
	call	print
retsave:
	;reset dma buffer
	call	setdmabuff
	jmp	retcom
fullmsg:
	db "NO SPACE",0


rename:
	;rename a file on a specific disk
	call	fillfcb0	;must be unambiguous
	jnz	comerr
	lda	sdisk		;save for later compare
	push	psw
	call	setdisk		;disk selected
	call	searchcom	;is new name already there?
	jnz	renerr3
	;file doesn't exist, move to second half of fcb
	lxi	h,comfcb
	lxi	d,comfcb+16
	mvi	b,16
	call	move0
	;check for = or left arrow
	lhld	comaddr
	xchg
	call	deblank
	cpi	'='		;ok if =
	jz	ren1
	cpi	la
	jnz	renerr2
ren1:	xchg			;past delimiter
	inx	h
	shld	comaddr
	;proper delimiter found
	call	fillfcb0
	jnz	renerr2
	;check for drive conflict
	pop	psw		;previous drive number
	mov	b,a
	lxi	h,sdisk
	mov	a,m
	ora	a
	jz	ren2
	;drive name was specified.  same one?
	cmp	b
	mov	m,b
	jnz	renerr2
ren2:	mov	m,b		;store the name in case drives switched
	xra	a		;is old file there?
	sta	comfcb
	call	searchcom
	jz	renerr1

	;everything is ok, rename the file
	lxi	d,comfcb
	call	renam
	jmp	retcom

renerr1:; no file on disk
	call	nofile
	jmp	retcom
renerr2:; ambigous reference/name conflict
	call	resetdisk
	jmp	comerr
renerr3:; file already exists
	lxi	b,renmsg
	call	print
	jmp	retcom
renmsg:	db "FILE EXISTS",0

user:
	;set user number
	call	getnumber	; leaves the value in the accumulator
	cpi	16		; must be between 0 and 15
	jnc	comerr
	mov	e,a		;save for setuser call
	lda	comfcb+1
	cpi	' '
	jz	comerr
	call	setuser		;new user number set
	jmp	endcom

userfunc:
	ifndef	noserialize
	call	serialize	;check serialization
	endif
	;load user function and set up for execution
	lda	comfcb+1
	cpi	' '
	jnz	user0
	;no file name, but may be disk switch
	lda	sdisk		;no disk name if 0
	ora	a
	jz	endcom
	dcr	a		;set user/disk
	sta	cdisk
	call	setdiska
	call	select
	jmp	endcom
user0:	;file name is present
	lxi	d,comfcb+9	;type ' '
	ldax	d
	cpi	' '
	jnz	comerr
	push	d		;.com
	call	setdisk
	pop	d
	lxi	h,comtype
	call	movename	;file type is set to .com
	call	openc
	jz	userer
	;file opened properly, read it into memory
	lxi	h,tran		;transient program base
load0:	push	h		;save dma address
	xchg
	call	setdma
	lxi	d,comfcb
	call	diskread
	jnz	load1
	;sector loaded, set new dma address and compare
	pop	h
	lxi	d,128
	dad	d
	lxi	d,tranm		;has the load overflowed?
	mov	a,l
	sub	e
	mov	a,h
	sbb	d
	jnc	loaderr
	jmp	load0		;for another sector

load1:	pop	h		;end file is 1
	dcr	a
	jnz	loaderr
	call	resetdisk	;back to original disk
	call	fillfcb0
	lxi	h,sdisk
	push	h
	mov	a,m		;drive number set
	sta	comfcb
	mvi	a,16		;move entire fcb to memory
	call	fillfcb
	pop	h
	mov	a,m
	sta	comfcb+16
	xra	a		;record number set to zero
	sta	comrec
	lxi	d,fcb
	lxi	h,comfcb
	mvi	b,33
	call	move0
	;move command line to buff
	lxi	h,combuf
bmove0:	mov	a,m
	ora	a
	jz	bmove1
	cpi	' '
	jz	bmove1
	inx	h		;for another scan
	jmp	bmove0
	;first blank position found
bmove1:	mvi	b,0		;ready for the move
	lxi	d,buff+1
bmove2:	mov	a,m
	stax	d
	ora	a
	jz	bmove3
	;more to move
	inr	b
	inx	h
	inx	d
	jmp	bmove2
bmove3:	;b has character count
	mov	a,b
	sta	buff
	call	crlf
	;now go to the loaded program
	call	setdmabuff	;default dma
	call	saveuser	;user code saved
	;low memory diska contains user code
	call	tran		;gone to the loaded program
	lxi	sp,stack	;may come back here
	call	setdiska
	call	select
	jmp	ccp

userer:	;arrive here on command error
	call	resetdisk
	jmp	comerr

loaderr:;cannot load the program
	lxi	b,loadmsg
	call	print
	jmp	retcom
loadmsg:
	db "BAD LOAD",0
comtype:
	db "COM"		;for com files


retcom:	;reset disk before end of command check
	call	resetdisk

endcom:	;end of intrinsic command
	call	fillfcb0	;to check for garbage at end of line
	lda	comfcb+1
	sui	' '
	lxi	h,sdisk
	ora	m
	;0 in accumulator if no disk selected, and blank fcb
	jnz	comerr
	jmp	ccp



;	data areas
	ds	16	;8 level stack
stack:

;	'submit' file control block
submit:	db	0	;00 if no submit file, ff if submitting
subfcb:	db	0,"$$$     "	;file name is $$$
	db	"SUB",0,0	;file type is sub
submod:	db	0	;module number
subrc:	ds	1	;record count filed
	ds	16	;disk map
subcr:	ds	1	;current record to read

;	command file control block
comfcb:	ds	32	;fields filled in later
comrec:	ds	1	;current record to read/write
dcnt:	ds	1	;disk directory count (used for error codes)
cdisk:	ds	1	;current disk
sdisk:	ds	1	;selected disk for current operation
			;none=0, a=1, b=2 ...
bptr:	ds	1	;buffer pointer
	end	ccploc
