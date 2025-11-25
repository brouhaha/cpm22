; Reformatted and converted for cross-assembly by Macro Assembler AS
; Eric Smith <spacewar@gmail.com> 2018-01-24
; --- Clean modern CCP header for fixed CP/M memory map ---
; This version removes Digital Research’s origin/BDOS auto-calculation.
; CCP and BDOS locations must be provided explicitly.
;
; Captain’s layout:
;     BDOS = 0E000h
;     CCP  = 0E900h
;
; You may override these from the command line:
;     sjasmplus --define=CCP_ORG=0xE900 --define=BDOS_ORG=0xE000 ccp.asm
; ------------------------------------------------------------

;	.cpu	8080

;	title	"console command processor (CCP), ver 2.0"
; CPU 8080

_false	equ	0000h
_true	equ	~_false
testing	equ	_false        ; debugging disabled

; ------------------------------------------------------------
; Explicit CP/M memory layout
; ------------------------------------------------------------
; If caller does not define CCP_ORG or BDOS_ORG, use defaults.
; These match the Captain's architecture:
;   CCP:  0xE900
;   BDOS: 0xE000
; ------------------------------------------------------------

	ifndef	CCP_ORG
CCP_ORG	equ	0E900h
	endif

	ifndef	BDOS_ORG
BDOS_ORG	equ	0E000h
	endif

; CCP starts here
	org	CCP_ORG

; Explicit BDOS entry address, never computed relative to CCP
bdosl	equ	BDOS_ORG

; ------------------------------------------------------------
; Remaining legacy symbols retained intact
; ------------------------------------------------------------
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

		jp ccpstart ;start ccp with possible initial command
		jp ccpclear ;clear the command buffer
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
		ld e,a
		ld c,pcharf
		jp bdos

printbc:
	;print character, but save b,c registers
		push bc
	call	printchar
		pop bc
	ret

crlf:
		ld a,cr
	call	printbc
		ld a,lf
		jp printbc

blank:
		ld a,' '
		jp printbc

print:	;print string starting at b,c until next 00 entry
		push bc ;now print the string
	call	crlf
		pop hl
prin0:	ld a,(hl) ;stop on 00
		or a
		ret z
		inc hl ;ready for next
		push hl
	call	printchar	;character printed
		pop hl
		jp prin0 ;for another character

initialize:
		ld c,initf
		jp bdos

select:
		ld e,a
		ld c,self
		jp bdos

bdos_inr:
	call	bdos
		ld (dcnt),a
		inc a
	ret

open:	;open the file given by d,e
		ld c,openf
		jp bdos_inr

openc:	;open comfcb
		xor a ;clear next record to read
		ld (comrec),a
		ld de,comfcb
		jp open

close:	;close the file given by d,e
		ld c,closef
		jp bdos_inr

search:	;search for the file given by d,e
		ld c,searf
		jp bdos_inr

searchn:
	;search for the next occurrence of the file given by d,e
		ld c,searnf
		jp bdos_inr

searchcom:
	;search for comfcb file
		ld de,comfcb
		jp search

delete:	;delete the file given by d,e
		ld c,delf
		jp bdos

bdos_cond:
	call	bdos
		or a
	ret

diskread:
	;read the next record from the file given by d,e
		ld c,dreadf
		jp bdos_cond

diskreadc:
	;read the comfcb file
		ld de,comfcb
		jp diskread

diskwrite:
	;write the next record to the file given by d,e
		ld c,dwritf
		jp bdos_cond

make:	;create the file given by d,e
		ld c,makef
		jp bdos_inr

renam:	;rename the file given by d,e
		ld c,renf
		jp bdos

getuser:
	;return current user code in a
		ld e,0ffh ;drop through to setuser

setuser:
        	ld c,userf ;sets user number
		jp bdos

saveuser:
	;save user#/disk# before possible ^c or transient
	call	getuser		;code to a
		add a,a ;rot left
		add a,a
		add a,a
		add a,a
		ld hl,cdisk ;4b=user, 4b=disk
		or (hl)
		ld (diska),a ;stored away in memory for later
	ret

setdiska:
		ld a,(cdisk) ;user/disk
		ld (diska),a
	ret

translate:
	;translate character in register A to upper case
		cp 61h ;return if below lower case a
		ret c
		cp 7bh ;return if above lower case z
		ret nc
		and 5fh ;translated to upper case
	ret

readcom:
	;read the next command into the command buffer
	;check for submit file
		ld a,(submit)
		or a
		jp z,nosub
	;scanning a submit file
	;change drives to open and read the file
		ld a,(cdisk)
		or a
		ld a,0
	call nz,select
	;have to open again in case xsub present
		ld de,subfcb ;skip if no sub
	call	open
		jp z,nosub
		ld a,(subrc) ;read last record(s) first
		dec a
		ld (subcr),a ;current record to read
		ld de,subfcb ;end of file if last record
	call	diskread
		jp nz,nosub
	;disk read is ok, transfer to combuf
		ld de,comlen
		ld hl,buff
		ld b,128
	call	move0
	;line is transferred, close the file with a
	;deleted record
		ld hl,submod ;clear fwflag
		ld (hl),0
		inc hl ;one less record
		dec (hl)
		ld de,subfcb
	call	close
		jp z,nosub
	;close went ok, return to original drive
		ld a,(cdisk)
		or a
	call nz,select
	;print to the 00
		ld hl,combuf
	call	prin0
	call	break_key
		jp z,noread
	call	del_sub		;break key depressed
		jp ccp

nosub:	;no submit file
	call	del_sub
	;translate to upper case, store zero at end
	call	saveuser	;user # save in case control c
		ld c,rbuff
		ld de,maxlen
	call	bdos
	call	setdiska	;no control c, so restore diska
noread:	;enter here from submit file
	;set the last character to zero for later scans
		ld hl,comlen ;length is in b
		ld b,(hl)
readcom0:
		inc hl ;end of scan?
		ld a,b
		or a
		jp z,readcom1 ;get character and translate
		ld a,(hl)
	call	translate
		ld (hl),a
		dec b
		jp readcom0

readcom1: ;end of scan, h,l address end of command
		ld (hl),a ;store a zero
		ld hl,combuf ;ready to scan to zero
		ld (comaddr),hl
	ret

break_key:
	;check for a character ready at the console
		ld c,breakf
	call	bdos
		or a
		ret z
		ld c,rcharf ;character cleared
	call	bdos
		or a
	ret

cselect:
	;get the currently selected drive number to reg-A
		ld c,cself
		jp bdos

setdmabuff:
	;set default buffer dma address
		ld de,buff ;(drop through)

setdma:
	;set dma address to d,e
		ld c,dmaf
		jp bdos

del_sub:
	;delete the submit file, and set submit flag to false
		ld hl,submit ;return if no sub file
		ld a,(hl)
		or a
		ret z
		ld (hl),0 ;submit flag is set to false
		xor a ;on drive a to erase file
	call	select
		ld de,subfcb
	call	delete
		ld a,(cdisk) ;back to original drive
		jp select

	ifndef	noserial
serialize:
	;check serialization
		ld de,serial ;check six bytes
		ld hl,bdosl
		ld b,6
ser0:	ld a,(de)
		cp (hl)
		jp nz,badserial
		inc de
		inc hl
		dec b
		jp nz,ser0
	ret			;serial number is ok
	endif

comerr:
	;error in command string starting at position
	;'staddr' and ending with first delimiter
	call	crlf		;space to next line
		ld hl,(staddr) ;h,l address first to print
comerr0: ;print characters until blank or zero
		ld a,(hl) ; not blank
		cp ' '
		jp z,comerr1
		or a ; not zero, so print it
		jp z,comerr1
		push hl
	call	printchar
		pop hl
		inc hl
		jp comerr0 ;	for another character
comerr1: ;print question mark,and delete sub file
		ld a,'?'
	call	printchar
	call	crlf
	call	del_sub
		jp ccp ;restart with next command

; fcb scan and fill subroutine (entry is at fillfcb below)
	;fill the comfcb, indexed by A (0 or 16)
	;subroutines
delim:	;look for a delimiter
		ld a,(de) ;not the last element
		or a
		ret z
		cp ' ' ;non graphic
		jp c,comerr
		ret z ;treat blank as delimiter
		cp '='
		ret z
		cp la ;left arrow
		ret z
		cp '.'
		ret z
		cp ':'
		ret z
		cp ';'
		ret z
		cp '<'
		ret z
		cp '>'
		ret z
	ret			;delimiter not found

deblank: ;deblank the input line
		ld a,(de) ;treat end of line as blank
		or a
		ret z
		cp ' '
		ret nz
		inc de
		jp deblank

addh: ;add a to h,l
		add a,l
		ld l,a
		ret nc
		inc h
	ret

fillfcb0:
	;equivalent to fillfcb(0)
		ld a,0

fillfcb:
		ld hl,comfcb ;fcb rescanned at end
	call	addh
		push hl
		push hl
		xor a ;clear selected disk (in case A:...)
		ld (sdisk),a
		ld hl,(comaddr) ;command address in d,e
		ex de,hl
	call	deblank		;to first non-blank character
		ex de,hl ;in case of errors
		ld (staddr),hl
		ex de,hl ;d,e has command, h,l has fcb address
		pop hl
	;look for preceding file name A: B: ...
		ld a,(de) ;use current disk if empty command
		or a
		jp z,setcur0
	sbc a,'A'-1		;disk name held in b if : follows
		ld b,a
		inc de ;set disk name if :
		ld a,(de)
		cp ':'
		jp z,setdsk

setcur: ;set current disk
		dec de ;back to first character of command
setcur0:
		ld a,(cdisk)
		ld (hl),a
		jp setname

setdsk: ;set disk to name in register b
		ld a,b ;mark as disk selected
		ld (sdisk),a
		ld (hl),b ;past the :
		inc de

setname: ;set the file name field
		ld b,8 ;file name length (max)
setnam0:
	call	delim		;not a delimiter
		jp z,padname
		inc hl ;must be ?'s
		cp '*'
		jp nz,setnam1
		ld (hl),'?' ;to dec count
		jp setnam2

setnam1:
		ld (hl),a ;store character to fcb
		inc de
setnam2:
		dec b ;count down length
		jp nz,setnam0

	;end of name, truncate remainder
trname:	call	delim		;set type field if delimiter
		jp z,setty
		inc de
		jp trname

padname:
		inc hl
		ld (hl),' '
		dec b
		jp nz,padname

setty: ;set the type field
		ld b,3 ;skip the type field if no .
		cp '.'
		jp nz,padty
		inc de ;past the ., to the file type field
setty0: ;set the field from the command buffer
	call	delim
		jp z,padty
		inc hl
		cp '*'
		jp nz,setty1
		ld (hl),'?' ;since * specified
		jp setty2

setty1: ;not a *, so copy to type field
		ld (hl),a
		inc de
setty2: ;decrement count and go again
		dec b
		jp nz,setty0

	;end of type field, truncate
trtyp: ;truncate type field
	call	delim
		jp z,efill
		inc de
		jp trtyp

padty:	;pad the type field with blanks
		inc hl
		ld (hl),' '
		dec b
		jp nz,padty

efill: ;end of the filename/filetype fill, save command address
	;fill the remaining fields for the fcb
		ld b,3
efill0:	inc hl
		ld (hl),0
		dec b
		jp nz,efill0
		ex de,hl ;set new starting point
		ld (comaddr),hl

	;recover the start address of the fcb and count ?'s
		pop hl ;b=0, c=8+3
		ld bc,11
scnq:	inc hl
		ld a,(hl)
		cp '?'
		jp nz,scnq0
	;? found, count it in b
		inc b
scnq0:	dec c
		jp nz,scnq

	;number of ?'s in c, move to a and return with flags set
		ld a,b
		or a
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
		ld hl,intvec ;c counts intrinsics as scanned
		ld c,0
intrin0:
		ld a,c ;done with scan?
		cp intlen
		ret nc
	;no, more to scan
		ld de,comfcb+1 ;beginning of name
		ld b,4 ;length of match is in b
intrin1:
		ld a,(de) ;match?
		cp (hl)
		jp nz,intrin2 ;skip if no match
		inc de
		inc hl
		dec b
		jp nz,intrin1 ;loop while matching

	;complete match on name, check for blank in fcb
		ld a,(de) ;otherwise matched
		cp ' '
		jp nz,intrin3
		ld a,c ;with intrinsic number in a
	ret

intrin2: ;mismatch, move to end of intrinsic
		inc hl
		dec b
		jp nz,intrin2

intrin3: ;try next intrinsic
		inc c ;to next intrinsic number
		jp intrin0 ;for another round

ccpclear:
	;clear the command buffer
		xor a
		ld (comlen),a
	;drop through to start ccp
ccpstart:
	;enter here from boot loader
		ld sp,stack ;save initial disk number
		push bc
        ;(high order 4bits=user code, low 4bits=disk#)
		ld a,c ;user code
		rr a
		rr a
		rr a
		rr a
		and 0fh
	
		ld e,a ;user code selected
	call	setuser
	;initialize for this user, get $ flag
        call	initialize	;0ffh in accum if $ file present
        	ld (submit),a ;submit flag set if $ file present
        	pop bc ;recall user code and disk number
		ld a,c ;disk number in accumulator
		and 0fh
        	ld (cdisk),a ;clears user code nibble
	call	select		;proper disk is selected, now check sub files
	;check for initial command
		ld a,(comlen) ;assume typed already
		or a
		jp nz,ccp0

ccp:
	;enter here on each command or error condition
		ld sp,stack
	call	crlf		;print d> prompt, where d is disk name
	call	cselect		;get current disk number
		add a,'A'
	call	printchar
		ld a,'>'
	call	printchar
	call	readcom		;command buffer filled
ccp0:	;(enter here from initialization with command full)
		ld de,buff ;default dma address at buff
	call	setdma
	call	cselect		;current disk number saved
		ld (cdisk),a
	call	fillfcb0	;command fcb filled
	call nz,comerr		;the name cannot be an ambiguous reference
		ld a,(sdisk)
		or a
		jp nz,userfunc
	;check for an intrinsic function
	call	intrinsic
		ld hl,jmptab ;index is in the accumulator
		ld e,a ;index in d,e
		ld d,0
		add hl,de
		add hl,de
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		jp (hl)
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
		ld hl,76F3H ;'DI HLT' instructions.	
	;typo "lxi h,di or (hlt shl 8)" here originally,
	;corrected by comparing to disassembly of Clark Calkins.
		ld (ccploc),hl
		ld hl,ccploc
		jp (hl)
	endif

;utility subroutines for intrinsic handlers
readerr:
	;print the read error message
		ld bc,rdmsg
		jp print
rdmsg: db "READ ERROR",0

nofile:
	;print no file message
		ld bc,nofmsg
		jp print
nofmsg: db "NO FILE",0

getnumber: ;read a number from the command line
	call	fillfcb0	;should be number
		ld a,(sdisk) ;cannot be prefixed
		or a
		jp nz,comerr
	;convert the byte value in comfcb to binary
		ld hl,comfcb+1 ;(b=0, c=11)
		ld bc,11
	;value accumulated in b, c counts name length to zero
conv0:	ld a,(hl)
		cp ' '
		jp z,conv1
	;more to scan, convert char to binary and add
		inc hl ;valid?
		sub '0'
		cp 10
		jp nc,comerr
		ld d,a ;save value
		ld a,b ;mult by 10
		and 11100000b
		jp nz,comerr
		ld a,b ;recover value
		rlca ;*8
		rlca
		rlca
		add a,b
		jp c,comerr
		add a,b ;*8+*2 = *10
		jp c,comerr
		add a,d ;+digit
		jp c,comerr
		ld b,a ;for another digit
		dec c
		jp nz,conv0
	ret
conv1:	;end of digits, check for all blanks
		ld a,(hl) ;blanks?
		cp ' '
		jp nz,comerr
		inc hl
		dec c
		jp nz,conv1
		ld a,b ;recover value
	ret

movename:
	;move 3 characters from h,l to d,e addresses
		ld b,3
move0:	ld a,(hl)
		ld (de),a
		inc hl
		inc de
		dec b
		jp nz,move0
	ret

addhcf:	;buff + a + c to h,l followed by fetch
		ld hl,buff
		add a,c
	call	addh
		ld a,(hl)
	ret

setdisk:
	;change disks for this command, if requested
		xor a ;clear disk name from fcb
		ld (comfcb),a
		ld a,(sdisk) ;no action if not specified
		or a
		ret z
		dec a ;already selected
		ld hl,cdisk
		cp (hl)
		ret z
		jp select

resetdisk:
	;return to original disk after command
		ld a,(sdisk) ;no action if not selected
		or a
		ret z
		dec a ;same disk
		ld hl,cdisk
		cp (hl)
		ret z
		ld a,(cdisk)
		jp select

;individual intrinsics follow
direct:
	;directory search
	call	fillfcb0	;comfcb gets file name
	call	setdisk		;change disk drives if requested
		ld hl,comfcb+1 ;may be empty request
		ld a,(hl)
		cp ' ' ;skip fill of ??? if not blank
		jp nz,dir1
	;set comfcb to all ??? for current disk
		ld b,11 ;length of fill ????????.???
dir0:	ld (hl),'?'
		inc hl
		dec b
		jp nz,dir0
	;not a blank request, must be in comfcb
dir1:	ld e,0 ;E counts directory entries
		push de
	call	searchcom	;first one has been found
	call z,nofile		;not found message
dir2:	jp z,endir
	;found, but may be system file
		ld a,(dcnt) ;get the location of the element
		rrca
		rrca
		rrca
		and 1100000b
		ld c,a
	;c contains base index into buff for dir entry
		ld a,sysfile ;value to A
	call	addhcf
		rl a ;skip if system file
		jp c,dir6
	;c holds index into buffer
	;another fcb found, new line?
		pop de
		ld a,e
		inc e
		push de
	;e=0,1,2,3,...new line if mod 4 = 0
		and 11b ;and save the test
		push af
		jp nz,dirhdr0 ;header on current line
	call	crlf
		push bc
	call	cselect
		pop bc
	;current disk in A
		add a,'A'
	call	printbc
		ld a,':'
	call	printbc
		jp dirhdr1 ;skip current line hdr
dirhdr0:
	call	blank		;after last one
		ld a,':'
	call	printbc
dirhdr1:
	call	blank
	;compute position of name in buffer
		ld b,1 ;start with first character of name
dir3:	ld a,b ;buff+a+c fetched
	call	addhcf
		and 7fh ;mask flags
	;may delete trailing blanks
		cp ' ' ;check for blank type
		jp nz,dir4
		pop af ;may be 3rd item
		push af
		cp 3 ;place blank at end if not
		jp nz,dirb
		ld a,9 ;first char of type
	call	addhcf
		and 7fh
		cp ' '
		jp z,dir5
	;not a blank in the file type field
dirb:	ld a,' ' ;restore trailing filename chr
dir4:
	call	printbc		;char printed
		inc b
		ld a,b
		cp 12
		jp nc,dir5
	;check for break between names
		cp 9 ;for another char
		jp nz,dir3
	;print a blank between names
	call	blank
		jp dir3

dir5:	;end of current entry
		pop af ;discard the directory counter (mod 4)
dir6:	call	break_key	;check for interrupt at keyboard
		jp nz,endir ;abort directory search
	call	searchn		;for another entry
		jp dir2
endir:	;end of directory scan
		pop de ;discard directory counter
		jp retcom


erase:
	call	fillfcb0	;cannot be all ???'s
		cp 11
		jp nz,erasefile
	;erasing all of the disk
		ld bc,ermsg
	call	print
	call	readcom
		ld hl,comlen ;bad input
		dec (hl)
		jp nz,ccp
		inc hl
		ld a,(hl)
		cp 'Y'
		jp nz,ccp
	;ok, erase the entire diskette
		inc hl ;otherwise error at retcom
		ld (comaddr),hl
erasefile:
	call	setdisk
		ld de,comfcb
	call	delete
		inc a ;255 returned if not found
	call z,nofile		;no file message if so
		jp retcom

ermsg:	db	"ALL (Y/N)?",0

type:
	call	fillfcb0	;don't allow ?'s in file name
		jp nz,comerr
	call	setdisk		;open the file
	call	openc
		jp z,typerr ;zero flag indicates not found
	;file opened, read 'til eof
	call	crlf		;read first buffer
		ld hl,bptr
		ld (hl),255
type0:	;loop on bptr
		ld hl,bptr ;end buffer
		ld a,(hl)
		cp 128
		jp c,type1 ;carry if 0,1,...,127
		push hl
	;read another buffer full
	call	diskreadc
		pop hl ;recover address of bptr
		jp nz,typeof ;hard end of file
		xor a ;bptr = 0
		ld (hl),a
type1:	;read character at bptr and print
		inc (hl) ;bptr = bptr + 1
		ld hl,buff ;h,l addresses char
	call	addh
		ld a,(hl)
		cp eofile
		jp z,retcom
	call	printchar
	call	break_key	;abort if break
		jp nz,retcom
		jp type0 ;for another character

typeof:	;end of file, check for errors
		dec a
		jp z,retcom
	call	readerr
typerr:	call	resetdisk
		jp comerr

save:
	call	getnumber	; value to register a
		push af ;save it for later

	;should be followed by a file to save the memory image
	call	fillfcb0
		jp nz,comerr ;cannot be ambiguous
	call	setdisk		;may be a disk change
		ld de,comfcb ;existing file removed
		push de
	call	delete
		pop de
	call	make		;create a new file on disk
		jp z,saverr ;no directory space
		xor a ;clear next record field
		ld (comrec),a
		pop af ;#pages to write is in a, change to #sectors
		ld l,a
		ld h,0
		add hl,hl
		ld de,tran ;h,l is sector count, d,e is load address
save0:	;check for sector count zero
		ld a,h ;may be completed
		or l
		jp z,save1
		dec hl ;sector count = sector count - 1
		push hl ;save it for next time around
		ld hl,128 ;next dma address saved
		add hl,de
		push hl
	call	setdma		;current dma address set
		ld de,comfcb
	call	diskwrite
		pop de ;dma address, sector count
		pop hl
		jp nz,saverr ;may be disk full case
		jp save0 ;for another sector

save1:	;end of dump, close the file
		ld de,comfcb
	call	close
		inc a ;255 becomes 00 if error
		jp nz,retsave ;for another command
saverr:	;must be full or read only disk
		ld bc,fullmsg
	call	print
retsave:
	;reset dma buffer
	call	setdmabuff
		jp retcom
fullmsg:
	db "NO SPACE",0


rename:
	;rename a file on a specific disk
	call	fillfcb0	;must be unambiguous
		jp nz,comerr
		ld a,(sdisk) ;save for later compare
		push af
	call	setdisk		;disk selected
	call	searchcom	;is new name already there?
		jp nz,renerr3
	;file doesn't exist, move to second half of fcb
		ld hl,comfcb
		ld de,comfcb+16
		ld b,16
	call	move0
	;check for = or left arrow
		ld hl,(comaddr)
		ex de,hl
	call	deblank
		cp '=' ;ok if =
		jp z,ren1
		cp la
		jp nz,renerr2
ren1:	ex de,hl ;past delimiter
		inc hl
		ld (comaddr),hl
	;proper delimiter found
	call	fillfcb0
		jp nz,renerr2
	;check for drive conflict
		pop af ;previous drive number
		ld b,a
		ld hl,sdisk
		ld a,(hl)
		or a
		jp z,ren2
	;drive name was specified.  same one?
		cp b
		ld (hl),b
		jp nz,renerr2
ren2:	ld (hl),b ;store the name in case drives switched
		xor a ;is old file there?
		ld (comfcb),a
	call	searchcom
		jp z,renerr1

	;everything is ok, rename the file
		ld de,comfcb
	call	renam
		jp retcom

renerr1:; no file on disk
	call	nofile
		jp retcom
renerr2:; ambigous reference/name conflict
	call	resetdisk
		jp comerr
renerr3:; file already exists
		ld bc,renmsg
	call	print
		jp retcom
renmsg:	db "FILE EXISTS",0

user:
	;set user number
	call	getnumber	; leaves the value in the accumulator
		cp 16 ; must be between 0 and 15
		jp nc,comerr
		ld e,a ;save for setuser call
		ld a,(comfcb+1)
		cp ' '
		jp z,comerr
	call	setuser		;new user number set
		jp endcom

userfunc:
	ifndef	noserialize
	call	serialize	;check serialization
	endif
	;load user function and set up for execution
		ld a,(comfcb+1)
		cp ' '
		jp nz,user0
	;no file name, but may be disk switch
		ld a,(sdisk) ;no disk name if 0
		or a
		jp z,endcom
		dec a ;set user/disk
		ld (cdisk),a
	call	setdiska
	call	select
		jp endcom
user0:	;file name is present
		ld de,comfcb+9 ;type ' '
		ld a,(de)
		cp ' '
		jp nz,comerr
		push de ;.com
	call	setdisk
		pop de
		ld hl,comtype
	call	movename	;file type is set to .com
	call	openc
		jp z,userer
	;file opened properly, read it into memory
		ld hl,tran ;transient program base
load0:	push hl ;save dma address
		ex de,hl
	call	setdma
		ld de,comfcb
	call	diskread
		jp nz,load1
	;sector loaded, set new dma address and compare
		pop hl
		ld de,128
		add hl,de
		ld de,tranm ;has the load overflowed?
		ld a,l
	sub	e
		ld a,h
		sbc a,d
		jp nc,loaderr
		jp load0 ;for another sector

load1:	pop hl ;end file is 1
		dec a
		jp nz,loaderr
	call	resetdisk	;back to original disk
	call	fillfcb0
		ld hl,sdisk
		push hl
		ld a,(hl) ;drive number set
		ld (comfcb),a
		ld a,16 ;move entire fcb to memory
	call	fillfcb
		pop hl
		ld a,(hl)
		ld (comfcb+16),a
		xor a ;record number set to zero
		ld (comrec),a
		ld de,fcb
		ld hl,comfcb
		ld b,33
	call	move0
	;move command line to buff
		ld hl,combuf
bmove0:	ld a,(hl)
		or a
		jp z,bmove1
		cp ' '
		jp z,bmove1
		inc hl ;for another scan
		jp bmove0
	;first blank position found
bmove1:	ld b,0 ;ready for the move
		ld de,buff+1
bmove2:	ld a,(hl)
		ld (de),a
		or a
		jp z,bmove3
	;more to move
		inc b
		inc hl
		inc de
		jp bmove2
bmove3:	;b has character count
		ld a,b
		ld (buff),a
	call	crlf
	;now go to the loaded program
	call	setdmabuff	;default dma
	call	saveuser	;user code saved
	;low memory diska contains user code
	call	tran		;gone to the loaded program
		ld sp,stack ;may come back here
	call	setdiska
	call	select
		jp ccp

userer:	;arrive here on command error
	call	resetdisk
		jp comerr

loaderr:;cannot load the program
		ld bc,loadmsg
	call	print
		jp retcom
loadmsg:
	db "BAD LOAD",0
comtype:
	db "COM"		;for com files


retcom:	;reset disk before end of command check
	call	resetdisk

endcom:	;end of intrinsic command
	call	fillfcb0	;to check for garbage at end of line
		ld a,(comfcb+1)
		sub ' '
		ld hl,sdisk
		or (hl)
	;0 in accumulator if no disk selected, and blank fcb
		jp nz,comerr
		jp ccp



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
	end
