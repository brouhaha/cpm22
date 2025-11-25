; Minimal CP/M 2.2 BIOS for the emulator
;
; Provides console I/O via port 0 (mapped to the GUI terminal),
; stubs disk routines, and jumps to the CCP on warm boot.

BDOS_ADDR	equ	0E000h
CCP_START	equ	0EC5Ch	; from ccp.lst

		org	0F200h

; Jump table (standard CP/M order)
	jp	boot
	jp	wboot
	jp	const
	jp	conin
	jp	conout
	jp	list
	jp	punch
	jp	reader
	jp	home
	jp	seldsk
	jp	settrk
	jp	setsec
	jp	setdma
	jp	read
	jp	write
	jp	listst
	jp	sectran

; ------------------------------------------------------------
; Console I/O
; ------------------------------------------------------------
boot:
	; set JMP vectors at 0000h and 0005h
	ld	a,0C3h		; JMP opcode
	ld	(0000h),a
	ld	hl,wboot
	ld	(0001h),hl
	ld	(0005h),a
	ld	hl,BDOS_ADDR
	ld	(0006h),hl
	; defaults
	ld	hl,0080h
	ld	(dmaaddr),hl
	ld	a,0FFh
	ld	(kbbuf),a
	; fall through to warm boot
wboot:
	ld	sp,0F800h	; place stack high
	ld	c,0		; default drive/user in C for CCP
	jp	CCP_START

const:
	ld	a,(kbbuf)
	cp	0FFh
	jr	nz,_const_ready
	in	a,(0)		; pull from terminal; 0 means none
	cp	0
	jr	z,_const_empty
	ld	(kbbuf),a
_const_ready:
	or	a		; Z if none, NZ if ready
	ret
_const_empty:
	xor	a
	ret

conin:
	ld	a,(kbbuf)
	cp	0FFh
	jr	nz,_con_use_buf
_con_read:
	in	a,(0)
	cp	0
	jr	z,_con_read
	jp	_con_done_port
_con_use_buf:
	; A already holds buffered char
_con_done_port:
	ld	b,a		; save char
	ld	a,0FFh
	ld	(kbbuf),a		; buffer now empty
	ld	a,b
	ret

conout:
	ld	a,c
	out	(0),a
	ret

list:
punch:
reader:
	xor	a		; no devices
	ret

; ------------------------------------------------------------
; Disk stubs / bookkeeping
; ------------------------------------------------------------
home:
	ld	hl,0
	ld	(curtrk),hl
	ret

seldsk:
	; drive in C, return HL -> dpb (always drive 0)
	ld	a,c
	and	0Fh
	ld	(curdrv),a
	ld	hl,dpb
	ret

settrk:
	ld	(curtrk),hl
	ret

setsec:
	ld	(cursec),hl
	ret

setdma:
	ld	(dmaaddr),hl
	ret

read:
write:
	xor	a		; 0 => OK
	ret

listst:
	xor	a
	ret

sectran:
	; no translation; HL already logical sector
	ret

; ------------------------------------------------------------
; Data
; ------------------------------------------------------------
curtrk:	dw	0
cursec:	dw	0
curdrv:	db	0
dmaaddr:dw	0080h
kbbuf:	db	0FFh

; Simple DPB (placeholder, not used by stubs)
dpb:
	dw	32	; spt
	db	3	; bsh (1K blocks)
	db	7	; blm
	db	0	; exm
	dw	242	; dsm
	dw	63	; drm
	db	080h	; al0
	db	000h	; al1
	dw	0	; cks
	dw	2	; off
