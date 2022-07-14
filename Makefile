# Example Makefile for CP/M 2.2 bdos and ccp using Macro Assembler AS

all: bdos-44k.bin bdos-44k.lst ccp-44k.bin ccp-44k.lst

bdos-44k.p bdos-44k.lst: bdos.asm
	asl -D origin=9c00h -o bdos-44k.p -L -OLIST bdos-44k.lst bdos.asm

bdos-44k.bin: bdos-44k.p
	p2bin -l '$$00' -r '$$9c00-$$a9ff' bdos-44k.p

ccp-44k.p ccp-44k.lst: ccp.asm
	asl -D origin=9400h -o ccp-44k.p -L -OLIST ccp-44k.lst ccp.asm

ccp-44k.bin: ccp-44k.p
	p2bin -l '$$00' -r '$$9400-$$9bff' ccp-44k.p

clean:
	rm -f *.p *.bin *.lst
