# cpm22 - source code for CP/M 2.2 CCP and BDOS

Hosted at the
[cpm22 Github repository](https://github.com/brouhaha/cpm22/).

## Introduction

Introduced in 1974, CP/M by Digital Research was one of the first
microcomputer operating systems that was not tied to a single computer
vendor.  It could be adapted to run on almost any 8080 or Z80
microcomputer that had at least 16KB of RAM starting at address 0000h.

Originally much of CP/M was written in the PL/M programming language.
With the introduction of CP/M 2.0, the command processor (CCP) and kernel
(BDOS) were rewritten in 8080 assembly language.

The Digital Research ASM80 assembler allowed code to be written with
mulitple assembly language instructions per line, separated by an
exclamation mark ("!"). Although a semicolon (";") was used to introduce
a comment, an exclamation mark in a comment would start a new instruction.
There are two problems with this syntax:

* This syntax makes the source code rather difficult to read, when one
  is accustomed to normal assembler syntax.

* Few if any assemblers other than DRI's ASM80 support this syntax.

This repository contains the CP/M 2.2 CCP adn BDOS source code, reformatted
to cross-assemble with Macro Assembler AS:

    http://john.ccac.rwth-aachen.de:8000/as/

The source code has been verified to assemble to the exact binary present
on several actual CP/M 2.2 disks, with the exception of the six-byte serial
numbers present in the CCP and BDOS.

It is likely that with only minor changes, the source code could be
assembled using other assemblers (native or cross).
