;		Forth Interest Group  8086 FORTH 
;
;	   Adapted to run under Microsoft's MS-DOS 8086 operating
;	system by:
;
;		J. E. Smith
;		Univ. of Pennsylvania, Dept. of Chemistry
;		250 S. 33rd St.
;		Philadelphia, PA 19104 .
;
;	   Additional modifications and enhancements
;	as described below were also implemented by Mr. Smith.
;	These changes are more fully described in a text file
;	FORTH.DOC which should accompany this source code.
;
;	   This listing is placed in the public domain, and may
;	be freely distributed.
;
;
;	Current Source Version:
;
;	1.01	06-02-82	First to assemble with no errors;
;				all CPM/86 code, but 86-DOS ASM
;				source format.
;	1.02	06-02-82	Deleted all CPM/86 dependant code,
;					substituted 86-DOS calls
;					for console i/o.
;				Changed R/W to RAM simulation.
;	1.03	06-11-82	First working version !  Some minor
;					aesthetic modifications.
;	1.10	06-12-82	Initial disk-based version.
;	1.1B	06-22-82	Configured to use 64K and 2 screens.
;					Set ^C to cause warm start.
;	1.2A	07-02-82	Modified to word align pointers.
;					Aside from assembler source
;					alignment, the following FORTH
;					words were modified:
;					(FIND),PFA,NFA,and CREATE.
;	1.2B	07-08-82	1+, 2+ changed to CODE; added 1-, 2-.
;	1.2C	07-14-82	Added (ARRAY), (2ARR), and (XOF)
;	1.2D	07-18-82	Added (CARR), (2CARR) and PRINTER for
;					echo to list output.
;	1.2E	08-18-82	Added :@, :!, :C@, :C!, MYSEG,
;					DATE@, DATE!, TIME@, TIME!.
;				Changed ^C to use (ABORT).
;				Replaced all parameters with symbols
;					defined by EQU at the start.
;---------------------------------------------------------------------
;	1.2E distributed as version 1.0
;---------------------------------------------------------------------
;
;		( Page 2 )
;
; Version numbering and ASCII equates:
;
FIGREL		EQU	1
FIGREV		EQU	0
USRVER		EQU	0
;
ABL		EQU	20H
ACR		EQU	0DH
ADOT		EQU	2EH
BELL		EQU	07H
BSIN		EQU	7FH
BSOUT		EQU	08H
DLE		EQU	10H
LF		EQU	0AH
FF		EQU	0CH
;
; Memory allocation parameters:
;
EM		EQU	0000		;64K top of memory + 1
NSCR		EQU	2		;No. of 1024 byte screens
KBBUF		EQU	128		;No. of bytes per block
US		EQU	40H		;User area size ( in bytes )
RTS		EQU	0A0H		;Return stack/TIB size
;
CO		EQU	KBBUF+4		;No. bytes per block buffer
NBUF		EQU	16		;No. of block buffers =
					; NSCR*1024 / KBBUF
BUF1		EQU	0F7C0H		;Addr. of first block buffer =
					; EM - CO*NBUF
INITR0		EQU	BUF1-US		;Start of return stack (R0)
INITS0		EQU	INITR0-RTS	;Start of param. stack (S0)
;
; Disk parameters:
;
TRKS		EQU	77		;Tracks on 8" disk
SPT2		EQU	52		;8" Double density sectors/track
SPT1		EQU	26		;8" Single density sectors/track
SPDRV2		EQU	3744		;8" Double density sectors/drive
SPDRV1		EQU	1872		;8" Single density sectors/drive
BPS		EQU	128		;Bytes/sector
SPBL		EQU	1		;Sectors/block=KBBUF/BPS
BPSC		EQU	8		;Blocks/screen=1024/KBBUF
MXDRV		EQU	2		;Max. number of disk drives
DD		EQU	0		;Density(0=single,1=double)
;
;
;		( Page 3 )
;
		ORG	100H
ORIG: 		NOP
		JMP	CLD
		NOP
		JMP	WRM
;
		DB	FIGREL
		DB	FIGREV
		DB	USRVER
		DB	0EH
		DW	TASK-8
		DW	BSIN
		DW	INITR0
;
		DW	INITS0
		DW	INITR0
		DW	INITS0
		DW	32
		DW	0
		DW	INITDP
		DW	INITDP
		DW	FORTH+6
;
		DW	05H,0B326H		;"8086" ( in base 36 ! )
UP: 		DW	INITR0
RPP: 		DW	INITR0
;
;		( Page 6 )
;
BIP: 		DW	0
BIPE: 		DW	0
;
;		( Page 7 )
;
TNEXT: 		PUSHF
		PUSH	AX
		MOV	AX,[BIP]
		OR	AX,AX
		JZ	TNEXT2
		CMP	AX,-1
		JZ	TNEXT1
		CMP	AX,SI
		JZ	TNEXT1
		JA	TNEXT2
		MOV	AX,[BIPE]
		OR	AX,AX
		JZ	TNEXT2
		CMP	AX,SI
		JB	TNEXT2
;
TNEXT1: 	POP	AX
		POPF
BREAK: 		JP	TNEXT3
TNEXT2: 	POP	AX
		POPF
TNEXT3: 	LODW
		MOV	BX,AX
		JP	NEXT1
;
;		( Page 8 )
;
DPUSH: 		PUSH	DX
APUSH: 		PUSH	AX
;
NEXT: 		LODW
		MOV	BX,AX
NEXT1: 		MOV	DX,BX
		INC	DX
		JMP	[BX]
;		( Page 9 )
;
	ALIGN
DP0: 		DM	83H,"LIT"
		DW	0
LIT: 		DW	$ + 2
		LODW
		JMP	APUSH
;
	ALIGN
		DM	87H,"EXECUTE"
		DW	LIT - 6
EXEC: 		DW	$ + 2
		POP	BX
		JMP	NEXT1
;
	ALIGN
		DM	86H,"BRANCH"
	ALIGN
		DW	EXEC - 10
BRAN: 		DW	$ + 2
BRAN1: 		ADD	SI,[SI]
		JMP	NEXT
;
	ALIGN
		DM	87H,"0BRANCH"
		DW	BRAN - 10
ZBRAN: 		DW	$ + 2
		POP	AX
		OR	AX,AX
		JZ	BRAN1
		INC	SI
		INC	SI
		JMP	NEXT
;
;		( Page 10 )
;
	ALIGN
		DM	86H,"(LOOP)"
	ALIGN
		DW	ZBRAN - 10
XLOOP: 		DW	$ + 2
		MOV	BX,1
XLOO1: 		ADD	[BP],BX
		MOV	AX,[BP]
		SUB	AX,[BP+2]
		XOR	AX,BX
		JS	BRAN1
;
		ADD	BP,4
		INC	SI
		INC	SI
		JMP	NEXT
;
	ALIGN
		DM	87H,"(+LOOP)"
		DW	XLOOP - 10
XPLOO: 		DW	$ + 2
		POP	BX
		JMP	XLOO1
;
	ALIGN
		DM	84H,"(DO)"
	ALIGN
		DW	XPLOO - 10
XDO: 		DW	$ + 2
		POP	DX
		POP	AX
		XCHG	BP,SP
		PUSH	AX
		PUSH	DX
		XCHG	BP,SP
		JMP	NEXT
;
;************************
;*			*
;*	(XOF)		*
;*			*
;************************
;
;	Code added for Dr. Eaker's CASE construct
;	After John Cassady's 8080 code in FD 3:187 1982
;	(jes ver1.2C,1982)
;
	ALIGN
		DM	85H,"(XOF)"
		DW	XDO - 8
XOF:		DW	$ + 2
		POP	BX		;BX := case tag
		POP	AX		;AX := search tag
		CMP	AX,BX		;This one ?
		JE	XOF1		;Yes...
		PUSH	AX		;No, save search tag,
		JMP	BRAN1		;   and check the next case.
XOF1:		INC	SI		;...skip the branch offset,
		INC	SI		;   and
		JMP	NEXT		;   don't save the search tag.
;
;		( Page 11 )
;
	ALIGN
		DM	81H,"I"
		DW	XOF - 8
IDO: 		DW	$ + 2
		MOV	AX,[BP]
		JMP	APUSH
;
	ALIGN
		DM	85H,"DIGIT"
		DW	IDO - 4
DIGIT: 		DW	$ + 2
		POP	DX
		POP	AX
		SUB	AL,'0'
		JB	DIGI2
		CMP	AL,9
		JBE	DIGI1
		SUB	AL,7
		CMP	AL,10
		JB	DIGI2
DIGI1: 		CMP	AL,DL
		JAE	DIGI2
		SUB	DX,DX
		MOV	DL,AL
		MOV	AL,1
		JMP	DPUSH
DIGI2: 		SUB	AX,AX
		JMP	APUSH
;
;		( Page 12 )
;
	ALIGN
		DM	86H,"(FIND)"
	ALIGN
		DW	DIGIT - 8
PFIND: 		DW	$ + 2
		MOV	AX,DS
		MOV	ES,AX
		POP	BX
		POP	CX
PFIN1: 		MOV	DI,CX
		MOV	AL,[BX]
		MOV	DL,AL
		XOR	AL,[DI]
		AND	AL,3FH
		JNZ	PFIN5
PFIN2: 		INC	BX
		INC	DI
		MOV	AL,[BX]
		XOR	AL,[DI]
		ADD	AL,AL
		JNZ	PFIN5
		JNB	PFIN2
;
		ADD	BX,6		;Compute PFA (could be 5 or 6)
		AND	BX,0FFFEH	;Clear LSB to align
;
		PUSH	BX
		MOV	AX,1
		SUB	DH,DH
		JMP	DPUSH
PFIN5: 		INC	BX
		JB	PFIN6
		MOV	AL,[BX]
		ADD	AL,AL
		JMP	PFIN5
;
PFIN6:		INC	BX		;This could be one too many...
		AND	BX,0FFFEH	;Clear LSB to align
;
		MOV	BX,[BX]
		OR	BX,BX
		JNZ	PFIN1
		MOV	AX,0
		JMP	APUSH
;
;		( Page 13 )
;
	ALIGN
		DM	87H,"ENCLOSE"
		DW	PFIND - 10
ENCL: 		DW	$ + 2
		POP	AX
		POP	BX
		PUSH	BX
		MOV	AH,0
		MOV	DX,-1
		DEC	BX
ENCL1: 		INC	BX
		INC	DX
		CMP	AL,[BX]
		JZ	ENCL1
		PUSH	DX
		CMP	AH,[BX]
		JNZ	ENCL2
		MOV	AX,DX
		INC	DX
		JMP	DPUSH
ENCL2: 		INC	BX
		INC	DX
		CMP	AL,[BX]
		JZ	ENCL4
		CMP	AH,[BX]
		JNZ	ENCL2
ENCL3: 		MOV	AX,DX
		JMP	DPUSH
ENCL4: 		MOV	AX,DX
		INC	AX
		JMP	DPUSH
;
;		( Page 14 )
;
	ALIGN
		DM	84H,"EMIT"
	ALIGN
		DW	ENCL - 10
EMIT: 		DW	DOCOL
		DW	PEMIT
		DW	ONE,OUTT
		DW	PSTOR,SEMIS
;
	ALIGN
		DM	83H,"KEY"
		DW	EMIT - 8
KEY: 		DW	$ + 2
		JMP	PKEY
;
	ALIGN
		DM	89H,"?TERMINAL"
		DW	KEY - 6
QTERM: 		DW	$ + 2
		JMP	PQTER
;
	ALIGN
		DM	82H,"CR"
	ALIGN
		DW	QTERM - 12
CR: 		DW	$ + 2
		JMP	PCR
;
	ALIGN
		DM	85H,"CMOVE"
		DW	CR - 6
CMOVE: 		DW	$ + 2
		CLD
		MOV	BX,SI
		POP	CX
		POP	DI
		POP	SI
		MOV	AX,DS
		MOV	ES,AX
		REP
		MOVB
		MOV	SI,BX
		JMP	NEXT
;
	ALIGN
		DM	82H,"U*"
	ALIGN
		DW	CMOVE - 8
USTAR: 		DW	$ + 2
		POP	AX
		POP	BX
		MUL	AX,BX
		XCHG	AX,DX
		JMP	DPUSH
;
	ALIGN
		DM	82H,"U/"
	ALIGN
		DW	USTAR - 6
USLAS: 		DW	$ + 2
		POP	BX
		POP	DX
		POP	AX
		CMP	DX,BX
		JNB	DZERO
		DIV	AX,BX
		JMP	DPUSH
DZERO: 		MOV	AX,-1
		MOV	DX,AX
		JMP	DPUSH
;
;		( Page 16 )
;
	ALIGN
		DM	83H,"AND"
		DW	USLAS - 6
ANDD: 		DW	$ + 2
		POP	AX
		POP	BX
		AND	AX,BX
		JMP	APUSH
;
	ALIGN
		DM	82H,"OR"
	ALIGN
		DW	ANDD - 6
ORR: 		DW	$ + 2
		POP	AX
		POP	BX
		OR	AX,BX
		JMP	APUSH
;
	ALIGN
		DM	83H,"XOR"
		DW	ORR - 6
XORR: 		DW	$ + 2
		POP	AX
		POP	BX
		XOR	AX,BX
		JMP	APUSH
;
;		( Page 17 )
;
	ALIGN
		DM	83H,"SP@"
		DW	XORR - 6
SPAT: 		DW	$ + 2
		MOV	AX,SP
		JMP	APUSH
;
	ALIGN
		DM	83H,"SP!"
		DW	SPAT - 6
SPSTO: 		DW	$ + 2
		MOV	BX,[UP]
		MOV	SP,[BX+6]
		JMP	NEXT
;
	ALIGN
		DM	83H,"RP@"
		DW	SPSTO - 6
RPAT: 		DW	$ + 2
		MOV	AX,BP
		JMP	APUSH
;
	ALIGN
		DM	83H,"RP!"
		DW	RPAT - 6
RPSTO: 		DW	$ + 2
		MOV	BX,[UP]
		MOV	BP,[BX+8]
		JMP	NEXT
;
;		( Page 18 )
;
	ALIGN
		DM	82H,";S"
	ALIGN
		DW	RPSTO - 6
SEMIS: 		DW	$ + 2
		MOV	SI,[BP]
		INC	BP
		INC	BP
		JMP	NEXT
;
	ALIGN
		DM	85H,"LEAVE"
		DW	SEMIS - 6
LEAVE: 		DW	$ + 2
		MOV	AX,[BP]
		MOV	[BP+2],AX
		JMP	NEXT
;
;		( Page 19 )
;
	ALIGN
		DM	82H,">R"
	ALIGN
		DW	LEAVE - 8
TOR: 		DW	$ + 2
		POP	BX
		DEC	BP
		DEC	BP
		MOV	[BP],BX
		JMP	NEXT
;
	ALIGN
		DM	82H,"R>"
	ALIGN
		DW	TOR - 6
FROMR: 		DW	$ + 2
		MOV	AX,[BP]
		INC	BP
		INC	BP
		JMP	APUSH
;
	ALIGN
		DM	81H,"R"
		DW	FROMR - 6
RR: 		DW	IDO + 2
;
;		( Page 20 )
;
	ALIGN
		DM	82H,"0="
	ALIGN
		DW	RR - 4
ZEQU: 		DW	$ + 2
		POP	AX
		OR	AX,AX
		MOV	AX,1
		JZ	ZEQU1
		DEC	AX
ZEQU1:		JMP	APUSH
;
	ALIGN
		DM	82H,"0<"
	ALIGN
		DW	ZEQU - 6
ZLESS: 		DW	$ + 2
		POP	AX
		OR	AX,AX
		MOV	AX,1
		JS	ZLESS1
		DEC	AX
ZLESS1:		JMP	APUSH
;
	ALIGN
		DM	81H,"+"
		DW	ZLESS - 6
PLUS: 		DW	$ + 2
		POP	AX
		POP	BX
		ADD	AX,BX
		JMP	APUSH
;
;		( Page 21 )
;
	ALIGN
		DM	82H,"D+"
	ALIGN
		DW	PLUS - 4
DPLUS: 		DW	$ + 2
		POP	AX
		POP	DX
		POP	BX
		POP	CX
		ADD	DX,CX
		ADC	AX,BX
		JMP	DPUSH
;
	ALIGN
		DM	85H,"MINUS"
		DW	DPLUS - 6
MINUS: 		DW	$ + 2
		POP	AX
		NEG	AX
		JMP	APUSH
;
	ALIGN
		DM	86H,"DMINUS"
	ALIGN
		DW	MINUS - 8
DMINU: 		DW	$ + 2
		POP	BX
		POP	CX
		SUB	AX,AX
		MOV	DX,AX
		SUB	DX,CX
		SBB	AX,BX
		JMP	DPUSH
;
;		( Page 22 )
;
	ALIGN
		DM	84H,"OVER"
	ALIGN
		DW	DMINU - 10
OVER: 		DW	$ + 2
		POP	DX
		POP	AX
		PUSH	AX
		JMP	DPUSH
;
	ALIGN
		DM	84H,"DROP"
	ALIGN
		DW	OVER - 8
DROP: 		DW	$ + 2
		POP	AX
		JMP	NEXT
;
	ALIGN
		DM	84H,"SWAP"
	ALIGN
		DW	DROP - 8
SWAP: 		DW	$ + 2
		POP	DX
		POP	AX
		JMP	DPUSH
;
	ALIGN
		DM	83H,"DUP"
		DW	SWAP - 8
DUP: 		DW	$ + 2
		POP	AX
		PUSH	AX
		JMP	APUSH
;
;		( Page 23 )
;
	ALIGN
		DM	84H,"2DUP"
	ALIGN
		DW	DUP - 6
TDUP: 		DW	$ + 2
		POP	AX
		POP	DX
		PUSH	DX
		PUSH	AX
		JMP	DPUSH
;
	ALIGN
		DM	82H,"+!"
	ALIGN
		DW	TDUP - 8
PSTOR: 		DW	$ + 2
		POP	BX
		POP	AX
		ADD	[BX],AX
		JMP	NEXT
;
	ALIGN
		DM	86H,"TOGGLE"
	ALIGN
		DW	PSTOR - 6
TOGGL: 		DW	$ + 2
		POP	AX
		POP	BX
		XOR	[BX],AL
		JMP	NEXT
;
	ALIGN
		DM	81H,"@"
		DW	TOGGL - 10
AT: 		DW	$ + 2
		POP	BX
		MOV	AX,[BX]
		JMP	APUSH
;
;		( Page 24 )
;
	ALIGN
		DM	82H,"C@"
	ALIGN
		DW	AT - 4
CAT: 		DW	$ + 2
		POP	BX
		MOV	AL,[BX]
		SUB	AH,AH
		JMP	APUSH
;
	ALIGN
		DM	82H,"2@"
	ALIGN
		DW	CAT - 6
TAT: 		DW	$ + 2
		POP	BX
		MOV	AX,[BX]
		MOV	DX,[BX+2]
		JMP	DPUSH
;
	ALIGN
		DM	81H,"!"
		DW	TAT - 6
STORE: 		DW	$ + 2
		POP	BX
		POP	AX
		MOV	[BX],AX
		JMP	NEXT
;
	ALIGN
		DM	82H,"C!"
	ALIGN
		DW	STORE - 4
CSTOR: 		DW	$ + 2
		POP	BX
		POP	AX
		MOV	[BX],AL
		JMP	NEXT
;
;		( Page 25 )
;
	ALIGN
		DM	82H,"2!"
	ALIGN
		DW	CSTOR - 6
TSTOR: 		DW	$ + 2
		POP	BX
		POP	AX
		MOV	[BX],AX
		POP	AX
		MOV	[BX+2],AX
		JMP	NEXT
;
;********************************************************
;*							*
;*	long fetch/store operators:	:@, :!		*
;*					:C@, :C!	*
;*					MYSEG		*
;*							*
;********************************************************
;
	ALIGN
		DM	82H,":@"
	ALIGN
		DW	TSTOR - 6
FARAT:		DW	$ + 2
		POP	BX		;Offset
		MOV	DX,DS		;Save current segment
		POP	DS		;Segment
		MOV	AX,[BX]		;Fetch word at DS:BX
		MOV	DS,DX		;Restore segment register
		JMP	APUSH		;Return
;
	ALIGN
		DM	82H,":!"
	ALIGN
		DW	FARAT - 6
FARST:		DW	$ + 2
		MOV	DX,DS
		POP	BX		;Offset
		POP	DS		;Segment
		POP	AX		;Data
		MOV	[BX],AX
		MOV	DS,DX
		JMP	NEXT
;
	ALIGN
		DM	83H,":C@"
		DW	FARST - 6
FARCAT:		DW	$ + 2
		MOV	DX,DS
		POP	BX
		POP	DS
		MOV	B,AL,[BX]
		XOR	AH,AH
		MOV	DS,DX
		JMP	APUSH
;
	ALIGN
		DM	83H,":C!"
		DW	FARCAT - 6
FARCST:		DW	$ + 2
		MOV	DX,DS
		POP	BX
		POP	DS
		POP	AX
		MOV	B,[BX],AL
		MOV	DS,DX
		JMP	NEXT
;
	ALIGN
		DM	85H,"MYSEG"
		DW	FARCST - 6
MYSEG:		DW	$ + 2
		MOV	AX,DS
		JMP	APUSH
;
;		( Page 26 )
;
	ALIGN
		DM	0C1H,":"
		DW	MYSEG - 8
COLON: 		DW	DOCOL
		DW	QEXEC,	SCSP
		DW	CURR,	AT
		DW	CONT,	STORE
		DW	CREAT,	RBRAC
		DW	PSCOD
DOCOL: 		INC	DX
		DEC	BP
		DEC	BP
		MOV	[BP],SI
		MOV	SI,DX
		JMP	NEXT
;
	ALIGN
		DM	0C1H,";"
		DW	COLON - 4
SEMI: 		DW	DOCOL
		DW	QCSP,	COMP
		DW	SEMIS,	SMUDG
		DW	LBRAC,	SEMIS
;
	ALIGN
		DM	84H,"NOOP"
	ALIGN
		DW	SEMI - 4
NOOP: 		DW	DOCOL,	SEMIS
;
;		( Page 27 )
;
	ALIGN
		DM	88H,"CONSTANT"
	ALIGN
		DW	NOOP - 8
CON: 		DW	DOCOL
		DW	CREAT,	SMUDG
		DW	COMMA,	PSCOD
DOCON: 		INC	DX
		MOV	BX,DX
		MOV	AX,[BX]
		JMP	APUSH
;
	ALIGN
		DM	88H,"VARIABLE"
	ALIGN
		DW	CON - 12
VAR: 		DW	DOCOL
		DW	CON,	PSCOD
DOVAR: 		INC	DX
		PUSH	DX
		JMP	NEXT
;
	ALIGN
		DM	84H,"USER"
	ALIGN
		DW	VAR - 12
USER:		DW	DOCOL
		DW	CON,	PSCOD
DOUSE: 		INC	DX
		MOV	BX,DX
		MOV	BL,[BX]
		SUB	BH,BH
		MOV	DI,[UP]
		LEA	AX,[BX+DI]
		JMP	APUSH
;
;************************
;*			*
;*	(ARRAY)		*
;*			*
;************************
;
;	Code added to support array references.
;	Used by ARRAY to calculate the address of the
;	nth element of the array.
;	(jes ver1.2c,1982)
;
	ALIGN
		DM	87H,"(ARRAY)"
		DW	USER - 8
PARR:		DW	$ + 2
		POP	BX		;BX -> SIZE
		POP	AX		;AX := n
		ADD	AX,AX		;AX := AX*2
		ADD	AX,BX		;AX -> ARRAY[n]
		ADD	AX,2		;Offset to ARRAY[0]
		JMP	APUSH
;
	ALIGN
		DM	86H,"(2ARR)"
	ALIGN
		DW	PARR - 10
P2ARR:		DW	$ + 2
		POP	BX		;BX -> rowsize
		POP	CX		;CX := column
		POP	AX		;AX := row
		MUL	AX,[BX]		;AX := row*row dim.
		ADD	AX,CX		;AX := AX + col
		ADD	AX,AX		;2 bytes per element
		ADD	AX,BX		;AX := AX+PFA
		ADD	AX,4		;Offset to ARRAY[0]
		JMP	APUSH
;
	ALIGN
		DM	86H,"(CARR)"
	ALIGN
		DW	P2ARR - 10
PCARR:		DW	$ + 2
		POP	BX
		POP	AX
		ADD	AX,BX
		ADD	AX,2
		JMP	APUSH
;
	ALIGN
		DM	87H,"(2CARR)"
		DW	PCARR - 10
P2CAR:		DW	$ + 2
		POP	BX
		POP	CX
		POP	AX
		MUL	AX,[BX]
		ADD	AX,CX
		ADD	AX,BX
		ADD	AX,4
		JMP	APUSH
;
;		( Page 28 )
;
	ALIGN
		DM	81H,"0"
		DW	P2CAR - 10
ZERO: 		DW	DOCON
		DW	0
;
		DM	81H,"1"
		DW	ZERO - 4
ONE: 		DW	DOCON
		DW	1
;
		DM	81H,"2"
		DW	ONE - 4
TWO: 		DW	DOCON
		DW	2
;
		DM	81H,"3"
		DW	TWO - 4
THREE: 		DW	DOCON
		DW	3
;
		DM	82H,"BL"
	ALIGN
		DW	THREE - 4
BLS: 		DW	DOCON
		DW	20H
;
;		( Page 29 )
;
		DM	83H,"C/L"
		DW	BLS - 6
CSLL: 		DW	DOCON
		DW	64
;
		DM	85H,"FIRST"
		DW	CSLL - 6
FIRST: 		DW	DOCON
		DW	BUF1
;
		DM	85H,"LIMIT"
		DW	FIRST - 8
LIMIT: 		DW	DOCON
		DW	EM
;
		DM	85H,"B/BUF"
		DW	LIMIT - 8
BBUF: 		DW	DOCON
		DW	KBBUF
;
		DM	85H,"B/SCR"
		DW	BBUF - 8
BSCR: 		DW	DOCON
		DW	BPSC		; 400H/KBBUF
;
;		( Page 30 )
;


		DM	87H,"+ORIGIN"
		DW	BSCR - 8
PORIG: 		DW	DOCOL
		DW	LIT,	ORIG
		DW	PLUS,	SEMIS
;
;		( Page 31 )
;
		DM	82H,"S0"
	ALIGN
		DW	PORIG - 10
SZERO: 		DW	DOUSE
		DW	6
;
		DM	82H,"R0"
	ALIGN
		DW	SZERO - 6
RZERO: 		DW	DOUSE
		DW	8
;
		DM	83H,"TIB"
		DW	RZERO - 6
TIB: 		DW	DOUSE
		DW	10
;
		DM	85H,"WIDTH"
		DW	TIB - 6
WIDTH: 		DW	DOUSE
		DW	12
;
		DM	87H,"WARNING"
		DW	WIDTH - 8
WARN: 		DW	DOUSE
		DW	14
;
;		( Page 32 )
;
		DM	85H,"FENCE"
		DW	WARN - 10
FENCE: 		DW	DOUSE
		DW	16
;
		DM	82H,"DP"
	ALIGN
		DW	FENCE - 8
DP: 		DW	DOUSE
		DW	18
;
		DM	88H,"VOC-LINK"
	ALIGN
		DW	DP - 6
VOCL: 		DW	DOUSE
		DW	20
;
		DM	83H,"BLK"
		DW	VOCL - 12
BLK: 		DW	DOUSE
		DW	22
;
;		( Page 33 )
;
		DM	82H,"IN"
	ALIGN
		DW	BLK - 6
INN: 		DW	DOUSE
		DW	24
;
		DM	83H,"OUT"
		DW	INN - 6
OUTT: 		DW	DOUSE
		DW	26
;
		DM	83H,"SCR"
		DW	OUTT - 6
SCR: 		DW	DOUSE
		DW	28
;
		DM	86H,"OFFSET"
	ALIGN
		DW	SCR - 6
OFSET: 		DW	DOUSE
		DW	30
;
		DM	87H,"CONTEXT"
		DW	OFSET - 10
CONT: 		DW	DOUSE
		DW	32
;
		DM	87H,"CURRENT"
		DW	CONT - 10
CURR: 		DW	DOUSE
		DW	34
;
		DM	85H,"STATE"
		DW	CURR - 10
STATE: 		DW	DOUSE
		DW	36
;
		DM	84H,"BASE"
	ALIGN
		DW	STATE - 8
BASE: 		DW	DOUSE
		DW	38
;
		DM	83H,"DPL"
		DW	BASE - 8
DPL: 		DW	DOUSE
		DW	40
;
		DM	83H,"FLD"
		DW	DPL - 6
FLD: 		DW	DOUSE
		DW	42
;
;		( Page 35 )
;
		DM	83H,"CSP"
		DW	FLD - 6
CSPP: 		DW	DOUSE
		DW	44
;
		DM	82H,"R#"
	ALIGN
		DW	CSPP - 6
RNUM: 		DW	DOUSE
		DW	46
;
		DM	83H,"HLD"
		DW	RNUM - 6
HLD: 		DW	DOUSE
		DW	48
;
;		( Page 36 )
;
		DM	82H,"1+"
	ALIGN
		DW	HLD - 6
ONEP: 		DW	$ + 2
		POP	AX
		INC	AX
		JMP	APUSH
;
	ALIGN
		DM	82H,"2+"
	ALIGN
		DW	ONEP - 6
TWOP: 		DW	$ + 2
		POP	AX
		INC	AX
		INC	AX
		JMP	APUSH
;
	ALIGN
		DM	82H,"1-"
	ALIGN
		DW	TWOP - 6
ONEM:		DW	$ + 2
		POP	AX
		DEC	AX
		JMP	APUSH
	ALIGN
		DM	82H,"2-"
	ALIGN
		DW	ONEM - 6
TWOM:		DW	$ + 2
		POP	AX
		DEC	AX
		DEC	AX
		JMP	APUSH
	ALIGN
		DM	84H,"HERE"
	ALIGN
		DW	TWOM - 6
HERE: 		DW	DOCOL
		DW	DP,	AT,	SEMIS
;
		DM	85H,"ALLOT"
		DW	HERE - 8
ALLOT: 		DW	DOCOL
		DW	DP,	PSTOR,	SEMIS
;
;		( Page 37 )
;
		DM	81H,","
		DW	ALLOT - 8
COMMA: 		DW	DOCOL
		DW	HERE,	STORE
		DW	TWO,	ALLOT,	SEMIS
;
		DM	82H,"C,"
	ALIGN
		DW	COMMA - 4
CCOMM: 		DW	DOCOL
		DW	HERE,	CSTOR
		DW	ONE,	ALLOT,	SEMIS
;
		DM	81H,"-"
		DW	CCOMM - 6
SUBB: 		DW	$ + 2
		POP	DX
		POP	AX
		SUB	AX,DX
		JMP	APUSH
;
;		( Page 38 )
;
	ALIGN
		DM	81H,"="
		DW	SUBB - 4
EQUAL: 		DW	DOCOL
		DW	SUBB,	ZEQU,	SEMIS
;
		DM	81H,"<"
		DW	EQUAL - 4
LESS: 		DW	$ + 2
		POP	DX
		POP	AX
		MOV	BX,DX
		XOR	BX,AX
		JS	LES1
		SUB	AX,DX
LES1: 		OR	AX,AX
		MOV	AX,0
		JNS	LES2
		INC	AX
LES2: 		JMP	APUSH
;
	ALIGN
		DM	82H,"U<"
	ALIGN
		DW	LESS - 4
ULESS: 		DW	DOCOL
		DW	TDUP,	XORR,	ZLESS
		DW	ZBRAN,	ULES1-$-2
		DW	DROP,	ZLESS,	ZEQU
		DW	BRAN,	ULES2-$-2
ULES1: 		DW	SUBB,	ZLESS
ULES2: 		DW	SEMIS
;
;		( Page 39 )
;
		DM	81H,">"
		DW	ULESS - 6
GREAT: 		DW	DOCOL
		DW	SWAP,	LESS,	SEMIS
;
		DM	83H,"ROT"
		DW	GREAT - 4
ROT: 		DW	$ + 2
		POP	DX
		POP	BX
		POP	AX
		PUSH	BX
		JMP	DPUSH
;
	ALIGN
		DM	85H,"SPACE"
		DW	ROT - 6
SPACE: 		DW	DOCOL
		DW	BLS,	EMIT,	SEMIS
;
		DM	84H,"-DUP"
	ALIGN
		DW	SPACE - 8
DDUP: 		DW	DOCOL
		DW	DUP
		DW	ZBRAN,	DDUP1-$-2
		DW	DUP
DDUP1: 		DW	SEMIS
;
;		( Page 40 )
;
		DM	88H,"TRAVERSE"
	ALIGN
		DW	DDUP - 8
TRAV: 		DW	DOCOL
		DW	SWAP
TRAV1: 		DW	OVER,	PLUS
		DW	LIT,	7FH
		DW	OVER,	CAT,	LESS
		DW	ZBRAN,	TRAV1-$-2
		DW	SWAP,	DROP,	SEMIS
;
		DM	86H,"LATEST"
	ALIGN
		DW	TRAV - 12
LATES: 		DW	DOCOL
		DW	CURR,	AT,	AT,	SEMIS
;
		DM	83H,"LFA"
		DW	LATES - 10
LFA: 		DW	DOCOL
		DW	LIT,	4
		DW	SUBB,	SEMIS
;
;		( Page 41 )
;
		DM	83H,"CFA"
		DW	LFA - 6
CFA: 		DW	DOCOL
		DW	TWO,	SUBB,	SEMIS
;
		DM	83H,"NFA"
		DW	CFA - 6
NFA: 		DW	DOCOL
		DW	LIT,	5		;Could be 5 or 6
		DW	SUBB
		DW	DUP,	CAT
		DW	LIT,	80H,	ANDD,	ZEQU
		DW	ZBRAN,	NFA1-$-2	;MSB set, OK
		DW	ONEM			;MSB not set, adjust
NFA1:		DW	LIT,	-1
		DW	TRAV,	SEMIS
;
		DM	83H,"PFA"
		DW	NFA - 6
PFA: 		DW	$ + 2
		POP	BX		;BX:=NFA
		MOV	AL,[BX]		;AL:=count
		AND	AL,1FH		;Only lowest 5 bits
		ADD	AL,6
		SUB	AH,AH
		ADD	BX,AX		;BX:=NFA+count+6
		AND	BX,0FFFEH	;Clear LSB to align
		MOV	AX,BX
		JMP	APUSH		;Save PFA
;
;		( Page 42 )
;
	ALIGN
		DM	84H,"!CSP"
	ALIGN
		DW	PFA - 6
SCSP: 		DW	DOCOL
		DW	SPAT,	CSPP
		DW	STORE,	SEMIS
;
		DM	86H,"?ERROR"
	ALIGN
		DW	SCSP - 8
QERR: 		DW	DOCOL
		DW	SWAP
		DW	ZBRAN,	QERR1-$-2
		DW	ERROR
		DW	BRAN,	QERR2-$-2
QERR1: 		DW	DROP
QERR2: 		DW	SEMIS
;
		DM	85H,"?COMP"
		DW	QERR - 10
QCOMP: 		DW	DOCOL
		DW	STATE,	AT
		DW	ZEQU,	LIT,	17
		DW	QERR,	SEMIS
;
;		( Page 43 )
;
		DM	85H,"?EXEC"
		DW	QCOMP - 8
QEXEC: 		DW	DOCOL
		DW	STATE,	AT
		DW	LIT,	18
		DW	QERR,	SEMIS
;
		DM	86H,"?PAIRS"
	ALIGN
		DW	QEXEC - 8
QPAIR: 		DW	DOCOL
		DW	SUBB
		DW	LIT,	19
		DW	QERR,	SEMIS
;
		DM	84H,"?CSP"
	ALIGN
		DW	QPAIR - 10
QCSP: 		DW	DOCOL
		DW	SPAT,	CSPP,	AT,	SUBB
		DW	LIT,	20
		DW	QERR,	SEMIS
;
		DM	88H,"?LOADING"
	ALIGN
		DW	QCSP - 8
QLOAD: 		DW	DOCOL
		DW	BLK,	AT,	ZEQU
		DW	LIT,	22
		DW	QERR,	SEMIS
;
;		( Page 45 )
;
		DM	87H,"COMPILE"
		DW	QLOAD - 12
COMP: 		DW	DOCOL
		DW	QCOMP
		DW	FROMR,	DUP,	TWOP,	TOR
		DW	AT,	COMMA,	SEMIS
;
		DM	0C1H,"["
		DW	COMP - 10
LBRAC: 		DW	DOCOL
		DW	ZERO,	STATE,	STORE,	SEMIS
;
		DM	81H,"]"
		DW	LBRAC - 4
RBRAC: 		DW	DOCOL
		DW	LIT,	0C0H
		DW	STATE,	STORE,	SEMIS
;
;		( Page 46 )
;
		DM	86H,"SMUDGE"
	ALIGN
		DW	RBRAC - 4
SMUDG: 		DW	DOCOL
		DW	LATES
		DW	LIT,	20H
		DW	TOGGL,	SEMIS
;
		DM	83H,"HEX"
		DW	SMUDG - 10
HEX: 		DW	DOCOL
		DW	LIT,	16
		DW	BASE,	STORE,	SEMIS
;
		DM	87H,"DECIMAL"
		DW	HEX - 6
DECA: 		DW	DOCOL
		DW	LIT,	10
		DW	BASE,	STORE,	SEMIS
;
;		( Page 47 )
;
		DM	87H,"(;CODE)"
		DW	DECA - 10
PSCOD: 		DW	DOCOL
		DW	FROMR,	LATES,	PFA
		DW	CFA,	STORE,	SEMIS
;
		DM	0C5H,";CODE"
		DW	PSCOD - 10
SEMIC: 		DW	DOCOL
		DW	QCSP
		DW	COMP,	PSCOD,	LBRAC
SEMI1		DW	NOOP
		DW	SEMIS
;
		DM	87H,"<BUILDS"
		DW	SEMIC - 8
BUILD: 		DW	DOCOL
		DW	ZERO,	CON,	SEMIS
;
		DM	85H,"DOES>"
		DW	BUILD - 10
DOES: 		DW	DOCOL
		DW	FROMR,	LATES,	PFA,	STORE
		DW	PSCOD
DODOE: 		XCHG	BP,SP
		PUSH	SI
		XCHG	BP,SP
		INC	DX
		MOV	BX,DX
		MOV	SI,[BX]
		INC	DX
		INC	DX
		PUSH	DX
		JMP	NEXT
;
;		( Page 48 )
;
	ALIGN
		DM	85H,"COUNT"
		DW	DOES - 8
COUNT: 		DW	DOCOL
		DW	DUP,	ONEP,	SWAP,	CAT,	SEMIS
;
		DM	84H,"TYPE"
	ALIGN
		DW	COUNT - 8
TYPES: 		DW	DOCOL
		DW	DDUP
		DW	ZBRAN,	TYPE1-$-2
		DW	OVER,	PLUS
		DW	SWAP,	XDO
TYPE2: 		DW	IDO,	CAT,	EMIT
		DW	XLOOP,	TYPE2-$-2
		DW	BRAN,	TYPE3-$-2
TYPE1: 		DW	DROP
TYPE3: 		DW	SEMIS
;
;		( Page 49 )
;
		DM	89H,"-TRAILING"
		DW	TYPES - 8
DTRAI: 		DW	DOCOL
		DW	DUP,	ZERO,	XDO
DTRA1: 		DW	OVER,	OVER,	PLUS
		DW	ONE,	SUBB,	CAT
		DW	BLS,	SUBB
		DW	ZBRAN,	DTRA2-$-2
		DW	LEAVE
		DW	BRAN,	DTRA3-$-2
DTRA2: 		DW	ONE,	SUBB
DTRA3: 		DW	XLOOP,	DTRA1-$-2
		DW	SEMIS
;
;		( Page 50 )
;
		DM	84H,'(.")'
	ALIGN
		DW	DTRAI - 12
PDOTQ: 		DW	DOCOL
		DW	RR
		DW	COUNT,	DUP,	ONEP
		DW	FROMR,	PLUS,	TOR
		DW	TYPES,	SEMIS
;
		DM	0C2H,'."'
	ALIGN
		DW	PDOTQ - 8
DOTQ: 		DW	DOCOL
		DW	LIT,	'"'
		DW	STATE,	AT
		DW	ZBRAN,	DOTQ1-$-2
		DW	COMP
		DW	PDOTQ,	WORDS,	HERE
		DW	CAT,	ONEP,	ALLOT
		DW	BRAN,	DOTQ2-$-2
DOTQ1: 		DW	WORDS,	HERE,	COUNT,	TYPES
DOTQ2: 		DW	SEMIS
;
;		( Page 51 )
;
		DM	86H,"EXPECT"
	ALIGN
		DW	DOTQ - 6
EXPEC: 		DW	DOCOL
		DW	OVER,	PLUS,	OVER
		DW	XDO
EXPE1: 		DW	KEY,	DUP
		DW	LIT,	0EH
		DW	PORIG,	AT,	EQUAL
		DW	ZBRAN,	EXPE2-$-2
		DW	DROP,	DUP,	IDO
		DW	EQUAL,	DUP,	FROMR
		DW	TWO,	SUBB,	PLUS
		DW	TOR
		DW	ZBRAN,	EXPE6-$-2
		DW	LIT,	BELL
		DW	BRAN,	EXPE7-$-2
EXPE6: 		DW	LIT,	BSOUT,	EMIT
		DW	BLS,	EMIT
		DW	LIT,	BSOUT
EXPE7: 		DW	BRAN,	EXPE3-$-2
EXPE2: 		DW	DUP,	LIT,	ACR
		DW	EQUAL
		DW	ZBRAN,	EXPE4-$-2
		DW	LEAVE,	DROP,	BLS,	ZERO
		DW	BRAN,	EXPE5-$-2
EXPE4: 		DW	DUP
EXPE5: 		DW	IDO
		DW	CSTOR,	ZERO,	IDO,	ONEP
		DW	STORE
EXPE3: 		DW	EMIT
		DW	XLOOP,	EXPE1-$-2
		DW	DROP,	SEMIS
;
;		( Page 52 )
;
		DM	85H,"QUERY"
		DW	EXPEC - 10
QUERY: 		DW	DOCOL
		DW	TIB,	AT
		DW	LIT,	80,	EXPEC
		DW	ZERO,	INN,	STORE,	SEMIS
;
;		( Page 53 )
;
		DB	0C1H,80H
		DW	QUERY - 8
NULL: 		DW	DOCOL
		DW	BLK,	AT
		DW	ZBRAN,	NULL1-$-2
		DW	ONE,	BLK,	PSTOR
		DW	ZERO,	INN,	STORE
		DW	BLK,	AT
		DW	BSCR,	ONE,	SUBB,	ANDD
		DW	ZEQU
		DW	ZBRAN,	NULL2-$-2
		DW	QEXEC,	FROMR,	DROP
NULL2: 		DW	BRAN,	NULL3-$-2
NULL1: 		DW	FROMR,	DROP
NULL3: 		DW	SEMIS
;
		DM	84H,"FILL"
	ALIGN
		DW	NULL - 4
FILL: 		DW	$ + 2
		POP	AX
		POP	CX
		POP	DI
		MOV	BX,DS
		MOV	ES,BX
		CLD
		REP
		STOB
		JMP	NEXT
;
;		( Page 54 )
;
	ALIGN
		DM	85H,"ERASE"
		DW	FILL - 8
ERASEE: 	DW	DOCOL
		DW	ZERO,	FILL,	SEMIS
;
		DM	86H,"BLANKS"
	ALIGN
		DW	ERASEE - 8
BLANK: 		DW	DOCOL
		DW	BLS,	FILL,	SEMIS
;
		DM	84H,"HOLD"
	ALIGN
		DW	BLANK - 10
HOLD: 		DW	DOCOL
		DW	LIT,	-1
		DW	HLD,	PSTOR
		DW	HLD,	AT,	CSTOR,	SEMIS
;
		DM	83H,"PAD"
		DW	HOLD - 8
PAD: 		DW	DOCOL
		DW	HERE,	LIT,	68,	PLUS,	SEMIS
		DW	PLUS,	SEMIS
;
;		( Page 55 )
;
		DM	84H,"WORD"
	ALIGN
		DW	PAD - 6
WORDS: 		DW	DOCOL
		DW	BLK,	AT
		DW	ZBRAN,	WORD1-$-2
		DW	BLK,	AT,	BLOCK
		DW	BRAN,	WORD2-$-2
WORD1: 		DW	TIB,	AT
WORD2: 		DW	INN,	AT,	PLUS,	SWAP
		DW	ENCL,	HERE
		DW	LIT,	34
		DW	BLANK,	INN,	PSTOR
		DW	OVER,	SUBB,	TOR
		DW	RR,	HERE,	CSTOR
		DW	PLUS,	HERE,	ONEP
		DW	FROMR,	CMOVE,	SEMIS
;
;		( Page 56 )
;
		DM	88H,"(NUMBER)"
	ALIGN
		DW	WORDS - 8
PNUMB: 		DW	DOCOL
PNUM1: 		DW	ONEP
		DW	DUP,	TOR
		DW	CAT,	BASE,	AT,	DIGIT
		DW	ZBRAN,	PNUM2-$-2
		DW	SWAP,	BASE,	AT,	USTAR
		DW	DROP,	ROT,	BASE,	AT
		DW	USTAR,	DPLUS
		DW	DPL,	AT,	ONEP
		DW	ZBRAN,	PNUM3-$-2
		DW	ONE,	DPL,	PSTOR
PNUM3: 		DW	FROMR
		DW	BRAN,	PNUM1-$-2
PNUM2: 		DW	FROMR,	SEMIS
;
;		( Page 57 )
;
		DM	86H,"NUMBER"
	ALIGN
		DW	PNUMB - 12
NUMB: 		DW	DOCOL
		DW	ZERO,	ZERO
		DW	ROT,	DUP,	ONEP,	CAT
		DW	LIT,	"-",	EQUAL
		DW	DUP,	TOR,	PLUS
		DW	LIT,	-1
NUMB1: 		DW	DPL,	STORE
		DW	PNUMB
		DW	DUP,	CAT,	BLS,	SUBB
		DW	ZBRAN,	NUMB2-$-2
		DW	DUP,	CAT
		DW	LIT,	".",	SUBB
		DW	ZERO,	QERR,	ZERO
		DW	BRAN,	NUMB1-$-2
NUMB2: 		DW	DROP,	FROMR
		DW	ZBRAN,	NUMB3-$-2
		DW	DMINU
	ALIGN
NUMB3: 		DW	SEMIS
;
;		( Page 58 )
;
		DM	85H,"-FIND"
		DW	NUMB - 10
DFIND: 		DW	DOCOL
		DW	BLS,	WORDS
		DW	HERE,	CONT,	AT,	AT
		DW	PFIND,	DUP,	ZEQU
		DW	ZBRAN,	DFIN1-$-2
		DW	DROP
		DW	HERE,	LATES,	PFIND
DFIN1: 		DW	SEMIS
;
		DM	87H,"(ABORT)"
		DW	DFIND - 8
PABOR: 		DW	DOCOL
		DW	ABORT,	SEMIS
;
		DM	85H,"ERROR"
		DW	PABOR - 10
ERROR: 		DW	DOCOL
		DW	WARN,	AT,	ZLESS
		DW	ZBRAN,	ERRO1-$-2
		DW	PABOR
ERRO1: 		DW	HERE,	COUNT,	TYPES
		DW	PDOTQ
		DB	2,"? "
		DW	MESS
		DW	SPSTO
		DW	BLK,	AT,	DDUP
		DW	ZBRAN,	ERRO2-$-2
		DW	INN,	AT,	SWAP
ERRO2: 		DW	QUIT
;
;		( Page 59 )
;
	ALIGN
		DM	83H,"ID."
		DW	ERROR - 8
IDDOT: 		DW	DOCOL
		DW	PAD
		DW	LIT,	32
		DW	LIT,	'_'
		DW	FILL
		DW	DUP,	PFA,	LFA
		DW	OVER,	SUBB
		DW	PAD,	SWAP,	CMOVE
		DW	PAD,	COUNT
		DW	LIT,	1FH
		DW	ANDD,	TYPES,	SPACE,	SEMIS
;
;		( Page 60 )
;
		DM	86H,"CREATE"
	ALIGN
		DW	IDDOT - 6
CREAT: 		DW	DOCOL
		DW	DFIND
		DW	ZBRAN,	CREA1-$-2
		DW	DROP,	NFA,	IDDOT
		DW	LIT,	4,	MESS
		DW	SPACE
CREA1: 		DW	HERE,	DUP,	CAT
		DW	WIDTH,	AT,	MIN
		DW	ONEP,	ALLOT
		DW	DUP
		DW	LIT,	0A0H
		DW	TOGGL
		DW	HERE,	ONE,	SUBB
		DW	LIT,	80H
		DW	TOGGL
;
		DW	DP,	AT
		DW	ONEP
		DW	LIT,	0FFFEH,	ANDD
		DW	DP,	STORE
;
		DW	LATES,	COMMA
		DW	CURR,	AT,	STORE
		DW	HERE,	TWOP,	COMMA,	SEMIS
;
;		( Page 61 )
;
		DM	0C9H,"[COMPILE]"
		DW	CREAT - 10
BCOMP: 		DW	DOCOL
		DW	DFIND
		DW	ZEQU,	ZERO,	QERR
		DW	DROP,	CFA,	COMMA,	SEMIS
;
		DM	0C7H,"LITERAL"
		DW	BCOMP - 12
LITER: 		DW	DOCOL
		DW	STATE,	AT
		DW	ZBRAN,	LITE1-$-2
		DW	COMP,	LIT,	COMMA
LITE1: 		DW	SEMIS
;
;		( Page 62 )
;
		DM	0C8H,"DLITERAL"
	ALIGN
		DW	LITER - 10
DLITE: 		DW	DOCOL
		DW	STATE,	AT
		DW	ZBRAN,	DLIT1-$-2
		DW	SWAP,	LITER,	LITER
DLIT1:		DW	SEMIS
;
		DM	86H,"?STACK"
	ALIGN
		DW	DLITE-12
QSTAC:		DW	DOCOL
		DW	SPAT,	SZERO,	AT
		DW	SWAP,	ULESS,	ONE,	QERR
		DW	SPAT,	HERE
		DW	LIT,	80H
		DW	PLUS,	ULESS
		DW	LIT,	7
		DW	QERR
		DW	SEMIS
;
;		( Page 63 )
;
		DM	89H,"INTERPRET"
		DW	QSTAC - 10
INTER: 		DW	DOCOL
INTE1: 		DW	DFIND
		DW	ZBRAN,	INTE2-$-2
		DW	STATE,	 AT,	LESS
		DW	ZBRAN,	INTE3-$-2
		DW	CFA,	COMMA
		DW	BRAN,	INTE4-$-2
INTE3: 		DW	CFA,	EXEC
INTE4: 		DW	QSTAC
		DW	BRAN,	INTE5-$-2
INTE2: 		DW	HERE,	NUMB,	DPL,	AT,	ONEP
		DW	ZBRAN,	INTE6-$-2
		DW	DLITE
		DW	BRAN,	INTE7-$-2
INTE6: 		DW	DROP,	LITER
INTE7:		DW	QSTAC
INTE5:		DW	BRAN,	INTE1-$-2
;
;		( Page 64 )
;
		DM	89H,"IMMEDIATE"
		DW	INTER-12
IMMED:		DW	DOCOL
		DW	LATES
		DW	LIT,	40H
		DW	TOGGL,	SEMIS
;
		DM	8AH,"VOCABULARY"
	ALIGN
		DW	IMMED - 12
VOCAB: 		DW	DOCOL
		DW	BUILD
		DW	LIT,	0A081H
		DW	COMMA
		DW	CURR,	AT
		DW	CFA,	COMMA,	HERE,	VOCL
		DW	AT,	COMMA,	VOCL,	STORE
		DW	DOES
DOVOC: 		DW	TWOP,	CONT,	STORE,	SEMIS
;
;		( Page 65 )
;
		DM	0C5H,"FORTH"
		DW	VOCAB - 14
FORTH: 		DW	DODOE
		DW	DOVOC
		DW	0A081H
		DW	TASK - 8
		DW	0
;
		DM	8BH,"DEFINITIONS"
		DW	FORTH - 8
DEFIN: 		DW	DOCOL
		DW	CONT,	AT
		DW	CURR,	STORE,	SEMIS
;
		DM	0C1H,"("
		DW	DEFIN - 14
PAREN: 		DW	DOCOL
		DW	LIT,	')',	WORDS,	SEMIS
;;		( Page 66 )
;
		DM	84H,"QUIT"
	ALIGN
		DW	PAREN - 4
QUIT: 		DW	DOCOL
		DW	ZERO,	BLK,	STORE
		DW	LBRAC
QUIT1: 		DW	RPSTO,	CR,	QUERY
		DW	INTER
		DW	STATE,	AT,	ZEQU
		DW	ZBRAN,	QUIT2-$-2
		DW	PDOTQ
		DB	2,"ok"
QUIT2: 		DW	BRAN,	QUIT1-$-2
;
	ALIGN
		DM	85H,"ABORT"
		DW	QUIT - 8
ABORT: 		DW	DOCOL
		DW	SPSTO,	DECA,	QSTAC,	CR
		DW	DOTCPU,	PDOTQ
		DB	16H,'Fig-FORTH  Version '
		DB	FIGREL+30H, ADOT, FIGREV+30H
		DW	LIT,	10,	PORIG,	CAT
		DW	LIT,	41H,	PLUS,	EMIT
		DW	FORTH,	DEFIN
		DW	LIT,	0,	PRTER,	STORE	;Reset echo
		DW	QUIT
;
;		( Page 67 )
;
CTRLC:		
WRM: 		MOV	SI,WRM1
		JMP	NEXT
WRM1		DW	PABOR
;
	ALIGN
		DM	84H,"WARM"
	ALIGN
		DW	ABORT - 8
WARM: 		DW	DOCOL
		DW	MTBUF,	ABORT
;
CLD: 		MOV	SI,CLD1
		MOV	AX,CS
		MOV	DS,AX
		MOV	SP,[ ORIG + 12H ]
		MOV	SS,AX
		MOV	ES,AX
		CLD
		MOV	BP,[RPP]
;
		MOV	AH,37
		MOV	AL,35
		MOV	DX,CTRLC
		INT	33		;Set ^C exit address
;
		JMP	NEXT
CLD1: 		DW	COLD
;
	ALIGN
		DM	84H,"COLD"
	ALIGN
		DW	WARM - 8
COLD: 		DW	DOCOL
		DW	MTBUF
		DW	ZERO,	DENSTY,	STORE
		DW	FIRST,	USE,	STORE
		DW	FIRST,	PREV,	STORE
		DW	DRZER
		DW	LIT,	ORIG+12H
		DW	LIT,	UP,	AT
		DW	LIT,	6,	PLUS
		DW	LIT,	16,	CMOVE
		DW	LIT,	ORIG+12,AT
		DW	LIT,	FORTH+6,STORE
		DW	LIT,	4,	SCR,	STORE
		DW	ABORT
;
;		( Page 69 )
;
		DM	84H,"S->D"
	ALIGN
		DW	COLD - 8
STOD: 		DW	$ + 2
		POP	DX
		SUB	AX,AX
		OR	DX,DX
		JNS	STOD1
		DEC	AX
STOD1: 		JMP	DPUSH
;
	ALIGN
		DM	82H,"+-"
	ALIGN
		DW	STOD - 8
PM: 		DW	DOCOL
		DW	ZLESS
		DW	ZBRAN,	PM1-$-2
		DW	MINUS
PM1: 		DW	SEMIS
;
		DM	83H,"D+-"
		DW	PM - 6
DPM: 		DW	DOCOL
		DW	ZLESS
		DW	ZBRAN,	DPM1-$-2
		DW	DMINU
DPM1: 		DW	SEMIS
;
		DM	83H,"ABS"
		DW	DPM - 6
ABS: 		DW	DOCOL
		DW	DUP,	PM,	SEMIS
;;		( Page 70 )
;
		DM	84H,"DABS"
	ALIGN
		DW	ABS - 6
DABS: 		DW	DOCOL
		DW	DUP,	DPM,	SEMIS
;
		DM	83H,"MIN"
		DW	DABS - 8
MIN: 		DW	DOCOL
		DW	TDUP,	GREAT
		DW	ZBRAN,	MIN1-$-2
		DW	SWAP
MIN1: 		DW	DROP,	SEMIS
;
		DM	83H,"MAX"
		DW	MIN - 6
MAX: 		DW	DOCOL
		DW	TDUP,	LESS
		DW	ZBRAN,	MAX1-$-2
		DW	SWAP
MAX1: 		DW	DROP,	SEMIS
;
;		( Page 71 )
;
		DM	82H,"M*"
	ALIGN
		DW	MAX - 6
MSTAR:		DW	DOCOL
		DW	TDUP,	XORR,	TOR
		DW	ABS
		DW	SWAP,	ABS,	USTAR
		DW	FROMR,	DPM,	SEMIS
;
		DM	82H,"M/"
	ALIGN
		DW	MSTAR - 6
MSLAS: 		DW	DOCOL
		DW	OVER,	TOR,	TOR
		DW	DABS
		DW	RR,	ABS,	USLAS
		DW	FROMR,	RR,	XORR
		DW	PM,	SWAP,	FROMR
		DW	PM,	SWAP,	SEMIS
;
		DM	81H,"*"
		DW	MSLAS - 6
STAR: 		DW	DOCOL
		DW	MSTAR,	DROP,	SEMIS
;
;		( Page 72 )
;
		DM	84H,"/MOD"
	ALIGN
		DW	STAR - 4
SLMOD: 		DW	DOCOL
		DW	TOR,	STOD,	FROMR
		DW	MSLAS,	SEMIS
;
		DM	81H,"/"
		DW	SLMOD - 8
SLASH: 		DW	DOCOL
		DW	SLMOD,	SWAP,	DROP,	SEMIS
;
		DM	83H,"MOD"
		DW	SLASH - 4
MODD: 		DW	DOCOL
		DW	SLMOD,	DROP,	SEMIS
;
		DM	85H,"*/MOD"
		DW	MODD - 6
SSMOD: 		DW	DOCOL
		DW	TOR,	MSTAR,	FROMR
		DW	MSLAS,	SEMIS
;
;		( Page 73 )
;
		DM	82H,"*/"
	ALIGN
		DW	SSMOD - 8
SSLA: 		DW	DOCOL
		DW	SSMOD,	SWAP,	DROP,	SEMIS
;
		DM	85H,"M/MOD"
		DW	SSLA - 6
MSMOD: 		DW	DOCOL
		DW	TOR,	ZERO,	RR,	USLAS
		DW	FROMR,	SWAP,	TOR
		DW	USLAS,	FROMR,	SEMIS
;
;		( Page 74 )
;
		DM	86H,"(LINE)"
	ALIGN
		DW	MSMOD - 8
PLINE: 		DW	DOCOL
		DW	TOR
		DW	LIT,	64
		DW	BBUF,	SSMOD
		DW	FROMR,	BSCR,	STAR
		DW	PLUS
		DW	BLOCK,	PLUS
		DW	LIT,	64,	SEMIS
;
		DM	85H,".LINE"
		DW	PLINE - 10
DLINE: 		DW	DOCOL
		DW	PLINE,	DTRAI,	TYPES,	SEMIS
;
		DM	87H,"MESSAGE"
		DW	DLINE - 8
MESS: 		DW	DOCOL
		DW	WARN,	AT
		DW	ZBRAN,	MESS1-$-2
		DW	DDUP
		DW	ZBRAN,	MESS2-$-2
		DW	LIT,	4
		DW	OFSET,	AT,	BSCR,	SLASH
		DW	SUBB,	DLINE,	SPACE
MESS2: 		DW	BRAN,	MESS3-$-2
MESS1: 		DW	PDOTQ
		DB	6,"MSG # "
		DW	DOT
MESS3: 		DW	SEMIS
;
;		( Page 76 )
;
	ALIGN
		DM	83H,"PC@"
		DW	MESS - 10
PTCAT: 		DW	$ + 2
		POP	DX
		INB	DX
		SUB	AH,AH
		JMP	APUSH
;
	ALIGN
		DM	83H,"PC!"
		DW	PTCAT - 6
PTCSTO: 	DW	$ + 2
		POP	DX
		POP	AX
		OUTB	DX
		JMP	NEXT
;
	ALIGN
		DM	82H,"P@"
	ALIGN
		DW	PTCSTO - 6
PTAT: 		DW	$ + 2
		POP	DX
		INW	DX
		JMP	APUSH
;
;		( Page 77 )
;
	ALIGN
		DM	82H,"P!"
	ALIGN
		DW	PTAT - 6
PTSTO: 		DW	$ + 2
		POP	DX
		POP	AX
		OUTW	DX
		JMP	NEXT
;
;		( Page 78 )
;
;		Disk Interface Words for MS-DOS, etc.
;		--------------------------------
;
;
	ALIGN
		DM	85H,"DRIVE"
		DW	PTSTO - 6
DRIVE: 		DW	DOVAR,	0
;
		DM	86H,"RECORD"	;Not in fig listing
	ALIGN
		DW	DRIVE - 8
REC: 		DW	DOVAR,	0
;
;		( Page 79 )
;
		DM	83H,"USE"
		DW	REC - 10
USE: 		DW	DOVAR,	BUF1
;
		DM	84H,"PREV"
	ALIGN
		DW	USE - 6
PREV: 		DW	DOVAR,	BUF1
;
		DM	87H,"SEC/BLK"
		DW	PREV - 8
SPBLK: 		DW	DOCON,	SPBL	; KBBUF / BPS
;
;		( Page 80 )
;
		DM	85H,"#BUFF"
		DW	SPBLK - 10
NOBUF: 		DW	DOCON,	NBUF
;
		DM	87H,"DENSITY"
		DW	NOBUF - 8
DENSTY: 	DW	DOVAR,	DD
;
		DM	8AH,"DISK-ERROR"
	ALIGN
		DW	DENSTY - 10
DSKERR: 	DW	DOVAR,	0
;
		DM	87H,"PRINTER"		;EPRINT in fig
		DW	DSKERR - 14
PRTER:		DW	DOVAR, 0
;
;		( Page 81 )
;
		DM	84H,"+BUF"
	ALIGN
		DW	PRTER - 10
PBUF: 		DW	DOCOL
		DW	BBUF,	TWOP,	TWOP	;B/BUF+4
		DW	PLUS,	DUP,	LIMIT,	EQUAL
		DW	ZBRAN,	PBUF1-$-2
		DW	DROP,	FIRST
PBUF1: 		DW	DUP,	PREV,	AT
		DW	SUBB,	SEMIS
;
		DM	86H,"UPDATE"
	ALIGN
		DW	PBUF - 8
UPDAT: 		DW	DOCOL
		DW	PREV,	AT,	AT
		DW	LIT,	8000H
		DW	ORR
		DW	PREV,	AT,	STORE,	SEMIS
;
		DM	8DH,"EMPTY-BUFFERS"
		DW	UPDAT - 10
MTBUF: 		DW	DOCOL
		DW	FIRST,	LIMIT,	OVER
		DW	SUBB,	ERASEE,	SEMIS
;
;		( Page 82 )
;
		DM	83H,"DR0"
		DW	MTBUF - 16
DRZER: 		DW	DOCOL
		DW	ZERO,	OFSET,	STORE,	SEMIS
;
		DM	83H,"DR1"
		DW	DRZER - 6
DRONE: 		DW	DOCOL
		DW	DENSTY,	AT
		DW	ZBRAN,	DRON1-$-2
		DW	LIT,	SPDRV2
		DW	BRAN,	DRON2-$-2
DRON1: 		DW	LIT,	SPDRV1
DRON2: 		DW	OFSET,	STORE,	SEMIS
;
;		( Page 83 )
;
		DM	86H,"BUFFER"
	ALIGN
		DW	DRONE - 6
BUFFE: 		DW	DOCOL
		DW	USE,	AT,	DUP,	TOR
BUFF1: 		DW	PBUF
		DW	ZBRAN,	BUFF1-$-2
		DW	USE,	STORE
		DW	RR,	AT,	ZLESS
		DW	ZBRAN,	BUFF2-$-2
		DW	RR,	TWOP
		DW	RR,	AT
		DW	LIT,	7FFFH
		DW	ANDD,	ZERO,	RSLW
BUFF2: 		DW	RR,	STORE
		DW	RR,	PREV,	STORE
		DW	FROMR,	TWOP,	SEMIS
;
;		( Page 84 )
;
		DM	85H,"BLOCK"
		DW	BUFFE - 10
BLOCK: 		DW	DOCOL
		DW	OFSET,	AT,	PLUS,	TOR
		DW	PREV,	AT,	DUP
		DW	AT,	RR,	SUBB
		DW	DUP,	PLUS
		DW	ZBRAN,	BLOC1-$-2
BLOC2: 		DW	PBUF,	ZEQU
		DW	ZBRAN,	BLOC3-$-2
		DW	DROP,	RR
		DW	BUFFE,	DUP
		DW	RR,	ONE,	RSLW
		DW	TWO,	SUBB
BLOC3: 		DW	DUP,	AT,	RR,	SUBB
		DW	DUP,	PLUS,	ZEQU
		DW	ZBRAN,	BLOC2-$-2
		DW	DUP,	PREV,	STORE
BLOC1: 		DW	FROMR,	DROP
		DW	TWOP,	SEMIS
;
;		( Page 85 )
;		( Page 86 )
;
		DM	87H,"T&SCALC"
		DW	BLOCK-8
TSCALC:		DW	DOCOL
		DW	DENSTY,	AT
		DW	ZBRAN,	TSCALS-$-2
		DW	LIT,	SPDRV2,	SLMOD
;		DW	LIT,	MXDRV,	MIN
		DW	DRIVE,	STORE
		DW	REC,	STORE,	SEMIS
;		single density calculations :
TSCALS:		DW	LIT,	SPDRV1,	SLMOD
;		DW	LIT,	MXDRV,	MIN
		DW	DRIVE,	STORE
		DW	REC,	STORE,	SEMIS
;
;		( Page 87 )
;
		DM	8AH,"BLOCK-READ"
	ALIGN
		DW	TSCALC - 10
BLKRD:		DW	$ + 2
		MOV	[DSKERR+2],0	;reset error flag
		MOV	AX,[DRIVE+2]	;AL = drive no.
		MOV	BX,[USE+2]	;BX = transfer address
		MOV	CX,[SPBLK+2]	;CX = no. records to transfer
		MOV	DX,[REC+2]	;DX = logical record #
		PUSH	SI
		PUSH	BP
		INT	37		;BIOS disk read function
		JNC	READOK
		MOV	B,[DSKERR+2],AL	;READ ERROR!
READOK:		POPF
		POP	BP
		POP	SI
		JMP	NEXT
;
	ALIGN
		DM	8BH,"BLOCK-WRITE"
		DW	BLKRD - 14
BLKWRT:		DW	$ + 2
		MOV	[DSKERR+2],0	;reset error flag
		MOV	AX,[DRIVE+2]
		MOV	BX,[USE+2]
		MOV	CX,[SPBLK+2]
		MOV	DX,[REC+2]
		PUSH	SI
		PUSH	BP
		INT	38		;BIOS disk write function
		JNC	WRTOK
		XOR	AH,AH		;return negative error code
		NEG	AX
		MOV	[DSKERR+2],AX	;WRITE ERROR!
WRTOK:		POPF
		POP	BP
		POP	SI
		JMP	NEXT
;
;		( Page 88 )
;
	ALIGN
		DM	83H,"R/W"
		DW	BLKWRT - 14
RSLW: 		DW	DOCOL
		DW	USE,	AT,	TOR
		DW	TOR
		DW	SWAP,	USE, STORE
		DW	SPBLK,	STAR
		DW	TSCALC
		DW	FROMR
		DW	ZBRAN,	RSLW1-$-2
		DW	BLKRD
		DW	BRAN,	RSLW2-$-2
RSLW1:		DW	BLKWRT
RSLW2:		DW	FROMR,	USE,	STORE
		DW	DSKERR,	AT,	DDUP
		DW	ZBRAN,	RSLW5-$-2		;OK
		DW	ZLESS
		DW	ZBRAN,	RSLW3-$-2
		DW	LIT,	9			;Write error
		DW	BRAN,	RSLW4-$-2
RSLW3:		DW	LIT,	8			;Read error
RSLW4:		DW	ZERO,	PREV,	AT,	STORE	;This  buffer
							; is no good!
		DW	QERR
RSLW5:		DW	SEMIS
;
;		( Page 89 )
;
		DM	85H,"FLUSH"
		DW	RSLW - 6
FLUSH: 		DW	DOCOL
		DW	NOBUF,	ONEP
		DW	ZERO,	XDO
FLUS1: 		DW	ZERO,	BUFFE,	DROP
		DW	XLOOP,	FLUS1-$-2
		DW	SEMIS
;
		DM	84H,"LOAD"
	ALIGN
		DW	FLUSH - 8
LOAD: 		DW	DOCOL
		DW	BLK,	AT,	TOR
		DW	INN,	AT,	TOR
		DW	ZERO,	INN,	STORE
		DW	BSCR,	STAR,	BLK,	STORE
		DW	INTER
		DW	FROMR,	INN,	STORE
		DW	FROMR,	BLK,	STORE
		DW	SEMIS
;
;		( Page 90 )
;
		DM	0C3H,"-->"
		DW	LOAD - 8
ARROW: 		DW	DOCOL
		DW	QLOAD
		DW	ZERO,	INN,	STORE
		DW	BSCR,	BLK,	AT
		DW	OVER,	MODD,	SUBB
		DW	BLK,	PSTOR,	SEMIS
;
;		( Page 91 )
;
;****************************************
;*					*
;*	i/o primitives :		*
;*					*
;*	PQTER, PKEY, PEMIT, PCR,	*
;*	CONOUT, LSTOUT			*
;*					*
;****************************************
;
REQUEST		EQU	33		;BIOS function request intr.
CONOUT		EQU	2		;BIOS console output function
LSTOUT		EQU	5		;BIOS printer output function
CONIO		EQU	8		;BIOS console i/o fctn, no echo
CONSTAT		EQU	11		;BIOS console status check fctn
;
ACTRLC		EQU	3		;ASCII ^C
;
PQTER: 		MOV	AH,CONSTAT
		INT	REQUEST
		SUB	AH,AH
		JMP	APUSH
;
PKEY:		MOV	DX,0FFH
		MOV	AH,CONIO
		INT	REQUEST
		OR	AL,AL
		JZ	PKEY
		AND	AX,7FH
		CMP	AL,ACTRLC	;check for ^C
		JNZ	PKEY1		;pass anything else
		INT	35		;Force ^C interrupt 
PKEY1:		JMP	APUSH
;
PEMIT: 		DW	$ + 2
		POP	DX
		CALL	POUT
		JMP	NEXT
;
;		( Page 92 )
;
PCR: 		MOV	DX,ACR
		CALL	POUT
		MOV	DX,LF
		CALL	POUT
		JMP	NEXT
;
POUT: 		AND	DX,7FH
		MOV	AH,CONOUT
		INT	REQUEST
		MOV	BX,[ PRTER+2 ]	;Check echo flag
		OR	BX,BX
		JZ	RET
		MOV	AH,LSTOUT
		INT	REQUEST		;Echo to printer
		RET
;
;********************************************************
;*							*
;*		TIME@, TIME!, DATE@, DATE!		*
;*							*
;********************************************************
;
	ALIGN
		DM	85H,"TIME@"
		DW	ARROW - 6
TIMAT:		DW	$ + 2
		MOV	AH,2CH		;Get time
		INT	REQUEST
		PUSH	DX		;[sec sec/100]
		PUSH	CX		;[hr min]
		JMP	NEXT
;
	ALIGN
		DM	85H,"TIME!"
		DW	TIMAT - 8
TIMST:		DW	$ + 2
		POP	CX		;[hr min]
		POP	DX		;[sec sec/100]
		MOV	AH,2DH
		INT	REQUEST
		JMP	NEXT
;
	ALIGN
		DM	85H,"DATE@"
		DW	TIMST - 8
DATAT:		DW	$ + 2
		MOV	AH,2AH
		INT	REQUEST
		PUSH	CX		;year
		MOV	AL,DH		;month
		XOR	AH,AH
		XOR	DH,DH
		JMP	DPUSH		;DL=day
;
	ALIGN
		DM	85H,"DATE!"
		DW	DATAT - 8
DATST:		DW	$ + 2
		POP	CX		;year
		POP	DX		;DL=day
		POP	AX
		MOV	DH,AL		;DH=month
		MOV	AH,2BH
		INT	REQUEST
		JMP	NEXT
;
;		( Page 93 )
;		( Page 94 )
;
EXIT: 		INT	32
;
;		( Page 96 )
;		( Page 98 )
;
	ALIGN
		DM	0C1H,"'"
		DW	DATST - 8
TICK: 		DW	DOCOL
		DW	DFIND,	ZEQU
		DW	ZERO,	QERR
		DW	DROP,	LITER,	SEMIS
;
		DM	86H,"FORGET"
	ALIGN
		DW	TICK - 4
FORG: 		DW	DOCOL
		DW	CURR,	AT
		DW	CONT,	AT
		DW	SUBB
		DW	LIT,	24,	QERR
		DW	TICK,	DUP
		DW	FENCE,	AT,	LESS
		DW	LIT,	21,	QERR
		DW	DUP
		DW	NFA,	DP,	STORE
		DW	LFA,	AT
		DW	CONT,	AT,	STORE,	SEMIS
;
;		( Page 99 )
;
		DM	84H,"BACK"
	ALIGN
		DW	FORG - 10
BACK: 		DW	DOCOL
		DW	HERE,	SUBB
		DW	COMMA,	SEMIS
;
		DM	0C5H,"BEGIN"
		DW	BACK - 8
BEGIN: 		DW	DOCOL
		DW	QCOMP
		DW	HERE,	ONE,	SEMIS
;
		DM	0C5H,"ENDIF"
		DW	BEGIN - 8
ENDIFF: 	DW	DOCOL
		DW	QCOMP
		DW	TWO,	QPAIR
		DW	HERE,	OVER,	SUBB
		DW	SWAP,	STORE,	SEMIS
;
;		( Page 100 )
;
		DM	0C4H,"THEN"
	ALIGN
		DW	ENDIFF - 8
THEN: 		DW	DOCOL
		DW	ENDIFF,	SEMIS
;
		DM	0C2H,"DO"
	ALIGN
		DW	THEN - 8
DO: 		DW	DOCOL
		DW	COMP,	XDO
		DW	HERE,	THREE,	SEMIS
;
		DM	0C4H,"LOOP"
	ALIGN
		DW	DO - 6
LOOPC: 		DW	DOCOL
		DW	THREE,	QPAIR
		DW	COMP,	XLOOP
		DW	BACK,	SEMIS
;
;		( Page 101 )
;
		DM	0C5H,"+LOOP"
		DW	LOOPC - 8
PLOOP: 		DW	DOCOL
		DW	THREE,	QPAIR
		DW	COMP,	XPLOO
		DW	BACK,	SEMIS
;
		DM	0C5H,"UNTIL"
		DW	PLOOP - 8
UNTIL: 		DW	DOCOL
		DW	ONE,	QPAIR
		DW	COMP,	ZBRAN
		DW	BACK,	SEMIS
;
		DM	0C3H,"END"
		DW	UNTIL - 8
ENDD: 		DW	DOCOL
		DW	UNTIL,	SEMIS
;
;		( Page 102 )
;
		DM	0C5H,"AGAIN"
		DW	ENDD - 6
AGAIN: 		DW	DOCOL
		DW	ONE,	QPAIR
		DW	COMP,	BRAN
		DW	BACK,	SEMIS
;
		DM	0C6H,"REPEAT"
	ALIGN
		DW	AGAIN - 8
REPEA: 		DW	DOCOL
		DW	TOR,	TOR
		DW	AGAIN
		DW	FROMR,	FROMR
		DW	TWO,	SUBB
		DW	ENDIFF,	SEMIS
;
		DM	0C2H,"IF"
	ALIGN
		DW	REPEA - 10
IFF: 		DW	DOCOL
		DW	COMP,	ZBRAN
		DW	HERE,	ZERO,	COMMA
		DW	TWO,	SEMIS
;
;		( Page 103 )
;
		DM	0C4H,"ELSE"
	ALIGN
		DW	IFF - 6
ELSEE: 		DW	DOCOL
		DW	TWO,	QPAIR
		DW	COMP,	BRAN
		DW	HERE,	ZERO,	COMMA
		DW	SWAP
		DW	TWO,	ENDIFF,	TWO
		DW	SEMIS
;
		DM	0C5H,"WHILE"
		DW	ELSEE - 8
WHILE: 		DW	DOCOL
		DW	IFF,	TWOP,	SEMIS
;
;		( Page 104 )
;
		DM	86H,"SPACES"
	ALIGN
		DW	WHILE - 8
SPACS: 		DW	DOCOL
		DW	ZERO,	MAX
		DW	DDUP
		DW	ZBRAN,	SPAX1-$-2
		DW	ZERO,	XDO
SPAX2: 		DW	SPACE
		DW	XLOOP,	SPAX2-$-2
SPAX1: 		DW	SEMIS
;
		DM	82H,"<#"
	ALIGN
		DW	SPACS - 10
BDIGS: 		DW	DOCOL
		DW	PAD,	HLD,	STORE
		DW	SEMIS
;
		DM	82H,"#>"
	ALIGN
		DW	BDIGS - 6
EDIGS: 		DW	DOCOL
		DW	DROP,	DROP
		DW	HLD,	AT
		DW	PAD
		DW	OVER,	SUBB,	SEMIS
;
;		( Page 105 )
;
		DM	84H,"SIGN"
	ALIGN
		DW	EDIGS - 6
SIGN: 		DW	DOCOL
		DW	ROT,	ZLESS
		DW	ZBRAN,	SIGN1-$-2
		DW	LIT,	'-',	HOLD
SIGN1: 		DW	SEMIS
;
		DM	81H,"#"
		DW	SIGN - 8
DIG: 		DW	DOCOL
		DW	BASE,	AT,	MSMOD
		DW	ROT
		DW	LIT,	9
		DW	OVER,	LESS
		DW	ZBRAN,	DIG1-$-2
		DW	LIT,	7,	PLUS
DIG1: 		DW	LIT,	'0',	PLUS
		DW	HOLD,	SEMIS
;
		DM	82H,"#S"
	ALIGN
		DW	DIG - 4
DIGS: 		DW	DOCOL
DIGS1: 		DW	DIG
		DW	OVER,	OVER
		DW	ORR,	ZEQU
		DW	ZBRAN,	DIGS1-$-2
		DW	SEMIS
;
;		( Page 106 )
;
		DM	83H,"D.R"
		DW	DIGS - 6
DDOTR: 		DW	DOCOL
		DW	TOR,	SWAP,	OVER
		DW	DABS
		DW	BDIGS
		DW	DIGS,	SIGN
		DW	EDIGS
		DW	FROMR,	OVER,	SUBB
		DW	SPACS,	TYPES,	SEMIS
;
		DM	82H,".R"
	ALIGN
		DW	DDOTR - 6
DOTR: 		DW	DOCOL
		DW	TOR
		DW	STOD,	FROMR,	DDOTR,	SEMIS
;
;		( Page 107 )
;
		DM	82H,"D."
	ALIGN
		DW	DOTR - 6
DDOT: 		DW	DOCOL
		DW	ZERO
		DW	DDOTR,	SPACE,	SEMIS
;
		DM	81H,"."
		DW	DDOT - 6
DOT: 		DW	DOCOL
		DW	STOD,	DDOT,	SEMIS
;
		DM	81H,"?"
		DW	DOT - 4
QUES: 		DW	DOCOL
		DW	AT,	DOT,	SEMIS
;
		DM	82H,"U."
	ALIGN
		DW	QUES - 4
UDOT: 		DW	DOCOL
		DW	ZERO,	DDOT,	SEMIS
;
;		( Page 108 )
;
		DM	85H,"VLIST"
		DW	UDOT - 6
VLIST: 		DW	DOCOL
		DW	LIT,	80H
		DW	OUTT,	STORE
		DW	CONT,	AT,	AT
VLIS1: 		DW	OUTT,	AT
		DW	CSLL,	GREAT
		DW	ZBRAN,	VLIS2-$-2
		DW	CR
		DW	ZERO,	OUTT,	STORE
VLIS2: 		DW	DUP
		DW	IDDOT
		DW	SPACE,	SPACE
		DW	PFA,	LFA,	AT
		DW	DUP,	ZEQU
		DW	QTERM,	ORR
		DW	ZBRAN,	VLIS1-$-2
		DW	DROP,	SEMIS
;
		DM	83H,"BYE"
		DW	VLIST - 8
BYE: 		DW	$ + 2
		JMP	EXIT
;
;		( Page 109 )
;
	ALIGN
		DM	84H,"LIST"
	ALIGN
		DW	BYE - 6
LISTC: 		DW	DOCOL
		DW	DECA,	CR
		DW	DUP,	SCR,	STORE
		DW	PDOTQ
		DB	6,"SCR # "
		DW	DOT
		DW	LIT,	16,	ZERO,	XDO
LIST1: 		DW	CR,	IDO
		DW	LIT,	3,	DOTR,	SPACE
		DW	IDO,	SCR,	AT,	DLINE
		DW	QTERM
		DW	ZBRAN,	LIST2-$-2
		DW	LEAVE
LIST2: 		DW	XLOOP,	LIST1-$-2
		DW	CR,	SEMIS
;
	ALIGN
		DM	85H,"INDEX"
		DW	LISTC - 8
INDEX: 		DW	DOCOL
		DW	LIT,	FF,	EMIT,	CR
		DW	ONEP,	SWAP,	XDO
INDE1: 		DW	CR,	IDO
		DW	LIT,	3,	DOTR,	SPACE
		DW	ZERO,	IDO,	DLINE
		DW	QTERM
		DW	ZBRAN,	INDE2-$-2
		DW	LEAVE
INDE2: 		DW	XLOOP,	INDE1-$-2
		DW	SEMIS
;
;		( Page 110 )
;
		DM	85H,"TRIAD"
		DW	INDEX - 8
TRIAD: 		DW	DOCOL
		DW	LIT,	FF,	EMIT
		DW	LIT,	3,	SLASH
		DW	LIT,	3,	STAR
		DW	LIT,	3,	OVER
		DW	PLUS,	SWAP,	XDO
TRIA1: 		DW	CR,	IDO,	LISTC
		DW	QTERM
		DW	ZBRAN,	TRIA2-$-2
		DW	LEAVE
TRIA2: 		DW	XLOOP,	TRIA1-$-2
		DW	CR
		DW	LIT,	15,	MESS,	CR
		DW	SEMIS
;
		DM	84H,".CPU"
	ALIGN
		DW	TRIAD - 8
DOTCPU: 	DW	DOCOL
		DW	BASE,	AT
		DW	LIT,	36,	BASE,	STORE
		DW	LIT,	22H,	PORIG,	TAT
		DW	DDOT
		DW	BASE,	STORE,	SEMIS
;
;		( Page 111 )
;
		DM	85H,"MATCH"
		DW	DOTCPU - 8
MATCH: 		DW	$ + 2
		MOV	DI,SI
		POP	CX
		POP	BX
		POP	DX
		POP	SI
		PUSH	SI
MAT1: 		LODB
		CMP	AL,[BX]
		JNZ	MAT3
		PUSH	BX
		PUSH	CX
		PUSH	SI
MAT2: 		DEC	CX
		JZ	MATCHOK
		DEC	DX
		JZ	NOMATCH
		INC	BX
		LODB
		CMP	AL,[BX]
		JZ	MAT2
		POP	SI
		POP	CX
		POP	BX
MAT3: 		DEC	DX
		JNZ	MAT1
		JMP	MAT4
MATCHOK:
NOMATCH: 	POP	CX
		POP	CX
		POP	CX
MAT4: 		MOV	AX,SI
		POP	SI
		SUB	AX,SI
		MOV	SI,DI
		JMP	DPUSH
;
;		( Page 113 )
;
	ALIGN
		DM	84H,"TASK"
	ALIGN
		DW	MATCH - 8
TASK: 		DW	DOCOL
		DW	SEMIS
;
INITDP		EQU	$
