;#=================================================================#
;=	PROGRAM-ID. Assignment Six.									   =
;=	AUTHOR. Rev. Taylor Rainwater.								   =
;=	DATE-WRITTEN. 26.11.2015.									   =
;=							   029A								   =
;#=================================================================#

;------------------------------MAIN--------------------------------
	.orig 	x3000
	JSR GETHD
	HALT

	;Variables
	PRNT0 .STRINGZ 	"Multiply two numbers\n"
	PRNT1 .STRINGZ 	"Enter a 4 digit hex number: "
	PRNT2 .STRINGZ	"Enter a 4 digit hex number: "
	PRNT3 .STRINGZ	"The product is: "

;---------------------------SUBROUTINES-----------------------------

;******************************************************************
;*** GETHD - Get a single hex digit from the user. 	    		***
;***         Loop until the user enters a value from 			***
;***         0 to 9, 'A' to 'F', or 'a' to 'f'.          	    ***
;***------------------------------------------------------------***
;***			INPUT  - Uses R0 from GETC.						***
;***			OUTPUT - Return the result in R0.				***
;******************************************************************
GETHD		 	
	;Save used registers
	ST	R1, GN_SR1
	ST	R2, GN_SR2
	ST	R7, GN_SR7

	;Cleaning
	AND R0, R0, #0
	AND R1, R1, #0
	AND R2, R2, #0
	
GN_TOP
	GETC				;Get a character from the user

GN_FIRST
	LD	R1, GN_N30		;Load negative value
	ADD	R2, R0, R1		;Check for below zero
	BRn GN_TOP			;If negative, get new

	LD 	R1, GN_N39		;Load negative
	ADD R2, R0, R1		;Check if above 9
	BRp GN_SECOND		;If positive above 9, goto second

	OUT					;Print character

	LD 	R1, GN_N30		;Load negative value
	ADD R0, R0, R1		;Convert ASCII (0-9) to decimal
	BR GN_END

GN_SECOND
	LD 	R1, GN_N41		;Load negative value
	ADD R2, R0, R1		;Check if below 'A'
	BRn GN_TOP			;If negative between 9 and 'A'

	LD 	R1, GN_N46		;Load negative value
	ADD R2, R0, R1		;Check if above 'F'
	BRp GN_THIRD		;If positive above 'F', goto third

	OUT					;Print character

	LD 	R1, GN_CB		;Load negative value
	ADD R0, R0, R1		;Convert ASCII ('A'-'F') to decimal
	BR 	GN_END

GN_THIRD
	LD 	R1, GN_N61		;Load negative value
	ADD R2, R0, R1		;Check if below 'a'
	BRn GN_TOP			;If negative between 'F' and 'a'

	LD 	R1, GN_N66		;Load negative value
	ADD R2, R0, R1		;Check if above 'f' in ASCII
	BRp GN_TOP			;If positive above 'f', get new

	OUT					;Print character

	LD 	R1, GN_CBR		;Load negative value
	ADD R0, R0, R1		;Convert ASCII ('a'-'f') to decimal

GN_END
	;Restore the registers
	LD	R1, GN_SR1	
	LD	R2, GN_SR2
	LD	R7, GN_SR7
	RET
	
;Saved registers
GN_SR1	.fill 0
GN_SR2	.fill 0
GN_SR3	.fill 0
GN_SR7	.fill 0
;Variables
GN_N30	.fill xFFD0		; -48  -> below 0 & number convert
GN_N39  .fill xFFC7 	; -57  -> above 9
GN_N41	.fill xFFBF 	; -65  -> below 'A'
GN_N46	.fill xFFBA 	; -70  -> above 'F'
GN_N61	.fill xFF9F 	; -97  -> below 'a'
GN_N66  .fill xFF9A		; -102 -> above 'f'
GN_CB 	.fill xFFC9		; -55  -> capital char convert, offset by ten
GN_CBR	.fill xFFA9 	; -87  -> lower char convert, offset by ten

;******************************************************************
;***GETH - Take a four digit hex number and convert it to an 	***
;***       integer value. 										***
;***------------------------------------------------------------***
;***			INPUT  - Uses R0 from GETHD.					***
;***			OUTPUT - Return the result in R0.				***
;******************************************************************
GETH
	;Save used registers
	ST R1, GH_SR1
	ST R2, GH_SR2
	ST R3, GH_SR3
	ST R7, GH_SR7		;This is supposed to be here you fuck, don't take it out. 
GH_TOP
	;Cleaning
	AND R0, R0, #0		;Reset R0
	AND R1, R1, #0		;Reset R1
	AND R2, R2, #0		;Reset R2
	AND R3, R3, #0		;Reset R3

	;Column 4 run
	LD 	R2, GH_C3		;Load column value
	JSR GETHD			;Get digit
	ADD R1, R0, #0		;Copy R0 to R1
	JSR MULTE			;Multiply column value into R0
	ADD R3, R3, R0		;Add column value into R3 from R0

	;Column 3 run
	LD 	R2, GH_C2		;Load column value
	JSR GETHD			;Get digit
	ADD R1, R0, #0		;Copy R0 to R1
	JSR MULTE			;Multiply column value into R0
	ADD R3, R3, R0		;Add column value into R3 from R0

	;Column 2 run
	LD 	R2, GH_C1		;Load column value
	JSR GETHD			;Get digit
	ADD R1, R0, #0		;Copy R0 to R1
	JSR MULTE			;Multiply column value into R0
	ADD R3, R3, R0		;Add column value into R3 from R0

	;Column 1 run
	LD 	R2, GH_C0		;Load column value
	JSR GETHD			;Get digit
	ADD R1, R0, #0		;Copy R0 to R1
	JSR MULTE			;Multiply column value into R0
	ADD R3, R3, R0		;Add column value into R3 from R0

	;Now clean up
	LEA R0, GH_NL		;Load newline
	PUTS				;Print newline
	AND R0, R0, #0		;Reset R0
	ADD R0, R3, #0		;Move sum in R3 to R0

GH_END
	;Restore registers
	LD 	R1, GH_SR1
	LD 	R2, GH_SR2
	LD 	R3, GH_SR3
	LD 	R7, GH_SR7
	RET

;Saved registers
GH_SR1 .fill 0
GH_SR2 .fill 0
GH_SR3 .fill 0
GH_SR7 .fill 0
;Variables
GH_C0  .fill x0001		;Col 1 -> 1
GH_C1  .fill x0010		;Col 2 -> 16
GH_C2  .fill x0100		;Col 3 -> 256
GH_C3  .fill x1000		;Col 4 -> 4096
GH_NL  .STRINGZ "\n"	;Newline string

;******************************************************************
;*** PRINTH - Take a integer from R0 and convert it to hex then ***
;***       	  print the hex value.								***
;***------------------------------------------------------------***
;***			INPUT  - Uses R0.								***
;***			OUTPUT - Prints to console.						***
;******************************************************************
PRINTH
	;Save used registers
	ST R0, PH_SR0
	ST R1, PH_SR1
	ST R2, PH_SR2
 	ST R7, PH_SR7

PH_TOP
	;Cleaning
	AND R1, R1, #0
	AND R2, R2, #0

	ADD R1, R0, #0		;Copy R0 to R1
	LD  R2, PH_C3		;Load R2 with x1000
	JSR DIV 			;Divide R1 by 4096 into R0
	JSR PRINTHD			;Print

	JSR MOD 			;Mod R1 and R2
	ADD R1, R0, #0		;Copy R0 to R1
	LD  R2, PH_C2		;Load R2 with x0100
	JSR DIV 			;Divide R1 by 256 into R0
	JSR PRINTHD			;Print

	JSR MOD 			;Mod R1 and R2
	ADD R1, R0, #0		;Copy R0 to R1
	LD  R2, PH_C1		;Load R2 with x0010
	JSR DIV 			;Divide R1 by 16 into R0
	JSR PRINTHD			;Print

	JSR MOD 			;Mod R1 and R2
	ADD R1, R0, #0		;Copy R0 to R1
	LD  R2, PH_C0		;Load R2 with x0001
	JSR DIV 			;Divide R1 by 1 into R0
	JSR PRINTHD			;Print

PH_END
	;Restore registers
	LD  R0, PH_SR0
	LD 	R1, PH_SR1
	LD 	R2, PH_SR2
	LD  R7, PH_SR7
	RET

;Saved registers
PH_SR0 .fill 0
PH_SR1 .fill 0
PH_SR2 .fill 0
PH_SR7 .fill 0

;Variables
PH_C0  .fill x0001		;Col 1 -> 1
PH_C1  .fill x0010		;Col 2 -> 16
PH_C2  .fill x0100		;Col 3 -> 256
PH_C3  .fill x1000		;Col 4 -> 4096
PH_NL  .STRINGZ "\n"	;Newline string

;******************************************************************
;*** PRINTHD - Prints a single hex digit. 						***
;***------------------------------------------------------------***
;***			INPUT  - Uses R0.								***
;***			OUTPUT - Prints to console.						***
;******************************************************************
PRINTHD
	;Save used registers
	ST  R0, PD_SR0
	ST 	R1, PD_SR1
	ST 	R2, PD_SR2
	ST  R7, PD_SR7

	;Cleaning
	AND R1, R1, #0

PD_TOP
	;Check if above 9
	LD  R2, PD_CK		;Load R2 with -10
	ADD R1, R0, R2		;Add R0 and R2 into R1
	BRzp PD_OVER		;If zero or positive goto PD_OVER

	LD  R2, PD_30		;Load R2 with x0030
	ADD R0, R0, R2		;Add R0 and R2 into R0
	BR PD_END			;Branch unconditionally to PD_END

PD_OVER
	LD  R2, PD_55		;Load R2 with x0037
	ADD R0, R0, R2		;Add R0 and R2 into R0

PD_END
	;Print R0
	OUT
	;Restore registers
	LD  R0, PD_SR0
	LD  R1, PD_SR1
	LD  R2, PD_SR2
	LD  R7, PD_SR7
	RET

;Saved registers
PD_SR0  .fill 0
PD_SR1	.fill 0
PD_SR2	.fill 0
PD_SR7	.fill 0
;Variables
PD_CK	.fill #-10		; check number between 0 and 9
PD_30	.fill x0030		; 48  -> number convert
PD_55 	.fill x0037		; 55  -> capital char convert, offset by ten

;---------------------------ARITHMATIC-----------------------------

;******************************************************************
;*** MULTE - Multiply the values in R1 and R2 efficiently.      ***
;***------------------------------------------------------------***
;***			INPUT  - Uses R1 and R2.						***
;***			OUTPUT - Return the result in R0.				***
;******************************************************************
MULTE	
	;Save used registers.
	ST 	R1, ME_SR1
	ST	R2, ME_SR2
	ST 	R3, ME_SR3

	;Cleaning
	AND R0, R0, #0

	;Check for zeros
	ADD	R1, R1, #0		;Check R1 for 0
	BRz	ME_END			;If 0 goto ME_END
	ADD	R2, R2, #0		;Check R2 for 0
	BRz	ME_END			;If 0 goto ME_END

ME_TOP
	;Check which value is largest
	ADD R3, R1, #0
	NOT R3, R3
	ADD R3, R3, #1
	ADD R3, R2, R3
	BRp ME_R2

ME_R1
	ADD	R2, R2, #-1		;Subtract 1 from one mutiplicand
	ADD R0, R1, R0		;Add one multiplicand to sum
	ADD	R2, R2, #0		;Subtract 1 from one mutiplicand
	BRp	ME_R1			;If not zero, add again
	BR 	ME_END

ME_R2
	ADD	R1, R1, #-1		;Subtract 1 from one mutiplicand
	ADD R0, R2, R0		;Add one multiplicand to sum
	ADD	R1, R1, #0		;Subtract 1 from one mutiplicand
	BRp	ME_R2			;If not zero, add again

ME_END
	;Restore registers
	LD 	R1, ME_SR1
	LD	R2, ME_SR2
	LD 	R3, ME_SR3
	RET

;Saved Registers
ME_SR1 .fill 0
ME_SR2 .fill 0
ME_SR3 .fill 0

;******************************************************************
;*** DIV - Takes two numbers from the user then divides both.   ***
;***------------------------------------------------------------***
;***		INPUT  - Takes in R1 and R2.						***
;***		OUTPUT - Returns to R0.                  			***
;******************************************************************
DIV
	;Save used registers.
	ST  R1, D_SR1
	ST	R2, D_SR2

	;Cleaning
	AND R0, R0, #0

	;Convert R2 to negative 
	NOT  R2, R2
	ADD  R2, R2, #1

	;Check for zeros
	ADD	R1, R1, #0		;Check R1 for 0
	BRz	D_END			;Restore and return if Zero
	ADD	R2, R2, #0		;Check R2 for 0
	BRz	D_ZERO			;R1 set to -1 then restore and return if Zero

D_TOP	
	ADD	R1, R1, R2		;Subtract divisor from one dividend 
	BRN D_END			;Branch to D_END
	ADD R0, R0, #1		;Add one to quotient
	BR D_TOP			;Branch unconditionally

D_END	
	;Restore registers
	LD  R1, D_SR1
	LD	R2, D_SR2
	RET

D_ZERO
	;Set R0 to -1
	AND R0, R0, #0
	ADD R0, R0, #-1

	;Restore registers
	LD  R1, D_SR1
	LD	R2, D_SR2
	RET
		
D_SR1 .fill 0
D_SR2 .fill 0


;******************************************************************
;*** MOD - Takes two numbers from the user then uses modulo on  ***
;***       both. 												***
;***------------------------------------------------------------***
;***		INPUT  - Takes in R1 and R2.						***
;***		OUTPUT - Returns to R0.                  			***
;******************************************************************
MOD
	;Save used registers.
	ST  R1, MD_SR1
	ST	R2, MD_SR2

	;Cleaning
	AND R0, R0, #0

	;Convert R2 to negative 
	NOT  R2, R2
	ADD  R2, R2, #1

	ADD R0, R0, R1		;Copy R1 into R0

	;Check for zeros
	ADD	R1, R1, #0		;Check R1 for 0
	BRZ	MD_ZERO1		;Restore and return if Zero
	ADD	R2, R2, #0		;Check R2 for 0
	BRZ	MD_ZERO2		;R1 set to -1 then restore and return if Zero

MD_TOP	
	ADD R0, R0, R2		;Subtract one from quotient
	ADD	R1, R1, R2		;Subtract divisor from one dividend 
	BRzp MD_TOP			;Looping

	;Convert R2 to positive
	ADD R2, R2, #-1
	NOT R2, R2

	ADD R0, R0, R2		;Add R2 to R0, keep positive

MD_END	
	;Restore registers
	LD  R1, MD_SR1
	LD	R2, MD_SR2
	RET

MD_ZERO1
	;Reset R0
	AND R0, R0,  #0

	;Restore registers
	LD  R1, MD_SR1
	LD	R2, MD_SR2
	RET

MD_ZERO2
	;Set R0 to -1
	AND R0, R0,  #0
	ADD R0, R0, #-1

	;Restore registers
	LD  R1, MD_SR1
	LD	R2, MD_SR2
	RET
		
MD_SR1 .fill 0
MD_SR2 .fill 0

.END