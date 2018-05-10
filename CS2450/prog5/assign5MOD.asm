;------------------------------MAIN--------------------------------
	.orig 	x3000
	JSR	GET_NUM		;Get a single digit number into R0
	ADD	R1, R0, #0	;Move R0 to R1.
	JSR	GET_NUM		;Get a single digit number into R0.
	ADD	R2, R0, #0	;Move R0 to R2
	JSR	MOD			;Multiply R1 and R2. Result is in R0.
	
	HALT

;Replace JSR MULT with JSR DIV and submit as assign5_DIV.obj	
;Replace JSR MULT with JSR MOD and submit as assign5_MOD.obj
	
;---------------------------SUBROUTINES-----------------------------

;******************************************************************
;*** GET_NUM - Get a single number from the user. Loop until    ***
;***           the user enters a value from 0 to 9.             ***
;***           Return the result in R0                          ***
;******************************************************************
GET_NUM		 	
	
	;Save used registers
	ST	R7, GN_SR7	;R7 altered by GETC and PUTS
	ST	R1, GN_SR1	;R1 used for intermediate result
	ST	R2, GN_SR2	;R2 Store print character
	
GN_TOP	
	LEA	R0, GN_S1
	PUTS			;Print prompt
	GETC			;Get a character from the user
	
	ADD	R2, R0, #0	;Save the character in R2 for later printing
	
	LD	R1, GN_N30	;Subtract 48 from the ASCII code
	ADD	R0, R0, R1	;to create the correct value. 
				
				
	BRN	GN_TOP	;If the number is negative after this
				;it was below 0 on the ASCII chart.
				;Get another character
	
	ADD	R1, R0, #-10	;If the number -10 is positive it is
	BRP	GN_TOP		;something above 9 on the ASCII chart.
				;Get another character

	ADD	R1, R0, #0	;Move number from R0 to R1
	ADD	R0, R2, #0	;Move character from R2 to R0
	OUT			;Print character
	AND	R0, R0, #0	;Clear R0
	ADD	R0, R0, #10	;Put newline in R0
	OUT			;Print newline
	
	ADD	R0, R1, #0	;Move numeric answer back to R0
	
	;Restore the registers
	LD	R7, GN_SR7
	LD	R1, GN_SR1	
	LD	R2, GN_SR2	
	RET
	
GN_SR1	.fill 0
GN_SR2	.fill 0
GN_SR7	.fill 0
GN_N30	.fill xFFD0	; -0x30 or -48
GN_48	.fill 48	; 
GN_S1 	.STRINGZ	"Enter a single digit (0-9): "

;******************************************************************
;*** MULT - Multiply the values in R1 and R2.                   ***
;***        Return the result in R0                             ***
;******************************************************************
MULT	
	;Save used registers.
	ST	R2, M_SR2	;R2 used for intermediate result


	;Add R1 to R0, then subtract 1 from R2.
	;Repeat until R2 = 0.  

	AND R0, R0, #0	;Clear out R0

	ADD	R1, R1, #0	;Check R1 for 0
	BRZ	M_END		;Restore and return if Zero
	ADD	R2, R2, #0	;Check R2 for 0
	BRZ	M_END		;Restore and return if Zero
	

M_TOP	
	ADD R0, R1, R0	;Add one multiplicand to sum
	ADD	R2, R2, #-1	;Subtract 1 from one mutiplicand
	BRP	M_TOP		;If not zero, add again

	;Restore registers
M_END	
	LD	R2, M_SR2	;Restore R2 before returning
	RET			;Ends the MULT subroutine

		
M_SR2 .fill 0

;Create DIV and MOD subroutines below this line but before the .end

;******************************************************************
;***DIV - Takes two numbers from the user and divides them.     ***
;***      Takes in R1 and R2. Returns to R0.                    ***
;******************************************************************
DIV
	;Save used registers.
	ST  R1, D_SR1   ;R1 for swap
	ST	R2, D_SR2	;R2 used for intermediate result

	;Store R2 in R3 and convert R3 to negative 
	NOT  R2, R2
	ADD  R2, R2, #1

	;Add R3 to R0, then subtract 1 from R2.
	;Repeat until R2 = 0.  

	AND R0, R0, #0	;Clear out R0

	ADD	R1, R1, #0	;Check R1 for 0
	BRZ	D_END		;Restore and return if Zero
	ADD	R2, R2, 0	;Check R2 for 0
	BRZ	D_ZERO		;R1 set to -1 then restore and return if Zero

D_TOP	
	ADD	R1, R1, R2	;Subtract divisor from one dividend 
	BRN D_END
	ADD R0, R0, #1	;Add one to quotient
	BRNZP D_TOP		;Looping

	;Restore registers
D_END	
	LD  R1, D_SR1	;Restore R1
	LD	R2, D_SR2	;Restore R2 before returning
	RET			    ;Ends the DIV subroutine

D_ZERO
	AND R0, R0, #0
	ADD R0, R0, #-1

	LD  R1, D_SR1   ;Restore R1
	LD	R2, D_SR2	;Restore R2 before returning
	RET			    ;Ends the DIV subroutine
		
D_SR1 .fill 0
D_SR2 .fill 0

MOD
	;Save used registers.
	ST  R1, MD_SR1   ;R1 for swap
	ST	R2, MD_SR2	;R2 used for intermediate result

	;Store R2 in R3 and convert R3 to negative 
	NOT  R2, R2
	ADD  R2, R2, #1

	ADD R0, R0, R1	;Clear out R0

	ADD	R1, R1, #0	;Check R1 for 0
	BRZ	MD_ZERO1		;Restore and return if Zero
	ADD	R2, R2, #0	;Check R2 for 0
	BRZ	MD_ZERO2		;R1 set to -1 then restore and return if Zero

MD_TOP	
	ADD R0, R0, R2	;Subtract one from quotient
	ADD	R1, R1, R2	;Subtract divisor from one dividend 
	BRZP MD_TOP		;Looping

	;Restore registers
MD_END	
	LD  R1, MD_SR1	;Restore R1
	LD	R2, MD_SR2	;Restore R2 before returning
	RET			    ;Ends the DIV subroutine

MD_ZERO1
	AND R0, R0,  #0

	LD  R1, MD_SR1   ;Restore R1
	LD	R2, MD_SR2	;Restore R2 before returning
	RET			    ;Ends the DIV subroutine

MD_ZERO2
	AND R0, R0,  #0
	ADD R0, R0, #-1

	LD  R1, MD_SR1   ;Restore R1
	LD	R2, MD_SR2	;Restore R2 before returning
	RET			    ;Ends the DIV subroutine
		
MD_SR1 .fill 0
MD_SR2 .fill 0

.end