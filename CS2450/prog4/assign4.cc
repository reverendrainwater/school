#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. assign4.
//	AUTHOR. Taylor R. Rainwater.
//	INSTALLATION. alduin.
//	DATE-WRITTEN. 19.10.15.
//	DESCRIPTION. Assignment Four for CS2450.
//
/////////////////////////////////////////////////////////////////////
/**************ENVIRONMENTAL-VARIABLES.**************/
  // Defined Opcodes
	#define start 0x3000
	#define addOp 0x1
	#define andOp 0x5
	#define brOp 0x0
	#define jmpOp 0xC
	#define jsrOp 0x4
	#define ldOp 0x2
	#define ldiOp 0xA
	#define ldrOp 0x6
	#define leaOp 0xE
	#define notOp 0x9
	#define rtiOp 0x8
	#define stOp 0x3
	#define stiOp 0xB
	#define strOp 0x7
	#define trapOp 0xF
  // Masks
	int opcMsk = 0xF000;	// Opcode Mask
	int msk0 = 0xE00;		// Reg Mask 0 
	int msk1 = 0x1C0;		// Reg Mask 1
	int msk2 = 0x7;			// Reg Mask 2
	int brCC = 0xE00;		// Branch CC 
	int brN = 0x800;		// Branch N
	int brZ = 0x400;		// Branch Z
	int brP = 0x200;		// Branch P
	int flgMsk = 0x20;		// Immediate or Reg Mask 
	int jsrMsk = 0x800;		// JSR or JSRR mask
	int off9Msk = 0x1FF;	// Offset of 9
	int off11Msk = 0x7FF;	// Offset of 11
	int off6Msk = 0x3F;		// Offset of 6
	int immMsk = 0x1F;		// Immediate for Add
	int trpMsk = 0xFF; 		// Trap Vector Mask
  // Opcode Strings
	char add[] = "ADD";
	char an[] = "AND";
	char br[] = "BR";
	char jmp[] = "JMP";
	char jsr[] = "JSR";
	char jsrr[] = "JSRR";
	char ld[] = "LD";
	char ldi[] = "LDI";
	char ldr[] = "LDR";
	char lea[] = "LEA";
	char no[] = "NOT";
 	char ret[] = "RET";
  	char rti[] = "RTI";
  	char st[] = "ST";
  	char sti[] = "STI";
  	char str[] = "STR";
  	char trap[] = "TRAP";
  // Misc Strings
	char nStr[] = "N";
  	char zStr[] = "Z";
  	char pStr[] = "P";
  	char comma[] = ",";
  	char halt[] = "HALT";
	char kbd[] = "KEYBOARD";
	char disp[] = "DISPLAY";
  	char space[] = " ";
	char tab[] = "\t";
  	char rStr[] = "R";		// As in register, R1, R2, ...
  	char error[] = "ERROR";
  	char hash[] = "#";	// For numbers from immediates
  	char tst0[] = "test0.hex";
  	char tst1[] = "test1.hex";
  	char tst2[] = "test2.hex";
  // Specific Variables
  	//int pc = 0x0;		// PC Counter
	int reg = 0x0;		// For use in the opcode functions
	int flg = 0x0;		// For use when finding the flag of an opcode
	int imm = 0x0; 		// For use in immediate swap
	int immFlg = 0x0;	// Immediate flag swap
	int nzp = 0x0;		// NZP Flags swap
	int off = 0x0;		// Offset swap
  // Prototypes
	void printAdd( int n);
	void printAnd( int n);
	void printBr( int n, int o);
	void printJmpRet( int n);
	void printJsrJsrr( int n, int o);
	void printLd( int n, int o);
	void printLdi( int n, int o);
	void printLdr( int n);
	void printLea( int n, int o);
	void printNot( int n);
	void printRti( int n);
	void printSt( int n, int o);
	void printSti( int n, int o);
	void printStr( int n);
	void printTrap( int n);
	int negPos( int n, int v);

/**************PROCEDURES-DIVISION.**************/

/**************************************
   Takes a hex file name as an argument
   then prints the instruction for each 
   hex value. 
**************************************/
void printAssembly(char *in){
/*FUNCTION-VARIABLES.*/
	FILE *f;
	int num[80];
	int instr = 0x0;
	int instrPtr = 1;			// Offset by one for the PC
	int i = 0;
	int pc = 0;

/*FUNCTIONS-DIVISION.*/
	f = fopen(in, "r");
	if (f == NULL){
		fprintf(stderr, "Error 0: File Not Found\n");
  		exit(1);
	}
	while (!feof(f)){
        	fscanf(f, "%x", &num[i]);
			i++;
    	}
    	fclose(f);
	pc = num[0] - 1;				// Offset by one to account for first instr
	while (num[instrPtr] != 0){
		pc = pc + 1;
		printf("%.4x  |  ", pc);
		instr = num[instrPtr];
		instr = (instr & opcMsk) >> 12;
		switch(instr){
			case addOp :	
				printAdd(num[instrPtr]);
				break;
			case andOp :	
				printAnd(num[instrPtr]);
				break;
			case brOp :
				printBr(num[instrPtr], pc);
				break;
			case jmpOp :
				printJmpRet(num[instrPtr]);
				break;
			case jsrOp :
				printJsrJsrr(num[instrPtr], pc);
				break;
			case ldOp :
				printLd(num[instrPtr], pc);
				break;
			case ldiOp :
				printLdi(num[instrPtr], pc);
				break;
			case ldrOp :	
				printLdr(num[instrPtr]);
				break;
			case leaOp :
				printLea(num[instrPtr], pc);
				break;
			case notOp :	
				printNot(num[instrPtr]);
				break;
			case rtiOp :	
				printRti(num[instrPtr]);
				break;
			case stOp :
				printSt(num[instrPtr], pc);
				break;
			case stiOp :
				printSti(num[instrPtr], pc);
				break;
			case strOp :	
				printStr(num[instrPtr]);
				break;
			case trapOp :	
				printTrap(num[instrPtr]);
				break;
		}
	++instrPtr;
	}
}

void printAdd( int n){
	printf("%s%s%s", add, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d, %s", reg, rStr);
	reg = (n &msk1) >> 6;
	printf("%d%s ", reg, comma);
	flg = (n & flgMsk) >> 5;
	if (flg == 0){ 
		reg = (n & msk2);
		printf("%s%d", rStr, reg);
	} else if (flg == 1) {				
		imm = (n & immMsk);
		immFlg = (imm & 0x0010) >> 4;
		if (immFlg == 0){				// Is Positive
			printf("%s%d", hash, imm);
		} else {
			imm = negPos(imm, 5);
			printf("%s-%d", hash, imm);
		}
	}
	printf("\n");
	reg = 0x0; flg = 0x0; imm = 0x0;
}

void printAnd( int n){
	printf("%s%s%s", an, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	reg = (n & msk1) >> 6;
	printf("%s%d%s ", rStr, reg, comma);
	flg = (n & flgMsk) >> 4;
	if (flg == 0){
		reg = (n & msk2);
		printf("%s%d", rStr, reg);
	} else {
		imm = (n & immMsk);
		immFlg = (imm & 0x0010) >> 4;
		if (immFlg == 0){				// Is Positive
			printf("%s%d", hash, imm);
		} else {
			imm = negPos(imm, 5);
			printf("%s-%d", hash, imm);
		}
	}
	printf("\n");
	reg = 0x0; flg = 0x0; imm = 0x0;
}

void printBr( int n, int o){
	printf("%s", br);
	nzp = (n & brN) >> 11;
	if (nzp == 1){ printf("%s", nStr);}
	nzp = (n & brZ) >> 10;
	if (nzp == 1){ printf("%s", zStr);}
	nzp = (n & brP) >> 9;
	if (nzp == 1){ printf("%s ", pStr);}
	printf("%s", tab);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		off = negPos(off, 9);
		o = (o - off) + 1;
	}
	printf("x%.4X", o);						
	printf("\n");
}
void printJmpRet( int n){
	reg = (n & msk1) >> 6;
	if (reg - 7 == 0){ printf("%s", ret);}
	else {
		printf("%s%s%s%d", jmp, tab, rStr, reg);
	}
	printf("\n");
	reg = 0x0;
}

void printJsrJsrr( int n, int o){
	flg = (n & jsrMsk) >> 11;
	if (flg > 0){
		printf("%s%s", jsr, tab);
		off = (n & off11Msk);
		flg = (off & 0x0100) >> 8;
		if (flg == 0){ 
			o = (o + off) + 1;
		}
		else { 
			off = negPos(off, 9);
			o = (o - off) + 1;
		}
	printf("x%.4X", o);	
	} else {
		reg = (n & msk1) >> 6;
		printf("%s%s%s%d", jsrr, tab, rStr, reg);
	}
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printLd( int n, int o){
	printf("%s%s%s", ld, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		off = negPos(off, 9);
		o = (o - off) + 1;
	}
	printf("x%.4X", o);	
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printLdi( int n, int o){
	printf("%s%s%s", ldi, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		o = (o - off) + 0x0101;
	}
	printf("x%.4X", o);	
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printLdr( int n){
	printf("%s%s%s", ldr, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	reg = (n & msk1) >> 6;					
	printf("%s%d%s ", rStr, reg, comma);
	off = n & off6Msk;
	flg = (off & 0x0020) >> 5;
	if (flg == 0){							// Is Positive
		printf("%s%d", hash, off);
	} else {
		off = negPos(off, 6);
		printf("%s-%d", hash, off);
	}
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printLea( int n, int o){
	printf("%s%s%s", lea, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		off = negPos(off, 9);
		o = (o - off) + 1;
	}
	printf("x%.4X", o);	
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printNot( int n){
	printf("%s%s%s", no, tab, rStr);
	reg = (n & msk0) >>9;
	printf("%d%s %s", reg, comma, rStr);
	reg = (n & msk1) >> 6;
	printf("%d", reg);
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printRti( int n){
	printf("%s", rti);
	printf("\n");
}

void printSt( int n, int o){
	printf("%s%s%s", st, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		off = negPos(off, 9);
		o = (o - off) + 1;
	}
	printf("x%.4X", o);	
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printSti( int n, int o){
	printf("%s%s%s", sti, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	off = (n & off9Msk);
	flg = (off & 0x0100) >> 8;
	if (flg == 0){ 
		o = (o + off) + 1;
	}
	else { 
		negPos(off, 9);
		o = (o - off) + 0x0101;
	}
	printf("x%.4X", o);	
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printStr( int n){
	printf("%s%s%s", str, tab, rStr);
	reg = (n & msk0) >> 9;
	printf("%d%s ", reg, comma);
	reg = (n & msk1) >> 6;					
	printf("%s%d%s ", rStr, reg, comma);
	off = n & off6Msk;
	flg = (off & 0x0020) >> 5;
	if (flg == 0){							// Is Positive
		printf("%s%d", hash, off);
	} else {
		off = negPos(off, 6);
		printf("%s-%d", hash, off);
	}
	printf("\n");
	reg = 0x0; flg = 0x0; off = 0x0;
}

void printTrap( int n){
	printf("%s%s", trap, tab);
	if (n == 0xF021){ printf("%s", disp);}
	else if (n == 0xF023){ printf("%s", kbd);}
	else if (n == 0xF025){ printf("%s", halt);}
	printf("\n");
}

/**
 * @brief Negative 2's Complement to Positive
 * @details Takes negative and converts to positive.
 * 
 * @param n negative number
 * @param v offset size
 * @return positive number
 */
int negPos( int n, int v){
	if (v == 5){
		n = n & 0x000F;
		n = n ^ 0x000F;
		n = n + 0x0001;
	} else if (v == 6){
		n = n & 0x001F;
		n = n ^ 0x001F;
		n = n + 0x0001;
	} else if (v == 9){
		n = n & 0x00FF;
		n = n ^ 0x00FF;
		n = n + 0x0001;
	}
	return n;
}

int main(){
	char x = 'B';

	printf("Enter Test Option:\n");
	printf("A. Test 0\nB. Test 1\nC. Test 2\n");
	scanf("%c", &x);
	printf("-------------------------------------------------\n");
	printf("PC	Instruction\n");
	printf("-------------------------------------------------\n");
	if (x == 'A' || x == 'a'){printAssembly(tst0);}
	else if (x == 'B' || x == 'b'){printAssembly(tst1);}
	else if (x == 'C' || x == 'c'){printAssembly(tst2);}
	else {printf("Error 1: No Test Found");}
}
