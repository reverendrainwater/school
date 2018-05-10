/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION-DIVISION.
//	PROGRAM-ID. trrdis.cpp.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 28.1.2016.
//	DESCRIPTION. y86 object file disassembler.
//	FORWARD. Please refer to the formatting division for any 
//			 unknown documentation formatting style.
//			 
/////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////
//
//	FORMATTING-DIVISION.
//	DEFINITION-FORMAT. [#] [SYMBOL] - [DESCRIPTION]
//	
//	1. Rf. - refer to/reference to
//	2. @param - arguments for funtion with <TYPE> provided 
//	3. @return - the return type of the function 
//
/////////////////////////////////////////////////////////////////////

#include "y86dis.h"

//*************************PROTOTYPES*************************//
// in order of appearance, excluding main. 
void myprint(const char n[], int nl, int dly);
void trim(char* n, char* o, int left, int right);
void removeWhite(char* n);
void strrev(int right, char* n);
int chex_ihex(char* n);
int detectOp(char n[]);
int read(char n[]);
void quads(char* n);
void deep(int op, char* addr, char* dest);
void process(char n[]);
//****************************MAIN****************************//
int main(int argc, char* argv[])
{   
    process(argv[1]);
}
/**
 * myprint - print the c string provided by the 'n' arg with the amount
 * 			 of newlines printed based on the 'nl' arg and the dly on 
 * 			 each char print based on the 'dly' arg.
 * @reasoning - I made this print function because I wanted to print 
 *            	a string without having the, for what I consider 
 *            	untidy, newline characters within the printf. I also
 *            	sometimes like to pretend I am programming on the WOPR
 *            	(Rf. War Games), so I have the option of creating a 
 *            	delayed print on each character. 
 * @param n   c string i[n]put, <TYPE> c string
 * @param nl  amount of newlines, <TYPE> int
 * @param dly amount of delay in microseconds, <TYPE> int
 */
void myprint(const char n[], int nl, int dly)
{	int len = strlen(n); // c string length

    for (int i = 0; i < len; i++){
        std::cout << n[i];
        fflush(stdout);
        usleep(dly*10000);
    }
    while (nl > 0){
        printf("\n");
        nl--;
    }

}
/**
 * trim - using the left and right arg as indices for the beginning
 * 		  and end of the section to trim, this function "trims" all
 * 		  characters inbetween them. That is, it prints all characters
 * 		  between 'left' and 'right' from the c string provided 
 *        by the 'n' arg. 
 * @param n     c string i[n]put, <TYPE> c string
 * @param left  left indice, <TYPE> int
 * @param right right indice, <TYPE> int
 */
void trim(char* n, char* o, int left, int right)
{   std::stringstream ss;
    char swp[512];

    for (int i = left; i <= right; i++) {
            ss << n[i];
    }
    ss >> swp;
    strcpy(o, swp); 
}
/**
 * removeWhite - removes whitespace from a string provided by the 'n' arg
 * 
 * @param n c string i[n]put, <TYPE> c string
 */
void removeWhite(char* n)
{   int size = strlen(n);

    for (int i = 0; i < size; i++){
        if(n[i] == ' ' || n[i] == '\0'){
            n[i] = '\b';
        }
    }
}
/**
 * strrev - reverses any c string provided by
 *          the 'n' arg.
 * @param n c string i[n]put, <TYPE> c string
 * @param right int of end, <TYPE> int
 */
void strrev(int right, char* n)
{   char swp[32];

    swp[15] = n[1];
    swp[14] = n[0];
    swp[13] = n[3];
    swp[12] = n[2];
    swp[11] = n[5];
    swp[10] = n[4];
    swp[9] = n[7];
    swp[8] = n[6];
    swp[7] = n[9];
    swp[6] = n[8];
    swp[5] = n[11];
    swp[4] = n[10];
    swp[3] = n[13];
    swp[2] = n[12];
    swp[1] = n[15];
    swp[0] = n[14];

    swp[right] = '\0';             // Truncate array
    strcpy(n, swp);                    // Copy to origin
}
/**
 * chex_ihex - Char hex to int hex, this converts using a strigstream
 *         
 * @param  n c string i[n]put, <TYPE> c string
 * @return   int value of 'n' arg
 */
int chex_ihex(char* n)
{   int out;
    std::stringstream ss;

    ss << n;
    ss >> std::hex >> out;
    return out;
}
/**
 * detectOp - detect the OP code of a c string provided by the 'n' arg 
 *            then returns the int value of what mnemonic to print and
 *            which instruction function should be selected.
 * @param n c string i[n]put, <TYPE> c string 
 * @return  int for the map key of OP code
 */
int detectOp(char n[])
{   auto find = operations.find(n);

    return find->second;
}
/**
 * read - read takes a file name and reads the file line by line into
 *        the 2D array read_heap for process to later use. 
 * @param n file name arg, <TYPE> c string
 * @return  int of line count, <TYPE> int
 */
int read(char n[])
{   std::ifstream fin (n);
    std::string fswp;
    char swp[512];
    int i = 0;

    if (fin.is_open()) {
        while (getline(fin, fswp)){
            strcpy(swp, fswp.c_str());
            strcpy(read_heap[i], swp);
            i++;
        }
    } else {
        myprint("ERROR IN READ", 2, 7);
    }
    return i;
}
/**
 * quad - a special funtion for quad (eight byte words) which uses
 *        the c string passed to it for creation. 
 * @param n c string i[n]put, <TYPE> c string
 */
void quads(char* n)
{   char qval[32];
    char quad[6] = ".quad";

    strrev(16, n);
    strcpy(qval, n);
    std::cout << std::setw(10) << std::left << quad;                   // Push .quad
    std::cout << std::setw(2) << "0x";                                 // Push hex tag
    std::cout << std::setw(16) << std::right << std::setfill('0') 
        << std::hex << qval << std::endl;                              // Push qval and newline
}
/**
 * deep - deep takes the now confirmed non-halt, illegal, or quad
 *        instruction via the c string 'dest' arg, its address from
 *        the 'addr' arg, and its key from the 'op' arg and 
 *        completes the more complicated instruction printing.     
 * @param op  map key i[n]put, <TYPE> int
 * @param addr address i[n]put, <TYPE> c string
 * @param i  int index i[n]put, <TYPE> int
 */
void deep(int op, int i, char* addr)
{   int i_v;
    int i_d;
    int i_rA;
    int i_rB;
    char v[32];
    char d[32];
    char rA[8];
    char rB[8];
    auto fetch = instructions.find(op);
    auto reg = registers.find(0);

    switch( op ) {
        case 1: 
            std::cout << std::setw(10) << std::left << fetch->second << std::endl;   // Push nop
            break;
        case 2: 
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 3: 
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 4:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 5:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 6:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 7:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 8:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 9:  
            trim(read_heap[i], rB, 10, 10);                                          // Get register
            trim(read_heap[i], v, 11, 26);                                           // Get immediate
            i_rB = chex_ihex(rB);                                                    // Convert register to int
            strrev(16, v);                                                           // Reverse string
            i_v = chex_ihex(v);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "$0x" << std::hex << i_v;                                   // Push immediate 
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 10:  
            trim(read_heap[i], rA, 9, 9);                                            // Get register A
            trim(read_heap[i], rB, 10, 10);                                          // Get register B
            trim(read_heap[i], d, 11, 26);                                           // Get immediate
            i_rA = chex_ihex(rA);                                                    // Conver rA to int
            i_rB = chex_ihex(rB);                                                    // Convert rB to int
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << reg->second << ", ";                                        // Push rA
            std::cout << "$0x" << i_d;                                               // Push immediate 
            reg = registers.find(i_rB);
            std::cout << "(" << reg->second << ")" << std::endl;                     // Push rB
            break;
        case 11:  
            trim(read_heap[i], rA, 9, 9);                                            // Get register A
            trim(read_heap[i], rB, 10, 10);                                          // Get register B
            trim(read_heap[i], d, 11, 26);                                           // Get immediate
            i_rA = chex_ihex(rA);                                                    // Conver rA to int
            i_rB = chex_ihex(rB);                                                    // Convert rB to int
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "$0x" << i_d;                                               // Push immediate 
            reg = registers.find(i_rB);
            std::cout << "(" << reg->second << "), ";                                // Push rA
            reg = registers.find(i_rA);
            std::cout << reg->second << std::endl;                                   // Push rB
            break;
        case 12:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 13:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 14:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 15:  
            trim(read_heap[i], rA, 9, 9);                                            // Get registers
            trim(read_heap[i], rB, 10, 10);
            i_rA = chex_ihex(rA);                                                    // Convert to int
            i_rB = chex_ihex(rB);
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            reg = registers.find(i_rA);
            std::cout << std::left << reg->second;                                   // Push first register
            reg = registers.find(i_rB);
            std::cout << ", " << reg->second << std::endl;                           // Push second register
            break;
        case 16:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 17:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 18:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 19:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 20:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 21:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 22:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 23:  
            trim(read_heap[i], d, 9, 25);                                            // Get immediate
            strrev(16, d);                                                           // Reverse string
            i_d = chex_ihex(d);                                                      // Convert to int
            std::cout << std::setw(10) << std::left << fetch->second;                // Push mnemonic
            std::cout << "0x" << std::hex << i_d << std::endl;                       // Push immediate 
            break;
        case 24:  
            std::cout << std::setw(10) << fetch->second << std::endl;
            break;
        case 25:  
            trim(read_heap[i], rA, 9, 9);
            i_rA = chex_ihex(rA);
            reg = registers.find(i_rA);
            std::cout << std::setw(10) << fetch->second 
                << reg->second << std::endl;
            break;
        case 26:  
            trim(read_heap[i], rA, 9, 9);
            i_rA = chex_ihex(rA);
            reg = registers.find(i_rA);
            std::cout << std::setw(10) << fetch->second 
                << reg->second << std::endl;
            break;
    }

}
/**
 * process - this is where everything is linked back together
 *           in a single function which takes the file name
 *           from the 'n' arg, then reads each line followed by 
 *           detecting the operation and processing the 
 *           instruction. 
 * @reasoning I like to leave main "clean" so I can use it as
 *            pseudo testbed with option menus that won't 
 *            accidentally mess up the primary operations. 
 * @param n   c string i[n]put, <TYPE> c string
 */
void process(char n[])
{   int op;
    int size;
    char addr[CAP];
    char opc[CAP];
    char dest[CAP];
    char empty[CAP];
    char ill[18] = "illegal op";
    auto search = instructions.find(detectOp(opc));
    auto illegal = operations.find(opc);

    size = read(n);                                                              // Get file line count
    for (int i = 0; i < size; i++){                                              // Begin iteration of read_heap
        trim(read_heap[i], addr, 0, 6);                                          // Get address into addr
        trim(read_heap[i], empty, 7, 28);                                        // Get none address characters into empty
        if (strcmp(empty, "\n") > 0 || strcmp(empty, "") > 0){                     // Check for empty line or random newlines
            trim(read_heap[i], opc, 7, 8);                                         // Get OP code byte into opc
            trim(read_heap[i], dest, 8, 28);                                       // Get destination into dest
            op = detectOp(opc);                                                    // Get OP code map key using detectOp()
            removeWhite(empty);                                                    // Remove white space
            std::cout << std::setw(10) << std::left << addr;                       // Push address
            if (instructions.count(op) && op != 0 && strlen(empty) != 16){
                deep(op, i,  addr);                                              // Call the deep() function for advances instructions
            } else if (strlen(empty) == 16){                                     // Check if .quad
                quads(empty);
            } else if (operations.find(opc) == operations.end()){
                std::cout << std::setw(10) << std::left << ill << std::endl;       // Push illegal operation
            } else if (op == 0){
                search = instructions.find(op);
                std::cout << std::setw(10) << std::left << search->second          // Push halt
                    << std::endl;
            } else {
                std::cout << "ERROR IN PROCESSING" << std::endl;
            }
        } else {
            std::cout << addr << std::endl;                                       // If empty line, print address and end cycle
        }           
        std::cout.fill(' ');                                                      // Reset fill character
    }
}
