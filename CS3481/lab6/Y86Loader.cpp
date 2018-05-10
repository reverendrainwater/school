/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. Y86Loader.cpp.
//	AUTHOR. Rev Taylor R Rainwater + Andrew Watson.
//	INSTALLATION. student.
//	DATE-WRITTEN. 10.02.2016.
//	DESCRIPTION. Y86 Loader.
//
/////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////
//
//    ERROR-DEFINITIONS.
//      ERROR 0a. Address Size Invalid.
//      ERROR 0b. Address Value Invalid.
//      ERROR 1. Pipe Character Placement Invalid.
//      ERROR 2. Failure to Write to Memory.
//
/////////////////////////////////////////////////////////////////////

#include "Y86.h"

//***************************CLASS-VARIABLES****************************//
int Y86::line_count;
char Y86::read_heap[CAP][CAP];

//**************************PROCEDURE-DIVISION**************************//

/**
 * trim - using the left and right arg as indices for the beginning
 *        and end of the section to trim, this function "trims" all
 *        characters inbetween them. That is, it prints all characters
 *        between 'left' and 'right' from the c string provided 
 *        by the 'n' arg. 
 * @param n     c string i[n]put, <TYPE> c string
 * @param left  left indice, <TYPE> int
 * @param right right indice, <TYPE> int
 */
void Y86::trim(char* n, char* o, int left, int right)
{   std::stringstream ss;
    char swp[512] = "\0";

    if (left <= right){
        for (int i = left; i <= right; i++) {
                ss << n[i];
        }
        ss >> swp;
        strcpy(o, swp); 
    } else {
        std::cout << "--+--ERROR IN TRIM: LEFT INDEX LARGER THAN RIGHT" << std::endl;
    }
}
/**
 * remove_white - removes whitespace from a string provided by the 'n' arg
 * 
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::remove_white(char* n)
{   int size = strlen(n);

    for (int i = 0; i < size; i++){
        if(n[i] == ' ' || n[i] == '\0'){
            n[i] = '\b';
        }
    }
}
/**
 * instr_len - returns length of an instruction 
 *             string.
 * @param  n c string i[n]put, <TYPE> c string
 * @return   length of an instruction
 */
int Y86::instr_len(char* n)
{
    remove_white(n);
    return strlen(n);
} 
/**
 * rev_instr - transposes bytes in the string 'n'.
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::rev_instr(char* n)
{   int len = instr_len(n);
    
    swab(n, n, len);
    std::reverse(n, n + strlen(n));
}
/**
 * check_ext - checks the extension of argument 'n' then 
 *             returns if the extension matches ".yo" in 
 *             the form of a boolean.
 * @param  n c string i[n]put, <TYPE> c string
 * @return   boolean value from 'valid'
 */
bool Y86::check_ext(char* n)
{	bool valid = false;
	char swap[CAP];
	char* arg = n;
	char ext[4] = ".yo";
	int arg_size = strlen(arg);
	int arg_off = arg_size - 3;

	trim(arg, swap, arg_off, arg_size);
	if (strcmp(swap, ext) == 0) {
		valid = true;
	}
	return valid;
}
/**
 * check_blank - takes the argument 'n' then checks if the c string is blank. 
 *               That is the string is either a newline, null, etc. 
 * @param  n c string i[n]put, <TYPE> c string
 * @return   boolean value from 'valid'
 */
bool Y86::check_blank(char* n)
{   bool valid = false;
    char c_swap[2];

    trim(n, c_swap, 0, 1);
    if (c_swap[0] != '0'){
        valid = true;
    }

    return valid;
}
/**
 * check_addrVal - takes the argument 'n' and determines whether the string
 *                 is a valid address value.
 * @param  n c string i[n]put, <TYPE> c string
 * @return   boolean from 'valid'
 */
bool Y86::check_addrVal(char* n)
{   bool valid = false;
    char swap[CAP];
    std::string s_swap;
    std::stringstream ss;

    strcpy(swap, n);
    if (swap[5] == ':' ? (swap[5] = '\0') : (swap[6] = '\0'))   // Terminate c string appropriately
    ;
    ss << swap;
    ss >> s_swap;
    valid = s_swap.compare(0, 2, "0x") == 0
      && s_swap.size() > 2
      && s_swap.find_first_not_of("0123456789abcdefABCDEF", 2) == std::string::npos;
    return valid;
}
/**
 * check_pipe - checks to makes sure the pipe character (i.e. "|")
 *              is located in the proper location of the string.
 * @param  n c string i[n]put, <TYPE> c string
 * @param  s integer [s]ize of address (3 or 4), <TYPE> integer
 * @return   boolean value from 'valid'
 */
bool Y86::check_pipe(char* n, int s)
{   bool valid = false;
    char swap[CAP];
    char* arg = n;
    char pipe[2] = "|";

    if (s == 3)
        trim(arg, swap, 28, 28);
    else
        trim(arg, swap, 29, 29);
    if (strcmp(swap, pipe) == 0) {
        valid = true;
    }
    return valid;
}
/**
 * get_addr - using argument 'n' this gets the instruction and puts
 *            it into the argument 'o'.
 * @param o c string [o]utput, <TYPE> c string
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::get_addr(char* o, char* n)
{
    trim(n, o, 0, 6);
}
/**
 * get_instruct - using argument 'n' this gets the instruction and puts
 *                it into the argument 'o'.
 * @param o c string [o]utput, <TYPE> c string
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::get_instruct(char* o, char* n)
{
    trim(n, o, 7, 26);
}
/**
 * check_addrSiz - takes the argument 'n' then determines whether the 
 *                 address within is three or four digits in length
 *                 then returns to 'o' the corresponding value. 
 * @param  n c string i[n]put, <TYPE> c string
 * @param  o integer [o]utput, <TYPE> integer
 */
void Y86::get_addrSiz(int &o, char* n)
{  
    if (n[5] == ':') {   // Look for colon
        o = 3;
    } else {
        o = 4; 
    }
}
/**
 * get_addrVal - takes argument 'n' and interprets the address value then
 *               returns the value to 'o'
 * @param o value [o]utput, <TYPE> uint64_t
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::get_addrVal(uint64_t &o, char* n)
{   std::string s_swap;
    std::stringstream ss;

    if (n[5] == ':' ? (n[5] = '\0') : (n[6] = '\0'))   // Terminate c string appropriately
    ;
    ss << n;
    ss >> std::hex >> o;

}
/**
 * get_instrVal - takes argument 'n' and interprets the instruction value then
 *                returns the value to 'o'
 * @param o value [o]utput, <TYPE> uint64_t
 * @param n c string i[n]put, <TYPE> c string
 */
void Y86::get_instrVal(uint64_t &o, char* n)
{   std::string s_swap;
    std::stringstream ss;

    rev_instr(n);
    ss << n;
    ss >> std::hex >> o;

}
/**
 * read - reads a Y86 object file line by line into the 2D array 
 *        'read_heap', checking the extension prior to reading.
 * @param  n c string i[n]put, <TYPE> c string
 * @return   line count of i
 */
bool Y86::read(char* n)
{   struct stat buffer;
    std::ifstream fin (n);
    std::string fswp;   // Swap String variable
    char swp[512];      // Swap c string variable
    int lines = 0;      // Counting variable
    bool valid = true;

    if (check_ext(n) != 1 || stat(n, &buffer) != 0){ 
        std::cout << "FILE FAILED OPEN: " << n << std::endl;
        std::cout << "USAGE: yess <filename>.yo" << std::endl;
        valid = false;
    } else if (fin.is_open()) {
        while (getline(fin, fswp)){
            strcpy(swp, fswp.c_str());
            strcpy(read_heap[lines], swp);
            lines++;
        }
    } 
    line_count = lines;
    return valid;
}
/**
 * validate - using the arguments provided, this function determines 
 *            whether the focused line is correctly formatted. This is
 *            not part of the "check_" function family because it needs 
 *            to be distinguished as unique.
 * @param  index       line number, <TYPE> integer
 * @param  addrSize    address size, <TYPE> integer
 * @param  address     address, <TYPE> c string
 * @param  instruction instruction, <TYPE> c string
 * @param  focus       focused file line, <TYPE> c string
 * @return             boolean of whether the file is correctly formatted
 */
bool Y86::validate(int index, int addrSize, char* address, char* instruction)
{   bool valid = false;

    if (addrSize != 0 && check_addrVal(address)){                    // Make sure size is not zero and is valid value
        if (check_pipe(read_heap[index], addrSize)){                 // Check pipe character placement
            if (instruction[0] != '\0'){
                valid = true;
            } 
        } else {
            std::cout << "ERROR ON LINE: " << (index + 1) << std::endl         // Error for bad pipe character
            << read_heap[index] << std::endl << ERROR1 << std::endl;
        }
    } else if (check_addrVal(read_heap[index]) == false){
        if (check_blank(read_heap[index]) == false){                           // Make sure not comment line
            std::cout << "ERROR ON LINE: " << (index + 1) << std::endl         // Error for invalid address value
            << read_heap[index] << std::endl << ERROR0B << std::endl;
        }
    } else if (addrSize == 0){
        std::cout << "ERROR ON LINE: " << (index + 1) << std::endl             // Error for invalid address size
        << read_heap[index] << std::endl << ERROR0A << std::endl;
    }
    return valid;
}
/**
 * write_mem - takes arguments as c strings and translates the c string
 *             to its 64 bit equivalent. 
 * @param address     address string, <TYPE> c string
 * @param instruction instruction string, <TYPE> c string
 */
void Y86::write_mem(char* address, char* instruction)
{   uint64_t addrVal = 0;   // Used for swap with address value
    uint64_t instrVal = 0;  // Used for swap with instruction value

    get_addrVal(addrVal, address);          // Get address value into 'addrVal'
    get_instrVal(instrVal, instruction);    // Get instruction value into 'instrVal'
    memory.putWord(addrVal, instrVal);        // Store the instruction
    //std::cout << address << " " << instruction << std::endl;
}
/**
 * load - the load function takes the arguments 'argv' as the file to be loaded
 *        and the index of the filename from 'argc'.
 * @param  argc index of argument array, <TYPE> integer
 * @param  argv argument array, <TYPE> c string array
 * @return boolean of true if the load was successful; from 'valid'
 */
bool Y86::load(int argc, char* argv[])
{   int addrSize = 0;       // Used for swap with address size value
    //char focus[CAP];        // Used for swap with 'read_heap'
    char address[8];        // Used for swap with address
    char instruction[20];   // Used for swap with instruction
    bool valid = true;      // Return variable

    if (argc > 1){
        if ((valid = read(argv[1]))){                             // Read the file into 'read_heap' 
            for (int i = 0; i < line_count; i++){              // Begin Iteration through 'read_heap'
                get_addr(address, read_heap[i]);                                        // Trim out address 
                get_addrSiz(addrSize, address);                                  // Get address size into 'addrSize'
                get_instruct(instruction, read_heap[i]);                                // Trim out instruction
                if (instruction[5] == ':') { instruction[0] = '\0'; };           // TODO: Fix this! Why does it sometime put address into instruction?
                if (validate(i, addrSize, address, instruction)){         // If valid formatting, continue
                    write_mem(address, instruction);
                } 
                // Reset variables
                addrSize = 0;
                memset(address, 0, strlen(address));
                memset(instruction, 0, strlen(instruction));
            }
        }
    } else {
        std::cout << "FILE FAILED OPEN." << std::endl;
        std::cout << "USAGE: yess <filename>.yo" << std::endl;
        valid = false;
    } 
    return valid;
}
