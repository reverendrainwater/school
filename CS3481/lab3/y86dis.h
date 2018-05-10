/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. y86dis.h.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. prophet.
//	DATE-WRITTEN. 29.1.2016.
//	DESCRIPTION. Y86 diassembler header file.
//
/////////////////////////////////////////////////////////////////////

#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstring>
#include <cstdlib>
#include <string.h>
#include <unistd.h>

//****************************GLOBAL-VARIABLES****************************//

#define CAP 512

// Maps for instructions
std::map<int, std::string> instructions = {{0, "halt"}, {1, "nop"}, {2, "rrmovq"}, {3, "cmovle"}, {4, "cmovl"},
										   {5, "cmove"}, {6, "cmovne"}, {7, "cmovge"}, {8, "cmovg"}, {9, "irmovq"},
										   {10, "rmmovq"}, {11, "mrmovq"}, {12, "addq"}, {13, "subq"}, {14, "andq"},
										   {15, "xorq"}, {16, "jmp"}, {17, "jle"}, {18, "jl"}, {19, "je"},
										   {20, "jne"}, {21, "jge"}, {22, "jg"}, {23, "call"}, {24, "ret"},
										   {25, "pushq"}, {26, "popq"}};
std::map<std::string, int> operations = {{"00", 0}, {"10", 1}, {"20", 2}, {"21", 3}, {"22", 4},
										 {"23", 5}, {"24", 6}, {"25", 7}, {"26", 8}, {"30", 9}, 
										 {"40", 10}, {"50", 11}, {"60", 12}, {"61", 13}, {"62", 14}, 
										 {"63", 15}, {"70", 16}, {"71", 17}, {"72", 18}, {"73", 19},
										 {"74", 20}, {"75", 21}, {"76", 22}, {"80", 23}, {"90", 24}, 
										 {"a0", 25}, {"b0", 26}};
// Map for registers										 
std::map<int, std::string> registers = {{0, "%rax"}, {1, "%rcx"}, {2, "%rdx"}, {3, "%rbx"},
										{4, "%rsp"}, {5, "%rbp"}, {6, "%rsi"}, {7, "%rdi"},
										{8, "%r8"}, {9, "%r9"}, {10, "%r10"}, {11, "%r11"},
										{12, "%r12"}, {13, "%r13"}, {14, "%r14"}, {15, ""}};

// 2D array for read() to use
char read_heap[CAP][CAP]; 
