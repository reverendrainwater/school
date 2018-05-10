/////////////////////////////////////////////////////////////////////
//
//  IDENTIFICATION DIVISION.
//  PROGRAM-ID. Y86Loader.h.
//  AUTHOR. Rev Taylor R Rainwater + Andrew Watson.
//  INSTALLATION. student.
//  DATE-WRITTEN. 10.2.2016.
//  DATE-UPDATED. 23.2.2016.
//  DESCRIPTION. Y86 header file.
//
/////////////////////////////////////////////////////////////////////

#ifndef Y86_H
#define Y86_H

// Start of c++ libraries
#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <algorithm>
// Start of c libraries
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <assert.h>
#include <unistd.h>
#include <sys/stat.h>
// Start of headers
#include "Memory.h"
#include "ProgRegisters.h"
#include "Sim.h"
#include "Tools.h"

// Definitions
#define CAP 512	   	  // Used for size limit of various strings and 'read_heap'

// Error Messages 
#define ERROR0A "ERROR 0A: INVALID ADDRESS SIZE (NOT 3 OR 4)"
#define ERROR0B "ERROR 0B: INVALID ADDRESS VALUE"
#define ERROR1 "ERROR 1: PIPE CHARACTER PLACEMENT"
#define ERROR2 "ERROR 2: FAILURE TO WRITE TO MEMORY"


// Y86Loader Class
class Y86 {
    Memory              memory;          
    ProgRegisters       regs;
	
	// Static Variables
	static int line_count;
	static char read_heap[CAP][CAP];

    /* Private member functions */
    //  ============ LOADER ============
		void	trim			(char*, char*, int, int);
		void 	remove_white	(char*);
		int 	instr_len		(char*);
		void 	rev_instr		(char*);
		bool	check_ext		(char*);
		bool	check_blank		(char*);
		bool 	check_addrVal	(char*);
		bool	check_pipe		(char*, int);
		void	get_addrSiz		(int&, char*);
		void 	get_addr		(char*, char*);
		void 	get_instruct	(char*, char*);
		void 	get_addrVal		(uint64_t&, char*);
		void 	get_instrVal	(uint64_t&, char*);
		bool	read 			(char*);
		bool 	validate		(int, int,  char*, char*);
		void	write_mem		(char*, char*);
	//  ============ DUMPER ============
		bool 		readFile		(std::ifstream& infile);
		int 		writeMemory		(std::string inst, uint64_t address);
    	void 		getLine			(uint64_t *, uint64_t);
		std::string getFlagsString	(void);
	
    /* Public member functions */
    public:
    			Y86 			();
        void	reset 			();
		void 	clockP0			();
		void 	clockP1			();
		bool 	load 			(int, char *[]);  // takes argc and argv[] parameters from main
		void 	dumpMemory();
		void 	dumpProgramRegisters();
		void 	dumpProcessorRegisters();
		
        Memory& getMemory(){return memory;}  
        ProgRegisters& getProgRegisters(){return regs;}    
};

#endif