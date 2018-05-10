/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. Y86.h.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 05.02.2016.
//	DESCRIPTION. Y86 constants.
//
/////////////////////////////////////////////////////////////////////

#ifndef Y86_H
#define Y86_H
#include <cstdint>
#include <assert.h>
#include "Memory.h"
#include "ProgRegisters.h"

class Y86
{
	private:
		Memory           memory;
		ProgRegisters    regs;

	public:
	                    Y86();    // constructor
		void            reset();
		void            clockP0();
		void            clockP1();  
		Memory&         getMemory(){return memory;}        // access to Memory for testing
		ProgRegisters&  getProgRegisters(){return regs;}   // access to Memory for testing
};

#endif