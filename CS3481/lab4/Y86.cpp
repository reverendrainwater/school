/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. Y86.cpp.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 05.02.2016.
//	DESCRIPTION. Y86.
//
/////////////////////////////////////////////////////////////////////

#include "Y86.h"

/**
 * constructor
 */
Y86::Y86()
{
	reset();
}
/**
 * reset - calls the reset functions of Memory and ProgRegisters.
 */
void Y86::reset()
{
	regs.reset();
	memory.reset();
}
/**
 * clockP0 - calls the clock function of ProgRegisters.
 */
void Y86::clockP0()
{
	regs.clock();
}
/**
 * clockP1 - null.
 */
void Y86::clockP1()
{
	
}
/**
 * getMemory - returns memory constant.
 * @return memory from Y86 class.
 */
Memory& getMemory();
/**
 * getProgRegisters - returns regs constants. 
 * @return regs from Y86 class.
 */
ProgRegisters& getProgRegisters();