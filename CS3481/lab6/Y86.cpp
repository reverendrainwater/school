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
	// Reset general registers and memory
	regs.reset();
	memory.reset();
	// Reset pointers to stage array
	stage[0] = &fetchStage;
	stage[1] = &decodeStage;
	stage[2] = &executeStage;
	stage[3] = &memoryStage;
	stage[4] = &writebackStage;
	// Reset each stage
	fetchStage.reset(&decodeStage, &memory);
	decodeStage.reset(&executeStage, &regs);
	executeStage.reset(&memoryStage);
	memoryStage.reset(&writebackStage, &memory);
	writebackStage.reset(&regs);
	cycles = 0;
}
/**
 * clock - the clock function calls on each stage within 
 * 		   the stage[] array then calls the appropriate 
 * 		   clock function
 */
void Y86::clock()
{
	for (int i = 0; i < 5; i++){
		stage[i]->clockP0();
	}
	for (int i = 0; i < 5; i++){
		stage[i]->clockP1();
	}
}

uint64_t Y86::getStat()
{	uint64_t status;

	status = writebackStage.getStat();
	if (status == SAOK || status == SBUB)
		return 0;
	else 
		return status;
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