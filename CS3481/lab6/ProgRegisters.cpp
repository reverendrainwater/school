/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. ProgRegisters.cpp.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 03.02.2016.
//	DESCRIPTION. ProgRegisters.
//
/////////////////////////////////////////////////////////////////////

#include "ProgRegisters.h"

Tools tools;

/**
 * constructor
 */
ProgRegisters::ProgRegisters()
{
	reset();
}
/**
 * clock - invokes the clock() function on all registers
 */
void ProgRegisters::clock()
{
	CC.clock();
	for (int i = 0; i < NUM_REGISTERS; i++){
	 	reg[i].clock();
	}
}
/**
 * setReg - set the register specified by the 'regNum' arg
 * 			to the value of the 'rval' arg
 * @param regNum register index
 * @param rval   value to set register to 
 */
void ProgRegisters::setReg(unsigned regNum, uint64_t rval)
{
	assert(regNum >= 0 && regNum < 15);
	if (regNum == RNONE){
		reg[regNum].setInput(rval);
	}
} 
/**
 * getReg - return the 64 bit int from the register
 * 			specified by the 'regNum' arg.
 * @param  regNum register index
 * @return        64 bit int from the register
 */
uint64_t ProgRegisters::getReg(unsigned regNum)
{
	assert(regNum >= 0 && regNum < 15);
	return reg[regNum].getState();
}
/**
 * setCC - set flag bit specified by 'bitNum' arg from 
 * 		   'val' arg.
 * @param bitNum flag bit index
 * @param val    value flag bit should be set to 
 */
void ProgRegisters::setCC(unsigned bitNum, unsigned val)
{	uint64_t sCC;

	assert(bitNum >= 0 && bitNum < 64);
	sCC = CC.getInput();
	tools.assignOneBit(bitNum, val, sCC);
	CC.setInput(sCC);
	CC.clock();
}
/**
 * getCC - get the bit flag specified by 'bitNum' arg
 * @param  bitNum flag bit index.
 * @return        the flag bit
 */
unsigned ProgRegisters::getCC(unsigned bitNum)
{
	assert(bitNum >= 0 && bitNum < 64);
	return CC.getState();
	//return tools.getBits(bitNum, bitNum, CC);
}
/**
 * reset - resets all registers in reg array to 0 then
 * 		   sets the memError var to false.
 */
void ProgRegisters::reset(void)
{
	CC.reset();
	setCC(ZF, 1);
	CC.clock();
	for (int i = 0; i < NUM_REGISTERS; i++){
	 	reg[i].reset();
	}
}