/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. WritebackStage.cpp.
//	AUTHOR. Andrew Watson.
//	INSTALLATION. student.
//	DATE-WRITTEN. 17.03.2016.
//	DESCRIPTION. Implementation of Writeback Stage for Y86 Simulator.
//
/////////////////////////////////////////////////////////////////////
#include "Y86.h"
#include "WritebackStage.h"

void WritebackStage::reset(ProgRegisters *ppr) {
	regs = ppr;
	stat = SBUB;
	icode = INOP;
	w_valE = 0;
	w_valM = 0;
	w_dstE = RNONE;
	w_dstM = RNONE;
}

void WritebackStage::updateWRegister(uint64_t pstat, uint64_t picode, uint64_t valE, uint64_t valM, 
					  				 Register<uint64_t> dstM, Register<uint64_t> dstE) 
{
	stat = pstat;
	icode = picode;
	w_valM = valM;
	w_valE = valE;
	w_dstE = dstE;
	w_dstM = dstM;
}

void WritebackStage::clockP0() {
	w_dstM.clock();
	w_dstE.clock();
}

void WritebackStage::clockP1() {
	//Does nothing in current implementation
}

