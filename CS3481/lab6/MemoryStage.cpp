/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. MemoryStage.cpp.
//	AUTHOR. Andrew Watson.
//	INSTALLATION. student.
//	DATE-WRITTEN. 17.03.2016.
//	DESCRIPTION. Implementation of Memory Stage for Y86 Simulator.
//
/////////////////////////////////////////////////////////////////////
#include "Y86.h"
#include "MemoryStage.h"

void MemoryStage::reset(WritebackStage *pwback, Memory *pmem)
{
	writebackStage = pwback;
	memoryStage = pmem;
	stat = SBUB;
	icode = INOP;
	m_cond = false;
	m_valE = 0;
	m_valA = 0;
	m_dstE = RNONE;
	m_dstM = RNONE;
}

void MemoryStage::updateMRegister(uint64_t pstat, uint64_t picode, uint64_t cond, uint64_t valE, uint64_t valA, Register<uint64_t> dstE, Register<uint64_t> dstM) {
	stat = pstat;
	icode = picode;
	m_cond = cond;
	m_valE = valE;
	m_valA = valA;
	m_dstM = dstM;
	m_dstE = dstE;
}

void MemoryStage::clockP0() {
	m_dstE.clock();
	m_dstM.clock();
}

void MemoryStage::clockP1() {
	writebackStage.updateWRegister(m_stat, m_icode, m_valE, m_valA, m_dstE, m_dstM);
}