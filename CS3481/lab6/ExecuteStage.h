/*
    File:   ExecuteStage.h
    Desc:   Declares ExecuteStage class and associated constants
    
*/
#ifndef EXECUTESTAGE_H
#define EXECUTESTAGE_H


#include "Sim.h"
#include "PipeStage.h"
#include "Register.h"
#include "MemoryStage.h"
#include "WritebackStage.h"


class ExecuteStage : public PipeStage
{
    Register<uint64_t> ifun;
    Register<uint64_t> valC;
    Register<uint64_t> valA;
    Register<uint64_t> valB;
    Register<uint64_t> srcA;
    Register<uint64_t> srcB;
    Register<uint64_t> dstE;
    Register<uint64_t> dstM;

    /* Pointers to Memory Stage */
	MemoryStage *memoryStage;	
	
    /* signals produced within the stage */
    bool needsValC;
    bool needsRegs;
    uint64_t e_PC;
    uint64_t e_stat;
    uint64_t e_icode;
    uint64_t e_ifun;
    uint64_t e_rA;
    uint64_t e_rB;
    uint64_t e_valC;
    uint64_t e_valA;
    uint64_t e_dstE;
    uint64_t e_dstM;
    uint64_t e_srcA;
    uint64_t e_srcB;

	
	public:
		void reset(MemoryStage *);
        /* (Virtual) Functions of superclass */
        void clockP0();
        void clockP1();
        void trace();
        void updateERegister(...);
    
    
};

#endif