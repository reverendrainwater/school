/*
    File:   DecodeStage.h
    Desc:   Declares DecodeStage class and associated constants
    
*/
#ifndef DECODESTAGE_H
#define DECODESTAGE_H


#include "Sim.h"
#include "PipeStage.h"
#include "Register.h"
#include "ExecuteStage.h"
#include "MemoryStage.h"


class DecodeStage : public PipeStage
{
    Register<uint64_t> ifun;
    Register<uint64_t> rA;
    Register<uint64_t> rB;
    Register<uint64_t> valC;
    Register<uint64_t> valP;

    /* Pointers to Execute Stage and ProgRegisters object */
	ExecuteStage       *executeStage;
	ProgRegisters      *regs;
	
	
    /* signals produced within the stage */
    uint64_t d_PC;
    bool needsValC;
    bool needsRegs;
    uint64_t d_ifun;
    uint64_t d_rA;
    uint64_t d_rB;
    uint64_t d_valC;
    uint64_t d_valP;


	public:
		void reset(ExecuteStage *, ProgRegisters *);

        /* (Virtual) Functions of superclass */
        void clockP0();
        void clockP1();
        void trace();
        void updateDRegister(uint64_t pstat, uint64_t picode, uint64_t rA, uint64_t rB, 
                             Register<uint64_t> dstM, Register<uint64_t> dstE);
    
    
};

#endif