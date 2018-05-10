/*
    File:   FetchStage.h
    Desc:   Declares FetchStage class and associated constants
    
*/
#ifndef FETCHSTAGE_H
#define FETCHSTAGE_H


#include "Sim.h"
#include "Register.h"
#include "PipeStage.h"
#include "DecodeStage.h"
#include "ExecuteStage.h"


class FetchStage : public PipeStage
{

    /* Register state */
    Register<uint64_t> predPC;    // Predicted PC value stored in F Register
	
    /* Pointers to Decode Stage and Memory object */
	DecodeStage *decodeStage;
	Memory 		*memory;
	
	
    /* signals produced within the stage */
    Register<bool> needsValC;
    Register<bool> needsRegs;
    uint64_t f_PC;
    uint64_t f_ifun;
    uint64_t f_rA;
    uint64_t f_rB;
    uint64_t f_valC;
    uint64_t f_valP;

    Tools tool;

	/* Example private methods - you may implement your own, and there will be others... */
    int get_instruction();
    void predict_pc();
    uint64_t select_pc();
	uint64_t get_valC(uint64_t);
    uint64_t get_valP(uint64_t);
	
	public:
        
		void reset(DecodeStage *, Memory *);

        /* (Virtual) Functions of superclass */
        void clockP0();
        void clockP1();
        void trace();
    
    
};

#endif