/*
	File:	MemoryStage.h
	Desc:	Declares MemoryStage class and associated constants

 */
#ifndef MEMORYSTAGE_H
#define MEMORYSTAGE_H

#include "Sim.h"
#include "Register.h"
#include "PipeStage.h"
// #include "FetchStage.h"
// #include "DecodeStage.h"
// #include "ExecuteStage.h"
class FetchStage;
class DecodeStage;
class ExecuteStage;
class WritebackStage;

class MemoryStage : public PipeStage
{
	/* Register State */
	Register<uint64_t> cnd;
	Register<uint64_t> valE;
	Register<uint64_t> valA;
	Register<uint64_t> dstE;
	Register<uint64_t> dstM;

	/* Pointers to Stage objects */
	FetchStage 		*fetchStage;
	DecodeStage 	*decodeStage;
	ExecuteStage 	*executeStage;
	Memory 			*memory;

	/* Signals produced within stage */
    uint64_t m_cnd;
    uint64_t m_valE;
    uint64_t m_valA;


	/* Public Member Functions */
	public:
		void reset(WritebackStage *, Memory *);
		void clockP0();
		void clockP1();
		void trace();
		void updateMRegister(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, Register<uint64_t>, Register<uint64_t>);
};
#endif