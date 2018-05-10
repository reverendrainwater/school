/*
	File:	WritebackStage.h
	Desc:	Declares WritebackStage class and associated constants

 */
#ifndef WRITEBACKSTAGE_H
#define WRITEBACKSTAGE_H

#include "Sim.h"
#include "PipeStage.h"
#include "Register.h"
#include "DecodeStage.h"
#include "ExecuteStage.h"
#include "FetchStage.h"
#include "MemoryStage.h"

class WritebackStage : public PipeStage
{
	/* Register State */
	Register<uint64_t> valE;
	Register<uint64_t> valM;
	Register<uint64_t> dstE;
	Register<uint64_t> dstM;

	/* Pointers to Stage objects */
	FetchStage 		*fetchStage;
	DecodeStage 	*decodeStage;
	ExecuteStage 	*executeStage;
	MemoryStage		*memoryStage;
	Memory 			*memory;


	/* Public Member Functions */
	public:
		void reset(ProgRegisters *);
		void clockP0();
		void clockP1();
		void trace();
		void updateWRegister(uint64_t, uint64_t, uint64_t, uint64_t, Register<uint64_t>, Register<uint64_t>);


};

#endif