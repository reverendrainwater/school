/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. ProgRegisters.h.
//	AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 03.02.2016.
//	DESCRIPTION. ProgRegisters constants.
//
/////////////////////////////////////////////////////////////////////

#ifndef PROGREGISTERS_H
#define PROGREGISTERS_H
#include <cstdint>
#include <assert.h>
#include "Sim.h"
#include "Register.h"
#include "Tools.h"

class ProgRegisters
{
	private:
		Register<uint64_t>   reg[NUM_REGISTERS];
		uint64_t             CC;

	public:
	                ProgRegisters();   // constructor
		void        clock ();
		void        setReg (unsigned regNum, uint64_t rval); 
		uint64_t    getReg (unsigned regNum);
		void        setCC (unsigned bitNum, unsigned val);
		unsigned    getCC (unsigned bitNum);
		void        reset (void);
};

#endif