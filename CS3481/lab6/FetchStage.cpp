/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//      PROGRAM-ID. FetchStage.cpp.
//          AUTHOR. Rev Taylor R Rainwater.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 16-3-2016.
//     DESCRIPTION. Fetch Stage of YESS.
//
/////////////////////////////////////////////////////////////////////
#include "Y86.h"
#include "FetchStage.h"

/*---------------------------------------------------------------------------
    reset- used to connect to other Y86 components
     
-----------------------------------------------------------------------------*/
void FetchStage::reset(DecodeStage *pdecode, Memory *pmem)
{
		decodeStage = pdecode;  // "Connect" fetch stage to decode stage
		memory = pmem;
		predPC.reset();
}

/*---------------------------------------------------------------------------
    clockP0 - (pure virtual from PipeStage)
         Performs operations of clock Phase0 for this stage
-----------------------------------------------------------------------------*/
void FetchStage::clockP0()
{
    // Must implement--declared pure-virtual 
	// In Phase0 we:
	//     Check for STALL or BUBBLE condition, update Register variable inputs
	//     		stall/bubble come from conditions in E, M, W stages (see HCL)
	//     Call the clock method on all Register variables
	//     Perform all Phase0 actions:
	//          For fetch, since selectPC uses outputs from other stages
	//			all work happens during Phase1
	//	
	predPC.clock();
	
}
/*---------------------------------------------------------------------------
    clockP1 - (pure virtual from PipeStage)
         Performs operations of clock Phase1 for this stage
-----------------------------------------------------------------------------*/
void FetchStage::clockP1()
{
   // For fetch stage, all work done here (since we only know fetch address
   // when forwarding conditions are known from Memory and Writeback stages)
   // f_PC = selectPC() 
   // check for memory address error
   // check for illegal opcode
   // if any error, then call with f_stat set to error type (SADR, SINS):
   //      decodeStage->updateDRegister(f_stat,INOP,FNONE,RNONE,RNONE,0UL,0UL);
   // if no error:
   // 	determine icode, ifun, valP (which will be incremented PC based on instruction size) 
   //   don't have to worry about rA, rB, valC for this lab--only implementing NOP and HALT
   // Good idea to create several helper functions, even if only stubs at this point, to implement
   // FetchStage actions
   
   // Last, update Decode pipeline register with info determined during Fetch stage:
   //    decodeStage->updateDRegister(f_stat,f_icode,f_ifun, f_rA, f_rB, f_valC, f_valP);
   f_PC = selectPC();

	
}

/*----------------------------------------------------------------------------
   predictPC - updates predPC based on f_icode, f_valC and f_valP

----------------------------------------------------------------------------
*/
void FetchStage::predic_pc()
{

}
/*----------------------------------------------------------------------------
  getInstruction - (this is just an example method--something else might be better)
				returns the number of bytes in the instruction if valid,
                otherwise returns -1. Fetches remaining instruction uint64_ts
				and updates fetch stage signals:
					f_stat
					f_icode
					needsRegs
					needsValC
					f_rA
					f_rB
					f_valC
					f_valP
------------------------------------------------------------------------------
*/
int FetchStage::get_instruction()
{ 	uint64_t instruction;
	uint64_t iop;

	instruction = predPC.getState();
	iop = tool.getBits(0, 7, instruction);
	if (iop >= 0 || iop <= 196){
		
	} 
	return -1;
}
/*----------------------------------------------------------------------------
   getValC - returns 8-byte valC from specified uint64_t address.

----------------------------------------------------------------------------
*/
uint64_t FetchStage::get_valC(uint64_t addr)
{
	return tool.getBits(48, 55, addr);
}

uint64_t FetchStage::get_valP(uint64_t addr)
{
	return tool.getBits(56, 63, addr);
}
/*----------------------------------------------------------------------------
   selectPC - select next PC 

----------------------------------------------------------------------------
*/
uint64_t FetchStage::select_pc()
{
	// For now, selectPC returns predicted PC
    return predPC.getState();
}


