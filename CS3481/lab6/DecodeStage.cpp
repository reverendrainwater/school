/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//      PROGRAM-ID. DecodeStage.cpp.
//          AUTHOR. Rev Taylor R Rainwater.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 16-3-2016.
//     DESCRIPTION. Decode Stage of YESS.
//
/////////////////////////////////////////////////////////////////////
#include "Y86.h"
#include "DecodeStage.h"

/*---------------------------------------------------------------------------
    reset- used to connect to other Y86 components
     
-----------------------------------------------------------------------------*/
void DecodeStage::reset(ExecuteStage *pdecode, ProgRegisters *reg)
{
		ExecuteStage = pdecode;  // "Connect" decode stage to execute stage
		regs = reg;
}

/*---------------------------------------------------------------------------
    clockP0 - (pure virtual from PipeStage)
         Performs operations of clock Phase0 for this stage
-----------------------------------------------------------------------------*/
void DecodeStage::clockP0()
{
    	
}
/*---------------------------------------------------------------------------
    clockP1 - (pure virtual from PipeStage)
         Performs operations of clock Phase1 for this stage
-----------------------------------------------------------------------------*/
void DecodeStage::clockP1()
{
   	executeStage.updateERegister();
}

void DecodeStage::updateDRegister(...)
{

}
