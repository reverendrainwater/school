/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//      PROGRAM-ID. DecodeStage.cpp.
//          AUTHOR. Rev Taylor R Rainwater & Chase Watson.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 16-3-2016.
//     DESCRIPTION. Execute Stage of YESS.
//
/////////////////////////////////////////////////////////////////////
#include "Y86.h"
#include "ExecuteStage.h"

/*---------------------------------------------------------------------------
    reset- used to connect to other Y86 components
     
-----------------------------------------------------------------------------*/
void ExecuteStage::reset(MemoryStage *pdecode)
{
        memoryStage = pdecode;  // "Connect" execute stage to memory stage
        e_stat = SBUB;
        e_icode = INOP;
        e_ifun = FADDQ;
        e_valC = 0;
        e_valA = 0;
        e_dstE = RNONE;
        e_dstM = RNONE;
        e_srcA = RNONE;
        e_srcB = RNONE;
}

/*---------------------------------------------------------------------------
    clockP0 - (pure virtual from PipeStage)
         Performs operations of clock Phase0 for this stage
-----------------------------------------------------------------------------*/
void ExecuteStage::clockP0()
{
        
}
/*---------------------------------------------------------------------------
    clockP1 - (pure virtual from PipeStage)
         Performs operations of clock Phase1 for this stage
-----------------------------------------------------------------------------*/
void ExecuteStage::clockP1()
{
    
}
