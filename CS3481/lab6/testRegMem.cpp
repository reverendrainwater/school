/*
    File:   main.cpp
    Desc:   The main program. Accepts and verifies parameters and executes simulator
            on the specified object file. Other parameters are used to specify the stage or
            stages where trace outputs will be printed. Trace outputs may include the 
            stage register contents, memory contents, and/or register contents. Trace outputs
            are printed at the end of a cycle.
            
            Best way to capture internal state during simulation?
            Previous simulator used a special dump instruction. This requires
            adding this instruction to the instruction set. Dump routines were
            provided that allowed the dumping of program state during WRITEBACK stage.
            
*/
#include <iostream>
#include <iomanip>
#include <string>
#include "Sim.h"
#include "Y86.h"
#include "Memory.h"
#include "ProgRegisters.h"

#define 	MEM_SIZE_BYTES		8192

using namespace std;

Y86 y86;  // Declare global Y86 object. 

//------------------------------------------------------------------
// Test functions
//------------------------------------------------------------------
void testMemory(Memory&);
void testRegisters(ProgRegisters&);
void getAllCC(ProgRegisters& regs, unsigned *of, unsigned *sf, unsigned *zf);
void fail(const char *);
void pass(const char *);

bool testFail = false;

int main(int argc, char *argv[])
{



	Memory m = y86.getMemory();
	m.reset();
	ProgRegisters registers = y86.getProgRegisters();
	registers.reset();

    // do something with memory and registers...	
	testMemory(m);
	testRegisters(registers);

        
}
void fail(const char *s)
{
	cout << "\tTest: " << setw(20) << left << s << "\t failed." << endl;
	testFail = true;
}
void pass(const char *s)
{
	cout << "\tTest: " << setw(20) << left << s << "\t passed." << endl;
}
/*----------------------------------------------------------------------------------------
   testRegisters - setReg, getReg, clock, reset, getCC, setCC
   
----------------------------------------------------------------------------------------*/
void testRegisters(ProgRegisters& r)
{
	uint64_t val = 0;
	testFail = false;
	uint64_t patterns[] = {
		0x0123456789abcdef,
		0xfedcba9876543210,
		0xaaaaaaaaaaaaaaaa,
		0x5555555555555555,
		0xaa55aa55aa55aa55,
		0x55aa55aa55aa55aa,
		0xffffffff00000000,
		0x00000000ffffffff,
		0xff00ff00ff00ff00,
		0x00ff00ff00ff00ff,
		0xffff0000ffff0000,
		0x0000ffff0000ffff,
		0xcccccccccccccccc,
		0x3333333333333333,
		0xeeeeeeeeeeeeeeee
	};
	cout << "\nTesting Program Registers:" << endl;
	
	// Test reset function
	r.reset();
	for(int i = 0; i < NUM_REGISTERS; i++){
		val |= r.getReg(i);
	}
    if(val != 0){
		fail("reset");
	}
	else
		pass("reset");
	if(testFail)
		return;
	//-------------------------------------
	// Test setReg/getReg
	//-------------------------------------	
	r.reset(); // clear regs to zero
	
	for(int i = 0; i < NUM_REGISTERS; i++){
		r.setReg(i,patterns[i]);
	}
	// check for no change in output without clock
	val = 0;
	for(int i = 0; i < NUM_REGISTERS; i++){
		val |= r.getReg(i);
	}
    if(val != 0){
		fail("ProgRegisters::setReg");
		return;
	}
	// clock all registers
	r.clock(); 
	for(int i = 0; i < NUM_REGISTERS; i++){
		val = r.getReg(i);
		if(val != patterns[i]){
			fail("setReg");
			return;
		}
	}
	pass("getReg/setReg");	
	//---------------------------------------------
	// Test setCC/getCC
	//---------------------------------------------
	r.reset(); 
    unsigned of, sf, zf;
	
	getAllCC(r,&of, &sf, &zf);
//	cout << "of, sf, zf = (" << of << "," << sf << "," << zf << ")" << endl;
	if(of | sf | !zf){
		fail("reset CC");
		return;
	}
	r.setCC(OF,1);
	getAllCC(r,&of, &sf, &zf);
	if(!of || sf || !zf){
		fail("setCC(OF,1)");
		return;
	}
	r.setCC(SF,1);
	getAllCC(r,&of, &sf, &zf);
	if(!of || !sf || !zf){
		fail("setCC(SF,1)");
		return;
	}
	r.setCC(ZF,0);
	getAllCC(r,&of, &sf, &zf);
	if(!of || !sf || zf){
		fail("setCC(ZF,0)");
		return;
	}

	r.setCC(OF,0);
	r.setCC(ZF,1);
	getAllCC(r,&of, &sf, &zf);
	if(of || !sf || !zf){
		fail("setCC(OF,0)");
		return;
	}
	r.setCC(SF,0);
	getAllCC(r,&of, &sf, &zf);
	if(of || sf || !zf){
		fail("setCC(SF,0)");
		return;
	}
	r.setCC(ZF,0);
	getAllCC(r,&of, &sf, &zf);
	if(of || sf || zf){
		fail("setCC(ZF,0)");
		return;
	}
	r.setCC(OF,1);
	r.setCC(SF,1);
	r.setCC(ZF,1);
	getAllCC(r,&of, &sf, &zf);
	if(!of || !sf || !zf){
		fail("setCC(XX,1)");
		return;
	}
	
	r.reset();
	getAllCC(r,&of, &sf, &zf);
	if(of | sf | (!zf)){
		fail("reset CC");
		return;
	}
	pass("getCC/setCC");
}
void getAllCC(ProgRegisters& r, unsigned *of, unsigned *sf, unsigned *zf)
{
	*of = 0;
	*sf = 0;
	*zf = 0;
	
	r.clock();    // input --> state
	
	*of = r.getCC(OF);
	*sf = r.getCC(SF);
	*zf = r.getCC(ZF);	
//	unsigned v = r.getCC(ZF);	
//	*zf = v;
}

/*----------------------------------------------------------------------------------------
   testMemory - checks reset(), putByte/getByte, putWord/getWord, isError
   
----------------------------------------------------------------------------------------*/
void testMemory(Memory& m)
{
	cout << "Testing Memory:" << endl;
	//-----------------------------------------------------
	// Testing reset() - should clear all bytes to zero
	//-----------------------------------------------------
	unsigned char val = 0;
	m.reset();
	for(int i = 0; i < MEM_SIZE_BYTES; i++){
		val |= m.getByte(i);
		if(m.isError()){
			fail("reset-isError");
			break;
		}
	}
    if(val != 0){
		fail("reset");
	}
	else
		pass("reset");
	if(testFail)
		return;
	//-----------------------------------------------------
	// Testing putByte and getByte
	//-----------------------------------------------------
	for(int i = 0; i < MEM_SIZE_BYTES; i++){
		m.putByte((uint64_t)i,(unsigned char)(i+1));
		if(m.isError()){
			fail("putByte-isError");
			break;
		}
	}	
	if(testFail)
		return;
	for(int i = 0; i < MEM_SIZE_BYTES; i++){
		unsigned char val = m.getByte((uint64_t)i);
		if(m.isError()){
			fail("getByte-isError");
			break;
		}
		if(val != (unsigned char)(i+1)){
			fail("getByte");
			break;
		}
	}	
	if(testFail)
		return;
	pass("getByte/putByte");
	//-----------------------------------------------------
	// Testing putWord and getWord
	//-----------------------------------------------------
	for(int i = 0; i < MEM_SIZE_BYTES; i+=8){
		m.putWord((uint64_t)i,~((uint64_t)(i)));
		if(m.isError()){
			fail("putWord-isError");
			break;
		}
	}	
	if(testFail)
		return;
	for(int i = 0; i < MEM_SIZE_BYTES; i+=8){
		uint64_t val = m.getWord((uint64_t)i);
		if(m.isError()){
			fail("getWord-isError");
			break;
		}
		if(val != ~((uint64_t)(i))){
			fail("getWord");
			break;
		}
	}	
	if(testFail)
		return;
	pass("getWord/putWord");	
	
	m.reset();
	if(m.isError()){
		fail("Error test");
		return;
	}	
	m.putByte(8192,0xFF);
	if(!m.isError()){
		fail("putByte: Error test");
		return;
	}
	pass("putByte: Error test");
	m.getByte(8192);
	if(!m.isError()){
		fail("getByte: Error test");
		return;
	}
	pass("getByte: Error test");
	m.putWord(8192,0);
	if(!m.isError()){
		fail("putWord: Error test");
		return;
	}
	pass("putWord: Error test");
	m.getWord(8192);
	if(!m.isError()){
		fail("getWord: Error test");
		return;
	}
	pass("getWord: Error test");
} // end testMemory


