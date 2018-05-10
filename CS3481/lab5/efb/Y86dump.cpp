/*----------------------------------------------------------------------------
   File:    Y86dump.cpp
   
			Y86 simulator trace routines. 
----------------------------------------------------------------------------*/

#include <iostream>
#include <string>
#include <sstream>
#include <cstdint>
#include <iomanip>
#include "Sim.h"
#include "Y86.h"

#define	WORDSPERLINE	4
#define	WORDSIZE		8

using namespace std;

static	uint64_t prevLine[WORDSPERLINE] = {0};
static	uint64_t currLine[WORDSPERLINE] = {0};

static void dumpLine(uint64_t *line, uint64_t address);
static bool isEqual(uint64_t *line1, uint64_t *line2);
static void copyLine(uint64_t *dst, uint64_t *src);

void Y86::dumpProcessorRegisters()
{
	cout << "Processor State:" << endl;
	cout << getFlagsString() << endl << endl;;
	
}
void Y86::dumpProgramRegisters()
{
	cout << "Registers:" << endl;
	cout << "%rax:" << setw(16) << setfill('0') << hex << regs.getReg(RAX) << setfill(' ') << ' ';
	cout << "%rcx:" << setw(16) << setfill('0') << hex << regs.getReg(RCX) << setfill(' ') << ' ';
	cout << "%rdx:" << setw(16) << setfill('0') << hex << regs.getReg(RDX) << setfill(' ') << ' ';
	cout << "%rbx:" << setw(16) << setfill('0') << hex << regs.getReg(RBX) << setfill(' ');
	cout << endl;

	cout << "%rsp:" << setw(16) << setfill('0') << hex << regs.getReg(RSP) << setfill(' ') << ' ';
	cout << "%rbp:" << setw(16) << setfill('0') << hex << regs.getReg(RBP) << setfill(' ') << ' ';
	cout << "%rsi:" << setw(16) << setfill('0') << hex << regs.getReg(RSI) << setfill(' ') << ' ';
	cout << "%rdi:" << setw(16) << setfill('0') << hex << regs.getReg(RDI) << setfill(' ');
	cout << endl;

	cout << " %r8:" << setw(16) << setfill('0') << hex << regs.getReg(R8) << setfill(' ') << ' ';
	cout << " %r9:" << setw(16) << setfill('0') << hex << regs.getReg(R9) << setfill(' ') << ' ';
	cout << "%r10:" << setw(16) << setfill('0') << hex << regs.getReg(R10) << setfill(' ') << ' ';
	cout << "%r11:" << setw(16) << setfill('0') << hex << regs.getReg(R11) << setfill(' ');
	cout << endl;

	cout << "%r12:" << setw(16) << setfill('0') << hex << regs.getReg(R12) << setfill(' ') << " ";
	cout << "%r13:" << setw(16) << setfill('0') << hex << regs.getReg(R13) << setfill(' ') << " ";
	cout << "%r14:" << setw(16) << setfill('0') << hex << regs.getReg(R14) << setfill(' ');
	cout << endl << endl;
	
}
string Y86::getFlagsString()
{
	stringstream ss;
	ss << "CC: ZF=" << regs.getCC(ZF) << " SF=" << regs.getCC(SF) << " OF=" << regs.getCC(OF);
	return ss.str();
}
void Y86::dumpMemory()
{
	int address = 0;
	int memsize = MEMORY_SIZE*WORDSIZE;
	int linesize = WORDSPERLINE*WORDSIZE;
	bool star = false;
	
	cout << "Memory:" << endl;
	getLine(prevLine, address);
	dumpLine(prevLine, address);
	for(address = linesize; address < memsize; address+=linesize){
		getLine(currLine, address);
		if(isEqual(currLine,prevLine)){
			if(!star){
				cout << "*" << endl;  //star on prev line, then newline
				star = true;
			}
		}
		else {
			cout << endl;              // just newline
			dumpLine(currLine,address);// write newline
			star = false;
		}
		copyLine(prevLine,currLine);
	}
	cout << endl << endl;
}
void Y86::getLine(uint64_t *line, uint64_t address)
{
	for(int i = 0; i < WORDSPERLINE; i++){
		line[i] = memory.getWord(address);
		address += WORDSIZE;
	}
}
static void dumpLine(uint64_t *line, uint64_t address)
{
	cout << "0x" << setfill('0') << setw(4) << hex << (int)address << ":";
	for(int i = 0; i < WORDSPERLINE; i++)
		cout << " " << setw(16) << hex << setfill('0') << (unsigned long)line[i];
}
static bool isEqual(uint64_t *line1, uint64_t *line2)
{
	for(int i = 0; i < WORDSPERLINE; i++){
		if(line1[i] != line2[i])
			return false;
	}
	return true;
}

static void copyLine(uint64_t *dst, uint64_t *src)
{
	for(int i = 0; i < WORDSPERLINE; i++) dst[i] = src[i];
}

