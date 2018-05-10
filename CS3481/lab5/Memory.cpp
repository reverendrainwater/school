/////////////////////////////////////////////////////////////////////
//
//  IDENTIFICATION DIVISION.
//  PROGRAM-ID. Memory.cpp.
//  AUTHOR. Andrew Watson.
//  INSTALLATION. student.
//  DATE-WRITTEN. 09.02.2016.
//  DESCRIPTION. Memory.
//
/////////////////////////////////////////////////////////////////////
#include <cstdint>
#include "Memory.h"
#include "Tools.h"
  

/*
 * store private member function.
 *
 * @param waddr Word Address
 *        val Value to be stored at word address in memory
 */
 void Memory::store(uint64_t waddr, uint64_t val) 
 {  uint64_t upper = 1023;
    uint64_t lower = 0;

    if (waddr > upper || waddr < lower) { //checks to see if the word address is invalid
        memError = true; //if invalid, sets memError flag to true
        return; //breaks from method
    }
    mem[waddr] = val; //sets the value at word address equal to the passed val
 }  

/*
 * store private member function.
 *
 * @param waddr Word Address
 *        val Value to be stored at word address in memory
 */
 uint64_t Memory::fetch(uint64_t waddr) 
 {  uint64_t upper = 1023;
    uint64_t lower = 0;

    if (waddr > upper || waddr < lower) { //checks to see if the word address is invalid
        memError = true; //if invalid, sets memError flag to true
        return 0; //returns null if there is an error
    }
    return mem[waddr];
 } 

/*
 * Memory Constructor.
 *
 * Sets private field memError to false & mem[] to 0 by calling the reset function. 
 *
 */
Memory::Memory() 
{
	reset();
}

/*
 * getByte function - takes a byte address and returns the
 * char value at that location in memory.
 *
 * @param byteAddress - byte address in which to retrieve the char at that index
 * @return          char value held at the byte address given in parameter
 */
unsigned char Memory::getByte(uint64_t byteAddress) 
{
    Tools tools;
    uint64_t wordAddress = byteAddress >> 3; //right shift 3 to get word address from a byte address
	uint64_t word = fetch(wordAddress); //gets the word value at the word address
    int byte = (int)(tools.getBits(0,2,byteAddress)); //gets the lower end of the byte address which will tell
                                //which byte to retrieve
    return (unsigned char)(tools.getByteNumber(byte, word));
}

/*
 * putByte function - takes a byte address and a byte value, then
 * changes the value at the memory location to the value parameter.
 *
 * @param byteAddress - byte address in which a new byte will be placed
 *        value - new byte used to replace the one held at location in memory
 */
void Memory::putByte(uint64_t byteAddress, uint8_t value) 
{
        Tools tools;
        uint64_t wordAddress = byteAddress >> 3; //right shift 3 to get a word address from a byte address
        unsigned byteC = byteAddress & 7; //sets byteC to the byte within the word address to change
        uint64_t byteX = fetch(wordAddress); //retrieves 64 bit value to be altered
        if (memError)
            return;
        store(wordAddress, tools.putByteNumber(byteC, value, byteX)); //uses Tool function - putByteNumber to change byteX (source)
                                               //byte value at byteC address to value param
}

/*
 * getWord function - takes a byte address as a parameter and
 * returns the 64 bit value held at the word address.
 *
 * @param byteAddress - byte address in which the word begins
 */
uint64_t Memory::getWord(uint64_t byteAddress) 
{
	Tools tools;
    int wordAddress = byteAddress >> 3; //right shift 3 to get word address from a byte address
    uint64_t word = fetch(wordAddress);
    if (memError)
        return 0;
    return tools.getBits(0, 63, word);
}

/*
 * putWord function - takes a byte address and word value as parameters
 * and replaces the value held in the memory at the byte address to
 * the word value parameter.
 *
 * @param byteAddress - address in which the new word will replace
 *        wordValue - new word to be placed starting at the given byte address
 */
void Memory::putWord(uint64_t byteAddress, uint64_t wordValue) 
{
   int wordAddress = byteAddress >> 3; //right shift 3 to get word address from a byte address
   store(wordAddress, wordValue); //replaces the word at the word address with the given wordValue
}

/*
 * Reset method - resets the memError variable to false and sets the
 * mem array to 0.
 *
 */
void Memory::reset() 
{
    for (int i = 0; i < MEMORY_SIZE; i++) //loops through mem array
        mem[i] = 0; //setting (clearing) values within array
    memError = false; //set to false as there is no memory error
}

bool isError();
