/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. Tools.cpp.
//	AUTHOR. Rev Taylor Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 19.1.2015.
//	DESCRIPTION. Y86 Tool.
//
/////////////////////////////////////////////////////////////////////
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <cstdint>
#include "Tools.h"

namespace Y86 {
    
/**
 * getBits - gets the bits
 * @param  low    low bit
 * @param  high   high bit
 * @param  source in
 * @return        out
 */
uint64_t getBits(unsigned low, unsigned high, uint64_t source)
{	uint64_t msk;

	assert(high < 64 && (low <= high));
	msk = ~(~(uint64_t) 0 << (high - low + 1));
	return (source >> low) & msk;
}
/**
 * setBits - sets the bits
 * @param  low    low bit
 * @param  high   high bit
 * @param  source in
 * @return        out
 */
uint64_t setBits(unsigned low, unsigned high, uint64_t source)
{	uint64_t msk;

	assert(high < 64 && (low <= high));
	msk = ~((-1 - low) - high);
	return source | msk;
}
/**
 * clearBits - clears the bits
 * @param  low    low bit 
 * @param  high   high bit
 * @param  source in
 * @return        out
 */
uint64_t clearBits(unsigned low, unsigned high, uint64_t source)
{	uint64_t msk;

	assert(high < 64 && (low <= high));
	msk = ~(uint64_t) 0 << (uint64_t)(high - low + 1);
    return source & msk;
}

/**
 * assignOneBit - assign a specified bit a given value
 * @param  bitNum bit location
 * @param  bitVal new bit value
 * @param  source in 
 * @return        the updated in
 */
uint64_t assignOneBit(unsigned bitNum, unsigned bitVal, uint64_t source)
{	uint64_t swp = 0;

	assert((bitNum < 64 && bitNum >= 0) && (bitVal == 1 || bitVal == 0));
	if (bitVal == 1){
		swp = (swp + bitVal) << bitNum;
		source = source & ~swp;
		return source += swp;
	} else {
		swp = ~((swp + 1) << bitNum);
		return source & swp; 
	}
}

/**
 * getByteNumber - get value of a specified byte
 * @param  byteNum place value of byte desired
 * @param  source  in
 * @return         value of specified byte
 */
uint8_t getByteNumber(unsigned byteNum, uint64_t source)
{	// Masks
	uint64_t b0 = 0x00000000000000FF;
	uint64_t b1 = 0x000000000000FF00;
	uint64_t b2 = 0x0000000000FF0000;
	uint64_t b3 = 0x00000000FF000000;
	uint64_t b4 = 0x000000FF00000000;
	uint64_t b5 = 0x0000FF0000000000;
	uint64_t b6 = 0x00FF000000000000;
	uint64_t b7 = 0xFF00000000000000;
	// Swap
	uint64_t swp = 0;

	if (byteNum == 0){
		swp = source & b0;
	} else if (byteNum == 1){
		swp = source & b1;
		swp >>= 8;
	} else if (byteNum == 2){
		swp = source & b2;
		swp >>= 16;
	} else if (byteNum == 3){
		swp = source & b3;
		swp >>= 24;
	} else if (byteNum == 4){
		swp = source & b4;
		swp >>= 32;
	} else if (byteNum == 5){
		swp = source & b5;
		swp >>= 40;
	} else if (byteNum == 6){
		swp = source & b6;
		swp >>= 48;
	} else if (byteNum == 7){
		swp = source & b7;
		swp >>= 56;
	}
    return swp;
}
/**
 * putByteNumber - input byte value into int
 * @param  byteNum location value
 * @param  byteVal byte value
 * @param  source  in
 * @return         updated source
 */
uint64_t putByteNumber(unsigned byteNum, uint8_t byteVal, uint64_t source)
{	uint64_t swp = 0;

	assert(byteNum < 8 && byteNum >= 0);
	swp = ~((swp + 0xFF) << (byteNum * 8));
	source = source & swp;
	swp = (0 + byteVal) << (byteNum * 8);
	return source += swp;
}
/**
 * buildWord - create word from individual characters
 * @param  b0 byte 0
 * @param  b1 byte 1
 * @param  b2 byte 2
 * @param  b3 byte 3
 * @param  b4 byte 4
 * @param  b5 byte 5
 * @param  b6 byte 6
 * @param  b7 byte 7
 * @return    64bit word
 */
uint64_t buildWord(unsigned char b0, unsigned char b1,unsigned char b2, unsigned char b3,
                   unsigned char b4, unsigned char b5,unsigned char b6, unsigned char b7)
{	uint64_t swp = 0;

	swp += b7;
	swp <<= 8;
	swp += b6;
	swp <<= 8;
	swp += b5;
	swp <<= 8;
	swp += b4;
	swp <<= 8;
	swp += b3;
	swp <<= 8;
	swp += b2;
	swp <<= 8;
	swp += b1;
	swp <<= 8;
	swp += b0;
	swp <<= 8;
    return swp;                     
}
/**
 * isNegative - checks if value is is negative
 * @param  source in
 * @return        out - boolean
 */
bool isNegative(uint64_t source)
{	uint64_t msk = 0x8000000000000000;
	uint64_t swp;

	swp = source & msk;
	if (swp > 0){
		return true;
	} else {
	    return false;
	}
}
/**
 * expandBits - create string of expanded bits
 * @param source in
 * @param bits   string array of bits
 */
void expandBits(uint64_t source, char *bits)
{	unsigned int i;
  	int increment = 0;
	
	for (i = 0; i < 71; i++) {
	    if (i == 8 || i == 17 || i == 26 || i == 35 ||
	        i == 44 || i == 54 || i == 63) {
	      bits[(70 - i)] = ' ';
	      increment++;
		} else {
	    	bits[(70 - i)] = (source & (1 << (i - increment))) ? '1': '0';
		}
	}
	bits[71] = 0;
}
/**
 * clearBuffer - clear an array
 * @param pbuf pointer to array
 * @param size size of array
 */
void clearBuffer(char * pbuf, int size)
{	
	for (int i = 0; i < size; i++){
		pbuf[i] = 0;
	}
}

} // end namespace Y86
