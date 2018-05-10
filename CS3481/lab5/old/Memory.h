/*
  File: Memory.h 
 
  Desc: Declarations for memory functions for Y86 sim. 
 
 */

#ifndef MEMORY_H
#define MEMORY_H
#include <cstdint>
#include "Sim.h"
#include "Tools.h"

class Memory {
    private:
        uint64_t mem[MEMORY_SIZE];
        bool memError;

        void store(uint64_t waddr, uint64_t val);
        uint64_t fetch(uint64_t waddr);

    public:
        Memory();
        unsigned char getByte(uint64_t byteAddress);
        void putByte(uint64_t byteAddress, uint8_t value);
        uint64_t getWord(uint64_t byteAddress);
        void putWord(uint64_t byteAddress, uint64_t wordValue);
        void reset(void);
        bool isError(void) {
			return memError;
		}
};

#endif
