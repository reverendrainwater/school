/*
    Sim.h
    This file includes typedefs and constants used by all classes
    
*/
#ifndef SIM_H
#define SIM_H

typedef unsigned char byte;

#define     NUMSTAGES       5
#define     MEMORY_SIZE     1024     // 64-bit words 


#define     NUM_REGISTERS   15       // 64-bit registers
#define     RAX             0
#define     RCX             1
#define     RDX             2
#define     RBX             3
#define     RSP             4
#define     RBP             5
#define     RSI             6
#define     RDI             7
#define     R8              8
#define     R9              9
#define     R10             10
#define     R11             11
#define     R12             12
#define     R13             13
#define     R14             14
#define     RNONE           15

// Flags Register (CC) bit number definitions
#define     OF              0
#define     SF              1
#define     ZF              2


#endif
