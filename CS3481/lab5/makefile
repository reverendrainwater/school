#///////////////////////////////////////////////////////////////////#
#
#	IDENTIFICATION DIVISION.
#	PROGRAM-ID. makefile.
#	AUTHOR. Rev Taylor R Rainwater.
#	INSTALLATION. Student.
#	DATE-WRITTEN. 17-2-2016.
#	DESCRIPTION. makefile for YESS.
#
#///////////////////////////////////////////////////////////////////#

CX = clang++
CXXFLAGS = -o yess -g -Wall -std=c++0x -x c++
CPP = Y86.cpp Y86Loader.cpp ProgRegisters.cpp Memory.cpp Sim.cpp Tools.cpp Y86dump.cpp
HDRS = Y86.h ProgRegisters.h Memory.h Sim.h Tools.h
OBJS = Y86.o Y86Loader.o ProgRegisters.o Memory.o Sim.o Tools.o Y86dump.o 

yess: $(CPP)
	$(CX) $(CXXFLAGS) $(HDRS) $(CPP)
Sim.o: $(HDRS)
Y86.o: $(HDRS)
Tools.o: $(HDRS)
Memory.o: $(HDRS)
ProgRegisters.o: $(HDRS)
build:
	$(CX) $(CXXFLAGS) $(HDRS) $(CPP) -c
clean:
	rm -f $(OBJS) yess 
