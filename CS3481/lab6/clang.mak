#///////////////////////////////////////////////////////////////////#
#
#	IDENTIFICATION DIVISION.
#	PROGRAM-ID. makefile.
#	AUTHOR. Rev Taylor R Rainwater and Chase Watson.
#	INSTALLATION. Student.
#	DATE-WRITTEN. 17-2-2016.
#	DESCRIPTION. makefile for YESS.
#
#///////////////////////////////////////////////////////////////////#

CX = clang
CXXFLAGS = -Wall -std=c++0x -g -o yess
OBJS = Y86.o Y86Loader.o ProgRegisters.o Memory.o Sim.o Tools.o Y86trace.o \
	   FetchStage.o DecodeStage.o ExecuteStage.o MemoryStage.o WritebackStage.o
HDRS = Y86.h Y86Loader.h ProgRegisters.h Memory.h Sim.h Tools.h PipeStage.h \
	   FetchStage.h DecodeStage.h ExecuteStage.h MemoryStage.h WritebackStage.h
# Primary Executable
yess: $(OBJS)
	$(CX) $(CXXFLAGS) $(OBJS)
# Sim and Tools
Sim.o: 				Sim.h
Tools.o: 			Tools.h
# Core 
Y86.o: 				Y86.h
Memory.o: 			Memory.h
ProgRegisters.o: 	ProgRegisters.h
FetchStage.o:		FetchStage.h PipeStage.h
DecodeStage.o:		DecodeStage.h PipeStage.h
ExecuteStage.o:		ExecuteStage.h PipeStage.h
MemoryStage.o:		MemoryStage.h PipeStage.h
WritebackStage.o: 	WritebackStage.h PipeStage.h

clean:
	rm -f $(OBJS) yess 
