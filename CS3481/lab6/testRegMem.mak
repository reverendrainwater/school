CXX=g++
CXXFLAGS = -Wall -g -std=c++0x
OBJS = Tools.o Memory.o ../lab5/Y86.o ProgRegisters.o Y86Loader.o ../lab5/Y86dump.o testRegMem.o

testRegMem:	$(OBJS)
		$(CXX) $(CXXFLAGS) $(OBJS) -o testRegMem
		
		
Tools.o:            Tools.h
Y86.o:              Y86.h Memory.h Sim.h
Memory.o:           Memory.h Tools.h Sim.h
ProgRegisters.o:    ProgRegisters.h Register.h Sim.h
Y86Loader.o:		Y86.h
Y86dump.o:			Y86.h Sim.h
testRegMem.o:		Y86.h Sim.h  Memory.h ProgRegisters.h

clean:
		rm -f *.o testRegMem

