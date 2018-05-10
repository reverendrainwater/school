CX = g++
CXXFLAGS = -Wall -g -std=c++0x
OBJS = Tools.o Memory.o ProgRegisters.o Sim.o Y86.o
OBJTEST = Tools.o Memory.o ProgRegisters.o testSim.o Y86.o
HDRS = Tools.h Memory.h ProgRegisters.h Sim.h Y86.h

yess: $(OBJS)
	$(CX) $(CXXFLAGS) $(OBJS) -o Y86
test: $(OBJTEST)
	$(CX) $(CXXFLAGS) $(OBJTEST) -o ytest
Sim.o: $(HDRS)
Y86.o: $(HDRS)
Tools.o: $(HDRS)
Memory.o: $(HDRS)
ProgRegisters.o: $(HDRS)

clean:
		rm -f $(OBJS) Y86 ytest