###############################
# y86dis makefile
# Rev Taylor R Rainwater
# 29.1.2016
# prophet
###############################
CXX = g++
CXXFLAGS = -g -Wall -std=c++0x
HDRS = y86dis.h
OBJ = y86dis.o

y86dis: $(OBJ)
	$(CXX) $(CXXFLAGS) $(OBJ) -o y86dis
y86dis.o: $(HDRS)
clean:
	rm -f *.o