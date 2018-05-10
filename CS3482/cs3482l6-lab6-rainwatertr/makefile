CC = g++
CFLAGS = -O2 -g -c -Wall -std=c++11 
OBJS = main.o Resolve.o SymbolList.o FileHandler.o

.C.o:
	$(CC) $(CFLAGS) $< -o $@

all:
	scl enable devtoolset-3 '/bin/bash --rcfile <(echo "make resolve; exit")'

resolve: $(OBJS)
	$(CC) $(OBJS) -o resolve

Resolve.o: SymbolList.h FileHandler.h Resolve.h

FileHandler.o: SymbolList.h FileHandler.h

SymbolList.o: SymbolList.h

main.o: SymbolList.h Resolve.h FileHandler.h

clean:
	rm *.o resolve
