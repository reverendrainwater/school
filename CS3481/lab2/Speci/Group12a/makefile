CC = g++
H = Headers
HDRS = $(H)/Register.h $(H)/ProcessorStateWord.h $(H)/FpStatusWord.h\
	$(H)/IntRegisterFile.h $(H)/FpRegisterFile.h $(H)/Memory.h\
	$(H)/Cpu.h $(H)/Instruction.h $(H)/Group12a.h $(H)/Trap.h
OBJ = Group12a.o Trap.o

all: $(OBJ)
	$(CC) $(OBJ) -o trap
Group12a.o: $(HDRS)
Trap.o: $(HDRS)
clean:
	rm -f *.o