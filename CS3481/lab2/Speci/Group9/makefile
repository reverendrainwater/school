CC = g++
H = Headers
HDRS = $(H)/Register.h $(H)/ProcessorStateWord.h $(H)/FpStatusWord.h\
	$(H)/IntRegisterFile.h $(H)/FpRegisterFile.h $(H)/Memory.h\
	$(H)/Cpu.h $(H)/Instruction.h $(H)/Group9.h $(H)/Halt.h
OBJ = Group9.o Halt.o

all: $(OBJ)
	$(CC) $(OBJ) -o halt
Group9.o: $(HDRS)
Trap.o: $(HDRS)
clean:
	rm -f *.o