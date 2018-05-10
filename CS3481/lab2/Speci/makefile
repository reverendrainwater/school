CC = g++
H = Headers
G12A = Group12a
G9 = Group9
HDRS = $(H)/Register.h $(H)/ProcessorStateWord.h $(H)/FpStatusWord.h\
	$(H)/IntRegisterFile.h $(H)/FpRegisterFile.h $(H)/Memory.h\
	$(H)/Cpu.h $(H)/Instruction.h 
OBJ = FpRegisterFile.o Cpu.o Instruction.o FpStatusWord.o\
	IntRegisterFile.o Loader.o Memory.o Register.o\
	ProcessorStateWord.o speci.o $(G12A)/Group12a.o\
	$(G12A)/Trap.o $(G9)/Group9.o $(G9)/Halt.o 

speci: $(OBJ)
	$(CC) $(OBJ) -o speci
Cpu.o: $(HDRS)
FpStatusWord.o: $(HDRS)
FpRegisterFile.o: $(HDRS)
IntRegisterFile: $(HDRS)
Instruction: $(HDRS)
Loader: $(HDRS)
Memory: $(HDRS)
Register.o: $(HDRS)
ProcessorStateWord: $(HDRS)
speci.o: $(HDRS)
build:
	(cd Group12a; make)
	(cd Group9; make)
	make speci
clean:
	rm -f *.o
	(cd Group12a; rm -f *.o)
	(cd Group9; rm -f *.o)