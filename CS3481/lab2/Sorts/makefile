CC = gcc -g
SORT = sort.o bubbleSort.o mergeSort.o quickSort.o

sort: $(SORT)
		$(CC) $(SORT) -o Sort
bubbleSort.o: bubbleSort.h
mergeSort.o: mergeSort.h
quickSort.o: quickSort.h
sort.o: types.h
clean:
	rm -f *.o
