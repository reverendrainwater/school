#include <stdio.h>
#include <string.h>
#include "assign3.h"

int main()
{
	Person people[NUM_PEOPLE];
	
	printf("Getting people\n");
	getPeople(people, 3);
	
	printf("\nPrinting people\n");
	printPeople(people, 3);
}
