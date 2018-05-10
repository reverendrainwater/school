#include <stdio.h>
#include <string.h>

/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. assign1.
//	AUTHOR. Taylor R. Rainwater.
//	INSTALLATION. student.cs.appstate.edu
//	DATE-WRITTEN. 11.9.15.
//	DESCRIPTION. A program which runs two
//	*						functions: printChars and
//	*						printRect.
//
/////////////////////////////////////////////////////////////////////

/*  printChars takes an int and a char which 
	it then uses to print the int amount of the 
	char in a row*/
void printChars(int n, char c)
{
	int i = 0;
	
	for(; i < n; i++ )
	{
		printf("%1c", c);
	}
	
	printf("\n");
}

/*  printRect takes two ints and a char, it then
	prints a "rectangle" by using the first int as 
	the height of the rectangle then passes the 
	second int and char to the printChar function
	to print the rows of the rectangle*/
void printRect(int h, int w, char c)
{
	int i = 0;
	
	for(; i < h; i++ )
	{
		printChars(w, c);
	}
}
