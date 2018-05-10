#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#define STDOUT_FILENO 1

void printChars(int, char);
void printRect(int, int, char);
void testPrintChars();
void testPrintRect();
int checkPrintChars(int, char testChar);
int checkPrintRect(int, int, char);
void failMsg1(int, char);
void failMsg2(int, int, char);
void capture();
void uncapture();

char input[1024];
char msg[1024];
int stdout_save;
int score;

int main()
{
	printf("\nprintChars(7,'X');\n");
	printChars(7, 'X');
	printf("\nprintRect(5,8,'O');\n");
	printRect(5, 8, 'O');


	score = 0;
	printf("\n\nRunning tests ...\n_____________________\n");
	
	testPrintChars();
	testPrintRect();
	
	printf("____________________\nTotal Score: %i%%\n", score);
	msg[0] = 0;		
	strcat(msg, "\n**You will receive an additional 20% ");
	strcat(msg, "if you used printChars() to create ");
	strcat(msg, "your rectangle in printRect().");
	strcat(msg, "\n\n**You will lose 20% if you submit ");
	strcat(msg, "assign1.c in the wrong format.\n\n");
	printf("%s\n", msg);
}

void testPrintChars()
{
	srand(time(NULL));
	int n1 = rand() % 4 + 3;
	int n2 = rand() % 4 + 7;						
	char chrs [] = "=+-*^%$#@!";
	char c1 = chrs[rand() % 5];
	char c2 = chrs[rand() % 5 + 5];
	
	if( checkPrintChars(n1, c1) && checkPrintChars(n2, c2) )
	{
		printf("Function printChars(int, char) passed(+35%%)\n");
		score += 35;
		//TS_TRACE("Function printChars(int, char) passed.");
	}
}

int checkPrintChars(int testNum, char testChar)
{
	capture();
	printChars(testNum, testChar);
       uncapture();

	int i = 0;		
	
	for (i = 0; i<testNum; i++)
	{
		if (input[i] != testChar)
		{
			failMsg1(testNum, testChar);
			return 0;
		}
	}					
			
	if (input[testNum] != '\n')
	{
		failMsg1(testNum, testChar);
		return 0;
	}
	
	return 1;
}

void testPrintRect()
{
	srand(time(NULL));
	int h1 = rand() % 4 + 3;
	int h2 = rand() % 4 + 7;						
	int w1 = rand() % 4 + 3;
	int w2 = rand() % 4 + 7;						
	char chrs [] = "=+-*^%$#@!";
	char c1 = chrs[rand() % 5];
	char c2 = chrs[rand() % 5 + 5];
	
	if( checkPrintRect(h1, w1, c1) && checkPrintRect(h2, w2, c2) )
	{
		printf("Function printRect(int, int, char) passed(+45%%)\n");
		score += 45;
		//TS_TRACE("Function printRect(int, int, char) passed.");
	}
}

int checkPrintRect(int testHeight, int testWidth, char testChar)
{
	capture();
	printRect(testHeight, testWidth, testChar);
       uncapture();

	int i = 0, j = 0;				
	
	if ((signed)strlen(input) != testHeight * (testWidth + 1))
	{
		failMsg2(testHeight, testWidth, testChar);
		return 0;		
	}
			
	for (i = 0; i<testHeight; i++)			
	{
		for (j = 0; j < testWidth; j++)
		{
			if (input[i * (testWidth+1) + j] != testChar)
			{
				failMsg2(testHeight, testWidth, testChar);
				return 0;		
			}
		}
		
		if (input[i * (testWidth+1) + j] != '\n')
		{
				failMsg2(testHeight, testWidth, testChar);					
				return 0;		
		}
		
	}					
	
	return 1;
}

void failMsg1(int testNum, char testChar)
{
	sprintf(msg, "Function printChars(%i, '%c') failed.\n"
		, testNum, testChar); 				
	printf("%s",msg);
	//TS_FAIL(msg);		
}

void failMsg2(int testHeight, int testWidth, char testChar)
{
	sprintf(msg, "Function printRect(%i, %i, '%c') failed.\n"
		, testHeight, testWidth, testChar); 				
	//TS_FAIL(msg);		
	printf("%s",msg);
}
	
void capture()
{    
      fflush(stdout); //clean everything first
      stdout_save = dup(STDOUT_FILENO); //save the stdout state
      freopen("NUL", "a", stdout); //redirect stdout to null pointer
      setvbuf(stdout, input, _IOFBF, 1024); //set buffer to stdout
}

void uncapture()
{
      freopen("NUL", "a", stdout); //redirect stdout to null again
      dup2(stdout_save, STDOUT_FILENO); //restore the previous state of stdout
      setvbuf(stdout, NULL, _IONBF, 0); //disable buffer to print to screen instantly
}


