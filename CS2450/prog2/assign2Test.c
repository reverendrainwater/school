#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>

#define STDOUT_FILENO 1
#define DELTA 0.000001

double getIntArrayAverage(int[], int);
int getIntArrayMax(int[], int);
int getIntArrayMin(int[], int);
int getValueCount(int[], int, int);
int getMaxCount(int[], int);
void testGetIntArrayAverage();
void testGetIntArrayMax();
void testGetIntArrayMin();
void testGetValueCount();
void testGetMaxCount();
int checkGetIntArrayAverage();
int checkGetIntArrayMax();
int checkGetIntArrayMin();
int checkGetValueCount();
int checkGetMaxCount();
void showExampleOutput();

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
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {6,2,3,5,4,7};	
	showExampleOutput(a1, 10, 3);
	showExampleOutput(a2, 6, 4);
	printf("-----------------------\n");
	printf("\n\nRunning tests ...\n");
	printf("-----------------------\n");
	testGetIntArrayAverage();
	printf("-----------------------\n");
	testGetIntArrayMax();
	printf("-----------------------\n");	
	testGetIntArrayMin();
	printf("-----------------------\n");	
	testGetValueCount();
	printf("-----------------------\n");	
	testGetMaxCount();
	printf("-----------------------\n");	
	printf("**Note you will lose 20 points if you use any sort\n");
	printf("  of loop in getMaxCount().\n");
	return 0;
}
void testGetIntArrayAverage()
{
	int pass1 = 0;
	int pass2 = 0;
	
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {6,2,3,5,4,7};
	pass1 = checkGetIntArrayAverage(a1, 10, 4.1);
	pass2 = checkGetIntArrayAverage(a2, 6, 4.5);
	if (pass1 && pass2)
	{
		printf("Function getIntArrayAverage(int[], int) passed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) passed.\n");	
	}
	else
	{
		printf("Function getIntArrayAverage(int[], int) failed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) failed.\n");	
	}
}

int checkGetIntArrayAverage(int array[], int num, double avg)
{
	double max = avg + DELTA;
	double min = avg - DELTA;
	double studentAvg = getIntArrayAverage(array, num);
	if (studentAvg < min || studentAvg > max)
		return 0;
	else
		return 1;
	
}
void testGetIntArrayMax()
{
	int pass1=0;
	int pass2=0;
	
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {5,2,3,5,5,7};
	pass1 = checkGetIntArrayMax(a1, 10, 6);
	pass2 = checkGetIntArrayMax(a2, 6, 7);
	if (pass1 && pass2)
	{
		printf("Function getIntArrayMax(int[], int) passed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) passed.\n");	
	}
	else
	{
		printf("Function getIntArrayMax(int[], int) failed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) failed.\n");	
	}
}

int checkGetIntArrayMax(int array[], int num, int correct)
{
	double studentAnswer = getIntArrayMax(array, num);
	if (studentAnswer == correct)
		return 1;
	else
		return 0;
	
}
void testGetIntArrayMin()
{
	int pass1=0;
	int pass2=0;
	
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {5,2,3,5,5,7};
	pass1 = checkGetIntArrayMin(a1, 10, 1);
	pass2 = checkGetIntArrayMin(a2, 6, 2);
	if (pass1 && pass2)
	{
		printf("Function getIntArrayMin(int[], int) passed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) passed.\n");	
	}
	else
	{
		printf("Function getIntArrayMin(int[], int) failed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) failed.\n");	
	}
}

int checkGetIntArrayMin(int array[], int num, int correct)
{
	double studentAnswer = getIntArrayMin(array, num);
	if (studentAnswer == correct)
		return 1;
	else
		return 0;
	
}

void testGetValueCount()
{
	int pass1 = 0;
	int pass2 = 0;
	int pass3 = 0;
	int pass4 = 0;
	
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {5,2,3,5,5,7};
	pass1 = checkGetValueCount(a1, 10, 3, 2);
	pass2 = checkGetValueCount(a1, 10, 6, 3);
	pass3 = checkGetValueCount(a2, 6, 5, 3);
	pass4 = checkGetValueCount(a2, 6, 1, 0);
	if (pass1 && pass2 && pass3 && pass4)
	{
		printf("Function getValueCount(int[], int, int) passed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) passed.\n");	
	}
	else
	{
		printf("Function getValueCount(int[], int, int) failed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) failed.\n");	
	}
}

int checkGetValueCount(int array[], int num, int value, int correct)
{
	double studentAnswer = getValueCount(array, num, value);
	if (studentAnswer == correct)
		return 1;
	else
		return 0;
	
}

void testGetMaxCount()
{
	int pass1 = 0;
	int pass2 = 0;
	
	int a1[] = {2,3,6,3,5,1,6,6,5,4};
	int a2[] = {5,2,3,5,5,7};
	pass1 = checkGetMaxCount(a1, 10, 3);
	pass2 = checkGetMaxCount(a2, 6, 1);
	if (pass1 && pass2)
	{
		printf("Function getMaxCount(int[], int) passed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) passed.\n");	
	}
	else
	{
		printf("Function getMaxCount(int[], int) failed.\n");		
		//TS_TRACE("Function getIntArrayAverage(int[], int) failed.\n");	
	}
}

int checkGetMaxCount(int array[], int num, int correct)
{
	double studentAnswer = getMaxCount(array, num);
	if (studentAnswer == correct)
		return 1;
	else
		return 0;
	
}

void showExampleOutput(int array[], int num, int countThis)
{	
	printf("\n-----------------------\n");
	printf("Example Output 1:\n");
	printf("-----------------------\n");
	printf("Using int array {");
	for (int i = 0; i < num - 1; i++)
	{
		printf("%i, ", array[i]);
	}
	printf("%i}\n",array[num - 1]);

	double avg = getIntArrayAverage(array, num);
	int max = getIntArrayMax(array, num);
	int min = getIntArrayMin(array, num);
	int count = getValueCount(array, num, countThis);
	int maxCount = getMaxCount(array, num);

	printf("Average: %.2f\n", avg);
	printf("Max: %i\n", max);
	printf("Min: %i\n", min);
	printf("Count of %i: %i\n",countThis, count);
	printf("Count of max: %i\n", maxCount);	
	
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


