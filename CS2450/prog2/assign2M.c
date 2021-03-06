#include <stdio.h>
#include <string.h>

/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. assign2.
//	AUTHOR. Taylor R. Rainwater.
//	INSTALLATION. student.cs.appstate.edu
//	DATE-WRITTEN. 24.9.15.
//	DESCRIPTION. Assignment Two for CS2450.
//
/////////////////////////////////////////////////////////////////////

double getIntArrayAverage(int arr[], int l){
  double a = 0;

  int i = 0;
  for(; i < l; i++){
    a += arr[i];
  }
  a /= l;
  return a;
}

int getIntArrayMax(int arr[], int l){
  int m = 0;

  int i = 0;
  while(i < l){
    if(arr[i] > m){
      m = arr[i];
    }
    i++;
  }
  return m;
}

int getIntArrayMin(int arr[], int l){
  int m = arr[0];

  int i = 0;
  while(i < l){
    if(arr[i] < m){
      m = arr[i];
    }
    i++;
  }
  return m;
}

int getValueCount(int arr[], int l, int v){
  int c = 0;

  int i = 0;
  while(i < l){
    if(arr[i] == v){
      c++;
    }
    i++;
  }
  return c;
}

int getMaxCount(int arr[], int l){
  return getValueCount(arr, l, getIntArrayMax(arr, l));
}

int main(){
  int tarr[5] = { 1, 1, 6, 4, 5 };
  double a = getIntArrayAverage(tarr, 5);
  int mx = getIntArrayMax(tarr, 5);
  int mn = getIntArrayMin(tarr, 5);
  int c = getValueCount(tarr, 5, 1);
  int cm = getMaxCount(tarr, 5);

  printf("%.1f\n", a);
  printf("%d\n", mx);
  printf("%d\n", mn);
  printf("%d\n", c);
  printf("%d\n", cm);
}
