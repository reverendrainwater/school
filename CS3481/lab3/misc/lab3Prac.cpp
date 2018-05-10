/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//    PROGRAM-ID. lab3Prac.cpp.
//    AUTHOR. Taylor Rainwater.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 27.1.2016.
//    DESCRIPTION. Practice for CS3481 lab three.
//
/////////////////////////////////////////////////////////////////////
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstring>
#include <cstdlib>
#include <string.h>
#include <unistd.h>

//*************************PROTOTYPES*************************//
void slowPrint(char n[], int nl);

//****************************MAIN****************************//
int main(int argc, char* argv[])
{   std::string fswp;
    std::ifstream fin (argv[1]);
    char swp[512];
    char arr[512][512];
    int i = 0;

    if (argc < 2){
        std::cerr << "USAGE: " << argv[0] << " FILE_NAME" << std::endl;
        return 1;
    }
    while (getline(fin, fswp)){
        strcpy(swp, fswp.c_str());
        strcpy(arr[i], swp);
        i++;
    }
    for (int j = 0; j < 512; j++){
        slowPrint(arr[20], 1);
        slowPrint(arr[j], 1);
    }
    fin.close();
    return 0;
}

void slowPrint(char n[], int nl)
{   int i = 0;
    int dly = 70;
    int len = strlen(n);

    for (; i < len; i++){
        std::cout << n[i];
        fflush(stdout);
        usleep(dly*1000);
    }
    while (nl > 0){
        printf("\n");
        nl--;
    }
}