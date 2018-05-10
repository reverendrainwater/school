#include <iostream>
#include <string>
#include <unistd.h>
#include <sys/stat.h>
#include <cstdlib>
#include <stdio.h>
#include <fstream>
#include "SymbolList.h"
#include "FileHandler.h"
#include "Resolve.h"

//you need to implement printSymbolsAtEnd and fileExists.
//The stubs for these are below.

/* Resolve
 * Driver for the resolution process.
 * Uses a FileHandler object to handle each .a and .o
 * Maintains an undefined and defined list for symbols
 */
Resolve::Resolve(int argc, char * argv[])
{
   undefined = new SymbolList();
   defined = new SymbolList();
   handler = new FileHandler(defined, undefined);
   for (int i = 1; i < argc; i++)
   {
      
       if (!fileExists(argv[i]) || 
          (!handler->isArchive(argv[i]) && !handler->isObjectFile(argv[i])))
       {
          std::cout << "invalid file: "<< argv[i] << std::endl;
          exit(1);
       }
       if (handler->isArchive(argv[i])) handler->handleArchive(argv[i]);
       if (handler->isObjectFile(argv[i])) handler->handleObjectFile(argv[i]);
   }
   printSymbolsAtEnd();
   
}
/*
 * printSymbolsAtEnd
 * Checks to see if main is defined and has type T
 * Prints the contents of the undefined list
 * Prints the contents of the defined list
 */
void Resolve::printSymbolsAtEnd()
{	char * type = (char *) malloc(sizeof(char));
	// check for reference to main
	if (!defined->getSymbol("main", type)){
		std::cout << ": undefined reference to main" << std::endl;
	}
	undefined->startIterate();
	std::string current = undefined->getNext(type);
	while (current.size() > 0){
		std::cout << ": undefined reference to " << current << std::endl;
		current = undefined->getNext(type);
	}
	//print the contents of the defined list
	defined->printSymbols("Defined");
}

/* fileExists
 * returns true if filename exists
 * Ref: http://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c 
 */
bool Resolve::fileExists(std::string filename)
{
	return (access(filename.c_str(), F_OK) != -1);
}

