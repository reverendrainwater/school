/**
 * FileHandler - Linker Lab
 * Rev. Taylor R. Rainwater
 * 
 */
// Libraries
#include <iostream>
#include <iomanip>
#include <string.h>
#include <cstdlib>
// Proper headers
#include "SymbolList.h"
#include "FileHandler.h"

/* FileHandler constructor
 * constucts the things
 *
 */
FileHandler::FileHandler(SymbolList * defined, SymbolList * undefined)
{
	this->defined = defined;
	this->undefined = undefined;
}

/* handleObjectSymbol - handles symbols from an object file
 * 
 *   takes: string, char
 * returns: nothing
 */
void FileHandler::handleObjectSymbol(std::string name, char type)
{   char * type_temp = (char *) malloc(sizeof(char));
	static int count = 0;
	if (type == 'U'){
		if (!defined->getSymbol(name, type_temp)
			&& !undefined->getSymbol(name, type_temp)){
			undefined->insertSymbol(name, type);
		}
	} else if (type == 'T' || type == 'D'){
		if (!defined->getSymbol(name, type_temp)){
			defined->insertSymbol(name, type);
			if (undefined->getSymbol(name, type_temp)){
				undefined->removeSymbol(name);
			}
		} else if (defined->getSymbol(name, type_temp)){
			if (*type_temp == 'C'){
				defined->updateSymbol(name, type);
			} else {
				std::cout << ": multiple definition of " << name << std::endl;
			}
		} else if (undefined->getSymbol(name, type_temp)){
			undefined->removeSymbol(name);
			defined->insertSymbol(name, type);       
		}
	} else if (type == 'C'){
		if (!defined->getSymbol(name, type_temp)){
			defined->insertSymbol(name, type);	        
		}
		if (undefined->getSymbol(name, type_temp)){
			undefined->removeSymbol(name);
		}
	} else if (type == 'b' || type == 'd'){
		std::string updated_name = name + "." + std::to_string(count);
		count++;
		defined->insertSymbol(updated_name, type);   
	}
}

/* objectFileNeeded - determines if an object file in an archive 
 *                    is needed by the table
 * 
 *   takes: string
 * returns: bool
 */
bool FileHandler::objectFileNeeded(std::string filename)
{   int buff_size = 32;
	unsigned long * value = (unsigned long *) malloc(sizeof(unsigned long));
	char * name = (char *) malloc(buff_size * sizeof(char));
	char * type = (char *) malloc(sizeof(char));
	char * buffer = (char *) malloc(buff_size * sizeof(char));
	FILE * fp = popen(("nm tmp/" + filename).c_str(), "r");
	// read the output line for line
	while(fgets(buffer, buff_size, fp)){
		// not a digit, use specific formatting otherwise not
		if (!isdigit(buffer[0])){
			sscanf(buffer, " %c %s", type, name);
		} else {
			sscanf(buffer, "%lx %c %s", value, type, name);
		}
		// In undefined? Need it. Also, clean up.
		if (undefined->getSymbol(name, type)){
			free(value);
			free(type);
			free(buffer);
			return true;	
		}
	}
	// Clean up everything. 
	free(value);
	free(type);
	free(buffer);
	return false;
}

/* isArchive - determines if a file is a is an archive
 * 
 *   takes: string
 * returns: bool
 */
bool FileHandler::isArchive(std::string filename)
{   int length = filename.length();
	// get the extension 
	std::string extension = filename.substr(length - 2, length - 1);
	return (!extension.compare(".a") && length >= 2);
}

/* isObject - determines if a file is a is an object file
 * 
 *   takes: string
 * returns: bool
 */
bool FileHandler::isObjectFile(std::string filename)
{   int length = filename.length();
	// get the extension
	std::string extension = filename.substr(length - 2, length - 1);
	return (!extension.compare(".o") && length >= 2);
}

/* handleArchive - handles an archive file, determining what to send
 *                 to handleObjectFile
 * 
 *   takes: string
 * returns: nothing
 */
void FileHandler::handleArchive(std::string filename)
{   std::string name = "";
	bool changed = false;
	int buff_size = 32;
	int position = -1;
	char * buffer = (char *) malloc(32 * sizeof(char));
	// Perform system calls; ignore how ugly this is
	if (system("mkdir tmp") == -1){
		std::cout << "Error making directory tmp!" << std::endl;
		exit(EXIT_FAILURE);
	}
	if (system(("cp " + filename + " tmp/tmp.a").c_str()) == -1){
		std::cout << "Error copying archive!" << std::endl;
		exit(EXIT_FAILURE);
	}
	if (system("cd tmp; ar -x tmp.a") == -1){
		std::cout << "Error running ar!" << std::endl;
		exit(EXIT_FAILURE);
	}
	// Successfully ran the system calls now open pipe
	FILE * fp = popen("nm tmp/tmp.a", "r");
	// Based entirely on the recommended code in lab instructions
	do {
		changed = false;
		while (fgets(buffer, buff_size, fp)){
			position = -1;
			position = std::string(buffer).find(":");
			// Found an object file name!
			if(position != -1){
				name = std::string(buffer).substr(0, position);
				if(objectFileNeeded(name)){
					changed = true;
					handleObjectFile("tmp/" + name);
				}
			}
		}
	} while(changed);
	// Clean up after yourself!
	if (system("rm -rf tmp") == -1){
		std::cout << "Error removing tmp!" << std::endl;
		exit(EXIT_FAILURE);
	}
	free(buffer);
}

/* handleObjectFile - handles an object file and send the symbols
 *                    to handleObjectSymbol
 *
 *   takes: string
 * returns: nothing
 */
void FileHandler::handleObjectFile(std::string filename)
{   char type;
	int buff_size = 32;
	long unsigned int value = 0;
	FILE * fp = popen(("nm " + filename).c_str(), "r");
	char * buffer = (char *) malloc(buff_size * sizeof(char));
	char * name = (char*) malloc(512 * sizeof(char));
	if (fp == NULL) { std::cout << "popen failed\n"; exit(1); }
	while (fgets(buffer, buff_size, fp)){
		if (!isdigit(buffer[0])){
			sscanf(buffer, " %c %s", &type, name);
		} else {
			sscanf(buffer, "%lx %c %s", &value, &type, name);
		}
		handleObjectSymbol(std::string(name), type);
	}
	free(buffer);
	free(name);
	pclose(fp);	
}

// Why are you down here?
