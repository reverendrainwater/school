#include <iostream>
#include <iomanip>
#include <string>
#include <cstdlib>
#include "SymbolList.h"

/* SymbolList constructor
 * constructs things
 */
SymbolList::SymbolList()
{
	first = NULL;
}

/* printSymbols
 * prints a header and then the contents of the linked list
 */
void SymbolList::printSymbols(std::string header)
{
    symbolEntry * ptr = first;
    std::cout << header << " Symbol Table\n";
    std::cout << "-----------------------\n";
    while (ptr != NULL)
    {
       
       std::cout << std::setw(32) << std::left 
                 << ptr->name << " " << ptr->type 
                 << std::endl;
       ptr = ptr->next;
    }
}

/* getSymbol - returns true if symbol is in the list and updates
 *             the type
 *
 *   takes: string, char*
 * returns: bool 
 */
bool SymbolList::getSymbol(std::string symbolName, char * type)
{  	bool changed = false; 
	startIterate();	
	while (iterate != NULL){
		if (iterate->name == symbolName){
			(*type) = iterate->type;
			changed = true;
		}
		iterate = iterate->next;
	}
	return changed;
}

/* updateSymbol - updates symbol's type in list to type
 * 
 *   takes: string, char 
 * returns: nothing
 */
void SymbolList::updateSymbol(std::string symbolName, char type)
{   
    startIterate();
	while (iterate != NULL && iterate->name != symbolName){
	    iterate = iterate->next;
	}
	iterate->type = type;
}

/* insertSymbol - puts a new symbol of symbolName and type at 
 *                the end of the linked list
 * 
 *   takes: string, char
 * returns: nothing
 */
void SymbolList::insertSymbol(std::string symbolName, char type)
{   symbolEntry * new_symbol = new symbolEntry();
	new_symbol->name = symbolName;
	new_symbol->type = type;
	new_symbol->next = NULL;
	// set iterate to first
	startIterate();
    if (iterate == NULL){
		first = new_symbol;
	} else {
		while (iterate->next != NULL){
			iterate = iterate->next;
		}
		iterate->next = new_symbol;
	}
}

/* removeSymbol - removes a symbol from the linked list 
 * 
 *   takes: string
 * returns: nothing
 */
void SymbolList::removeSymbol(std::string symbolName) {
	bool end_list = false;
	startIterate();
	if(iterate->name == symbolName) {
		first = first->next;
		iterate->next = NULL;
		end_list = true;
	}
	while(!end_list) {
		if(iterate->next->name == symbolName) {
			iterate->next = iterate->next->next;
			end_list = true;       
		} else {
			iterate = iterate->next;
		}
	}
}

/* startIterate - sets iterate to first
 * 
 */
void SymbolList::startIterate()
{
	iterate = first;
}

/* getNext - returns NULL if iterate is NULL, otherwise it 
 *           returns the value of name, sets *type to type, 
 *           and sets iterate to next node
 * 
 *   takes: char*
 * returns: string
 */
std::string SymbolList::getNext(char * type)
{   std::string name = "";
	if (iterate == NULL){
		return name;
	}
	*type = iterate->type;
	name = iterate->name;
	iterate = iterate->next;
	return name;
}
