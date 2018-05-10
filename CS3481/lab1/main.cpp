#include <cstdlib>
#include <iostream> // for std::cout , etc.
#include <iomanip>  // additional formatting for cout
#include <cstdint>  // standard int types like uint64_t 
#include <stdio.h> // in case you want to use printf
#include "Tools.h" // Tools function declarations


using namespace std;
using namespace Y86;  

int main(int argc, char *argv[])
{
	// Test code here...
	cout << setfill('0') << hex << getBits(55,59,0x0123456789abcdef) << endl;
}



