#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <iostream>
#include <cctype>
#include <cstdlib>


/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. parser.cc.
//	AUTHOR. Taylor Rainwater.
//	INSTALLATION. alduin.
//	DATE-WRITTEN. 31.10.2015.
//	DESCRIPTION. Parsers a user provided string.
//
/////////////////////////////////////////////////////////////////////

void slowPrint(const char* n, int nl){
	//****FUNCTION-VARIABLES****//
	unsigned int i = 0;
	int dly = 50;

	//****FUNCTION-PROCEDURES****//
	for (; i < strlen(n); i++){
		printf("%c", n[i]);
		fflush(stdout);
		usleep(dly*1000);
	} 
	while (nl > 0){
		printf("\n");
		nl--;
	}
}

class parser{
	public:

		//********PROTOTYPES********//
		
		void match(char, char&);
		void error();
		void F(char&);
		void G(char&);
		void H(char&);
		void I(char&);
		void Expr(char&);
		void Term(char&);
		void AddOp(char&);
		void MulOp(char&);
		void Factor(char&);
		void Number(char&);
		void Digit(char&);
		void White(char&);
};

//*********PROCEDURE-DIVISION********//

void parser::match(char lookahead, char& curr){
	if (curr == lookahead){
		std::cin.get(curr);
	} else {
		error();
	}
}

void parser::error(){
	char ERROR[] = "SYNTAX ERROR.";
	slowPrint(ERROR, 1);
	exit(1);
}


void parser::F(char& curr){
	if (curr == '+' || curr == '-'){
		AddOp(curr);
	}
}

void parser::G(char& curr){
	if (curr == '+' || curr == '-'){
		AddOp(curr);
		Factor(curr);
		G(curr);
	}
}

void parser::H(char& curr){
	if (curr == '*' || curr == '/'){
		MulOp(curr);
		Factor(curr);
		H(curr);
	}
}

void parser::I(char& curr){
	if (isdigit(curr)){
		Digit(curr);
		I(curr);
	}
}

void parser::Expr(char& curr){
	if (curr == '('){
		F(curr);
		Term(curr);
		G(curr);
	} else if (isdigit(curr)){
		Term(curr);
		G(curr);
	}
}

void parser::Term(char& curr){
	if (curr == '(' || isdigit(curr)){
		Factor(curr);
		H(curr);
	}
}

void parser::AddOp(char& curr){
	if (curr == '+'){
		match('+', curr);
	} else if (curr == '-'){
		match('-', curr);
	} else {
		error();
	}
}

void parser::MulOp(char& curr){
	if (curr == '*'){
		match('*', curr);
	} else if (curr == '/'){
		match('/', curr);
	} else {
		error();
	}
}

void parser::Factor(char& curr){
	if (isdigit(curr)){
		Number(curr);
	} else if (curr == '('){
		match('(', curr);
		Expr(curr);
		match(')', curr);
	}
}

void parser::Number(char& curr){
	if (isdigit(curr)){
		Digit(curr);
		I(curr);
	} 
}

void parser::Digit(char& curr){
	if (isdigit(curr)){
		switch (curr){
			case '0': match('0', curr);
					  break;
			case '1': match('1', curr);
					  break;
			case '2': match('2', curr);
					  break;
			case '3': match('3', curr);
					  break;
			case '4': match('4', curr);
					  break;
			case '5': match('5', curr);
					  break;
			case '6': match('6', curr);
					  break;
			case '7': match('7', curr);
					  break;
			case '8': match('8', curr);
					  break;
			case '9': match('9', curr);
					  break;
		}
	}
}

void parser::White(char& curr){
	if (curr == ' '){
		match(' ', curr);
		White(curr);
	} else if (curr == '\t'){
		match('\t', curr);
		White(curr);
	}
}

//********MAIN********//
int main(void){
	parser p;
	const char* ENTER = "ENTER PARSABLE STRING: ";
	const char* VALID = "STRING IS VALID TO LANGUAGE.";
	char curr;

	slowPrint(ENTER, 0);
	std::cin.get(curr);
	p.Expr(curr);
	if (curr == '\n'){
		slowPrint(VALID, 1);
	} else {
		p.error();
	}
}
