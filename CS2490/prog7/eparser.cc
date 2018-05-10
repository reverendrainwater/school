/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. eparser.cc.
//	AUTHOR. Taylor Rainwater.
//	INSTALLATION. alduin.
//	DATE-WRITTEN. 4.11.2015.
//	DESCRIPTION. Parses a user provided string
//				 then evaluates the expression.
//
/////////////////////////////////////////////////////////////////////

//************************ENVIRONMENTAL-VARIABLES************************//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <iostream>
#include <cctype>
#include <cstdlib>

class parser
{	public:
		//********PROTOTYPES********//
		
		void match(char, char&);
		void error();
		void White(char&);
		double Expr(char&);
		double Number(char&);
		double Term(char&);
		double Factor(char&);
};

//**************************PROCEDURE-DIVISION**************************//

//--------------SUPPORT-FUNCTIONS--------------//

/**
 * @brief Governed Print.
 * @details Printing function which has
 *          the user specifiy (govern)
 *          the delay.
 *
 * @param n string in.
 * @param nl number of newlines.
 * @param dly delay govern.
 */
void govPrint(const char* n, int nl, int dly)
{	int i = 0;

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

void parser::match(char lookahead, char& curr)
{	if (curr == lookahead){
		std::cin.get(curr);
	} else {
		error();
	}
}

void parser::error()
{	const char* ERR0 = "ERROR 0: SYNTAX ERROR.";

	govPrint(ERR0, 1, 75);
	exit(1);
}

void parser::White(char& curr)
{	if (curr == ' '){
		match(' ', curr);
		White(curr);
	} else if (curr == '\t'){
		match('\t', curr);
		White(curr);
	}
}

//--------------PRIMARY-FUNCTIONS--------------//

double parser::Expr(char& curr)
{	int chk = 1;
	double out;

	if (curr == '+' || curr == '-'){
		if (curr == '-'){ chk = 0;}
		match(curr, curr);
	}
	out = Term(curr);
	if (chk == 0){ out = -out;}
	White(curr);
	while (curr == '+' || curr == '-'){
		if (curr == '+'){
			match('+', curr);
			White(curr);
			out = out + Term(curr);
		} else {
			match('-', curr);
			White(curr);
			out = out - Term(curr);
		}
	}

	return out;
}

double parser::Term(char& curr)
{	double out;	

	out = Factor(curr);
	White(curr);
	while (curr == '*' || curr == '/'){
		if (curr == '*'){
			match('*', curr);
			White(curr);
			out = out * Expr(curr);
		} else {
			match('/', curr);
			White(curr);
			out = out / Expr(curr);
		}
	}

	return out;
}

double parser::Factor(char& curr)
{	double out;

	if (isdigit(curr)){
		out = Number(curr);
		White(curr);
	} else if (curr == '('){
		match('(', curr);
		White(curr);
		out = Expr(curr);
		White(curr);
		match(')', curr);
	} else { error();}

	return out;
}

double parser::Number(char& curr)
{	double out;

	std::cin.unget();
	std::cin >> out;
	std::cin.get(curr);
	return out;
}

//--------------MAIN--------------//
int main(void)
{	parser parse;
	const char* ENTER = "ENTER PARSABLE STRING: ";
	const char* VALID = "STRING EVALUATES TO: ";
	char swp[] = "";
	char curr;
	double out;

	govPrint(ENTER, 0, 75);
	std::cin.get(curr);
	out = parse.Expr(curr);
	if (curr == '\n'){
		govPrint(VALID, 0, 75);
		sprintf(swp, "%.2f", out);
		govPrint(swp, 1, 75);
	} else {
		parse.error();
	}
}
