import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. prog1.
//	AUTHOR. Taylor R Rainwater.
//	INSTALLATION. student.cs.appstate.edu
//	DATE-WRITTEN. 15.9.15.
//	DESCRIPTION. A program which finds the number of times a string 
// *			 occurs in the given input. The strings are church,
// *			 duchess, reader, and stretch. 
//
/////////////////////////////////////////////////////////////////////

public class FiniteStateMachine {

    // State Table
    private final static int [][] STATE_TABLE =
    {
        // a,  c,  d,  e,  h,  r,  s,  t,  u,  other
        {  0,  1,  7,  0,  0, 14, 20,  0,  0,  0},  // State 0 (Start)
        {  0,  1,  7,  0,  2, 14, 20,  0,  0,  0},  // State 1
        {  0,  1,  7,  0,  0, 14, 20,  0,  3,  0},  // State 2
        {  0,  1,  7,  0,  0,  4, 20,  0,  0,  0},  // State 3
        {  0,  5,  7, 15,  0, 14, 20,  0,  0,  0},  // State 4
        {  0,  1,  7,  0,  6, 14, 20,  0,  0,  0},  // State 5
        {  0,  1,  7,  0,  0, 14, 20,  0,  3,  0},  // State 6
        {  0,  1,  7,  0,  0, 14, 20,  0,  8,  0},  // State 7
        {  0,  9,  7,  0,  0, 14, 20,  0,  0,  0},  // State 8
        {  0,  1,  7,  0, 10, 14, 20,  0,  0,  0},  // State 9
        {  0,  1,  7, 11,  0, 14, 20,  0,  0,  0},  // State 10
        {  0,  1,  7,  0,  0, 14, 12,  0,  0,  0},  // State 11
        {  0,  1,  7,  0,  0, 14, 13, 21,  0,  0},  // State 12
        {  0,  1,  7,  0,  0, 14, 20, 21,  0,  0},  // State 13
        {  0,  1,  7,  0,  0, 14, 20, 21,  0,  0},  // State 14
        { 16,  1,  7,  0,  0, 14, 20,  0,  0,  0},  // State 15
        {  0,  1, 17,  0,  0, 14, 20,  0,  0,  0},  // State 16
        {  0,  1,  7, 18,  0, 14, 20,  0,  8,  0},  // State 17
        {  0,  1,  7,  0,  0, 19, 20,  0,  0,  0},  // State 18
        {  0,  1,  7, 15,  0, 14, 20,  0,  0,  0},  // State 19
        {  0,  1,  7,  0,  0, 14, 20, 21,  0,  0},  // State 20
        {  0,  1,  7,  0,  0, 22, 20,  0,  0,  0},  // State 21
		{  0,  1,  7, 23,  0, 14, 20,  0,  0,  0},  // State 22
		{ 16,  1,  7,  0,  0, 14, 20, 24,  0,  0},  // State 23
		{  0, 25,  7,  0,  0, 14, 20,  0,  0,  0},  // State 24
		{  0,  1,  7,  0, 26, 14, 20,  0,  0,  0},  // State 25
		{  0,  1,  7,  0,  0, 14, 20,  0,  3,  0}   // State 26
    };

    // Table constants
    private final static int a_COLUMN   = 0;
    private final static int c_COLUMN   = 1;
    private final static int d_COLUMN   = 2;
    private final static int e_COLUMN   = 3;
    private final static int h_COLUMN   = 4;
    private final static int r_COLUMN   = 5;
    private final static int s_COLUMN   = 6;
    private final static int t_COLUMN   = 7;
    private final static int u_COLUMN   = 8;
    private final static int ERR_COLUMN = 9;

    // Special states
    private final static int START = 0;
    private final static int church_ACCEPT = 6;
    private final static int duchess_ACCEPT = 13;
    private final static int reader_ACCEPT = 19;
    private final static int stretch_ACCEPT = 26;



    private BufferedReader in;


    public FiniteStateMachine() {
        in = new BufferedReader(
                 new InputStreamReader(System.in));
    }


    public void run() throws IOException {
        int input;
        int state;
        int churchCount = 0;
        int duchessCount = 0;
        int readerCount = 0;
        int stretchCount = 0;


        input = in.read();
        state = START;
            
        while (input != -1) {
           char ch = (char) input;
           state = STATE_TABLE[state][charToColumn(ch)];
           input = in.read();
           if( state == church_ACCEPT)
               churchCount++;
           else if( state == duchess_ACCEPT)
               duchessCount++;
           else if( state == reader_ACCEPT)
               readerCount++;
           else if( state == stretch_ACCEPT)
               stretchCount++;
        }

        // display counts
        System.out.println("Occurrence counts:");
        System.out.println("church: " + churchCount);
        System.out.println("duchess: "  + duchessCount);
        System.out.println("reader: " + readerCount);
        System.out.println("stretch: " + stretchCount);
    }


    public int charToColumn(char ch) {
        int column = ERR_COLUMN;

        switch( ch ) {
        case 'a':
            column = a_COLUMN;
            break;
        case 'c':
            column = c_COLUMN;
            break;
        case 'd':
            column = d_COLUMN;
            break;
        case 'e':
            column = e_COLUMN;
            break;
        case 'h':
            column = h_COLUMN;
            break;
        case 'r':
            column = r_COLUMN;
            break;
        case 's':
            column = s_COLUMN;
            break;
        case 't':
            column = t_COLUMN;
            break;
        case 'u':
            column = u_COLUMN;
            break;
        }

        return column;
    }


    public static void main(String[] args) {
        try {
            FiniteStateMachine fsm = new FiniteStateMachine();
            fsm.run();
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}

