// Counts occurrences of robot, robber, ottoman and manual
// Andy Dalton and Dee Parks, 9/29/06

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class FSMTest {

    // State Table
    private final static int [][] STATE_TABLE =
    {
        // r,  o,  b,  e,  t,  m,  a,  n,  u,  l,  other
        { 14,  1,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 0
        { 14,  1,  0,  0,  2,  8,  0,  0,  0,  0,  0},  // State 1
        { 14,  1,  0,  0,  3,  8,  0,  0,  0,  0,  0},  // State 2
        { 14,  4,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 3
        { 14,  1,  0,  0,  2,  5,  0,  0,  0,  0,  0},  // State 4
        { 14,  1,  0,  0,  0,  8,  6,  0,  0,  0,  0},  // State 5
        { 14,  1,  0,  0,  0,  8,  0,  7,  0,  0,  0},  // State 6
        { 14,  1,  0,  0,  0,  8,  0,  0, 11,  0,  0},  // State 7
        { 14,  1,  0,  0,  0,  8,  9,  0,  0,  0,  0},  // State 8
        { 14,  1,  0,  0,  0,  8,  0, 10,  0,  0,  0},  // State 9
        { 14,  1,  0,  0,  0,  8,  0,  0, 11,  0,  0},  // State 10
        { 14,  1,  0,  0,  0,  8, 12,  0,  0,  0,  0},  // State 11
        { 14,  1,  0,  0,  0,  8,  0,  0,  0, 13,  0},  // State 12
        { 14,  1,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 13
        { 14, 15,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 14
        { 14,  1, 16,  0,  2,  8,  0,  0,  0,  0,  0},  // State 15
        { 14, 20, 17,  0,  0,  8,  0,  0,  0,  0,  0},  // State 16
        { 14,  1,  0, 18,  0,  8,  0,  0,  0,  0,  0},  // State 17
        { 19,  1,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 18
        { 14, 15,  0,  0,  0,  8,  0,  0,  0,  0,  0},  // State 19
        { 14,  1,  0,  0, 21,  8,  0,  0,  0,  0,  0},  // State 20
        { 14,  1,  0,  0,  3,  8,  0,  0,  0,  0,  0}  // State 21
    };

    // Table constants
    private final static int r_COLUMN   = 0;
    private final static int o_COLUMN   = 1;
    private final static int b_COLUMN   = 2;
    private final static int e_COLUMN   = 3;
    private final static int t_COLUMN   = 4;
    private final static int m_COLUMN   = 5;
    private final static int a_COLUMN   = 6;
    private final static int n_COLUMN   = 7;
    private final static int u_COLUMN   = 8;
    private final static int l_COLUMN   = 9;
    private final static int ERR_COLUMN = 10;

    // Special states
    private final static int START = 0;
    private final static int robber_ACCEPT = 19;
    private final static int robot_ACCEPT = 21;
    private final static int ottoman_ACCEPT = 7;
    private final static int manual_ACCEPT = 13;



    private BufferedReader in;


    public FSMTest() {
        in = new BufferedReader(
                 new InputStreamReader(System.in));
    }


    public void run() throws IOException {
        int input;
        int state;
        int robberCount = 0;
        int robotCount = 0;
        int ottomanCount = 0;
        int manualCount = 0;


        input = in.read();
        state = START;
            
        while (input != -1) {
           char ch = (char) input;
           state = STATE_TABLE[state][charToColumn(ch)];
           input = in.read();
           if( state == robber_ACCEPT)
               robberCount++;
           else if( state == robot_ACCEPT)
               robotCount++;
           else if( state == ottoman_ACCEPT)
               ottomanCount++;
           else if( state == manual_ACCEPT)
               manualCount++;
        }

        // display counts
        System.out.println("Occurrence counts:");
        System.out.println("robber: " + robberCount);
        System.out.println("robot: "  + robotCount);
        System.out.println("ottoman: " + ottomanCount);
        System.out.println("manual: " + manualCount);
    }


    public int charToColumn(char ch) {
        int column = ERR_COLUMN;

        switch( ch ) {
        case 'r':
            column = r_COLUMN;
            break;
        case 'o':
            column = o_COLUMN;
            break;
        case 'b':
            column = b_COLUMN;
            break;

        case 'e':
            column = e_COLUMN;
            break;
        case 't':
            column = t_COLUMN;
            break;
        case 'm':
            column = m_COLUMN;
            break;
        case 'a':
            column = a_COLUMN;
            break;
        case 'n':
            column = n_COLUMN;
            break;
        case 'u':
            column = u_COLUMN;
            break;
        case 'l':
            column = l_COLUMN;
            break;
        }

        return column;
    }


    public static void main(String[] args) {
        try {
            FSMTest fsm = new FSMTest();
            fsm.run();
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}

