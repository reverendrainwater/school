// Finite State Machine
// Accepts strings over {1,4,9} and prints predicted character.
// Rev. Taylor Rainwater, 9/29/15

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class finiteSleep
	{
		private final static int[][] STATE_TABLE = {
		//  1   2   4   x
			{  1,  2,  3,  7 },	// 0
			{  4,  6,  5,  7 },	// 1
			{  6,  5,  4,  7 },	// 2
			{  5,  4,  6,  7 },	// 3
			{  4,  6,  5,  7 },	// 4
			{  6,  5,  4,  7 },	// 5
			{  5,  4,  6,  7 },	// 6
			{  7,  7,  7,  7 }, // 7
    };

    private BufferedReader in;


    public finiteSleep()
	{
        in = new BufferedReader(
                 new InputStreamReader(System.in));
    }


    public void run() throws IOException
	{
        char ch;
        int  state;

        for (;;) {
            System.out.print("Enter your string: ");
            ch    = (char) in.read();
            state = 0;

            while (ch != '\n') {
               state = STATE_TABLE[state][charToColumn(ch)];
               ch    = (char) in.read();
            }

            if (state == 4) {
                System.out.println("1\n");
            } else if (state == 5) {
                System.out.println("4\n");
            } else if (state == 6){
                System.out.println("9\n");
						} else if (state == 7){
							  System.out.println("\n");
							  break;
						} else {
						  	System.err.println("Error 0: Cannot find next value.");
						}
        }
    }

    public int charToColumn(char ch)
	{
        int column = 2;

        switch( ch ) {
        case '1':
            column = 0;
            break;

        case '4':
            column = 1;
            break;

	      case '9':
            column = 2;
            break;

				case 'x':
				case 'X':
				    column = 3;
						break;
				}

        return column;
    }

    public static void main(String [] args)
	{
        try {
            finiteSleep fsm = new finiteSleep();
            fsm.run();
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}
