// Finite State Machine
// Accepts strings over {a,b} that contain aabab, bbbab, or baaa.
// Rev. Taylor Rainwater, 9/8/15

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class finiteMachine
	{
		private final static int[][] STATE_TABLE = {
			{  1,  4, 11 },
			{  2,  4, 11 },
			{  2,  3, 11 },
			{  8, 5, 11 },
			{  9,  5, 11 },
			{  9,  6, 11 },
			{  7,  6, 11 },
			{ 9,  8, 11 },
			{  8,  8, 11 },
			{ 10,  4, 11 },
			{ 8, 3, 11 },
			{ 11, 11,11 }
    };

    private BufferedReader in;


    public finiteMachine()
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

            if (state == 10) {
                System.out.println("Accept\n");
            } else {
                System.out.println("Reject\n");
            }
        }
    }

    public int charToColumn(char ch)
	{
        int column = 2;

        switch( ch ) {
        case 'a':
            column = 0;
            break;

        case 'b':
            column = 1;
            break;
        }

        return column;
    }

    public static void main(String [] args) 
	{
        try {
            finiteMachine fsm = new finiteMachine();
            fsm.run();
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}
