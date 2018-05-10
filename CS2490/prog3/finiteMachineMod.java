// Finite State Machine Mod
// Accepts strings over {a,b} that the number of 'a'%4= num of 'b'
// Rev. Taylor Rainwater, 9/23/15

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class finiteMachineMod
	{
		private final static int[][] STATE_TABLE = {
			{  1,  3,  4 },	// 0
			{  2,  0,  4 },	// 1
			{  3,  1,  4 },	// 2
			{  0,  2,  4 },	// 3
			{  4,  4,  4 }  // 4
    };

    private BufferedReader in;


    public finiteMachineMod()
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

            if (state == 0u) {
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
            finiteMachineMod fsm = new finiteMachineMod();
            fsm.run();
        } catch (IOException ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}
